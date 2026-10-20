;;;; agents.lisp — Claude and Codex behind one interface
;;;;
;;;; An AGENT takes one prompt at a time. AGENT-SUBMIT queues a prompt with a
;;;; function to call with the turn's final reply, and any files attached
;;;; to it; while a turn runs, later prompts wait their turn. What happens during a turn goes to the agent's
;;;; LISTENER as (listener agent kind &rest plist):
;;;;   :prompt TEXT          a prompt the agent is starting on
;;;;   :delta TEXT           more of a reply being written
;;;;   :text TEXT            a finished block of the reply
;;;;   :thinking TEXT
;;;;   :tool ID TITLE        a tool call (a command, an edit, a read…)
;;;;   :tool-result ID TEXT ERROR
;;;;   :approval ID TITLE TEXT  the agent asks to do something; answer
;;;;                         with AGENT-ANSWER-APPROVAL
;;;;   :approval-answered ID DECISION
;;;;   :turn-done TEXT ERROR the turn ended; TEXT is the final reply
;;;;   :note TEXT            something to tell the user (the CLI exited…)
;;;;   :busy BUSY            the agent started or stopped working
;;;;   :usage                the agent's usage changed (see AGENT-USAGE-WINDOWS)
;;;;
;;;; A turn that fails because the account is out of usage ends with ERROR
;;;; :out-of-usage, and the agent remembers until when (AGENT-OUT-UNTIL);
;;;; until then, when that is known, it turns prompts away without trying.
;;;; Everything is called through DELIVER, on the GUI thread in the app.
;;;;
;;;; The model, effort and permission can change at any time; a turn uses
;;;; the ones in force when it starts.
;;;;
;;;; A turn that goes quiet is noticed (AGENT-CHECK-STALL): no event for
;;;; a while, and nothing asked of the user. The user may keep waiting
;;;; (AGENT-KEEP-WAITING), have the turn interrupted and its prompt given
;;;; again once it has ended (AGENT-RETRY-TURN), or end it (AGENT-END-TURN).

(in-package #:agent-duet)

(defparameter *permissions* '(:ask :edits :full)
  ":ask — edit files in the directory; ask before running commands.
:edits — edit files in the directory; Claude runs no commands (but git's
read-only ones), Codex runs them in its sandbox without asking.
:full — no restrictions.")

(defclass agent ()
  ((id :initarg :id :reader agent-id :documentation ":claude or :codex")
   (name :initarg :name :reader agent-name)
   (partner-name :initarg :partner-name :accessor agent-partner-name)
   (directory :initarg :directory :initform nil :accessor agent-directory)
   (permission :initarg :permission :initform :ask :accessor agent-permission)
   (model :initarg :model :initform nil :accessor agent-model :documentation "Nil: the CLI's default")
   (effort :initarg :effort :initform nil :accessor agent-effort :documentation "Nil: the model's default")
   (deliver :initarg :deliver :initform #'funcall :accessor agent-deliver)
   (listener :initarg :listener :initform nil :accessor agent-listener)
   (busy :initform nil :accessor agent-busy)
   (queue :initform '() :accessor agent-queue :documentation "Waiting (prompt callback attachments)s, oldest first")
   (callback :initform nil :accessor agent-callback :documentation "The running turn's")
   (turn-images :initform '() :accessor agent-turn-images
                :documentation "The images (paths) to show the agent with the running turn's prompt")
   (turn-token :initform (list nil) :accessor agent-turn-token
               :documentation "(t) while the running turn may go on; AGENT-CANCEL makes it (nil)")
   (approvals :initform '() :accessor agent-approvals :documentation "Unanswered (id . answer-function)s")
   (stderr :initform '() :accessor agent-stderr :documentation "The last lines of standard error, newest first")
   (out-until :initform nil :accessor agent-out-until
              :documentation "When out of usage: the universal time it resets, or :unknown")
   (turn :initform nil :accessor agent-turn :documentation "The running turn's (prompt callback attachments)")
   (last-activity :initform (clock-seconds) :accessor agent-last-activity
                  :documentation "When the running turn last showed signs of life (CLOCK-SECONDS)")
   (stall-noticed :initform nil :accessor agent-stall-noticed
                  :documentation "True once AGENT-CHECK-STALL has reported this silence")
   (retry :initform nil :accessor agent-retry
          :documentation "AGENT-RETRY-TURN's progress: (:pending entry) until the turn is given again,
then :sent; :dropped if cancelled first")))

(defgeneric agent-usage-windows (agent)
  (:documentation "The account's usage WINDOWs, as far as known.")
  (:method ((agent agent)) '()))

(defun agent-out-of-usage-p (agent &optional (now (get-universal-time)))
  "Whether AGENT is known to be out of usage until a time still to come."
  (let ((until (agent-out-until agent)))
    (and (integerp until) (> until now))))

(defun agent-out-message (agent)
  (let ((until (agent-out-until agent)))
    (format nil "~a is out of usage~:[~*~; until ~a~]." (agent-name agent)
            (integerp until) (and (integerp until) (format-reset-time until)))))

(defun agent-ran-out (agent until)
  "AGENT's account is out of usage until UNTIL (a universal time, or nil if unknown)."
  (setf (agent-out-until agent) (or until :unknown))
  (agent-emit agent :usage))

(defun agent-has-usage (agent)
  "AGENT's account has usage again."
  (when (agent-out-until agent)
    (setf (agent-out-until agent) nil)
    (agent-emit agent :usage)))

(defgeneric agent-begin-turn (agent prompt)
  (:documentation "Start a turn on PROMPT. The turn ends with AGENT-TURN-FINISHED."))

(defgeneric agent-interrupt (agent)
  (:documentation "Ask the running turn to stop; it still ends with AGENT-TURN-FINISHED."))

(defgeneric agent-shutdown (agent)
  (:documentation "End the agent's process. The conversation resumes with the next prompt."))

(defgeneric agent-new-conversation (agent)
  (:documentation "End the agent's process and forget its conversation."))

(defgeneric agent-conversation-handle (agent)
  (:documentation "What resumes the agent's conversation (Claude's session, Codex's thread), or nil."))

(defgeneric agent-resume-conversation (agent handle)
  (:documentation "End the agent's process; its next turn carries on the conversation HANDLE
(from AGENT-CONVERSATION-HANDLE), or starts a new one when HANDLE is nil."))

(defmethod agent-interrupt ((agent agent)) nil)
(defmethod agent-shutdown ((agent agent)) nil)
(defmethod agent-new-conversation ((agent agent)) (agent-shutdown agent))
(defmethod agent-conversation-handle ((agent agent)) nil)

(defun agent-emit (agent kind &rest plist)
  (agent-touch agent)
  (when (agent-listener agent)
    (apply (agent-listener agent) agent kind plist)))

(defun agent-submit (agent prompt &optional callback attachments)
  "Have AGENT work on PROMPT, now or after the turns before it. CALLBACK
gets the final reply's text and whether the turn failed (then the text
says why). ATTACHMENTS are the paths of files given with PROMPT (see
PROMPT-WITH-ATTACHMENTS)."
  (setf (agent-queue agent) (append (agent-queue agent) (list (list prompt callback attachments))))
  (unless (agent-busy agent) (agent-next agent)))

(defgeneric agent-steer (agent prompt &optional images)
  (:documentation "Deliver PROMPT, with IMAGES (paths) attached, to the running turn without interrupting it."))

(defmethod agent-steer ((agent agent) prompt &optional images)
  (declare (ignore prompt images))
  (error "This agent does not support sending while working."))

(defun agent-send-now (agent prompt &optional attachments)
  "Send a side-pane message, with ATTACHMENTS (paths), steering if busy.
Preserve the running callback."
  (if (agent-busy agent)
      (progn
        (unless (car (agent-turn-token agent))
          (error "The agent is stopping; wait for it to finish."))
        (let ((text (prompt-with-attachments prompt attachments)))
          (agent-steer agent text (attachment-images attachments))
          (agent-emit agent :prompt :text text)))
      (agent-submit agent prompt nil attachments)))

(defun agent-next (agent)
  (let ((next (pop (agent-queue agent))))
    (when next
      (when (and (consp (agent-retry agent)) (eq next (second (agent-retry agent))))
        (setf (agent-retry agent) :sent))
      (destructuring-bind (prompt callback attachments) next
        (let ((text (prompt-with-attachments prompt attachments)))
          (setf (agent-busy agent) t
                (agent-turn agent) next
                (agent-stall-noticed agent) nil
                (agent-callback agent) callback
                (agent-turn-images agent) (attachment-images attachments)
                (agent-turn-token agent) (list t))
          (agent-emit agent :busy :busy t)
          (agent-emit agent :prompt :text text)
          (if (agent-out-of-usage-p agent)
              ;; Known to be out: don't spend a request finding out again.
              (agent-turn-finished agent (agent-out-message agent) :out-of-usage)
              (handler-case (agent-begin-turn agent text)
                (error (e) (agent-turn-finished agent (princ-to-string e) t)))))))))

(defun agent-turn-finished (agent text error)
  "The running turn ended with TEXT, its final reply, or with an ERROR (true;
:out-of-usage when the account ran out)."
  (unless error (agent-has-usage agent))
  (when (agent-busy agent)
    (let ((callback (agent-callback agent)))
      (setf (agent-busy agent) nil (agent-callback agent) nil (agent-turn agent) nil)
      (agent-deny-approvals agent)
      (agent-emit agent :turn-done :text text :error error)
      (agent-emit agent :busy :busy nil)
      (when callback
        (handler-case (funcall callback text error)
          (error (e) (agent-emit agent :note :text (format nil "Error after ~a's turn: ~a" (agent-name agent) e)))))
      (unless (agent-busy agent) (agent-next agent)))))

(defparameter *stopped-text* "Stopped by the user.")

(defun agent-cancel (agent)
  "Drop the waiting prompts and interrupt the running turn. Whoever is
waiting on them (a workflow) hears that they were stopped: told nothing,
it would wait for ever."
  (let ((waiting (mapcar #'second (agent-queue agent)))
        (running (and (agent-busy agent) (agent-callback agent))))
    (setf (agent-queue agent) '())
    (when (consp (agent-retry agent))
      (setf (agent-retry agent) :dropped))
    (setf (car (agent-turn-token agent)) nil)
    (agent-deny-approvals agent)
    (when (agent-busy agent)
      (setf (agent-callback agent) nil)
      (ignore-errors (agent-interrupt agent)))
    (dolist (callback (remove nil (cons running waiting)))
      (handler-case (funcall callback *stopped-text* t)
        (error (e) (agent-emit agent :note :text (format nil "Error after stopping ~a: ~a" (agent-name agent) e)))))))

;;; A turn gone quiet

(defvar *stall-minutes* 5 "How long a turn may go without an event before the user is asked about it.")

(defun clock-seconds ()
  "Seconds on a clock that only goes forward."
  (floor (get-internal-real-time) internal-time-units-per-second))

(defun agent-touch (agent)
  "AGENT showed signs of life: its silence starts again."
  (setf (agent-last-activity agent) (clock-seconds)
        (agent-stall-noticed agent) nil))

(defun agent-waiting-for-user-p (agent)
  "Whether AGENT's turn waits on the user: a permission or a question."
  (and (agent-approvals agent) t))

(defun agent-silence (agent &optional (now (clock-seconds)))
  "How long AGENT's running turn has gone without an event, in seconds; nil
when that doesn't count: no turn going on, one being stopped, or one
waiting on the user. Plan approval and usage pauses come between turns."
  (when (and (agent-busy agent) (car (agent-turn-token agent))
             (not (agent-waiting-for-user-p agent)))
    (max 0 (- now (agent-last-activity agent)))))

(defun agent-check-stall (agent &optional (limit (* 60 *stall-minutes*)) (now (clock-seconds)))
  "True, once for each silence, when AGENT's turn has gone LIMIT seconds
without an event (AGENT-SILENCE)."
  (let ((silence (agent-silence agent now)))
    (when (and silence (>= silence limit) (not (agent-stall-noticed agent)))
      (setf (agent-stall-noticed agent) t))))

(defun agent-keep-waiting (agent)
  "Give AGENT's quiet turn another while."
  (agent-touch agent))

(defun agent-retry-turn (agent)
  "Interrupt AGENT's running turn and give its prompt again, to whoever was
waiting on it, once that turn has ended: the turn's token is cleared now,
and the prompt goes first in the queue, so it starts only when the
interrupted turn is over. True if there was a turn to retry. Watch
AGENT-RETRY for when it is sent."
  (let ((entry (agent-turn agent)))
    (when (and (agent-busy agent) (car (agent-turn-token agent)) entry)
      (setf (car (agent-turn-token agent)) nil
            (agent-callback agent) nil
            (agent-retry agent) (list :pending entry))
      (push entry (agent-queue agent))
      (agent-deny-approvals agent)
      (ignore-errors (agent-interrupt agent))
      t)))

(defun agent-retry-pending-p (agent)
  (consp (agent-retry agent)))

(defun agent-end-turn (agent)
  "End AGENT's turn and what waits behind it, even if it doesn't answer an
interrupt: its process goes (the conversation carries on with the next
prompt)."
  (agent-cancel agent)
  (agent-shutdown agent)
  (when (agent-busy agent)
    (agent-turn-finished agent *stopped-text* t)))

;;; Approvals

(defun agent-ask-approval (agent id title text answer)
  "Ask the user whether AGENT may do TITLE. ANSWER gets the decision:
:allow, :always (for the rest of the conversation) or :deny."
  (push (cons id answer) (agent-approvals agent))
  (agent-emit agent :approval :id id :title title :text text))

(defun agent-ask-question (agent id questions answer)
  "Ask the user AGENT's QUESTIONS (AskUserQuestion's, a list of JSON objects
with question, header, options and multiSelect). ANSWER gets :deny, or
(:answers . ALIST) mapping each question's text to the chosen labels, joined
with \", \" when several were chosen."
  (push (cons id answer) (agent-approvals agent))
  (agent-emit agent :question :id id :questions questions))

(defun agent-answer-approval (agent id decision)
  "Answer AGENT's approval request ID with DECISION (see AGENT-ASK-APPROVAL)."
  (let ((entry (assoc id (agent-approvals agent) :test #'equal)))
    (when entry
      (setf (agent-approvals agent) (remove entry (agent-approvals agent)))
      (handler-case (funcall (cdr entry) decision)
        (error (e) (agent-emit agent :note :text (format nil "Couldn't answer ~a: ~a" (agent-name agent) e))))
      (agent-emit agent :approval-answered :id id :decision decision))))

(defun agent-deny-approvals (agent)
  (dolist (entry (agent-approvals agent))
    (agent-answer-approval agent (car entry) :deny)))

;;; Attachments: files dropped on a message box

(defparameter *attached-image-limit* (* 3 1024 1024)
  "The largest image file shown to an agent, in bytes. Larger ones are sent
by their path only (Claude takes at most about 5 MB of base64).")

(defun attachment-images (attachments)
  "Those of ATTACHMENTS (paths) the agents can be shown as images."
  (remove-if-not (lambda (path)
                   (and (image-media-type path)
                        (let ((size (ignore-errors
                                     (with-open-file (in path :element-type '(unsigned-byte 8))
                                       (file-length in)))))
                          (and size (<= size *attached-image-limit*)))))
                 attachments))

(defun shell-quote (string)
  "STRING as one word for a shell: as it is when every character is safe
there, otherwise in single quotes (in which a single quote is '\\'')."
  (if (and (plusp (length string))
           (every (lambda (c) (or (alphanumericp c) (find c "/._-+,:@%="))) string))
      string
      (format nil "'~{~a~^'\\''~}'" (uiop:split-string string :separator "'"))))

(defun prompt-with-attachments (prompt attachments)
  "PROMPT, followed by the paths of the files attached to it. Images are
also shown to the agent (ATTACHMENT-IMAGES); other files it reads itself."
  (if attachments
      (format nil "~a~%~%Attached file~p:~%~{- ~a~%~}" prompt (length attachments)
              (mapcar #'uiop:native-namestring attachments))
      prompt))

;;; Small things

(defun agent-note-stderr (agent line)
  (push line (agent-stderr agent))
  (when (> (length (agent-stderr agent)) 40)
    (setf (agent-stderr agent) (subseq (agent-stderr agent) 0 40))))

(defun agent-recent-stderr (agent &optional (n 5))
  (format nil "~{~a~^~%~}" (reverse (subseq (agent-stderr agent) 0 (min n (length (agent-stderr agent)))))))

(defun make-deliver-thunk (agent)
  (let ((deliver (agent-deliver agent)))
    (lambda (thunk) (funcall deliver thunk))))

;;; Shared project notes: each agent reads its own notes file (Claude
;;; CLAUDE.md, Codex AGENTS.md), and is given the other's in its
;;; instructions, so both follow the same conventions. They are read when
;;; the agent's process starts: a new conversation, or another folder.

(defvar *shared-notes* t "Whether the agents' instructions include the folder's notes for the other agent.")

(defparameter *shared-notes-limit* 20000 "The most of a notes file put in the instructions, in characters.")

(defun shared-notes-file (agent)
  "The notes file AGENT doesn't read by itself: the other agent's."
  (if (eq (agent-id agent) :claude) "AGENTS.md" "CLAUDE.md"))

(defun shared-notes (agent)
  "The text of the notes file in AGENT's folder that AGENT doesn't read by
itself, cut to *SHARED-NOTES-LIMIT*; nil when off, missing or empty."
  (let* ((directory (agent-directory agent))
         (file (and *shared-notes* directory
                    (probe-file (merge-pathnames (shared-notes-file agent)
                                                 (uiop:ensure-directory-pathname directory)))))
         (text (and file (not (uiop:directory-pathname-p file))
                    (ignore-errors (uiop:read-file-string file :external-format '(:utf-8 :replacement #\?))))))
    (when (and text (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
      (if (> (length text) *shared-notes-limit*)
          (format nil "~a~%~%[… cut: the file is ~:d characters long; read it for the rest.]"
                  (subseq text 0 *shared-notes-limit*) (length text))
          text))))

(defun collaboration-instructions (agent)
  (let ((partner (agent-partner-name agent))
        (notes (shared-notes agent)))
    (format nil "You are ~a, one of two coding agents (the other is ~a) working together on the user's ~
request in the same working directory. Prompts may come from the user directly or be relayed from ~a: ~
they may ask you to implement something, to review ~a's work, or to address its review of yours. ~
When you review, inspect the files themselves (and `git diff` if the directory is a git repository) ~
rather than trusting a summary, and don't change files unless asked to. When you implement, finish your ~
reply with a short summary of what you changed and in which files, since that summary is what ~a sees. ~
Running a command may need the user's approval; if one is denied, carry on without it and say so.~
~@[~%~%The project's notes for coding agents, from its `~a` (written for ~a, but they apply to you too):~
~%~%~a~]"
            (agent-name agent) partner partner partner partner
            (and notes (shared-notes-file agent)) partner notes)))

;;; Claude

(defparameter *claude-review-tools*
  '("Bash(git diff:*)" "Bash(git status:*)" "Bash(git log:*)" "Bash(git show:*)" "Bash(ls:*)")
  "Commands Claude may run without asking: enough to review.")

(defparameter *claude-efforts* '("low" "medium" "high" "xhigh" "max"))

(defclass claude-agent (agent)
  ((process :initform nil :accessor claude-agent-process)
   (config :initform nil :accessor claude-agent-config :documentation "What the process was started with")
   (session-id :initform nil :accessor claude-agent-session-id)
   (started :initform nil :accessor claude-agent-started :documentation "Whether the session exists, to resume")
   (limits :initform nil :accessor claude-agent-limits :documentation "The last rate_limit_info")
   (prompts :initform '() :accessor claude-agent-prompts
            :documentation "The running turn's prompt and what was sent while it ran, to send again:
(text . images)s")
   (sender :initform nil :accessor claude-agent-sender
           :documentation "The thread writing the last message sent in the background, while it runs")
   (limit-hit :initform nil :accessor claude-agent-limit-hit
              :documentation "Set when this turn's rate_limit_event said rejected: its reset, or :unknown"))
  (:default-initargs :id :claude :name "Claude" :partner-name "ChatGPT" :model *claude-model*))

(defun claude-agent-wanted-config (agent)
  (list (agent-model agent) (agent-effort agent) (agent-permission agent) (agent-directory agent)
        *shared-notes*))

(defun claude-agent-ensure-process (agent)
  "The agent's CLI process, started (or restarted, resuming the conversation)
when it isn't running with the current settings."
  (let ((cp (claude-agent-process agent))
        (config (claude-agent-wanted-config agent)))
    (when (and (claude-alive-p cp) (not (equal config (claude-agent-config agent))))
      (setf (claude-agent-process agent) nil)
      (stop-claude cp)
      (setf cp nil))
    (if (claude-alive-p cp)
        cp
        (let* ((resume (and (claude-agent-started agent) (claude-agent-session-id agent)))
               (session (or (claude-agent-session-id agent)
                            (setf (claude-agent-session-id agent) (make-uuid))))
               (permission (agent-permission agent))
               (arguments (claude-arguments
                           :session-id (unless resume session) :resume resume
                           :model (agent-model agent) :effort (agent-effort agent)
                           :permission-mode (if (eq permission :full) "bypassPermissions" "acceptEdits")
                           ;; Always, so AskUserQuestion reaches the user (see CLAUDE-AGENT-EVENT).
                           :permission-tool "stdio"
                           :allowed-tools (unless (eq permission :full) *claude-review-tools*)
                           :system-prompt (collaboration-instructions agent)))
               (new nil))
          (setf new (start-claude arguments
                                  :directory (agent-directory agent)
                                  :deliver (make-deliver-thunk agent)
                                  :on-event (lambda (event) (claude-agent-event agent new event))
                                  :on-stderr (lambda (line) (agent-note-stderr agent line))
                                  :on-exit (lambda (code) (claude-agent-exited agent new code))))
          (setf (claude-agent-process agent) new
                (claude-agent-config agent) config
                (claude-agent-started agent) t)
          new))))

(defun claude-tool-title (name input)
  (let ((detail (and (jobject-p input)
                     (let ((v (or (jget input "command") (jget input "file_path") (jget input "pattern")
                                  (jget input "path") (jget input "url") (jget input "description"))))
                       (and (stringp v) (substitute #\Space #\Newline (if (> (length v) 120) (subseq v 0 120) v)))))))
    (format nil "~a~@[  ~a~]" name detail)))

(defun claude-approval-title (name input)
  (let ((command (and (jobject-p input) (jget input "command"))))
    (if (stringp command)
        (format nil "$ ~a" command)
        (claude-tool-title name input))))

(defun question-answer-input (input answers)
  "AskUserQuestion's INPUT with the user's ANSWERS (question text → answer)."
  (cons :object (append (remove "answers" (cdr input) :key #'car :test #'equal)
                        (list (cons "answers" (cons :object answers))))))

(defun claude-agent-event (agent cp event)
  ;; A subagent's events aren't shown, but they are signs of life.
  (when (eq cp (claude-agent-process agent)) (agent-touch agent))
  (when (and (eq cp (claude-agent-process agent)) (not (event-subagent event)))
    (case (event-kind event)
      (:init (when (event-session-id event)
               (setf (claude-agent-session-id agent) (event-session-id event))))
      (:text-delta (agent-emit agent :delta :text (event-text event)))
      (:text (agent-emit agent :text :text (event-text event)))
      (:thinking (agent-emit agent :thinking :text (event-text event)))
      (:tool-start (agent-emit agent :tool :id (event-id event) :title (event-name event)))
      (:tool-use (agent-emit agent :tool :id (event-id event)
                             :title (claude-tool-title (event-name event) (event-input event))))
      (:tool-result (agent-emit agent :tool-result :id (event-id event) :text (event-text event)
                                :error (event-error event)))
      (:permission
       (let ((id (event-id event)) (input (event-input event))
             (suggestions (jlist (jget (event-data event) "permission_suggestions"))))
         (cond
          ((equal (event-name event) "AskUserQuestion")
           (agent-ask-question agent id (jlist (jget input "questions"))
                               (lambda (decision)
                                 (when (claude-alive-p cp)
                                   (if (consp decision)
                                       (claude-answer-permission cp id t
                                                                 :input (question-answer-input input (cdr decision)))
                                       (claude-answer-permission cp id nil
                                                                 :message "The user declined to answer."))))))
          ;; The CLI asks here only so questions reach the user; outside Ask
          ;; mode, anything else needing approval is refused as before.
          ((not (eq (third (claude-agent-config agent)) :ask))
           (claude-answer-permission cp id nil :message "Permission to use this tool hasn't been granted."))
          (t
           (agent-ask-approval agent id (claude-approval-title (event-name event) input) (event-text event)
                               (lambda (decision)
                                 (when (claude-alive-p cp)
                                   (claude-answer-permission cp id (not (eq decision :deny))
                                                             :input input
                                                             :always-rules (and (eq decision :always)
                                                                                suggestions)))))))))
      (:rate-limit
       (let ((info (event-data event)))
         (setf (claude-agent-limits agent) info)
         (if (equal (jget info "status") "rejected")
             (let ((reset (or (unix-to-universal (jget info "resetsAt"))
                              (exhausted-reset (claude-usage-windows info)))))
               (setf (claude-agent-limit-hit agent) (or reset :unknown))
               (agent-ran-out agent reset))
             (progn (agent-has-usage agent) (agent-emit agent :usage)))))
      (:result
       (if (and (event-error event) (search "No conversation found" (event-text event)))
           (claude-agent-resume-failed agent cp (event-text event))
           (claude-agent-result agent event))))))

(defun claude-agent-resume-failed (agent cp why)
  "The CLI hasn't the session it was to resume (deleted, or made in another
folder): carry on the turn in a new conversation."
  (setf (claude-agent-session-id agent) nil (claude-agent-started agent) nil
        (claude-agent-process agent) nil)
  (stop-claude cp)
  (agent-emit agent :note :text (format nil "Couldn't resume Claude's conversation (~a). Carrying on in a new one." why))
  (when (agent-busy agent)
    (let ((prompts (claude-agent-prompts agent)))
      (cond ((not (car (agent-turn-token agent))) (agent-turn-finished agent *stopped-text* t))
            ((null prompts) (agent-turn-finished agent why t))
            ;; The prompt, then what was sent while it ran, in order.
            (t (handler-case (let ((new (claude-agent-ensure-process agent)))
                               (loop for (prompt . images) in prompts
                                     do (claude-agent-send agent new prompt images
                                                           :on-error (turn-failure agent))))
                 (error (e) (agent-turn-finished agent (princ-to-string e) t))))))))

(defun claude-agent-result (agent event)
  ;; Out of usage only on this turn's evidence (its rate_limit_event said
  ;; rejected, or its error says so), never on what an earlier turn left
  ;; behind: a later sign-in failure is a sign-in failure.
  (let* ((text (event-text event))
         (error (event-error event))
         (hit (shiftf (claude-agent-limit-hit agent) nil)))
    (when (and error (or hit (claude-limit-text-p text)))
      ;; This turn's reset time, replacing any earlier one.
      (agent-ran-out agent (or (claude-limit-text-reset text) (and (integerp hit) hit)))
      (setf error :out-of-usage))
    ;; Streaming input can leave another response queued in the CLI.
    ;; Keep the workflow callback until that response has completed.
    (unless (and (not error) (plusp (or (event-queued-turn-count event) 0)))
      (agent-turn-finished agent text error))))

(defmethod agent-usage-windows ((agent claude-agent))
  (claude-usage-windows (claude-agent-limits agent)))

(defun claude-agent-exited (agent cp code)
  (when (eq cp (claude-agent-process agent))
    (setf (claude-agent-process agent) nil)
    (when (agent-busy agent)
      (let ((why (format nil "Claude Code exited (status ~a).~@[~%~a~]" code
                         (let ((s (agent-recent-stderr agent))) (and (plusp (length s)) s)))))
        (unless (cp-stopped cp) (agent-emit agent :note :text why))
        (agent-turn-finished agent why t)))))

(defun claude-agent-send (agent cp text images &key on-error)
  "Send TEXT, with IMAGES, to CP, after what was sent before. A message with
images is read, encoded and written on another thread, since a big one
could block the GUI thread while the CLI isn't reading; so is anything sent
while that is still going on, to keep the order. A message sent in the
background is dropped if the turn it was for is stopped (AGENT-CANCEL) or
over before it is written. Then ON-ERROR, if the write fails, gets why
through DELIVER; otherwise the error is signalled."
  (let ((previous (claude-agent-sender agent)))
    (if (and (null images) (not (claude-agent-sending-p agent)))
        (claude-send cp text)
        (let ((deliver (agent-deliver agent))
              (token (agent-turn-token agent)))
          (setf (claude-agent-sender agent)
                (sb-thread:make-thread
                 (lambda ()
                   (when previous (sb-thread:join-thread previous :default nil))
                   (handler-case (when (car token)
                                   (claude-send cp text images
                                                (lambda () (and (car token) (eq token (agent-turn-token agent))))))
                     (error (e)
                       (let ((why (princ-to-string e)))
                         (when on-error (funcall deliver (lambda () (funcall on-error why))))))))
                 :name "claude send"))))))

(defun claude-agent-sending-p (agent)
  "Whether a message is still being sent in the background."
  (let ((sender (claude-agent-sender agent)))
    (and sender (sb-thread:thread-alive-p sender))))

(defun turn-failure (agent)
  "A function ending AGENT's running turn as failed with the text it's given,
unless that turn is over by then."
  (let ((token (agent-turn-token agent)))
    (lambda (why)
      (when (eq token (agent-turn-token agent))
        (agent-turn-finished agent why t)))))

(defmethod agent-begin-turn ((agent claude-agent) prompt)
  (let ((images (agent-turn-images agent)))
    (setf (claude-agent-prompts agent) (list (cons prompt images)))
    (claude-agent-send agent (claude-agent-ensure-process agent) prompt images
                       :on-error (turn-failure agent))))

(defmethod agent-steer ((agent claude-agent) prompt &optional images)
  ;; Use the current process: changing settings must not restart a busy CLI.
  (let ((cp (claude-agent-process agent)))
    (unless (claude-alive-p cp) (error "Claude is not ready to receive a message yet."))
    (claude-agent-send agent cp prompt images
                       :on-error (lambda (why)
                                   (agent-emit agent :note
                                               :text (format nil "Could not send while working: ~a~%Message: ~a"
                                                             why prompt))))
    (setf (claude-agent-prompts agent) (append (claude-agent-prompts agent) (list (cons prompt images))))))

(defmethod agent-interrupt ((agent claude-agent))
  (let ((cp (claude-agent-process agent)))
    (when (claude-alive-p cp)
      (if (claude-agent-sending-p agent)
          ;; The input is busy with a message the CLI may be slow to take:
          ;; don't wait for it here. Queued ones see the turn stopped.
          (sb-thread:make-thread (lambda () (claude-interrupt cp)) :name "claude interrupt")
          (claude-interrupt cp)))))

(defmethod agent-shutdown ((agent claude-agent))
  (setf (car (agent-turn-token agent)) nil)
  (let ((cp (claude-agent-process agent)))
    (setf (claude-agent-process agent) nil)
    (agent-deny-approvals agent)
    (when cp (stop-claude cp))
    (when (agent-busy agent) (agent-turn-finished agent "Stopped." t))))

(defmethod agent-new-conversation ((agent claude-agent))
  (agent-shutdown agent)
  (setf (claude-agent-session-id agent) nil (claude-agent-started agent) nil))

(defmethod agent-conversation-handle ((agent claude-agent))
  (and (claude-agent-started agent) (claude-agent-session-id agent)))

(defmethod agent-resume-conversation ((agent claude-agent) handle)
  (agent-new-conversation agent)
  (when handle
    (setf (claude-agent-session-id agent) handle (claude-agent-started agent) t)))

;;; Codex

(defclass codex-agent (agent)
  ((server :initform nil :accessor codex-agent-server)
   (config :initform nil :accessor codex-agent-config :documentation "What the server was started with")
   (program :initarg :program :initform nil :accessor codex-agent-program)
   (thread-id :initform nil :accessor codex-agent-thread-id)
   (last-text :initform nil :accessor codex-agent-last-text :documentation "The turn's last agent message")
   (models :initform '() :accessor codex-agent-models :documentation "From model/list")
   (usage :initform nil :accessor agent-usage :documentation "Rate-limit buckets, (id . snapshot)s"))
  (:default-initargs :id :codex :name "ChatGPT" :partner-name "Claude" :model *codex-model*))

(defun codex-agent-sandbox (agent)
  (if (eq (agent-permission agent) :full) "danger-full-access" "workspace-write"))

(defun codex-agent-approval-policy (agent)
  (if (eq (agent-permission agent) :ask) "untrusted" "never"))

(defun codex-agent-ensure-server (agent)
  "The agent's app-server with its thread started, (re)started when it isn't
running in the current directory and sandbox. Blocks: call it off the GUI thread."
  (let ((server (codex-agent-server agent))
        (config (list (codex-agent-sandbox agent) (agent-directory agent) *shared-notes*)))
    (when (and (codex-alive-p server) (not (equal config (codex-agent-config agent))))
      (setf (codex-agent-server agent) nil)
      (stop-codex-app-server server)
      (setf server nil))
    (unless (codex-alive-p server)
      (let ((program (or (codex-agent-program agent) (find-codex-program)
                         (error "The Codex CLI was not found")))
            (new nil))
        (setf new (start-codex-app-server program (or (agent-directory agent) (uiop:getcwd))
                                          :deliver (make-deliver-thunk agent)
                                          :on-event (lambda (event) (codex-agent-event agent new event))
                                          :on-exit (lambda (code) (codex-agent-exited agent new code))))
        (setf server new
              (codex-agent-server agent) new
              (codex-agent-config agent) config)
        (codex-agent-open-thread agent server)))
    server))

(defun codex-agent-open-thread (agent server)
  "Start or resume the agent's thread on the new SERVER. If that fails, the
server is stopped and forgotten, so the next turn starts again from scratch
rather than using a server with no thread."
  (let ((ready nil))
    (unwind-protect
        (progn
          (codex-agent-setup-thread agent server)
          (setf ready t))
      (unless ready
        (when (eq server (codex-agent-server agent))
          (setf (codex-agent-server agent) nil))
        (stop-codex-app-server server)))))

(defun codex-agent-setup-thread (agent server)
  (let ((instructions (collaboration-instructions agent)))
    (when (codex-agent-thread-id agent)
      (codex-initialize-app-server server)
      ;; A thread Codex no longer has (deleted, or from an old history):
      ;; say so and start a new one, rather than failing every turn.
      (handler-case
          (progn
            (codex-request server "thread/resume"
                           (jobj "threadId" (codex-agent-thread-id agent)
                                 "cwd" (uiop:native-namestring (codex-directory server))
                                 "sandbox" (codex-agent-sandbox agent)
                                 "approvalPolicy" (codex-agent-approval-policy agent)
                                 "developerInstructions" instructions))
            (setf (codex-thread-id server) (codex-agent-thread-id agent)))
        (codex-request-error (e)
          (let ((why (format nil "Couldn't resume ChatGPT's conversation (~a). Starting a new one." e)))
            (setf (codex-agent-thread-id agent) nil)
            (funcall (agent-deliver agent) (lambda () (agent-emit agent :note :text why)))))))
    (unless (codex-agent-thread-id agent)
      (setf (codex-agent-thread-id agent)
            (codex-start-thread server :model (agent-model agent) :instructions instructions
                                :sandbox (codex-agent-sandbox agent)
                                :approval-policy (codex-agent-approval-policy agent))))))

(defun codex-agent-event (agent server event)
  (when (eq server (codex-agent-server agent))
    (agent-touch agent)
    (case (codex-event-kind event)
      (:text-delta (agent-emit agent :delta :text (codex-event-text event)))
      (:text (setf (codex-agent-last-text agent) (codex-event-text event))
       (agent-emit agent :text :text (codex-event-text event)))
      (:activity (agent-emit agent :tool :id (codex-event-id event) :title (codex-event-text event)))
      (:activity-completed (agent-emit agent :tool :id (codex-event-id event) :title (codex-event-name event))
       (agent-emit agent :tool-result :id (codex-event-id event) :text (codex-event-text event)
                   :error (codex-event-error event)))
      (:approval
       (let ((id (codex-event-id event)) (params (codex-event-data event)))
         (agent-ask-approval agent id (codex-event-text event)
                             (let ((reason (jget params "reason"))) (and (stringp reason) reason))
                             (lambda (decision)
                               (when (codex-alive-p server)
                                 (codex-answer-approval server id decision))))))
      (:rate-limits (setf (agent-usage agent) (merge-rate-limits (agent-usage agent) (codex-event-data event)))
       (codex-agent-usage-changed agent))
      ((:stderr :protocol-error) (agent-note-stderr agent (codex-event-text event)))
      (:result
       (let ((error (codex-event-error event)))
         (when (and error (codex-limit-error-p (codex-event-data event)))
           (agent-ran-out agent (exhausted-reset (codex-usage-windows (agent-usage agent))))
           (setf error :out-of-usage))
         (agent-turn-finished agent (if error
                                        (or (codex-event-text event) "The turn failed.")
                                        (or (codex-agent-last-text agent) ""))
                              error))
       (codex-agent-refresh-usage agent)))))

(defun codex-agent-usage-changed (agent)
  "New usage figures: if the agent is out with no reset time yet, take it from them."
  (when (eq (agent-out-until agent) :unknown)
    (let ((reset (exhausted-reset (codex-usage-windows (agent-usage agent)))))
      (when reset (setf (agent-out-until agent) reset))))
  (agent-emit agent :usage))

(defmethod agent-usage-windows ((agent codex-agent))
  (codex-usage-windows (agent-usage agent)))

(defun codex-agent-exited (agent server code)
  (when (eq server (codex-agent-server agent))
    (setf (codex-agent-server agent) nil)
    (agent-deny-approvals agent)
    (when (agent-busy agent)
      (let ((why (format nil "Codex exited (status ~a).~@[~%~a~]" code
                         (let ((s (agent-recent-stderr agent))) (and (plusp (length s)) s)))))
        (agent-emit agent :note :text why)
        (agent-turn-finished agent why t)))))

(defmethod agent-begin-turn ((agent codex-agent) prompt)
  (setf (codex-agent-last-text agent) nil)
  (let ((deliver (agent-deliver agent))
        (model (agent-model agent))
        (effort (agent-effort agent))
        (policy (codex-agent-approval-policy agent))
        (images (agent-turn-images agent))
        (token (agent-turn-token agent)))
    ;; Starting the app-server and the turn wait for replies: not on the GUI
    ;; thread. A Stop meanwhile finds no turn to interrupt, so the token is
    ;; checked before the turn starts, and again after, in case Stop came
    ;; while it was starting.
    (flet ((stopped ()
             (funcall deliver (lambda () (when (eq token (agent-turn-token agent))
                                           (agent-turn-finished agent *stopped-text* t))))))
      (sb-thread:make-thread
       (lambda ()
         (handler-case
             (let ((server (codex-agent-ensure-server agent)))
               (if (not (car token))
                   (stopped)
                   (let ((turn-id (jget (codex-start-turn server prompt :model model :effort effort
                                                          :approval-policy policy :images images)
                                        "turn" "id")))
                     (unless (car token)
                       (ignore-errors (codex-interrupt server :turn-id turn-id))))))
           (error (e)
                  (let ((why (princ-to-string e)))
                    (funcall deliver (lambda ()
                                       (when (eq token (agent-turn-token agent))
                                         (agent-turn-finished agent why t))))))))
       :name "codex turn"))))

(defmethod agent-steer ((agent codex-agent) prompt &optional images)
  (let* ((server (codex-agent-server agent))
         (turn-id (and server (codex-turn-id server)))
         (thread-id (and server (codex-thread-id server)))
         (deliver (agent-deliver agent)))
    (unless (and (codex-alive-p server) turn-id thread-id)
      (error "Codex is still starting; send again when it begins responding."))
    ;; JSON-RPC waits for acknowledgement, so keep it off the GUI thread.
    ;; Capture both IDs: never steer a later workflow turn by accident.
    (sb-thread:make-thread
     (lambda ()
       (handler-case (codex-steer-turn server prompt :thread-id thread-id :turn-id turn-id :images images)
         (error (e)
           (let ((why (format nil "Could not send while working: ~a~%Message: ~a" e prompt)))
             (funcall deliver (lambda () (agent-emit agent :note :text why)))))))
     :name "codex steer")))

(defmethod agent-interrupt ((agent codex-agent))
  (let ((server (codex-agent-server agent)))
    (when (codex-alive-p server)
      (sb-thread:make-thread (lambda () (ignore-errors (codex-interrupt server))) :name "codex interrupt"))))

(defmethod agent-shutdown ((agent codex-agent))
  (setf (car (agent-turn-token agent)) nil)
  (let ((server (codex-agent-server agent)))
    (setf (codex-agent-server agent) nil)
    (agent-deny-approvals agent)
    (when server (stop-codex-app-server server))
    (when (agent-busy agent) (agent-turn-finished agent "Stopped." t))))

(defmethod agent-new-conversation ((agent codex-agent))
  (agent-shutdown agent)
  (setf (codex-agent-thread-id agent) nil))

(defmethod agent-conversation-handle ((agent codex-agent))
  (codex-agent-thread-id agent))

(defmethod agent-resume-conversation ((agent codex-agent) handle)
  (agent-new-conversation agent)
  (setf (codex-agent-thread-id agent) handle))

(defun codex-agent-refresh-usage (agent)
  "Read the account's rate limits again, in the background."
  (let ((server (codex-agent-server agent))
        (deliver (agent-deliver agent)))
    (when (codex-alive-p server)
      (sb-thread:make-thread
       (lambda ()
         (let ((usage (ignore-errors (codex-read-rate-limits server))))
           (when usage
             (funcall deliver (lambda ()
                                (setf (agent-usage agent) usage)
                                (codex-agent-usage-changed agent))))))
       :name "codex usage"))))

(defun codex-agent-load-catalog (agent program)
  "Fetch the models and the usage with a short-lived app-server. Blocks:
call it off the GUI thread. Returns the models and the usage."
  (call-with-codex-server program (or (agent-directory agent) (user-homedir-pathname))
                          (lambda (server)
                            (values (ignore-errors (codex-list-models server))
                                    (ignore-errors (codex-read-rate-limits server))))))

(defun codex-model-id (model)
  (or (jget model "id") (jget model "model")))
