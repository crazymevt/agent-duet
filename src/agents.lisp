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
:full — no restrictions.

AGENT-READ-ONLY-P overrides all three while it is set (see below).")

;;; Read-only (review-only) turns: with AGENT-READ-ONLY-P set, whatever the
;;; permission, Claude runs without its edit tools and may only run
;;; read-only commands (READ-ONLY-COMMAND-P); Codex runs in its read-only
;;; sandbox, never asking to do more. Like the other settings it applies
;;; from the next turn on: that turn restarts the CLI (resuming the
;;; conversation) if it was started otherwise. Sending to a running turn
;;; whose CLI was started otherwise is refused.

(defclass agent ()
  ((context-tokens :initform nil :accessor agent-context-tokens)
   (context-limit :initform nil :accessor agent-context-limit)
   (metrics :initform nil :accessor agent-metrics)
   (metrics-id :initform nil :accessor agent-metrics-id)
   (metrics-start :initform nil :accessor agent-metrics-start)
   (metrics-reporter :initform nil :accessor agent-metrics-reporter)
   (metrics-seen :initform nil :accessor agent-metrics-seen)
   (metrics-missing :initform nil :accessor agent-metrics-missing)
   (id :initarg :id :reader agent-id :documentation ":claude or :codex")
   (name :initarg :name :reader agent-name)
   (partner-name :initarg :partner-name :accessor agent-partner-name)
   (directory :initarg :directory :initform nil :accessor agent-directory)
   (permission :initarg :permission :initform :ask :accessor agent-permission)
   (read-only :initarg :read-only :initform nil :accessor agent-read-only-p
              :documentation "True: no changing files, whatever the permission (a review)")
   (model :initarg :model :initform nil :accessor agent-model :documentation "Nil: the CLI's default")
   (effort :initarg :effort :initform nil :accessor agent-effort :documentation "Nil: the model's default")
   (deliver :initarg :deliver :initform #'funcall :accessor agent-deliver)
   (listener :initarg :listener :initform nil :accessor agent-listener)
   (busy :initform nil :accessor agent-busy)
   (queue :initform '() :accessor agent-queue :documentation "Waiting (prompt callback attachments)s, oldest first")
   (context-fallback :initform nil :accessor agent-context-fallback
                     :documentation "Recent (compact . full) prompts, newest first, for lost history")
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
   (partial-text :initform "" :accessor agent-partial-text)
   (editing-processes :initform nil :accessor agent-editing-processes)
   (process-record :initform nil :accessor agent-process-record)
   (turn-process-baseline :initform :unknown :accessor agent-turn-process-baseline)
   (retiring-records :initform nil :accessor agent-retiring-records)
   (editing-pids :initform nil :accessor agent-editing-pids)
   (stopping-processes :initform nil :accessor agent-stopping-processes)
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
  (when (and (member kind '(:delta :text)) (stringp (getf plist :text)))
    (setf (agent-partial-text agent)
          (concatenate 'string (agent-partial-text agent) (getf plist :text))))
  (agent-touch agent)
  (when (agent-listener agent)
    (apply (agent-listener agent) agent kind plist)))

(defun agent-submit (agent prompt &optional callback attachments metrics-reporter)
  "Have AGENT work on PROMPT, now or after the turns before it. CALLBACK
gets the final reply's text and whether the turn failed (then the text
says why). ATTACHMENTS are the paths of files given with PROMPT (see
PROMPT-WITH-ATTACHMENTS). METRICS-REPORTER, when supplied, receives a unique
turn ID and a metrics plist once, before CALLBACK (or at cancellation)."
  (setf (agent-queue agent) (append (agent-queue agent) (list (list prompt callback attachments metrics-reporter))))
  (unless (agent-busy agent) (agent-next agent)))

(defgeneric agent-steer (agent prompt &optional images)
  (:documentation "Deliver PROMPT, with IMAGES (paths) attached, to the running turn without interrupting it."))

(defmethod agent-steer ((agent agent) prompt &optional images)
  (declare (ignore prompt images))
  (error "This agent does not support sending while working."))

(defun agent-context-summary (agent)
  (if (agent-context-tokens agent)
      (format nil "Context~a: ~:d~@[ / ~:d~] tokens"
              (if (typep agent 'claude-agent) " ≈" "")
              (agent-context-tokens agent) (agent-context-limit agent))
      "Context: unknown"))

(defmethod agent-new-conversation :after ((agent agent))
  (setf (agent-context-tokens agent) nil (agent-context-limit agent) nil)
  (agent-emit agent :usage))

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
      (destructuring-bind (prompt callback attachments &optional metrics-reporter) next
        (let ((text (prompt-with-attachments prompt attachments)))
          (setf (agent-metrics agent) nil
                (agent-metrics-seen agent) nil
                (agent-metrics-missing agent) nil
                (agent-metrics-id agent) (make-uuid)
                (agent-metrics-start agent) (clock-seconds)
                (agent-metrics-reporter agent) metrics-reporter
                (agent-partial-text agent) ""
                (agent-busy agent) t
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

(defun metric-number (value)
  (and (realp value) (<= 0 value) value))

(defun agent-add-metrics (agent metrics &key id replace)
  "Accept each indexed result once; Codex snapshots replace rather than add."
  (when (and (agent-busy agent) (agent-metrics-start agent)
             (or (null id) (not (member id (agent-metrics-seen agent) :test #'equal))))
    (when id (push id (agent-metrics-seen agent)))
    (loop for (key value) on metrics by #'cddr
          for number = (metric-number value)
          do (cond (replace
                    (if number (setf (getf (agent-metrics agent) key) number)
                        (remf (agent-metrics agent) key)))
                   ((null number)
                    (pushnew key (agent-metrics-missing agent))
                    (remf (agent-metrics agent) key))
                   ((not (member key (agent-metrics-missing agent)))
                    (incf (getf (agent-metrics agent) key 0) number))))))

(defun report-agent-metrics (agent &key incomplete)
  "Publish once, before the reply callback can start another turn."
  (let ((reporter (shiftf (agent-metrics-reporter agent) nil)))
    (when (and reporter (agent-metrics-start agent))
      (funcall reporter (agent-metrics-id agent)
               (append (list :incomplete-p (and incomplete t) :elapsed-seconds (max 0 (- (clock-seconds) (agent-metrics-start agent))))
                       (copy-list (agent-metrics agent)))))))

(defun agent-turn-finished (agent text error)
  "The running turn ended with TEXT, its final reply, or with an ERROR (true;
:out-of-usage when the account ran out)."
  (unless error (agent-has-usage agent))
  (when (agent-busy agent)
    (report-agent-metrics agent :incomplete error)
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
      (report-agent-metrics agent :incomplete t)
      (setf (agent-callback agent) nil)
      (ignore-errors (agent-interrupt agent)))
    (dolist (callback (remove nil (cons running waiting)))
      (handler-case (funcall callback *stopped-text* :interrupted)
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

(defun own-notes-file (agent)
  "The notes file AGENT reads by itself."
  (if (eq (agent-id agent) :claude) "CLAUDE.md" "AGENTS.md"))

(defun read-notes-file (directory name)
  "The file NAME in DIRECTORY, as (truename . text); nil if missing or unreadable."
  (let ((file (probe-file (merge-pathnames name (uiop:ensure-directory-pathname directory)))))
    (when (and file (not (uiop:directory-pathname-p file)))
      (let ((text (ignore-errors (uiop:read-file-string file :external-format '(:utf-8 :replacement #\?)))))
        (and text (cons file text))))))

(defun notes-import-line-p (line name)
  "Whether LINE is a Claude Code import of the file NAME, such as @AGENTS.md."
  (member (string-trim '(#\Space #\Tab #\Return) line)
          (list (format nil "@~a" name) (format nil "@./~a" name)) :test #'string=))

(defun notes-fence (line)
  "For LINE (without leading space) starting a code fence: (character . length)."
  (let ((c (and (plusp (length line)) (char line 0))))
    (when (member c '(#\` #\~))
      (let ((n (or (position c line :test-not #'char=) (length line))))
        (when (>= n 3) (cons c n))))))

(defun notes-without-imports (text name)
  "TEXT without its lines importing NAME, trimmed, and whether it had any.
As in Claude Code, lines inside fenced code blocks aren't imports."
  (let ((fence nil) (found nil))
    (values
     (string-trim '(#\Space #\Tab #\Newline #\Return)
                  (format nil "~{~a~^~%~}"
                          (remove-if (lambda (line)
                                       (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
                                         (cond ((notes-fence trimmed)
                                                (let ((marker (notes-fence trimmed)))
                                                  (cond ((null fence) (setf fence marker))
                                                        ;; Closed by the same character, at least as
                                                        ;; many times, followed by nothing but space.
                                                        ((and (char= (car fence) (car marker))
                                                              (>= (cdr marker) (cdr fence))
                                                              (every (lambda (c) (member c '(#\Space #\Tab #\Return)))
                                                                     (subseq trimmed (cdr marker))))
                                                         (setf fence nil))))
                                                nil)
                                               ((and (null fence) (notes-import-line-p line name))
                                                (setf found t))
                                               (t nil))))
                                     (uiop:split-string text :separator '(#\Newline)))))
     found)))

(defun shared-notes (agent)
  "The text of the notes file in AGENT's folder that AGENT doesn't read by
itself, cut to *SHARED-NOTES-LIMIT*; nil when off, missing or empty, or when
AGENT already reads it: the same file or text as its own, or (Claude) imported
by its own. Lines of the other file that only import AGENT's own are left out."
  (let* ((directory (and *shared-notes* (agent-directory agent)))
         (other (and directory (read-notes-file directory (shared-notes-file agent))))
         (own (and other (read-notes-file directory (own-notes-file agent))))
         (text (and other (notes-without-imports (cdr other) (own-notes-file agent)))))
    (when (and text (plusp (length text))
               (not (and own (or (equal (car own) (car other))
                                 (string= text (notes-without-imports (cdr own) (shared-notes-file agent)))
                                 (and (eq (agent-id agent) :claude)
                                      (nth-value 1 (notes-without-imports (cdr own) (shared-notes-file agent))))))))
      (if (> (length text) *shared-notes-limit*)
          (format nil "~a~%~%[… cut: the file is ~:d characters long; read it for the rest.]"
                  (subseq text 0 *shared-notes-limit*) (length text))
          text))))

;; This read-time embedding is retained in saved applications. After editing
;; scripts/quiet-command.sh, force recompilation of agent-duet/core (ASDF's
;; :static-file timestamp alone may not invalidate this compiled file):
;; (asdf:compile-system "agent-duet/core" :force t), then rebuild the application.
(defparameter *quiet-command-script*
  #.(uiop:read-file-string
     (asdf:system-relative-pathname "agent-duet/core" "scripts/quiet-command.sh"))
  "Embedded at compile time so saved applications need no source checkout.")

(defun quiet-command-instruction (agent)
  "Offer the log-writing helper only with unrestricted filesystem access."
  (let ((path (and (eq :full (agent-permission agent))
                   (not (agent-read-only-p agent))
                   (ignore-errors
                     (let ((path (merge-pathnames "helpers/quiet-command.sh" (history-directory))))
                       (ensure-directories-exist path)
                       (unless (and (probe-file path)
                                    (string= *quiet-command-script* (uiop:read-file-string path)))
                         (with-open-file (out path :direction :output :if-exists :supersede
                                                  :external-format :utf-8)
                           (write-string *quiet-command-script* out)))
                       (namestring (truename path)))))))
    (format nil "Keep tool output focused: search for filenames and matching lines first, then read relevant sections. ~
Use bounded searches and file reads; avoid dumping whole files or broad diffs when a targeted read suffices. ~
Follow project command-wrapper instructions when present. ~a ~
Inspect relevant log excerpts when needed; a successful exit alone may not provide the requested results. ~
Preserve exit status and report verification failures. Never discard substantive answers or required evidence."
            (if path
                (format nil "For noisy builds, tests, installs and batch jobs without a project wrapper, use `/bin/sh ~a -- <command> [args...]`; ~
for pipelines, pass `sh -c '<script>'` as the command. It returns status and a full-log path, with bounded failure excerpts."
                        (shell-quote path))
                "For noisy commands, use an available permitted project wrapper or bounded output supported by the tool; respect read-only restrictions."))))

(defun collaboration-instructions (agent)
  (let ((partner (agent-partner-name agent))
        (notes (shared-notes agent)))
    (format nil "You are ~a, working with ~a (another coding agent) on the user's request in the same ~
directory. Prompts come from the user or are relayed from ~a: to implement something, review ~a's work, ~
or address its review of yours. When reviewing, inspect the files (and `git diff` in a git repository) ~
rather than trusting a summary, and change nothing unless asked. ~a sees only your reply: when handing off, ~
briefly give the outcome, changed paths, verification results and unresolved issues, citing review findings ~
by ID where applicable. Inline visualizations aren't rendered: link generated HTML files as Markdown with ~
absolute file:// URLs, including in handoffs. If a command needing approval is denied, carry on without it ~
and say so. ~a~
~@[~%~%The project's notes for coding agents, from its `~a` (written for ~a, but they apply to you too):~
~%~%~a~]"
            (agent-name agent) partner partner partner partner
            (quiet-command-instruction agent)
            (and notes (shared-notes-file agent)) partner notes)))

;;; Claude

(defparameter *claude-review-tools*
  '("Bash(git diff:*)" "Bash(git status:*)" "Bash(git log:*)" "Bash(git show:*)" "Bash(ls:*)")
  "Commands Claude may run without asking: enough to review.")

;;; The hook runs before inherited/session allow rules. Saved applications
;;; invoke their own Lisp image; development uses the already-required SBCL.
(defun claude-read-only-settings ()
  (json-string
   (jobj "hooks" (jobj "PreToolUse"
                       (vector (jobj "matcher" "*" "hooks"
                                     (vector (jobj "type" "command" "timeout" 10
                                                   "command" (review-hook-command)))))))))

(defparameter *claude-efforts* '("low" "medium" "high" "xhigh" "max"))

(defclass claude-agent (agent)
  ((cost-process :initform nil :accessor claude-agent-cost-process)
   (cost-total :initform 0 :accessor claude-agent-cost-total)
   (process :initform nil :accessor claude-agent-process)
   (config :initform nil :accessor claude-agent-config :documentation "What the process was started with")
   (session-id :initform nil :accessor claude-agent-session-id)
   (started :initform nil :accessor claude-agent-started :documentation "Whether the session exists, to resume")
   (next-result-index :initform 0 :accessor claude-agent-next-result-index
                      :documentation "One past the last result index in this CLI process")
   (turn-result-start :initform 0 :accessor claude-agent-turn-result-start
                      :documentation "The process result index at the start of this turn")
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
  "What the process is to be started with: (model effort permission directory
shared-notes read-only)."
  (list (agent-model agent) (agent-effort agent) (agent-permission agent) (agent-directory agent)
        *shared-notes* (and (agent-read-only-p agent) t)))

(defun claude-config-read-only (config)
  (sixth config))

(defun claude-agent-arguments (agent &key session-id resume)
  "The CLI's arguments for AGENT's current settings. Read-only, whatever the
permission: no edit tools, nothing run without asking but the read-only
tools, and every permission request answered by CLAUDE-AGENT-EVENT."
  (let ((permission (agent-permission agent))
        (read-only (agent-read-only-p agent)))
    (claude-arguments
     :session-id session-id :resume resume
     :model (agent-model agent) :effort (agent-effort agent)
     :permission-mode (cond (read-only "default")
                            ((eq permission :full) "bypassPermissions")
                            (t "acceptEdits"))
     ;; Always, so AskUserQuestion reaches the user (see CLAUDE-AGENT-EVENT).
     :permission-tool "stdio"
     :disallowed-tools (and read-only (append *claude-edit-tools* '("mcp__*" "Agent" "Task" "Skill")))
     :tools (and read-only (append *claude-read-only-tools* '("Bash" "AskUserQuestion")))
     :settings (and read-only (claude-read-only-settings))
     :setting-sources (and read-only "")
     :strict-mcp-config read-only
     :mcp-config (and read-only "{\"mcpServers\":{}}")
     :allowed-tools (cond (read-only *claude-read-only-tools*)
                          ((eq permission :full) nil)
                          (t *claude-review-tools*))
     :system-prompt (collaboration-instructions agent))))

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
               (arguments (claude-agent-arguments agent :session-id (unless resume session)
                                                        :resume resume))
               (new nil))
          (setf new (start-claude arguments
                                  :directory (agent-directory agent)
                                  :deliver (make-deliver-thunk agent)
                                  :on-event (lambda (event) (claude-agent-event agent new event))
                                  :on-stderr (lambda (line) (agent-note-stderr agent line))
                                  :on-exit (lambda (code) (claude-agent-exited agent new code))))
          (setf (claude-agent-process agent) new
                ;; A new process starts numbering at zero, including when
                ;; failed resume recovery resends the current turn's prompts.
                (claude-agent-next-result-index agent) 0
                (claude-agent-turn-result-start agent) 0
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

(defparameter *read-only-refusal*
  "This is a review-only turn: files can't be changed, and only read-only commands run (git diff, log, show, status and the like; ls, cat, head, tail, wc, grep), one at a time, without redirection. Carry on with what you can read."
  "What Claude is told when a read-only turn refuses it a tool.")

(defun claude-read-only-request-p (name input)
  "Whether NAME and INPUT are admissible. Bash must additionally pass through
the PreToolUse hook, which replaces its command with a helper-free invocation."
  (cond ((member name *claude-read-only-tools* :test #'equal) t)
        ((equal name "Bash") (read-only-command-p (and (jobject-p input) (jget input "command"))))
        (t nil)))

(defun claude-agent-event (agent cp event)
  ;; A subagent's events aren't shown, but they are signs of life.
  (when (eq cp (claude-agent-process agent)) (agent-touch agent))
  (when (and (eq cp (claude-agent-process agent)) (not (event-subagent event)))
    (case (event-kind event)
      (:init (when (event-session-id event)
               (setf (claude-agent-session-id agent) (event-session-id event))))
      (:context
       (let* ((usage (event-data event))
              (values (mapcar (lambda (key) (metric-number (jget usage key)))
                              '("input_tokens" "cache_read_input_tokens" "cache_creation_input_tokens"))))
         (setf (agent-context-tokens agent)
               (and (first values) (reduce #'+ values :key (lambda (v) (or v 0)))))
         (agent-emit agent :usage)))
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
          ;; Bash must be validated AND rewritten by our PreToolUse hook.
          ;; If it reaches the permission host anyway, fail closed. Other
          ;; tools are allowed only when they read; never ask to grant writes.
          ((or (claude-config-read-only (claude-agent-config agent)) (agent-read-only-p agent))
           (if (and (not (equal (event-name event) "Bash"))
                    (claude-read-only-request-p (event-name event) input))
               (claude-answer-permission cp id t :input input)
               (claude-answer-permission cp id nil :message *read-only-refusal*)))
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
                                     do (claude-agent-send agent new (prompt-without-history agent prompt) images
                                                           :on-error (turn-failure agent))))
                 (error (e) (agent-turn-finished agent (princ-to-string e) t))))))))

(defun claude-result-cost (agent total &optional index)
  "Claude reports a process-cumulative cost, unlike its per-result usage."
  (unless (eq (claude-agent-process agent) (claude-agent-cost-process agent))
    (setf (claude-agent-cost-process agent) (claude-agent-process agent)
          (claude-agent-cost-total agent) 0))
  (let* ((previous (claude-agent-cost-total agent))
         (current (metric-number total))
         (gap (and (integerp index) (> index (claude-agent-next-result-index agent))))
         (delta (and (not gap) previous current (>= current previous) (- current previous))))
    ;; An index gap or an absent cost field prevents attributing the delta
    ;; to one result. Reestablish the baseline, leaving that value unknown.
    ;; Without indexes, a missing result cannot be detected.
    (when (or (null previous) (null current) (>= current previous))
      (setf (claude-agent-cost-total agent) current))
    delta))

(defun claude-agent-result (agent event)
  ;; Out of usage only on this turn's evidence (its rate_limit_event said
  ;; rejected, or its error says so), never on what an earlier turn left
  ;; behind: a later sign-in failure is a sign-in failure.
  (let* ((text (event-text event))
         (error (event-error event))
         (index (event-result-index event))
         (hit (shiftf (claude-agent-limit-hit agent) nil))
         (cost (claude-result-cost agent (event-cost event) index)))
    (when (or (not (integerp index)) (>= index (claude-agent-turn-result-start agent)))
      (agent-add-metrics agent
                         (list :cost-usd cost :duration-ms (event-duration event)
                             :input-tokens (jget (event-data event) "input_tokens")
                             :output-tokens (jget (event-data event) "output_tokens")
                             :cache-read-tokens (jget (event-data event) "cache_read_input_tokens")
                             :cache-write-tokens (jget (event-data event) "cache_creation_input_tokens"))
                       :id (and (integerp index) (list (claude-agent-process agent) index))))
    ;; Advance before publishing completion: its callback may start the next
    ;; turn immediately on this same process. Use the index, not a received
    ;; result count, so gaps in delivery do not shift subsequent turns.
    (setf (claude-agent-next-result-index agent)
          (if (integerp index)
              (max (claude-agent-next-result-index agent) (1+ index))
              (1+ (claude-agent-next-result-index agent))))
    (when (and error (or hit (claude-limit-text-p text)))
      ;; This turn's reset time, replacing any earlier one.
      (agent-ran-out agent (or (claude-limit-text-reset text) (and (integerp hit) hit)))
      (setf error :out-of-usage))
    ;; Indexed results correspond to individual input messages, even when the
    ;; CLI reports no queued turns. Keep the callback until the last message's
    ;; result. Older CLIs without indexes retain their queue-count behavior.
    (unless (and (not error)
                 (if (integerp index)
                     (and (car (agent-turn-token agent))
                          (< (- (1+ index) (claude-agent-turn-result-start agent))
                             (length (claude-agent-prompts agent))))
                     (plusp (or (event-queued-turn-count event) 0))))
      (agent-turn-finished agent text error))))

(defmethod agent-usage-windows ((agent claude-agent))
  (claude-usage-windows (claude-agent-limits agent)))

(defun agent-transport-lost (agent why)
  "Retire the disconnected transport before publishing a recoverable failure.
Keep editor identities so recovery cannot race surviving editing processes."
  (let ((busy (agent-busy agent))
        (waiting (mapcar #'second (agent-queue agent))))
    ;; Prevent shutdown from finishing with a generic error or starting queued work.
    (when (agent-retry-pending-p agent)
      ;; The queued retry receives :disconnected and becomes explicit recovery.
      ;; Settle the stall card too; the old turn is no longer pending shutdown.
      (setf (agent-retry agent) :dropped))
    (setf (agent-busy agent) nil (agent-queue agent) nil)
    (unwind-protect (agent-shutdown agent)
      (setf (agent-busy agent) busy))
    (when busy (agent-emit agent :note :text why))
    (agent-turn-finished agent why :disconnected)
    (dolist (callback waiting)
      (when callback
        (handler-case (funcall callback why :disconnected)
          (error (e) (agent-emit agent :note :text (format nil "Error after disconnecting ~a: ~a"
                                                         (agent-name agent) e))))))))

(defun claude-agent-exited (agent cp code)
  (when (eq cp (claude-agent-process agent))
    (agent-transport-lost agent
                          (format nil "Claude Code ~a.~@[~%~a~]"
                                  (if code (format nil "exited (status ~a)" code) "connection closed")
                                  (let ((s (agent-recent-stderr agent))) (and (plusp (length s)) s))))))

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

(defparameter *context-fallback-limit* 4 "How many compact prompts an agent keeps the full versions of.")

(defun remember-context-fallback (agent compact full)
  "Keep FULL to send instead of COMPACT if AGENT's conversation is lost."
  (let ((kept (remove compact (agent-context-fallback agent) :key #'car :test #'equal)))
    (setf (agent-context-fallback agent)
          (cons (cons compact full) (subseq kept 0 (min (length kept) (1- *context-fallback-limit*)))))))

(defun prompt-without-history (agent prompt)
  "Expand a recent compact prompt if its conversation could not be resumed,
also one a recovery or follow-up wraps in more text."
  (let ((fallback (agent-context-fallback agent)))
    (or (cdr (assoc prompt fallback :test #'equal))
        (let ((entry (find-if (lambda (entry) (search (car entry) prompt)) fallback)))
          (if entry
              (let ((at (search (car entry) prompt)))
                (concatenate 'string (subseq prompt 0 at) (cdr entry)
                             (subseq prompt (+ at (length (car entry))))))
              prompt)))))

(defmethod agent-begin-turn ((agent claude-agent) prompt)
  (let ((images (agent-turn-images agent)))
    (setf (claude-agent-prompts agent) (list (cons prompt images)))
    (let* ((history (agent-conversation-handle agent))
           (cp (claude-agent-ensure-process agent)))
      (unless (equal history (agent-conversation-handle agent))
        (setf prompt (prompt-without-history agent prompt)
              (claude-agent-prompts agent) (list (cons prompt images))))
      (setf (claude-agent-turn-result-start agent) (claude-agent-next-result-index agent))
      (capture-agent-process-baseline agent)
      (claude-agent-send agent cp prompt images :on-error (turn-failure agent)))))

(defmethod agent-steer ((agent claude-agent) prompt &optional images)
  ;; Use the current process: changing settings must not restart a busy CLI.
  (let ((cp (claude-agent-process agent)))
    (when (and (agent-read-only-p agent) (not (claude-config-read-only (claude-agent-config agent))))
      (error "Claude's running turn may change files; send this once it has finished (review-only)."))
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
    (when cp (pushnew (cp-process cp) (agent-stopping-processes agent)) (stop-claude cp))
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

(defun catalog-effort (effort choices catalog-ready-p)
  "Keep EFFORT until the catalog is ready, then validate it against CHOICES."
  (if (or (not catalog-ready-p) (member effort choices :test #'equal)) effort nil))

(defclass codex-agent (agent)
  ((metrics-baseline :initform nil :accessor codex-agent-metrics-baseline)
   (server :initform nil :accessor codex-agent-server)
   (config :initform nil :accessor codex-agent-config :documentation "What the server was started with")
   (program :initarg :program :initform nil :accessor codex-agent-program)
   (thread-id :initform nil :accessor codex-agent-thread-id)
   (last-text :initform nil :accessor codex-agent-last-text :documentation "The turn's last agent message")
   (models :initform '() :accessor codex-agent-models :documentation "From model/list")
   (usage :initform nil :accessor agent-usage :documentation "Rate-limit buckets, (id . snapshot)s"))
  (:default-initargs :id :codex :name "ChatGPT" :partner-name "Claude" :model *codex-model*))

(defun codex-agent-sandbox (agent)
  (cond ((agent-read-only-p agent) "read-only")
        ((eq (agent-permission agent) :full) "danger-full-access")
        (t "workspace-write")))

(defun codex-agent-approval-policy (agent)
  ;; Read-only never asks: approving would let a command out of the sandbox.
  (if (and (eq (agent-permission agent) :ask) (not (agent-read-only-p agent))) "untrusted" "never"))

(defun codex-agent-read-only-server-p (agent)
  "Whether the agent's app-server was started in the read-only sandbox."
  (equal (first (codex-agent-config agent)) "read-only"))

(defun codex-agent-ensure-server (agent &key (sandbox (codex-agent-sandbox agent))
                                             (policy (codex-agent-approval-policy agent)))
  "The agent's app-server with its thread started, (re)started when it isn't
running in the current directory and SANDBOX. Blocks: call it off the GUI thread."
  (let ((server (codex-agent-server agent))
        (config (list sandbox (agent-directory agent) *shared-notes*)))
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
        (codex-agent-open-thread agent server sandbox policy)))
    server))

(defun codex-agent-open-thread (agent server sandbox policy)
  "Start or resume the agent's thread on the new SERVER. If that fails, the
server is stopped and forgotten, so the next turn starts again from scratch
rather than using a server with no thread."
  (let ((ready nil))
    (unwind-protect
        (progn
          (codex-agent-setup-thread agent server sandbox policy)
          (setf ready t))
      (unless ready
        (when (eq server (codex-agent-server agent))
          (setf (codex-agent-server agent) nil))
        (stop-codex-app-server server)))))

(defun codex-agent-setup-thread (agent server sandbox policy)
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
                                 "sandbox" sandbox
                                 "approvalPolicy" policy
                                 "developerInstructions" instructions))
            (setf (codex-thread-id server) (codex-agent-thread-id agent)))
        (codex-request-error (e)
          (let ((why (format nil "Couldn't resume ChatGPT's conversation (~a). Starting a new one." e)))
            (setf (codex-agent-thread-id agent) nil)
            (funcall (agent-deliver agent) (lambda () (agent-emit agent :note :text why)))))))
    (unless (codex-agent-thread-id agent)
      (setf (codex-agent-thread-id agent)
            (codex-start-thread server :model (agent-model agent) :instructions instructions
                                :sandbox sandbox :approval-policy policy)))))

(defun codex-agent-event (agent server event)
  (when (eq server (codex-agent-server agent))
    (agent-touch agent)
    (case (codex-event-kind event)
      (:text-delta (agent-emit agent :delta :text (codex-event-text event)))
      (:text (setf (codex-agent-last-text agent) (codex-event-text event))
       (agent-emit agent :text :text (codex-event-text event)))
      (:image
       (agent-emit agent :text :text (format nil "![Generated image](~a)"
                                           (image-artifact-uri (codex-event-text event)))))
      (:activity (agent-emit agent :tool :id (codex-event-id event) :title (codex-event-text event)))
      (:activity-completed (agent-emit agent :tool :id (codex-event-id event) :title (codex-event-name event))
       (agent-emit agent :tool-result :id (codex-event-id event) :text (codex-event-text event)
                   :error (codex-event-error event)))
      (:approval
       (let ((id (codex-event-id event)) (params (codex-event-data event)))
         (if (or (codex-agent-read-only-server-p agent) (agent-read-only-p agent))
             ;; Not asked for in read-only (policy never); refused if it is.
             (progn (codex-answer-approval server id :deny)
                    (agent-emit agent :note :text (format nil "Review-only: refused ~a's request: ~a"
                                                          (agent-name agent) (codex-event-text event))))
             (agent-ask-approval agent id (codex-event-text event)
                                 (let ((reason (jget params "reason"))) (and (stringp reason) reason))
                                 (lambda (decision)
                                   (when (codex-alive-p server)
                                     (codex-answer-approval server id decision)))))))
      (:token-usage
       (when (and (equal (codex-event-thread-id event) (codex-agent-thread-id agent))
                  (equal (codex-event-turn-id event) (codex-turn-id server)))
         (let* ((data (codex-event-data event))
                (last (jget data "last")) (total (jget data "total"))
                (mapping '((:input-tokens . "inputTokens") (:output-tokens . "outputTokens")
                           (:cache-read-tokens . "cachedInputTokens")
                           (:reasoning-tokens . "reasoningOutputTokens") (:total-tokens . "totalTokens"))))
           (setf (agent-context-tokens agent) (metric-number (jget last "totalTokens"))
                 (agent-context-limit agent) (metric-number (jget data "modelContextWindow")))
           (agent-emit agent :usage)
           ;; LAST is one model request, not necessarily the whole tool-using
           ;; turn. Anchor the cumulative thread total at its first update.
           ;; Subtracting LAST also excludes usage from a resumed conversation.
           (unless (codex-agent-metrics-baseline agent)
             (setf (codex-agent-metrics-baseline agent)
                   (loop for (key . field) in mapping
                         for now = (metric-number (jget total field))
                         for previous = (metric-number (jget last field))
                         collect (cons key (and now previous (>= now previous) (- now previous))))))
           (agent-add-metrics agent
                              (loop for (key . field) in mapping
                                    for baseline = (cdr (assoc key (codex-agent-metrics-baseline agent)))
                                    for now = (metric-number (jget total field))
                                    append (list key (and baseline now (>= now baseline) (- now baseline))))
                              :replace t))))
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
    (agent-transport-lost agent
                          (format nil "Codex ~a.~@[~%~a~]"
                                  (if code (format nil "exited (status ~a)" code) "connection closed")
                                  (let ((s (agent-recent-stderr agent))) (and (plusp (length s)) s))))))

(defmethod agent-begin-turn ((agent codex-agent) prompt)
  (setf (codex-agent-last-text agent) nil
        (codex-agent-metrics-baseline agent) nil)
  (let ((deliver (agent-deliver agent))
        (model (agent-model agent))
        (effort (agent-effort agent))
        (sandbox (codex-agent-sandbox agent))
        (policy (codex-agent-approval-policy agent))
        (images (agent-turn-images agent))
        (token (agent-turn-token agent))
        (history (agent-conversation-handle agent)))
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
             (let ((server (codex-agent-ensure-server agent :sandbox sandbox :policy policy)))
               (when (eq token (agent-turn-token agent)) (capture-agent-process-baseline agent))
               (if (not (car token))
                   (stopped)
                   (let ((turn-id (jget (codex-start-turn server
                                        (if (equal history (agent-conversation-handle agent)) prompt
                                            (prompt-without-history agent prompt)) :model model :effort effort
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
    (when (and (agent-read-only-p agent) (not (codex-agent-read-only-server-p agent)))
      (error "ChatGPT's running turn may change files; send this once it has finished (review-only)."))
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
    (when server (pushnew (codex-process server) (agent-stopping-processes agent)) (stop-codex-app-server server))
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

 ;;; Shutdown confirmation is separate from asking a process to terminate.
(defgeneric agent-cli-process (agent)
  (:method ((agent agent)) nil)
  (:method ((agent claude-agent))
    (let ((cp (claude-agent-process agent))) (and cp (cp-process cp))))
  (:method ((agent codex-agent))
    (let ((server (codex-agent-server agent))) (and server (codex-process server)))))

(defun recovery-pid-alive-p (pid)
  "Compatibility for in-memory callers only; checkpoints never trust raw PIDs."
  (handler-case (progn (sb-posix:kill pid 0) t)
    (sb-posix:syscall-error (e) (not (= (sb-posix:syscall-errno e) sb-posix:esrch)))))

(defun normalize-process-identity (text)
  (format nil "~{~a~^ ~}" (remove "" (uiop:split-string text :separator '(#\Space #\Tab)) :test #'equal)))

(defun recovery-process-record (pid)
  "Bind a PID to its launch time; exec may change the command without changing identity."
  (let ((identity (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (uiop:run-program (list "ps" "-p" (write-to-string pid) "-o" "lstart=")
                                                 :output :string :error-output :string :ignore-error-status t))))
    (unless (string= identity "")
      (jobj "pid" pid "identity" (normalize-process-identity identity)))))

(defun recovery-process-alive-p (record)
  "Only the same process counts as outstanding; a recycled PID does not."
  (and (jobject-p record) (integerp (jget record "pid")) (stringp (jget record "identity"))
       (handler-case
           (let ((now (recovery-process-record (jget record "pid"))))
             (and now (equal (jget now "identity") (jget record "identity"))))
         (error () t))))

(defun agent-process-tree (agent)
  "One ps snapshot, with process identities, limited to this CLI's descendants."
  (let ((process (agent-cli-process agent)))
    (when process
      (let* ((output (uiop:run-program '("ps" "-axo" "pid=,ppid=,lstart=") :output :string))
             (rows (loop for line in (uiop:split-string output :separator '(#\Newline))
                         for row = (with-input-from-string (in line)
                                     (let* ((*read-eval* nil) (pid (read in nil nil)) (ppid (read in nil nil))
                                            (identity (read-line in nil "")))
                                       (when (and (integerp pid) (integerp ppid))
                                         (list pid ppid (jobj "pid" pid "identity" (normalize-process-identity identity))))))
                         when row collect row))
             (root (sb-ext:process-pid process))
             (parents (list root)))
        (loop for children = (loop for (pid ppid) in rows
                                  when (and (member ppid parents) (not (member pid parents))) collect pid)
              while children do (setf parents (append children parents)))
        (values (loop for (pid nil record) in rows when (and (member pid parents) (/= pid root)) collect record)
                (third (assoc root rows)))))))

(defun capture-agent-process-baseline (agent)
  "Existing service processes belong to the CLI, not to the new turn."
  (multiple-value-bind (children root) (agent-process-tree agent)
    (setf (agent-turn-process-baseline agent) children
          (agent-process-record agent) root)))

(defun remember-agent-editors (agent)
  "Track only descendants created since this turn began, never persistent services.
An attached transport with no baseline must be shut down to confirm ownership."
  (multiple-value-bind (children root) (agent-process-tree agent)
    (when root (setf (agent-process-record agent) root))
    (setf (agent-editing-processes agent)
          (remove-duplicates
           (append (and (listp (agent-turn-process-baseline agent))
                        (set-difference children (agent-turn-process-baseline agent) :test #'equalp))
                   (agent-editing-processes agent)) :test #'equalp))))

(defgeneric agent-quiescent-p (agent)
  (:documentation "No active turn, queued work, retiring CLI, or surviving turn-owned editor."))
(defmethod agent-quiescent-p ((agent agent))
  (setf (agent-stopping-processes agent) (remove-if-not #'sb-ext:process-alive-p (agent-stopping-processes agent))
        (agent-editing-pids agent) (remove-if-not #'recovery-pid-alive-p (agent-editing-pids agent))
        (agent-editing-processes agent) (remove-if-not #'recovery-process-alive-p (agent-editing-processes agent))
        (agent-retiring-records agent) (remove-if-not #'recovery-process-alive-p (agent-retiring-records agent)))
  (and (not (agent-busy agent)) (null (agent-queue agent))
       (or (listp (agent-turn-process-baseline agent))
           (null (agent-cli-process agent))
           (not (sb-ext:process-alive-p (agent-cli-process agent))))
       (null (agent-stopping-processes agent)) (null (agent-editing-pids agent))
       (null (agent-editing-processes agent)) (null (agent-retiring-records agent))))

(defmethod agent-interrupt :before ((agent agent)) (remember-agent-editors agent))
(defmethod agent-shutdown :before ((agent agent))
  (remember-agent-editors agent)
  (when (agent-process-record agent) (pushnew (agent-process-record agent) (agent-retiring-records agent) :test #'equalp)))

(defmethod agent-interrupt :after ((agent agent))
  ;; A new descendant whose ownership is uncertain is stopped, never simply
  ;; forgotten. Restarting the CLI retains its conversation but rebuilds services.
  (when (and (agent-cli-process agent) (agent-editing-processes agent))
    (agent-shutdown agent)
    (dolist (record (agent-editing-processes agent))
      (when (recovery-process-alive-p record)
        (ignore-errors (sb-posix:kill (jget record "pid") 15))))))

(defmethod agent-resume-conversation ((agent agent) handle)
  (declare (ignore handle))
  nil)
