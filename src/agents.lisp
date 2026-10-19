;;;; agents.lisp — Claude and Codex behind one interface
;;;;
;;;; An AGENT takes one prompt at a time. AGENT-SUBMIT queues a prompt with a
;;;; function to call with the turn's final reply; while a turn runs, later
;;;; prompts wait their turn. What happens during a turn goes to the agent's
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
   (queue :initform '() :accessor agent-queue :documentation "Waiting (prompt . callback)s, oldest first")
   (callback :initform nil :accessor agent-callback :documentation "The running turn's")
   (turn-token :initform (list nil) :accessor agent-turn-token
               :documentation "(t) while the running turn may go on; AGENT-CANCEL makes it (nil)")
   (approvals :initform '() :accessor agent-approvals :documentation "Unanswered (id . answer-function)s")
   (stderr :initform '() :accessor agent-stderr :documentation "The last lines of standard error, newest first")
   (out-until :initform nil :accessor agent-out-until
              :documentation "When out of usage: the universal time it resets, or :unknown")))

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

(defmethod agent-interrupt ((agent agent)) nil)
(defmethod agent-shutdown ((agent agent)) nil)
(defmethod agent-new-conversation ((agent agent)) (agent-shutdown agent))

(defun agent-emit (agent kind &rest plist)
  (when (agent-listener agent)
    (apply (agent-listener agent) agent kind plist)))

(defun agent-submit (agent prompt &optional callback)
  "Have AGENT work on PROMPT, now or after the turns before it. CALLBACK
gets the final reply's text and whether the turn failed (then the text
says why)."
  (setf (agent-queue agent) (append (agent-queue agent) (list (cons prompt callback))))
  (unless (agent-busy agent) (agent-next agent)))

(defun agent-next (agent)
  (let ((next (pop (agent-queue agent))))
    (when next
      (setf (agent-busy agent) t
            (agent-callback agent) (cdr next)
            (agent-turn-token agent) (list t))
      (agent-emit agent :busy :busy t)
      (agent-emit agent :prompt :text (car next))
      (if (agent-out-of-usage-p agent)
          ;; Known to be out: don't spend a request finding out again.
          (agent-turn-finished agent (agent-out-message agent) :out-of-usage)
          (handler-case (agent-begin-turn agent (car next))
            (error (e) (agent-turn-finished agent (princ-to-string e) t)))))))

(defun agent-turn-finished (agent text error)
  "The running turn ended with TEXT, its final reply, or with an ERROR (true;
:out-of-usage when the account ran out)."
  (unless error (agent-has-usage agent))
  (when (agent-busy agent)
    (let ((callback (agent-callback agent)))
      (setf (agent-busy agent) nil (agent-callback agent) nil)
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
  (let ((waiting (mapcar #'cdr (agent-queue agent)))
        (running (and (agent-busy agent) (agent-callback agent))))
    (setf (agent-queue agent) '())
    (setf (car (agent-turn-token agent)) nil)
    (agent-deny-approvals agent)
    (when (agent-busy agent)
      (setf (agent-callback agent) nil)
      (ignore-errors (agent-interrupt agent)))
    (dolist (callback (remove nil (cons running waiting)))
      (handler-case (funcall callback *stopped-text* t)
        (error (e) (agent-emit agent :note :text (format nil "Error after stopping ~a: ~a" (agent-name agent) e)))))))

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

(defun collaboration-instructions (agent)
  (let ((partner (agent-partner-name agent)))
    (format nil "You are ~a, one of two coding agents (the other is ~a) working together on the user's ~
request in the same working directory. Prompts may come from the user directly or be relayed from ~a: ~
they may ask you to implement something, to review ~a's work, or to address its review of yours. ~
When you review, inspect the files themselves (and `git diff` if the directory is a git repository) ~
rather than trusting a summary, and don't change files unless asked to. When you implement, finish your ~
reply with a short summary of what you changed and in which files, since that summary is what ~a sees. ~
Running a command may need the user's approval; if one is denied, carry on without it and say so."
            (agent-name agent) partner partner partner partner)))

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
   (limit-hit :initform nil :accessor claude-agent-limit-hit
              :documentation "Set when this turn's rate_limit_event said rejected: its reset, or :unknown"))
  (:default-initargs :id :claude :name "Claude" :partner-name "ChatGPT" :model *claude-model*))

(defun claude-agent-wanted-config (agent)
  (list (agent-model agent) (agent-effort agent) (agent-permission agent) (agent-directory agent)))

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
          ((not (eq (agent-permission agent) :ask))
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
       ;; Out of usage only on this turn's evidence (its rate_limit_event
       ;; said rejected, or its error says so), never on what an earlier
       ;; turn left behind: a later sign-in failure is a sign-in failure.
       (let* ((text (event-text event))
              (error (event-error event))
              (hit (shiftf (claude-agent-limit-hit agent) nil)))
         (when (and error (or hit (claude-limit-text-p text)))
           ;; This turn's reset time, replacing any earlier one.
           (agent-ran-out agent (or (claude-limit-text-reset text) (and (integerp hit) hit)))
           (setf error :out-of-usage))
         (agent-turn-finished agent text error))))))

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

(defmethod agent-begin-turn ((agent claude-agent) prompt)
  (claude-send (claude-agent-ensure-process agent) prompt))

(defmethod agent-interrupt ((agent claude-agent))
  (let ((cp (claude-agent-process agent)))
    (when (claude-alive-p cp) (claude-interrupt cp))))

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
        (config (list (codex-agent-sandbox agent) (agent-directory agent))))
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
    (if (codex-agent-thread-id agent)
        (progn
          (codex-initialize-app-server server)
          (codex-request server "thread/resume"
                         (jobj "threadId" (codex-agent-thread-id agent)
                               "cwd" (uiop:native-namestring (codex-directory server))
                               "sandbox" (codex-agent-sandbox agent)
                               "approvalPolicy" (codex-agent-approval-policy agent)
                               "developerInstructions" instructions))
          (setf (codex-thread-id server) (codex-agent-thread-id agent)))
        (setf (codex-agent-thread-id agent)
              (codex-start-thread server :model (agent-model agent) :instructions instructions
                                  :sandbox (codex-agent-sandbox agent)
                                  :approval-policy (codex-agent-approval-policy agent))))))

(defun codex-agent-event (agent server event)
  (when (eq server (codex-agent-server agent))
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
                                                          :approval-policy policy)
                                        "turn" "id")))
                     (unless (car token)
                       (ignore-errors (codex-interrupt server :turn-id turn-id))))))
           (error (e)
                  (let ((why (princ-to-string e)))
                    (funcall deliver (lambda () (agent-turn-finished agent why t)))))))
       :name "codex turn"))))

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
