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
;;;;   :turn-done TEXT ERROR the turn ended; TEXT is the final reply
;;;;   :note TEXT            something to tell the user (the CLI exited…)
;;;;   :busy BUSY            the agent started or stopped working
;;;; Everything is called through DELIVER, on the GUI thread in the app.

(in-package #:agent-integration)

(defclass agent ()
  ((id :initarg :id :reader agent-id :documentation ":claude or :codex")
   (name :initarg :name :reader agent-name)
   (directory :initarg :directory :initform nil :accessor agent-directory)
   (permission :initarg :permission :initform :edits :accessor agent-permission
               :documentation ":edits (edit files in the directory) or :full (no restrictions)")
   (deliver :initarg :deliver :initform #'funcall :accessor agent-deliver)
   (listener :initarg :listener :initform nil :accessor agent-listener)
   (busy :initform nil :accessor agent-busy)
   (queue :initform '() :accessor agent-queue :documentation "Waiting (prompt . callback)s, oldest first")
   (callback :initform nil :accessor agent-callback :documentation "The running turn's")
   (stderr :initform '() :accessor agent-stderr :documentation "The last lines of standard error, newest first")))

(defgeneric agent-begin-turn (agent prompt)
  (:documentation "Start a turn on PROMPT. The turn ends with AGENT-TURN-FINISHED."))

(defgeneric agent-interrupt (agent)
  (:documentation "Ask the running turn to stop; it still ends with AGENT-TURN-FINISHED."))

(defgeneric agent-shutdown (agent)
  (:documentation "End the agent's process. The conversation can resume later."))

(defmethod agent-interrupt ((agent agent)) nil)
(defmethod agent-shutdown ((agent agent)) nil)

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
            (agent-callback agent) (cdr next))
      (agent-emit agent :busy :busy t)
      (agent-emit agent :prompt :text (car next))
      (handler-case (agent-begin-turn agent (car next))
        (error (e) (agent-turn-finished agent (princ-to-string e) t))))))

(defun agent-turn-finished (agent text error)
  "The running turn ended with TEXT, its final reply, or with an ERROR."
  (when (agent-busy agent)
    (let ((callback (agent-callback agent)))
      (setf (agent-busy agent) nil (agent-callback agent) nil)
      (agent-emit agent :turn-done :text text :error error)
      (agent-emit agent :busy :busy nil)
      (when callback
        (handler-case (funcall callback text error)
          (error (e) (agent-emit agent :note :text (format nil "Error after ~a's turn: ~a" (agent-name agent) e)))))
      (unless (agent-busy agent) (agent-next agent)))))

(defun agent-cancel (agent)
  "Drop the waiting prompts and interrupt the running turn."
  (setf (agent-queue agent) '())
  (when (agent-busy agent)
    (setf (agent-callback agent) nil)
    (ignore-errors (agent-interrupt agent))))

(defun agent-note-stderr (agent line)
  (push line (agent-stderr agent))
  (when (> (length (agent-stderr agent)) 40)
    (setf (agent-stderr agent) (subseq (agent-stderr agent) 0 40))))

(defun agent-recent-stderr (agent &optional (n 5))
  (format nil "~{~a~^~%~}" (reverse (subseq (agent-stderr agent) 0 (min n (length (agent-stderr agent)))))))

(defun make-deliver-thunk (agent)
  (let ((deliver (agent-deliver agent)))
    (lambda (thunk) (funcall deliver thunk))))

;;; What each agent is told about working here

(defun collaboration-instructions (agent partner-name)
  (format nil "You are ~a, one of two coding agents (the other is ~a) working together on the user's ~
request in the same working directory. Prompts may come from the user directly or be relayed from ~a: ~
they may ask you to implement something, to review ~a's work, or to address its review of yours. ~
When you review, inspect the files themselves (and `git diff` if the directory is a git repository) ~
rather than trusting a summary, and don't change files unless asked to. When you implement, finish your ~
reply with a short summary of what you changed and in which files, since that summary is what ~a sees."
          (agent-name agent) partner-name partner-name partner-name partner-name))

;;; Claude

(defparameter *claude-review-tools*
  '("Bash(git diff:*)" "Bash(git status:*)" "Bash(git log:*)" "Bash(git show:*)" "Bash(ls:*)")
  "Commands Claude may run when it may only edit files: enough to review.")

(defclass claude-agent (agent)
  ((process :initform nil :accessor claude-agent-process)
   (session-id :initform nil :accessor claude-agent-session-id)
   (started :initform nil :accessor claude-agent-started :documentation "Whether the session exists, to resume")
   (model :initarg :model :initform *claude-model* :accessor claude-agent-model)
   (partner-name :initarg :partner-name :initform "ChatGPT" :accessor agent-partner-name))
  (:default-initargs :id :claude :name "Claude"))

(defun claude-agent-ensure-process (agent)
  (let ((cp (claude-agent-process agent)))
    (if (claude-alive-p cp)
        cp
        (let* ((resume (and (claude-agent-started agent) (claude-agent-session-id agent)))
               (session (or (claude-agent-session-id agent)
                            (setf (claude-agent-session-id agent) (make-uuid))))
               (arguments (claude-arguments
                           :session-id (unless resume session) :resume resume
                           :model (claude-agent-model agent)
                           :permission-mode (if (eq (agent-permission agent) :full) "bypassPermissions" "acceptEdits")
                           :allowed-tools (unless (eq (agent-permission agent) :full) *claude-review-tools*)
                           :system-prompt (collaboration-instructions agent (agent-partner-name agent))))
               (new nil))
          (setf new (start-claude arguments
                                  :directory (agent-directory agent)
                                  :deliver (make-deliver-thunk agent)
                                  :on-event (lambda (event) (claude-agent-event agent event))
                                  :on-stderr (lambda (line) (agent-note-stderr agent line))
                                  :on-exit (lambda (code) (claude-agent-exited agent new code))))
          (setf (claude-agent-process agent) new
                (claude-agent-started agent) t)
          new))))

(defun claude-tool-title (name input)
  (let ((detail (and (jobject-p input)
                     (let ((v (or (jget input "command") (jget input "file_path") (jget input "pattern")
                                  (jget input "path") (jget input "url") (jget input "description"))))
                       (and (stringp v) (substitute #\Space #\Newline (if (> (length v) 120) (subseq v 0 120) v)))))))
    (format nil "~a~@[  ~a~]" name detail)))

(defun claude-agent-event (agent event)
  (unless (event-subagent event)
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
      (:result (agent-turn-finished agent (event-text event) (event-error event))))))

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
  (let ((cp (claude-agent-process agent)))
    (setf (claude-agent-process agent) nil)
    (when cp (stop-claude cp))
    (when (agent-busy agent) (agent-turn-finished agent "Stopped." t))))

(defun claude-agent-new-conversation (agent)
  (agent-shutdown agent)
  (setf (claude-agent-session-id agent) nil (claude-agent-started agent) nil))

;;; Codex

(defclass codex-agent (agent)
  ((server :initform nil :accessor codex-agent-server)
   (program :initarg :program :initform nil :accessor codex-agent-program)
   (thread-id :initform nil :accessor codex-agent-thread-id)
   (last-text :initform nil :accessor codex-agent-last-text :documentation "The turn's last agent message")
   (model :initarg :model :initform *codex-model* :accessor codex-agent-model)
   (partner-name :initarg :partner-name :initform "Claude" :accessor agent-partner-name))
  (:default-initargs :id :codex :name "ChatGPT"))

(defun codex-agent-sandbox (agent)
  (if (eq (agent-permission agent) :full) "danger-full-access" "workspace-write"))

(defun codex-agent-ensure-server (agent)
  "The agent's app-server with its thread started. Blocks: call it off the GUI thread."
  (let ((server (codex-agent-server agent)))
    (unless (codex-alive-p server)
      (let ((program (or (codex-agent-program agent) (find-codex-program)
                         (error "The Codex CLI was not found")))
            (new nil))
        (setf new (start-codex-app-server program (or (agent-directory agent) (uiop:getcwd))
                                          :deliver (make-deliver-thunk agent)
                                          :on-event (lambda (event) (codex-agent-event agent new event))
                                          :on-exit (lambda (code) (codex-agent-exited agent new code))))
        (setf server new (codex-agent-server agent) new)
        (let ((instructions (collaboration-instructions agent (agent-partner-name agent))))
          (if (codex-agent-thread-id agent)
              (progn
                (codex-initialize-app-server server)
                (codex-request server "thread/resume"
                               (jobj "threadId" (codex-agent-thread-id agent)
                                     "cwd" (uiop:native-namestring (codex-directory server))
                                     "sandbox" (codex-agent-sandbox agent) "approvalPolicy" "never"
                                     "developerInstructions" instructions))
                (setf (codex-thread-id server) (codex-agent-thread-id agent)))
              (setf (codex-agent-thread-id agent)
                    (codex-start-thread server :model (codex-agent-model agent) :instructions instructions
                                               :sandbox (codex-agent-sandbox agent)))))))
    server))

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
      (:permission-denied (agent-emit agent :note :text (format nil "Declined: ~a" (codex-event-name event))))
      ((:stderr :protocol-error) (agent-note-stderr agent (codex-event-text event)))
      (:result (agent-turn-finished agent (if (codex-event-error event)
                                              (or (codex-event-text event) "The turn failed.")
                                              (or (codex-agent-last-text agent) ""))
                                    (codex-event-error event))))))

(defun codex-agent-exited (agent server code)
  (when (eq server (codex-agent-server agent))
    (setf (codex-agent-server agent) nil)
    (when (agent-busy agent)
      (let ((why (format nil "Codex exited (status ~a).~@[~%~a~]" code
                         (let ((s (agent-recent-stderr agent))) (and (plusp (length s)) s)))))
        (agent-emit agent :note :text why)
        (agent-turn-finished agent why t)))))

(defmethod agent-begin-turn ((agent codex-agent) prompt)
  (setf (codex-agent-last-text agent) nil)
  (let ((deliver (agent-deliver agent)))
    ;; Starting the app-server and the turn wait for replies: not on the GUI thread.
    (sb-thread:make-thread
     (lambda ()
       (handler-case
           (codex-start-turn (codex-agent-ensure-server agent) prompt :model (codex-agent-model agent))
         (error (e)
           (let ((why (princ-to-string e)))
             (funcall deliver (lambda () (agent-turn-finished agent why t)))))))
     :name "codex turn")))

(defmethod agent-interrupt ((agent codex-agent))
  (let ((server (codex-agent-server agent)))
    (when (codex-alive-p server)
      (sb-thread:make-thread (lambda () (ignore-errors (codex-interrupt server))) :name "codex interrupt"))))

(defmethod agent-shutdown ((agent codex-agent))
  (let ((server (codex-agent-server agent)))
    (setf (codex-agent-server agent) nil)
    (when server (stop-codex-app-server server))
    (when (agent-busy agent) (agent-turn-finished agent "Stopped." t))))

(defun codex-agent-new-conversation (agent)
  (agent-shutdown agent)
  (setf (codex-agent-thread-id agent) nil))
