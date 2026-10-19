;;;; codex.lisp — Codex's local app-server protocol (from Cadre's codex/app-server.lisp)
;;;;
;;;; The app-server is a long-running JSON-RPC process over newline-delimited
;;;; stdin/stdout. Requests wait for their response; notifications become
;;;; CODEX-EVENTs, handed to DELIVER (the GTK main loop, in the app).
;;;;
;;;; Unlike Cadre, which gives Codex its own reviewed tools, here Codex uses
;;;; its native ones (shell commands, file changes) inside its sandbox.

(in-package #:agent-duet)

(defvar *codex-program* nil
  "The Codex CLI. Nil: `codex` on PATH, then the copy bundled with ChatGPT.app.")

(defvar *codex-model* nil
  "The Codex model ID. Nil uses the model Codex marks as its default.")

(defun find-codex-program ()
  "The Codex CLI executable, or nil."
  (if *codex-program*
      (and (probe-file *codex-program*) *codex-program*)
      (or (ignore-errors
           (let ((path (string-trim '(#\Newline #\Return #\Space #\Tab)
                                    (uiop:run-program '("/bin/sh" "-c" "command -v codex")
                                                      :output :string :error-output nil))))
             (and (plusp (length path)) path)))
          (some (lambda (path)
                  (and (probe-file path) (namestring path)))
                (list #P"/Applications/ChatGPT.app/Contents/Resources/codex-cli/CodexCLI.app/Contents/MacOS/codex"
                      (merge-pathnames "Applications/ChatGPT.app/Contents/Resources/codex-cli/CodexCLI.app/Contents/MacOS/codex"
                                       (user-homedir-pathname)))))))

(defstruct (codex-status (:conc-name codex-status-))
  program version logged-in error)

(defun codex-login-output-logged-in-p (output)
  "Whether CODEX LOGIN STATUS output positively indicates an active login."
  (and (search "logged in" output :test #'char-equal)
       (not (some (lambda (phrase) (search phrase output :test #'char-equal))
                  '("not logged in" "not authenticated" "authentication required")))))

(defun check-codex ()
  "Check whether Codex is installed and has a usable saved login. Slow:
call it off the GUI thread."
  (let ((program (find-codex-program)))
    (if (null program)
        (make-codex-status :error "The Codex CLI (codex) was not found.")
        (handler-case
            (let ((version (string-trim '(#\Newline #\Return #\Space)
                                        (uiop:run-program (list program "--version") :output :string
                                                          :error-output :output :ignore-error-status t))))
              (multiple-value-bind (login error-output exit-code)
                  (uiop:run-program (list program "login" "status") :output :string
                                    :error-output :output :ignore-error-status t)
                (declare (ignore error-output))
                (let ((ok (and (zerop exit-code) (codex-login-output-logged-in-p login))))
                  (make-codex-status :program program :version version :logged-in ok
                                     :error (unless ok (string-trim '(#\Newline #\Return #\Space) login))))))
          (error (e) (make-codex-status :program program :error (princ-to-string e)))))))

(defstruct (codex-event (:conc-name codex-event-))
  "A normalized app-server event. KIND is one of:
  :thread-started  THREAD-ID
  :text-delta      TEXT, more of a reply being written
  :text            TEXT, a finished agent message
  :activity        NAME (the item type), ID, TEXT: a command or file change starting
  :activity-completed  ID, TEXT (its output), ERROR
  :result          ERROR, TEXT (the error message), TURN-ID: the turn ended
  :approval        ID (the request's), NAME (its method), TEXT (what it asks), DATA: its params
  :rate-limits     DATA: a rate-limit snapshot
  :stderr, :protocol-error  TEXT"
  kind text name id input error thread-id turn-id data)

(defstruct (codex-pending (:constructor make-codex-pending ()))
  result error (done nil))

(defparameter *codex-request-timeout* 60
  "Maximum seconds to wait for an app-server JSON-RPC response.")

(defstruct (codex-app-server (:conc-name codex-))
  process program directory deliver on-event on-exit stdout-thread stderr-thread
  (thread-id nil) (turn-id nil) (stopped nil) (initialized nil)
  (next-id 0) (pending (make-hash-table :test 'equal))
  (lock (sb-thread:make-mutex :name "codex app-server"))
  (write-lock (sb-thread:make-mutex :name "codex app-server stdin")))

(defun codex-send-json (server object)
  (sb-thread:with-mutex ((codex-write-lock server))
    (let ((stream (sb-ext:process-input (codex-process server))))
      (unless (and stream (open-stream-p stream))
        (error "Codex app-server input is closed"))
      (write-line (json-string object) stream)
      (force-output stream))))

(defun codex-notify (server method &optional params)
  (codex-send-json server (jobj "jsonrpc" "2.0" "method" method
                                "params" (or params (jobj)))))

(defun codex-request (server method &optional params)
  "Send METHOD and wait at most *CODEX-REQUEST-TIMEOUT* seconds for its result."
  (let* ((id (sb-thread:with-mutex ((codex-lock server))
               (incf (codex-next-id server))))
         (pending (make-codex-pending))
         (deadline (+ (get-internal-real-time)
                      (round (* *codex-request-timeout* internal-time-units-per-second)))))
    (sb-thread:with-mutex ((codex-lock server))
      (setf (gethash id (codex-pending server)) pending))
    (handler-case
        (codex-send-json server (jobj "jsonrpc" "2.0" "id" id "method" method
                                      "params" (or params (jobj))))
      (error (e)
             (sb-thread:with-mutex ((codex-lock server))
               (remhash id (codex-pending server))
               (setf (codex-pending-error pending) e (codex-pending-done pending) t))))
    ;; Poll briefly instead of waiting on a condition queue with a timeout:
    ;; SBCL may return from a timed condition-wait without holding the mutex.
    (loop
     (when (sb-thread:with-mutex ((codex-lock server)) (codex-pending-done pending))
       (return))
     (when (>= (get-internal-real-time) deadline)
       (sb-thread:with-mutex ((codex-lock server))
         (unless (codex-pending-done pending)
           (remhash id (codex-pending server))
           (setf (codex-pending-error pending)
                 (format nil "request timed out after ~d seconds" *codex-request-timeout*)
                 (codex-pending-done pending) t)))
       (return))
     (sleep 0.01))
    (sb-thread:with-mutex ((codex-lock server))
      (when (codex-pending-error pending) (error "Codex app-server: ~a" (codex-pending-error pending)))
      (when (jget (codex-pending-result pending) "error")
        (error "Codex app-server ~a failed: ~a" method
               (jget (codex-pending-result pending) "error" "message")))
      (jget (codex-pending-result pending) "result"))))

(defun codex-deliver-event (server event)
  (when (codex-on-event server)
    (funcall (codex-deliver server)
             (lambda () (funcall (codex-on-event server) event)))))

(defun codex-finish-pending (server id object)
  (sb-thread:with-mutex ((codex-lock server))
    (let ((pending (gethash id (codex-pending server))))
      (when pending
        (remhash id (codex-pending server))
        (setf (codex-pending-result pending) object (codex-pending-done pending) t)))))

(defparameter *codex-approval-methods*
  '("item/commandExecution/requestApproval" "item/fileChange/requestApproval"))

(defun codex-server-request (server object)
  "Answer a request from the app-server. An approval request goes to the
client as an :approval event, answered later with CODEX-ANSWER-APPROVAL."
  (let ((method (jget object "method"))
        (params (jget object "params")))
    (if (member method *codex-approval-methods* :test #'equal)
        (codex-deliver-event server (make-codex-event :kind :approval :id (jget object "id") :name method
                                                      :text (codex-approval-summary method params)
                                                      :data params
                                                      :thread-id (jget params "threadId")
                                                      :turn-id (jget params "turnId")))
        (codex-send-json server (jobj "jsonrpc" "2.0" "id" (jget object "id")
                                      "error" (jobj "code" -32601
                                                    "message" (format nil "Unsupported request: ~a" method)))))))

(defun codex-approval-summary (method params)
  (if (equal method "item/commandExecution/requestApproval")
      (let ((command (jget params "command")))
        (format nil "$ ~a" (cond ((stringp command) command)
                                 ((vectorp command) (format nil "~{~a~^ ~}" (coerce command 'list)))
                                 (t "(a command)"))))
      (format nil "Change files~@[ under ~a~]" (let ((root (jget params "grantRoot"))) (and (stringp root) root)))))

(defun codex-answer-approval (server request-id decision)
  "Answer approval request REQUEST-ID with DECISION: :allow, :always (for
the rest of the session) or :deny."
  (codex-send-json server (jobj "jsonrpc" "2.0" "id" request-id
                                "result" (jobj "decision" (ecase decision
                                                            (:allow "accept")
                                                            (:always "acceptForSession")
                                                            (:deny "decline"))))))

(defun codex-item-summary (item)
  "A one-line description of a command or file change ITEM."
  (let ((type (jget item "type")))
    (cond ((equal type "commandExecution")
           (let ((command (jget item "command")))
             (format nil "$ ~a" (substitute #\Space #\Newline
                                            (if (stringp command) command (json-string command))))))
          ((equal type "fileChange")
           (format nil "Edit ~{~a~^, ~}"
                   (or (loop for change in (jlist (jget item "changes"))
                             for path = (jget change "path")
                             when (stringp path) collect (file-namestring path))
                       '("files"))))
          ((equal type "mcpToolCall") (format nil "~a.~a" (jget item "server") (jget item "tool")))
          ((equal type "webSearch") (format nil "Search: ~a" (or (jget item "query") "")))
          (t type))))

(defun codex-item-output (item)
  (let ((type (jget item "type")))
    (cond ((equal type "commandExecution")
           (let ((out (jget item "aggregatedOutput")))
             (format nil "~@[~a~]~@[~&(exit ~a)~]" (and (stringp out) out)
                     (and (integerp (jget item "exitCode")) (jget item "exitCode")))))
          ((equal type "fileChange")
           (format nil "~{~a~^~%~}"
                   (loop for change in (jlist (jget item "changes"))
                         for diff = (jget change "diff")
                         collect (format nil "~a~@[~%~a~]" (jget change "path") (and (stringp diff) diff)))))
          (t ""))))

(defparameter *codex-activity-types*
  '("commandExecution" "fileChange" "mcpToolCall" "webSearch"))

(defun codex-handle-notification (server object)
  (let* ((method (jget object "method"))
         (params (jget object "params"))
         (thread-id (jget params "threadId"))
         (turn-id (jget params "turnId")))
    (cond
     ((equal method "turn/started")
      (setf (codex-turn-id server) (jget params "turn" "id")))
     ((equal method "item/agentMessage/delta")
      (codex-deliver-event server (make-codex-event :kind :text-delta :text (jget params "delta")
                                                    :thread-id thread-id :turn-id turn-id)))
     ((equal method "item/started")
      (let ((item (jget params "item")))
        (when (member (jget item "type") *codex-activity-types* :test #'equal)
          (codex-deliver-event server (make-codex-event :kind :activity :name (jget item "type")
                                                        :id (jget item "id") :data item
                                                        :text (codex-item-summary item)
                                                        :thread-id thread-id :turn-id turn-id)))))
     ((equal method "item/completed")
      (let* ((item (jget params "item"))
             (type (jget item "type")))
        (cond ((equal type "agentMessage")
               (codex-deliver-event server (make-codex-event :kind :text :text (or (jget item "text") "")
                                                             :id (jget item "id")
                                                             :thread-id thread-id :turn-id turn-id)))
              ((member type *codex-activity-types* :test #'equal)
               (codex-deliver-event server (make-codex-event :kind :activity-completed
                                                             :id (jget item "id") :data item
                                                             :name (codex-item-summary item)
                                                             :text (codex-item-output item)
                                                             :error (member (jget item "status")
                                                                            '("failed" "declined" "cancelled")
                                                                            :test #'equal)
                                                             :thread-id thread-id :turn-id turn-id))))))
     ((equal method "turn/completed")
      (let* ((turn (jget params "turn"))
             (status (jget turn "status")))
        (setf (codex-turn-id server) nil)
        (codex-deliver-event server (make-codex-event :kind :result :error (not (equal status "completed"))
                                                      :text (let ((m (jget turn "error" "message")))
                                                              (if (stringp m) m
                                                                  (and (not (equal status "completed"))
                                                                       (format nil "Turn ~a" status))))
                                                      :turn-id (jget turn "id") :thread-id thread-id
                                                      :data turn))))
     ((equal method "account/rateLimits/updated")
      (codex-deliver-event server (make-codex-event :kind :rate-limits :data (jget params "rateLimits"))))
     ((equal method "error")
      (codex-deliver-event server (make-codex-event :kind :stderr
                                                    :text (format nil "~a" (or (jget params "error" "message")
                                                                               (json-string params))))))
     (t nil))))

(defun codex-handle-line (server line)
  (let ((object (ignore-errors (parse-json line))))
    (when (jobject-p object)
      (cond ((jget object "method")
             (if (jget object "id")
                 (codex-server-request server object)
                 (codex-handle-notification server object)))
            ((jget object "id") (codex-finish-pending server (jget object "id") object))))))

(defun codex-fail-pending (server reason)
  (sb-thread:with-mutex ((codex-lock server))
    (maphash (lambda (id pending)
               (declare (ignore id))
               (setf (codex-pending-error pending) reason (codex-pending-done pending) t))
             (codex-pending server))
    (clrhash (codex-pending server))))

(defun start-codex-app-server (program directory &key (deliver #'funcall) on-event on-exit)
  "Start the local Codex app-server in DIRECTORY."
  (let* ((process (sb-ext:run-program program '("app-server" "--listen" "stdio://")
                                      :search t :wait nil :directory (and directory (namestring directory))
                                      :input :stream :output :stream :error :stream
                                      :external-format '(:utf-8 :replacement #\?)))
         (server (make-codex-app-server :process process :program program :directory directory
                                        :deliver deliver :on-event on-event :on-exit on-exit)))
    ;; UTF-8 preserves Unicode in both directions. Replace malformed sequences
    ;; so a bad diagnostic cannot kill the RPC reader or stop stderr draining.
    (setf (codex-stderr-thread server)
          (sb-thread:make-thread
           (lambda ()
             (let ((stream (sb-ext:process-error process)))
               (loop for line = (ignore-errors (read-line stream nil))
                     while line
                     do (let ((line line))
                          (ignore-errors
                           (codex-deliver-event server (make-codex-event :kind :stderr :text line)))))))
           :name "codex app-server stderr"))
    (setf (codex-stdout-thread server)
          (sb-thread:make-thread
           (lambda ()
             (let ((stream (sb-ext:process-output process)))
               (loop for line = (ignore-errors (read-line stream nil))
                     while line
                     do (handler-case (codex-handle-line server line)
                          (error (e)
                                 (codex-deliver-event server (make-codex-event :kind :protocol-error
                                                                               :text (princ-to-string e)))))))
             (sb-ext:process-wait process)
             (codex-fail-pending server "the Codex app-server exited")
             (let ((exit-code (sb-ext:process-exit-code process)))
               (unwind-protect
                   ;; Notify before draining stderr: descendants may still
                   ;; hold that pipe open after the app-server has exited.
                   (unless (codex-stopped server)
                     (funcall deliver (lambda () (when on-exit (funcall on-exit exit-code)))))
                 (let ((stderr-thread (codex-stderr-thread server)))
                   (when (and stderr-thread (not (eq stderr-thread sb-thread:*current-thread*)))
                     (ignore-errors (sb-thread:join-thread stderr-thread :timeout 0.25 :default nil))
                     (when (sb-thread:thread-alive-p stderr-thread)
                       ;; This reader owns no state: release a blocked read
                       ;; before closing its stream.
                       (sb-thread:terminate-thread stderr-thread)
                       (ignore-errors (sb-thread:join-thread stderr-thread :timeout 0.25 :default nil)))))
                 (ignore-errors (sb-ext:process-close process)))))
           :name "codex app-server output"))
    server))

(defun codex-alive-p (server)
  (and server (not (codex-stopped server))
       (sb-ext:process-alive-p (codex-process server))
       (open-stream-p (sb-ext:process-input (codex-process server)))))

(defun codex-initialize-app-server (server)
  "Initialize SERVER once."
  (unless (codex-initialized server)
    (codex-request server "initialize"
                   (jobj "clientInfo" (jobj "name" "agent-duet" "title" "Agent Duet" "version" "0.1")
                         ;; The extended surface: rate limits, among others.
                         "capabilities" (jobj "experimentalApi" t)))
    (codex-notify server "initialized")
    (setf (codex-initialized server) t))
  server)

(defun codex-start-thread (server &key model instructions (sandbox "workspace-write") (approval-policy "never"))
  "Initialize the app-server and start a thread in its directory."
  (codex-initialize-app-server server)
  (let* ((result (codex-request server "thread/start"
                                (jobj "cwd" (uiop:native-namestring (codex-directory server))
                                      "model" (or model :null)
                                      "sandbox" sandbox
                                      "approvalPolicy" approval-policy
                                      "developerInstructions" (or instructions :null))))
         (thread-id (jget result "thread" "id")))
    (unless thread-id (error "Codex app-server did not return a thread id"))
    (setf (codex-thread-id server) thread-id)
    (codex-deliver-event server (make-codex-event :kind :thread-started :thread-id thread-id))
    thread-id))

(defun codex-start-turn (server text &key model effort approval-policy)
  "Start a user turn on SERVER's thread."
  (unless (codex-thread-id server) (error "Codex thread has not been started"))
  (codex-request server "turn/start"
                 (jobj "threadId" (codex-thread-id server)
                       "input" (vector (jobj "type" "text" "text" text))
                       "model" (or model :null)
                       "effort" (or effort :null)
                       "approvalPolicy" (or approval-policy :omit))))

;;; Models and usage

(defun codex-models-from-response (response)
  "The visible model choices in a `model/list` response."
  (remove-if (lambda (model) (eq (jget model "hidden") t))
             (jlist (jget response "data"))))

(defun codex-list-models (server)
  (codex-models-from-response (codex-request server "model/list" (jobj))))

(defun codex-model-efforts (model)
  "The reasoning efforts MODEL (from model/list) supports."
  (remove-duplicates
   (loop for entry in (jlist (jget model "supportedReasoningEfforts"))
         for effort = (if (stringp entry) entry (jget entry "reasoningEffort"))
         when (stringp effort) collect effort)
   :test #'equal :from-end t))

(defun codex-rate-limits-from-response (response)
  "Rate-limit buckets, (limit-id . snapshot)s, from `account/rateLimits/read`."
  (let ((buckets (jget response "rateLimitsByLimitId")))
    (cond ((and (jobject-p buckets) (cdr buckets)) (cdr buckets))
          ((jget response "rateLimits")
           (list (cons (or (jget response "rateLimits" "limitId") "codex")
                       (jget response "rateLimits"))))
          (t nil))))

(defun codex-read-rate-limits (server)
  (codex-rate-limits-from-response (codex-request server "account/rateLimits/read" (jobj))))

(defun merge-rate-limits (buckets snapshot)
  "BUCKETS with SNAPSHOT (from account/rateLimits/updated) in place of its bucket."
  (let ((id (or (jget snapshot "limitId") "codex")))
    (append (remove id buckets :key #'car :test #'equal) (list (cons id snapshot)))))

(defun rate-window-label (minutes)
  (cond ((and (integerp minutes) (zerop (mod minutes 1440))) (format nil "~dd" (/ minutes 1440)))
        ((and (integerp minutes) (zerop (mod minutes 60))) (format nil "~dh" (/ minutes 60)))
        ((integerp minutes) (format nil "~dm" minutes))
        (t nil)))

(defun codex-usage-summary (buckets)
  "What is left in each of BUCKETS' windows, as \"72% 5h / 40% 7d\", or nil."
  (usage-summary (codex-usage-windows buckets)))

(defun call-with-codex-server (program directory function)
  "Call FUNCTION with a fresh, initialized app-server, stopped afterwards."
  (let ((server (start-codex-app-server program directory)))
    (unwind-protect (funcall function (codex-initialize-app-server server))
      (stop-codex-app-server server))))

(defun codex-interrupt (server &key (thread-id (codex-thread-id server))
                               (turn-id (codex-turn-id server)))
  "Interrupt the given turn, by default the active one."
  (when (and thread-id turn-id)
    (codex-request server "turn/interrupt"
                   (jobj "threadId" thread-id "turnId" turn-id))))

(defun stop-codex-app-server (server)
  "Stop SERVER and wake any waiting JSON-RPC requests."
  (when server
    (setf (codex-stopped server) t)
    (codex-fail-pending server "the Codex app-server was stopped")
    (ignore-errors (close (sb-ext:process-input (codex-process server))))
    (when (sb-ext:process-alive-p (codex-process server))
      (sb-ext:process-kill (codex-process server) 15))))
