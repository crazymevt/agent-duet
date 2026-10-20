;;;; claude.lisp — driving the Claude Code CLI (from Cadre's claude/cli.lisp)
;;;;
;;;; A conversation is one `claude -p` process with stream-json input and
;;;; output. User messages go to its stdin, one JSON line each; every line
;;;; it prints is an event, turned here into a CLAUDE-EVENT. Lines are read
;;;; on a thread and handed to DELIVER (the GTK main loop, in the app).

(in-package #:agent-duet)

(defvar *claude-program* nil
  "The Claude Code CLI. Nil: `claude` on PATH, then ~/.local/bin/claude,
then the copy inside the Claude desktop app.")

(defvar *claude-model* "sonnet"
  "The model for Claude: an alias (sonnet, opus, haiku) or a full name.")

(defparameter *claude-models* '("sonnet" "opus" "haiku"))

(defun claude-startup-models (selected)
  "The provisional catalog, including a previously selected model."
  (if (and selected (not (member selected *claude-models* :test #'equal)))
      (cons selected *claude-models*)
      *claude-models*))

(defun claude-model-id (model)
  (if (stringp model) model (jget model "value")))

(defun claude-model-label (model)
  "The CLI's versioned model name, without pricing or other description text."
  (if (stringp model)
      (string-capitalize model)
      (let ((name (or (jget model "displayName")
                      (jget model "resolvedModel")
                      (claude-model-id model))))
        (if (equal (claude-model-id model) "default")
            (format nil "~a~@[ (~a)~]" name (jget model "resolvedModel"))
            name))))

(defun claude-list-models (program)
  "Read the CLI's initialization catalog without submitting a model turn."
  (let ((ready (sb-thread:make-semaphore))
        (models nil)
        (cp nil))
    (unwind-protect
        (progn
          (setf cp (start-claude
                    (append (claude-arguments) '("--safe-mode" "--strict-mcp-config"))
                    :program program :directory (uiop:temporary-directory)
                    :on-event
                    (lambda (event)
                      (let ((object (event-data event)))
                        (when (and (equal (jget object "type") "control_response")
                                   (equal (jget object "response" "request_id") "models"))
                          (setf models (remove-if-not #'claude-model-id
                                                      (jlist (jget object "response" "response" "models"))))
                          (sb-thread:signal-semaphore ready))))
                    :on-exit (lambda (code) (declare (ignore code)) (sb-thread:signal-semaphore ready))))
          (claude-write-line cp (json-string (jobj "type" "control_request" "request_id" "models"
                                                   "request" (jobj "subtype" "initialize"))))
          (sb-thread:wait-on-semaphore ready :timeout 10)
          models)
      (when cp (stop-claude cp)))))

;;; Random identifiers

(defun random-octets (n)
  (let ((octets (make-array n :element-type '(unsigned-byte 8))))
    (or (ignore-errors
         (with-open-file (in "/dev/urandom" :element-type '(unsigned-byte 8))
           (read-sequence octets in)
           octets))
        (let ((state (make-random-state t)))
          (dotimes (i n octets) (setf (aref octets i) (random 256 state)))))))

(defun make-uuid ()
  "A random (version 4) UUID string."
  (let ((o (random-octets 16)))
    (setf (aref o 6) (logior #x40 (logand (aref o 6) #x0f))
          (aref o 8) (logior #x80 (logand (aref o 8) #x3f)))
    (format nil "~(~{~2,'0x~}-~{~2,'0x~}-~{~2,'0x~}-~{~2,'0x~}-~{~2,'0x~}~)"
            (coerce (subseq o 0 4) 'list) (coerce (subseq o 4 6) 'list) (coerce (subseq o 6 8) 'list)
            (coerce (subseq o 8 10) 'list) (coerce (subseq o 10 16) 'list))))

(defun random-token ()
  (string-downcase (format nil "~{~2,'0x~}" (coerce (random-octets 24) 'list))))

;;; Finding the CLI

(defun desktop-app-claude ()
  "The newest Claude Code bundled with the Claude desktop app (macOS), or nil."
  (let ((root (merge-pathnames "Library/Application Support/Claude/claude-code/" (user-homedir-pathname))))
    (car (sort (directory (merge-pathnames "*/*/claude.app/Contents/MacOS/claude" root))
               #'string> :key #'namestring))))

(defun find-claude-program ()
  "The path of the Claude Code CLI, or nil."
  (if *claude-program*
      (and (probe-file *claude-program*) *claude-program*)
      (let ((on-path (ignore-errors
                      (string-trim '(#\Newline #\Space)
                                   (uiop:run-program '("/bin/sh" "-c" "command -v claude")
                                                     :output :string :ignore-error-status t)))))
        (or (and on-path (plusp (length on-path)) on-path)
            (loop for candidate in '(".local/bin/claude" ".claude/local/claude")
                  for path = (merge-pathnames candidate (user-homedir-pathname))
                  when (probe-file path) return (namestring path))
            (let ((app (desktop-app-claude))) (and app (namestring app)))))))

(defstruct (claude-status (:conc-name claude-status-))
  "What Cadre found out about the CLI."
  program version logged-in auth-method error
  detail)                               ; what `auth status` printed, when not signed in

(defun check-claude ()
  "Run the CLI's --version and auth status. Slow: call it off the GUI thread."
  (let ((program (find-claude-program)))
    (if (null program)
        (make-claude-status :error "The Claude Code CLI (claude) was not found.")
        (handler-case
            (let* ((version (string-trim '(#\Newline #\Space)
                                         (uiop:run-program (list program "--version") :output :string
                                                           :ignore-error-status t)))
                   (output (uiop:run-program (list program "auth" "status") :output :string
                                             :error-output :output
                                             :ignore-error-status t))
                   ;; From the first brace: stderr notices can come first.
                   (auth (let ((start (position #\{ output)))
                           (and start (ignore-errors (parse-json (subseq output start))))))
                   (logged-in (jtrue-p (jget auth "loggedIn"))))
              (make-claude-status :program program :version version
                                  :logged-in logged-in
                                  :auth-method (jget auth "authMethod")
                                  :detail (unless logged-in (string-trim '(#\Newline #\Space) output))))
          (error (e) (make-claude-status :program program :error (princ-to-string e)))))))

;;; The command line

(defun claude-arguments (&key session-id resume model effort mcp-config permission-tool permission-mode
                              allowed-tools disallowed-tools system-prompt isolated
                              tools settings setting-sources strict-mcp-config)
  "The arguments for a stream-json conversation (after the program name)."
  (append (list "-p" "--input-format" "stream-json" "--output-format" "stream-json"
                "--verbose" "--include-partial-messages")
          (cond (resume (list "--resume" resume))
                (session-id (list "--session-id" session-id)))
          (and model (list "--model" model))
          (and effort (list "--effort" effort))
          (and permission-mode (list "--permission-mode" permission-mode))
          (and mcp-config (list "--mcp-config" (namestring mcp-config)))
          (and permission-tool (list "--permission-prompt-tool" permission-tool))
          (and disallowed-tools (list "--disallowedTools" (format nil "~{~a~^,~}" disallowed-tools)))
          (and allowed-tools (list "--allowedTools" (format nil "~{~a~^,~}" allowed-tools)))
          (and tools (list "--tools" (format nil "~{~a~^,~}" tools)))
          (and settings (list "--settings" settings))
          (and setting-sources (list "--setting-sources" setting-sources))
          (and strict-mcp-config (list "--strict-mcp-config"))
          (and system-prompt (list "--append-system-prompt" system-prompt))
          (and isolated (list "--strict-mcp-config" "--setting-sources" "project"))))

(defun base64-encode (octets)
  "OCTETS (a vector of bytes) in base64."
  (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
        (n (length octets)))
    (with-output-to-string (out)
      (loop for i from 0 below n by 3
            do (let* ((count (min 3 (- n i)))
                      (bits (loop for j below 3
                                  sum (ash (if (< j count) (aref octets (+ i j)) 0) (* 8 (- 2 j))))))
                 (dotimes (j 4)
                   (write-char (if (<= j count) (char alphabet (ldb (byte 6 (* 6 (- 3 j))) bits)) #\=)
                               out)))))))

(defun image-media-type (path)
  "The media type of an image file Claude can be shown, from its name, or nil."
  (let ((type (string-downcase (or (pathname-type (pathname path)) ""))))
    (cdr (assoc type '(("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
                       ("gif" . "image/gif") ("webp" . "image/webp"))
                :test #'string=))))

(defun read-file-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun image-block (path)
  "A content block showing the image at PATH."
  (jobj "type" "image"
        "source" (jobj "type" "base64" "media_type" (image-media-type path)
                       "data" (base64-encode (read-file-octets path)))))

(defun user-message-line (text &optional images)
  "The stdin line that sends TEXT as a user message, with IMAGES (paths) attached."
  (json-string (jobj "type" "user"
                     "message" (jobj "role" "user"
                                     "content" (coerce (cons (jobj "type" "text" "text" text)
                                                             (mapcar #'image-block images))
                                                       'vector)))))

(defun interrupt-line (request-id)
  (json-string (jobj "type" "control_request" "request_id" request-id
                     "request" (jobj "subtype" "interrupt"))))

;;; Events

(defstruct (claude-event (:conc-name event-))
  "One line from the CLI, simplified. KIND is one of:
  :init      SESSION-ID, MODEL, and DATA: the MCP servers' (name . status)
  :text-delta  TEXT, more of the reply being written
  :tool-start  NAME, ID: a tool call being written
  :message-start
  :text      TEXT, a finished block of the reply
  :thinking  TEXT
  :tool-use  NAME, ID, INPUT (a JSON object)
  :tool-result  ID, TEXT, ERROR
  :result    TEXT, ERROR, COST, DURATION, SESSION-ID, DATA: usage
  :permission  ID (the request's), NAME, INPUT, TEXT (why), DATA: the request
  :rate-limit  DATA: the account's usage (rate_limit_info)
  :other     DATA: the JSON"
  kind text name id input error cost duration session-id model data subagent queued-turn-count)

(defun content-text (content)
  "A tool result's content (a string or an array of blocks) as text."
  (cond ((stringp content) content)
        ((vectorp content)
         (format nil "~{~a~^~%~}" (loop for block across content
                                        when (equal (jget block "type") "text") collect (jget block "text"))))
        (t "")))

(defun parse-claude-event (line)
  "The CLAUDE-EVENTs for one line of the CLI's stream-json output: a list,
since a message can hold several blocks. Nil for lines that are not JSON."
  (let ((json (ignore-errors (parse-json line))))
    (when (jobject-p json)
      (let ((type (jget json "type"))
            (subagent (jtrue-p (jget json "parent_tool_use_id"))))
        (flet ((event (&rest args) (apply #'make-claude-event :subagent subagent args)))
          (cond
           ((and (equal type "system") (equal (jget json "subtype") "init"))
            (list (event :kind :init :session-id (jget json "session_id") :model (jget json "model")
                         :data (loop for server in (jlist (jget json "mcp_servers"))
                                     collect (cons (jget server "name") (jget server "status"))))))
           ((equal type "stream_event")
            (let* ((e (jget json "event")) (etype (jget e "type")))
              (cond ((equal etype "message_start") (list (event :kind :message-start)))
                    ((and (equal etype "content_block_delta") (equal (jget e "delta" "type") "text_delta"))
                     (list (event :kind :text-delta :text (jget e "delta" "text"))))
                    ((and (equal etype "content_block_start") (equal (jget e "content_block" "type") "tool_use"))
                     (list (event :kind :tool-start :name (jget e "content_block" "name")
                                  :id (jget e "content_block" "id")))))))
           ((equal type "assistant")
            (loop for block in (jlist (jget json "message" "content"))
                  for btype = (jget block "type")
                  when (equal btype "text") collect (event :kind :text :text (jget block "text"))
                  when (equal btype "thinking") collect (event :kind :thinking :text (jget block "thinking"))
                  when (equal btype "tool_use")
                  collect (event :kind :tool-use :name (jget block "name") :id (jget block "id")
                                 :input (jget block "input"))))
           ((equal type "user")
            (loop for block in (jlist (jget json "message" "content"))
                  when (equal (jget block "type") "tool_result")
                  collect (event :kind :tool-result :id (jget block "tool_use_id")
                                 :text (content-text (jget block "content"))
                                 :error (jtrue-p (jget block "is_error")))))
           ((equal type "result")
            (list (event :kind :result :text (let ((r (jget json "result")))
                                               ;; A failure before any reply says why in "errors".
                                               (cond ((and (stringp r) (plusp (length r))) r)
                                                     ((jlist (jget json "errors"))
                                                      (format nil "~{~a~^~%~}" (remove-if-not #'stringp (jlist (jget json "errors")))))
                                                     (t "")))
                         :error (or (jtrue-p (jget json "is_error"))
                                    (not (equal (jget json "subtype") "success")))
                         :queued-turn-count (jget json "queued_turn_count")
                         :cost (jget json "total_cost_usd") :duration (jget json "duration_ms")
                         :session-id (jget json "session_id") :data (jget json "usage"))))
           ((equal type "rate_limit_event")
            (list (event :kind :rate-limit :data (jget json "rate_limit_info"))))
           ;; With --permission-prompt-tool stdio, a tool that needs
           ;; permission asks here; CLAUDE-ANSWER-PERMISSION answers.
           ((and (equal type "control_request") (equal (jget json "request" "subtype") "can_use_tool"))
            (list (event :kind :permission :id (jget json "request_id")
                         :name (jget json "request" "tool_name") :input (jget json "request" "input")
                         :text (let ((d (or (jget json "request" "description")
                                            (jget json "request" "decision_reason"))))
                                 (and (stringp d) d))
                         :data (jget json "request"))))
           (t (list (event :kind :other :data json)))))))))

;;; The process

(defstruct (claude-process (:conc-name cp-))
  process session-id (stopped nil) (lock (sb-thread:make-mutex :name "claude stdin")))

(defun start-claude (arguments &key program directory (deliver #'funcall) on-event on-stderr on-exit
                               environment)
  "Start the CLI with ARGUMENTS in DIRECTORY. Through DELIVER, calls ON-EVENT
with each CLAUDE-EVENT, ON-STDERR with each line of standard error, and
ON-EXIT with the exit code."
  (let* ((process (sb-ext:run-program (or program (find-claude-program) (error "Claude Code was not found"))
                                      arguments
                                      :search t :wait nil :directory (and directory (namestring directory))
                                      :input :stream :output :stream :error :stream
                                      :environment (append environment (sb-ext:posix-environ))
                                      :external-format :utf-8))
         (cp (make-claude-process :process process)))
    (watch-cli-output
     process
     (lambda ()
       (let ((stream (sb-ext:process-output process)))
         (loop for line = (read-line stream nil)
               while line
               do (let ((events (parse-claude-event line)))
                    (when events
                      (funcall deliver (lambda () (dolist (e events) (when on-event (funcall on-event e))))))))))
     (lambda (code)
       (funcall deliver (lambda () (when on-exit (funcall on-exit code))))))
    (sb-thread:make-thread
     (lambda ()
       (let ((stream (sb-ext:process-error process)))
         (loop for line = (ignore-errors (read-line stream nil))
               while line
               do (let ((line line)) (funcall deliver (lambda () (when on-stderr (funcall on-stderr line))))))))
     :name "claude stderr")
    cp))

(defun claude-alive-p (cp)
  "Whether CP can take messages: running, not stopped, its input open. (A
stopped process may run a little longer, finishing up.)"
  (and cp (not (cp-stopped cp))
       (sb-ext:process-alive-p (cp-process cp))
       (open-stream-p (sb-ext:process-input (cp-process cp)))))

(defun claude-write-line (cp line &optional wanted)
  "Write LINE to CP's input; with WANTED, only if it still returns true once
the input is ours (so nothing can come between it and the write). True if written."
  (sb-thread:with-mutex ((cp-lock cp))
    (when (or (null wanted) (funcall wanted))
      (let ((stream (sb-ext:process-input (cp-process cp))))
        (write-line line stream)
        (force-output stream)
        t))))

(defun claude-send (cp text &optional images wanted)
  "Send TEXT as the next user message, with IMAGES (paths) attached; with
WANTED, only if it's still wanted (see CLAUDE-WRITE-LINE)."
  (claude-write-line cp (user-message-line text images) wanted))

(defun session-rule (suggestion)
  "SUGGESTION, a permission suggestion from the CLI, kept for this session only."
  (cons :object (cons (cons "destination" "session")
                      (remove "destination" (cdr suggestion) :key #'car :test #'equal))))

(defun permission-response-line (request-id allow &key input always-rules message)
  "The stdin line answering permission request REQUEST-ID. ALWAYS-RULES, the
request's permission_suggestions, are added for the rest of the session."
  (json-string
   (jobj "type" "control_response"
         "response" (jobj "subtype" "success" "request_id" request-id
                          "response" (if allow
                                         (jobj "behavior" "allow"
                                               "updatedInput" (or input (jobj))
                                               "updatedPermissions" (if always-rules
                                                                        (map 'vector #'session-rule always-rules)
                                                                        :omit))
                                         (jobj "behavior" "deny"
                                               "message" (or message "The user denied this.")))))))

(defun claude-answer-permission (cp request-id allow &rest options)
  "Answer the CLI's permission request REQUEST-ID (see PERMISSION-RESPONSE-LINE)."
  (claude-write-line cp (apply #'permission-response-line request-id allow options)))

(defun claude-interrupt (cp)
  "Ask the CLI to stop the current turn."
  (ignore-errors (claude-write-line cp (interrupt-line (format nil "duet-~d" (random 1000000))))))

(defun stop-claude (cp)
  "End the conversation's process."
  (when (claude-alive-p cp)
    (setf (cp-stopped cp) t)
    ;; Without flushing: a message being written in the background (see
    ;; CLAUDE-AGENT-SEND) can fill the pipe, and a flush would wait on it.
    (ignore-errors (close (sb-ext:process-input (cp-process cp)) :abort t))
    (sb-ext:process-kill (cp-process cp) 15)))
