;;;; Fake stream-json Claude, implemented in the same Lisp as the app.
(load (merge-pathnames "fake-cli.lisp" *load-truename*))
(in-package #:agent-duet)

(defvar *fake-args* (rest sb-ext:*posix-argv*))
(defun fake-argument (flag) (second (member flag *fake-args* :test #'equal)))
(fake-append "args" (format nil "~{~a~^ ~}" *fake-args*))

(defun fake-finish-permission (behavior request)
  (fake-append "permission-answers" (json-string (jobj "behavior" behavior)))
  (when (and (equal behavior "allow") (equal (jget request "tool_name") "Bash")
             (probe-file "execute-command"))
    (multiple-value-bind (out err)
        (uiop:run-program (list "/bin/sh" "-c" (jget request "input" "command"))
                          :output :string :error-output :string :ignore-error-status t)
      (fake-write "command-output" (concatenate 'string out err))))
  behavior)

(defun fake-ask-permission ()
  (let* ((request (parse-json (uiop:read-file-string "permission-request")))
         (settings (and (fake-argument "--settings") (parse-json (fake-argument "--settings")))))
    ;; Hooks precede inherited approvals, just as they do in the real CLI.
    (loop for entry across (or (jget settings "hooks" "PreToolUse") #()) do
      (loop for hook across (jget entry "hooks") do
        (multiple-value-bind (out err code)
            (uiop:run-program (list "/bin/sh" "-c" (jget hook "command"))
                              :input (make-string-input-stream
                                      (json-string (jobj "tool_name" (jget request "tool_name")
                                                        "tool_input" (jget request "input"))))
                              :output :string :error-output :string :ignore-error-status t)
          (declare (ignore err))
          (let* ((decision (if (zerop code) (jget (parse-json out) "hookSpecificOutput")
                               (jobj "permissionDecision" "deny")))
                 (behavior (jget decision "permissionDecision")))
            (fake-append "hook-decisions" (json-string decision))
            (when (member behavior '("allow" "deny") :test #'equal)
              (when (jget decision "updatedInput")
                (setf (cdr (assoc "input" (cdr request) :test #'equal)) (jget decision "updatedInput")))
              (return-from fake-ask-permission (fake-finish-permission behavior request)))))))
    (when (probe-file "inherited-allow")
      (return-from fake-ask-permission (fake-finish-permission "allow" request)))
    (fake-send (jobj "type" "control_request" "request_id" "perm-1" "request"
                     (jobj "subtype" "can_use_tool" "tool_name" (jget request "tool_name")
                           "input" (jget request "input"))))
    (loop for message = (fake-read) while message
          when (equal (jget message "type") "control_response") do
            (let ((response (jget message "response" "response")))
              (fake-append "permission-answers" (json-string response))
              (return (jget response "behavior"))))))

(let* ((resume (fake-argument "--resume"))
       (sessions (and (probe-file "sessions") (uiop:read-file-lines "sessions")))
       (session (or resume (fake-argument "--session-id") "new")))
  (when (and resume (not (member resume sessions :test #'equal)))
    (fake-read)
    (when (probe-file "expect-steer") (fake-read))
    (fake-send (jobj "type" "result" "subtype" "error_during_execution" "is_error" t
                     "session_id" session "errors"
                     (vector (concatenate 'string "No conversation found with session ID: " session))))
    (uiop:quit 0))
  (unless resume (fake-append "sessions" session))
  (loop for message = (fake-read) while message do
    (when (equal (jget message "type") "user")
      (let ((reply "fresh reply"))
        (when (probe-file "expect-steer")
          (setf reply (format nil "~a: ~a + ~a" reply (fake-text message) (fake-text (fake-read)))))
        (fake-send (jobj "type" "system" "subtype" "init" "session_id" session))
        (when (probe-file "permission-request")
          (setf reply (format nil "~a (~a)" reply (fake-ask-permission))))
        (fake-send (jobj "type" "result" "subtype" "success" "is_error" :false
                         "result" reply "session_id" session))))))
