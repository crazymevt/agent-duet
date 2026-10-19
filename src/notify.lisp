;;;; notify.lisp — asynchronous desktop notifications

(in-package #:agent-duet)

(defvar *notifications-enabled* t)

(defun applescript-string (text)
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for char across text do
      (case char
        ((#\\ #\") (write-char #\\ out) (write-char char out))
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (t (write-char char out))))
    (write-char #\" out)))

(defun notification-command (title body &key sound
                              (platform #+darwin :macos #+linux :linux
                                        #-(or darwin linux) nil))
  "Build notification argv for PLATFORM without executing it."
  (case platform
    (:macos
     (list "osascript" "-e"
           (format nil "display notification ~a with title ~a~a"
                   (applescript-string body) (applescript-string title)
                   (if sound (format nil " sound name ~a"
                                     (applescript-string (if (stringp sound) sound "Glass"))) ""))))
    (:linux (append (list "notify-send")
                    (when sound (list "--hint=string:sound-name:message-new-instant"))
                    (list "--" title body)))))

(defun notify-user (title body &key sound)
  "Launch a desktop notification quietly and return NIL."
  (when *notifications-enabled*
    (ignore-errors
      (let ((command (notification-command title body :sound sound)))
        (when command
          (uiop:launch-program command :input nil :output nil :error-output nil)))))
  nil)
