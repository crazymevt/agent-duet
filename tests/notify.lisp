;;;; notify.lisp — notification command construction

(in-package #:agent-duet/tests)

(define-test notification-commands
    :parent agent-duet
  (is equal '("notify-send" "--" "Title" "Body")
      (ai::notification-command "Title" "Body" :platform :linux))
  (is equal '("osascript" "-e" "display notification \"Body\" with title \"Title\"")
      (ai::notification-command "Title" "Body" :platform :macos))
  (is string= "\"a\\\"b\\\\c\"" (ai::applescript-string "a\"b\\c"))
  (is string= "\"a\\nb\\rc\"" (ai::applescript-string (format nil "a~%b~cc" #\Return)))
  (is equal '("osascript" "-e" "display notification \"B\" with title \"T\" sound name \"Glass\"")
      (ai::notification-command "T" "B" :platform :macos :sound t))
  (is equal '("notify-send" "--hint=string:sound-name:message-new-instant" "--" "T" "B")
      (ai::notification-command "T" "B" :platform :linux :sound t))
  (false (ai::notification-command "T" "B" :platform :unknown))
  (let ((ai::*notifications-enabled* nil))
    (false (ai::notify-user "T" "B"))))
