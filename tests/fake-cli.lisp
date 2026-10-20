;;;; Shared, dependency-free bootstrap for the fake CLI processes.
(require :asdf)
(load (merge-pathnames "../src/package.lisp" *load-truename*))
(load (merge-pathnames "../src/json.lisp" *load-truename*))
(in-package #:agent-duet)

(defun fake-send (object)
  (write-line (json-string object))
  (finish-output))
(defun fake-write (path text &optional append)
  (with-open-file (out path :direction :output :if-exists (if append :append :supersede)
                           :if-does-not-exist :create)
    (write-string text out)))
(defun fake-append (path text) (fake-write path (format nil "~a~%" text) t))
(defun fake-read ()
  (let ((line (read-line *standard-input* nil)))
    (and line (parse-json line))))
(defun fake-text (message) (jget message "message" "content" 0 "text"))
(defun fake-turn-finished (&optional (text "hi") (id "m1"))
  (fake-send (jobj "method" "item/completed" "params"
                   (jobj "item" (jobj "type" "agentMessage" "id" id "text" text))))
  (fake-send (jobj "method" "turn/completed" "params"
                   (jobj "turn" (jobj "id" "turn-1" "status" "completed")))))
