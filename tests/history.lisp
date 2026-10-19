;;;; history.lisp — the headless conversation store

(in-package #:agent-duet/tests)

(defmacro with-history-directory (&body body)
  `(let ((ai::*history-directory*
           (merge-pathnames (format nil "duet-history-~a/" (ai::make-uuid)) (uiop:temporary-directory))))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ai::*history-directory* :validate t :if-does-not-exist :ignore))))

(defun saved-history (directory title updated)
  (let ((history (ai::make-history directory)))
    (ai::history-add-entry history :user :prompt title)
    (setf (ai::history-updated history) updated)
    (ai::save-history history)
    history))

(define-test history-round-trip
    :parent agent-duet
  (with-history-directory
    (let ((history (ai::make-history "/project/")))
      (setf (ai::history-handle history :claude) "session"
            (ai::history-handle history :codex) "thread")
      (loop for who in '(:user :claude :codex :middle :claude)
            for kind in '(:prompt :text :tool :note :error)
            do (ai::history-add-entry history who kind "hello λ"))
      (ai::save-history history)
      ;; Saving again replaces an existing file.
      (ai::save-history history)
      (let ((loaded (ai::load-history (ai::history-id history))))
        (true loaded)
        (is equal (ai::history-entries history) (ai::history-entries loaded))
        (is equal (ai::history-handles history) (ai::history-handles loaded))
        (is string= "/project/" (ai::history-directory loaded))
        (is = (ai::history-created history) (ai::history-created loaded))
        (is = (ai::history-updated history) (ai::history-updated loaded))
        (is string= "session" (ai::history-handle loaded :claude))
        (is string= "thread" (ai::history-handle loaded :codex))))))

(define-test history-title-and-empty
    :parent agent-duet
  (with-history-directory
    (let ((history (ai::make-history "/project"))
          (prompt (concatenate 'string (format nil "first~%line~c" #\Return) (make-string 100 :initial-element #\x))))
      (ai::save-history history)
      (false (probe-file (ai::history-file (ai::history-id history))))
      (ai::history-add-entry history :middle :note "not the title")
      (false (ai::history-title history))
      (ai::history-add-entry history :user :prompt prompt)
      (is = 80 (length (ai::history-title history)))
      (false (find #\Newline (ai::history-title history)))
      (false (find #\Return (ai::history-title history)))
      (true (search "first line" (ai::history-title history)))
      (let ((title (ai::history-title history)))
        (ai::history-add-entry history :user :prompt "later")
        (is string= title (ai::history-title history))))))

(define-test history-list-and-delete
    :parent agent-duet
  (with-history-directory
    (let* ((old (saved-history "/a/" "old" 10))
           (new (saved-history "/a" "new" 30))
           (other (saved-history "/b/" "other" 20)))
      (with-open-file (out (ai::history-file "corrupt") :direction :output)
        (write-string "not JSON" out))
      (false (ai::load-history "corrupt"))
      (false (ai::load-history "missing"))
      (false (ai::load-history "../outside"))
      (is equal (list (ai::history-id new) (ai::history-id other) (ai::history-id old))
          (mapcar (lambda (s) (getf s :id)) (ai::list-histories)))
      (is equal '("new" "old")
          (mapcar (lambda (s) (getf s :title)) (ai::list-histories :directory "/a")))
      (is = 1 (getf (first (ai::list-histories)) :entry-count))
      (is = 1 (length (ai::list-histories :limit 1)))
      (ai::delete-history (ai::history-id new))
      (false (ai::load-history (ai::history-id new)))
      (is = 2 (length (ai::list-histories))))))

(define-test history-prune
    :parent agent-duet
  (with-history-directory
    (saved-history "/a" "old" 10)
    (saved-history "/b" "middle" 20)
    (saved-history "/a" "new" 30)
    (ai::prune-histories :keep 2)
    (is equal '("new" "middle") (mapcar (lambda (s) (getf s :title)) (ai::list-histories)))
    (with-open-file (out (ai::history-file "corrupt") :direction :output)
      (write-string "bad" out))
    (ai::prune-histories :keep 0)
    (false (probe-file (ai::history-file "corrupt")))
    (false (ai::list-histories))))

(define-test history-note-before-prompt
    :parent agent-duet
  (with-history-directory
    (let ((history (ai::make-history "/a")))
      (ai::history-add-entry history :middle :note "starting")
      (ai::save-history history)
      (let ((loaded (ai::load-history (ai::history-id history))))
        (false (ai::history-title loaded))
        (ai::history-add-entry loaded :user :prompt "first prompt")
        (is string= "first prompt" (ai::history-title loaded)))
      (let* ((json (ai::history-json history))
             (entry (aref (ai::jget json "entries") 0)))
        (setf (cdr (assoc "kind" (cdr entry) :test #'equal)) "unknown-kind")
        (with-open-file (out (ai::history-file (ai::history-id history))
                             :direction :output :if-exists :supersede)
          (write-string (ai::json-string json) out))
        (false (ai::load-history (ai::history-id history)))))))
