;;;; Included by the GTK suite; exercise sidebar construction and folder inventory.
(in-package #:agent-duet)
(let* ((*settings* nil) (*controls* nil) (*checklist* nil)
       (*checklist-findings* nil) (*task-changes* nil)
       (book (make-workspace-sidebar)))
  (assert (= 4 (gtk:notebook-get-n-pages book)))
  (assert (eq book (control :checklist-panel)))
  (loop for index below 4
        for title in '("Tasks" "Changes" "Files" "Artifacts")
        for page = (gtk:notebook-get-nth-page book index)
        do (assert (equal title (gtk:label-get-text (gtk:notebook-get-tab-label book page)))))
  ;; The changes page must handle having no task, including repeated refreshes.
  (gtk:notebook-set-current-page book 1)
  (refresh-workspace-sidebar)
  (refresh-workspace-sidebar))
(let ((root (merge-pathnames (format nil "duet-sidebar-~a/" (make-uuid)) (uiop:temporary-directory))))
  (unwind-protect
       (progn
         (dolist (name '("src/main.lisp" "report.md" "figure.png" ".git/hidden.txt" "node_modules/skip.txt"))
           (let ((path (merge-pathnames name root)))
             (ensure-directories-exist path)
             (with-open-file (out path :direction :output) (write-string "test" out))))
         (let ((files (sidebar-files root)))
           (assert (equal '("figure.png" "report.md" "src/main.lisp") files))
           (assert (equal '("figure.png" "report.md") (remove-if-not #'sidebar-artifact-p files)))))
    (uiop:delete-directory-tree root :validate t)))
;; Embedding creates a widget rather than a standalone window, and disposal
;; remains safe before the background load has completed.
(multiple-value-bind (widget dispose)
    (show-changes-window *directory* nil nil :embedded t)
  (assert (typep widget 'gtk:paned))
  (assert (functionp dispose))
  (funcall dispose)
  (funcall dispose))
(format t "~&Workspace sidebar UI checks passed.~%")
