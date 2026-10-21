;;;; Read-only per-file snapshot diffs.
(in-package #:agent-duet)

(defun show-changes-window (directory start end &key (title "Task changes") on-close)
  "Present a new, read-only window comparing START and END snapshots. Return its GTK window.
END may instead be a function returning a snapshot, or nil and why. Git runs off the
GTK thread: END and the file list load once the window is up, and each file's diff
when it's first selected. ON-CLOSE, if given, is called with END's snapshot (or nil)
once the window has closed."
  (let* ((changes nil)
         (loaded nil)                   ; the file list is in
         (failure nil)                  ; why it couldn't be loaded
         (closed nil)
         (end-snapshot (unless (functionp end) end))
         (shown nil)
         (selected nil)                 ; the change whose diff is wanted
         (window (make-instance 'gtk:window :title title :default-width 1100 :default-height 820))
         (filter (make-instance 'gtk:search-entry :placeholder-text "Filter changed files"
                                :margin-start 8 :margin-end 8 :margin-top 8 :margin-bottom 8))
         (files (make-instance 'gtk:list-box :selection-mode :single :css-classes '("navigation-sidebar")))
         (view (make-instance 'gtk:text-view :editable nil :cursor-visible nil :monospace t
                              :wrap-mode :none :left-margin 16 :right-margin 16 :top-margin 16 :bottom-margin 16))
         (buffer (gtk:text-view-get-buffer view)))
    ;; Translucent backgrounds retain the theme's text color in both modes.
    (preview-tag buffer "diff-add" :paragraph-background "rgba(46,160,67,0.22)")
    (preview-tag buffer "diff-remove" :paragraph-background "rgba(230,65,65,0.22)")
    (preview-tag buffer "diff-hunk" :paragraph-background "rgba(70,140,230,0.22)" :weight 700)
    (labels ((show-text (text) (gtk:text-buffer-set-text buffer text -1))
             (show-diff (change)
               (setf selected change)
               (cond ((null change)
                      (show-text (cond ((not loaded) "Loading changes…")
                                       (failure (format nil "Couldn't load the changes: ~a" failure))
                                       (changes "No matching files")
                                       (t "No changes in this task"))))
                     ((snapshot-change-%diff change) (insert-diff (snapshot-change-diff change)))
                     (t (show-text "Loading diff…")
                        (sb-thread:make-thread
                         (lambda ()
                           (let ((diff (handler-case (snapshot-change-diff change)
                                         (error (e) (format nil "Couldn't load the diff: ~a" e)))))
                             (deliver (lambda ()
                                        (when (and (not closed) (eq change selected))
                                          (insert-diff diff))))))
                         :name "changes diff"))))
             (insert-diff (diff)
               (show-text diff)
               (let ((offset 0) (in-hunk nil))
                 (dolist (line (uiop:split-string diff :separator '(#\Newline)))
                   (let ((tag (cond ((uiop:string-prefix-p "@@" line)
                                     (setf in-hunk t) "diff-hunk")
                                    ((and in-hunk (uiop:string-prefix-p "+" line)) "diff-add")
                                    ((and in-hunk (uiop:string-prefix-p "-" line)) "diff-remove"))))
                     (when tag
                       (gtk:text-buffer-apply-tag-by-name buffer tag (iter-at buffer offset)
                                                         (iter-at buffer (+ offset (length line)))))
                     (incf offset (1+ (length line)))))))
             (refill ()
               (let ((query (gtk:editable-get-text filter)))
                 ;; Clear before removing rows: GTK emits selection changes.
                 (setf shown nil)
                 (gtk:list-box-remove-all files)
                 (setf shown (remove-if-not
                              (lambda (change)
                                (or (search query (snapshot-change-path change) :test #'char-equal)
                                    (and (snapshot-change-old-path change)
                                         (search query (snapshot-change-old-path change) :test #'char-equal)))) changes))
                 (dolist (change shown)
                   (gtk:list-box-append files
                     (make-instance 'gtk:label :xalign 0.0 :ellipsize :middle
                                    :margin-start 8 :margin-end 8 :margin-top 8 :margin-bottom 8
                                    :tooltip-text (snapshot-change-path change)
                                    :label (format nil "~a  ~a~@[ ← ~a~]"
                                                   (ecase (snapshot-change-status change)
                                                     (:added "A") (:modified "M") (:deleted "D") (:renamed "R"))
                                                   (snapshot-change-path change) (snapshot-change-old-path change)))))
                 (if shown
                     (gtk:list-box-select-row files (gtk:list-box-get-row-at-index files 0))
                     (show-diff nil)))))
      (gobject:connect filter :search-changed (lambda (entry) (declare (ignore entry)) (refill)))
      (gobject:connect files :row-selected
                       (lambda (list row) (declare (ignore list))
                         (when row (show-diff (nth (gtk:list-box-row-get-index row) shown)))))
      (let ((paned (make-instance 'gtk:paned :orientation :horizontal :position 300
                                  :shrink-start-child nil :shrink-end-child nil)))
        (gtk:paned-set-start-child paned
          (gtk:build (gtk:box :orientation :vertical filter
                             (gtk:scrolled-window :vexpand t :hscrollbar-policy :never :child files))))
        (gtk:paned-set-end-child paned
          (make-instance 'gtk:scrolled-window :vexpand t :hexpand t :child view))
        (gtk:window-set-child window paned))
      (gobject:connect window :close-request
                       (lambda (w) (declare (ignore w))
                         (setf closed t)
                         (when (and on-close loaded) (funcall on-close end-snapshot))
                         nil))
      (refill)
      (sb-thread:make-thread
       (lambda ()
         (multiple-value-bind (list why)
             (handler-case
                 (multiple-value-bind (snapshot why) (if (functionp end) (funcall end) end-snapshot)
                   (setf end-snapshot snapshot)
                   (if snapshot
                       (snapshot-changes directory start snapshot)
                       (values nil (or why "the folder is no longer a git repository with a commit"))))
               (error (e) (values nil (princ-to-string e))))
           (deliver (lambda ()
                      (setf changes list failure why loaded t)
                      (if closed
                          (when on-close (funcall on-close end-snapshot))
                          (refill))))))
       :name "changes list"))
    (gtk:window-present window)
    window))

(defun show-commit-preview (root &key start workflow (message "Commit current changes") opening-state on-committed on-continue)
  "Select whole files and review the exact prospective staged diff before committing."
  (let* ((window (make-instance 'gtk:window :title (if on-continue "Uncommitted changes before starting" "Commit changes")
                                :default-width 1000 :default-height 780 :modal t :transient-for *window*))
         (body (make-instance 'gtk:box :orientation :vertical :spacing 8 :margin-start 12 :margin-end 12
                             :margin-top 12 :margin-bottom 12))
         (files (make-instance 'gtk:box :orientation :vertical :spacing 4))
         (view (make-instance 'gtk:text-view :editable nil :monospace t :wrap-mode :none))
         (entry (make-instance 'gtk:entry :text message :placeholder-text "Commit message"))
         (status (make-instance 'gtk:label :xalign 0.0 :wrap t :label "Loading changes…"))
         (all (make-instance 'gtk:button :label "Select all changes"))
         (task (make-instance 'gtk:button :label "Select task-changed files" :visible (and start t)))
         (commit (make-instance 'gtk:button :label (if on-continue "Commit and start task" "Commit selected files")
                                :sensitive nil :css-classes '("suggested-action")))
         (cancel (make-instance 'gtk:button :label "Cancel"))
         (continue (and on-continue (make-instance 'gtk:button :label "Continue without committing")))
         (commit-controls (make-instance 'gtk:box :orientation :vertical :spacing 8 :vexpand t))
         (default-paths nil)
         (rows nil) (preview nil) (generation 0) (closed nil) (busy nil))
    (labels ((say (text) (gtk:label-set-text status text))
             (refresh (&optional initial)
               (let ((serial (incf generation))
                     (paths (loop for (path . check) in rows when (gtk:check-button-get-active check) collect path)))
                 (setf preview nil)
                 (gtk:widget-set-sensitive commit nil)
                 (in-background "commit preview"
                   (lambda () (if initial (prepare-repository-commit root :start start)
                                  (prepare-repository-commit root :start start :paths paths)))
                   (lambda (result why)
                     (when (and (not closed) (= serial generation))
                       (if why
                           (progn (say why)
                                  (when (and initial on-continue)
                                    (gtk:widget-set-visible commit-controls nil)
                                    (gtk:widget-set-visible commit nil)))
                           (progn
                             (setf preview result)
                             (when initial
                               (setf default-paths (copy-list (getf result :paths)))
                               (dolist (path (getf result :all))
                                 (let ((check (make-instance 'gtk:check-button
                                                :label (format nil "~a~a" path
                                                               (if (member path (getf result :dirty) :test #'equal)
                                                                   "  (changed before task)" ""))
                                                :active (and (member path (getf result :paths) :test #'equal) t))))
                                   (push (cons path check) rows)
                                   (gtk:box-append files check)
                                   (gobject:connect check :toggled (lambda (w) (declare (ignore w)) (refresh))))))
                             (gtk:text-buffer-set-text (gtk:text-view-get-buffer view) (getf result :diff) -1)
                             (say (format nil "~d file~:p selected. Existing staging is preserved for unselected files."
                                          (length (getf result :paths))))
                             (gtk:widget-set-sensitive commit (and (getf result :paths) (plusp (length (getf result :diff))) (not busy) t)))))))))
             (select-files (paths)
               (dolist (row rows)
                 (gtk:check-button-set-active (cdr row) (and (member (car row) paths :test #'equal) t)))
               (refresh)))
      (gtk:box-append body (make-instance 'gtk:label :xalign 0.0 :wrap t
        :label (if on-continue
                   "This folder has uncommitted changes. Commit them before starting, continue with them, or cancel."
                   "Choose all changes or task-changed files, then adjust the selection. Each selected file includes all its edits, including edits made before or during the task. Previously dirty files start unselected.")))
      (gtk:box-append commit-controls (gtk:build (gtk:box :spacing 8 all task)))
      (gtk:box-append commit-controls (make-instance 'gtk:scrolled-window :height-request 150 :child files))
      (gtk:box-append commit-controls (make-instance 'gtk:scrolled-window :vexpand t :hexpand t :child view))
      (gtk:box-append commit-controls entry)
      (gtk:box-append body commit-controls)
      (gtk:box-append body status)
      (let ((buttons (gtk:build (gtk:box :spacing 8 :halign :end cancel))))
        (when continue (gtk:box-append buttons continue))
        (gtk:box-append buttons commit)
        (gtk:box-append body buttons))
      (gtk:window-set-child window body)
      (gobject:connect all :clicked (lambda (w) (declare (ignore w)) (when preview (select-files (getf preview :all)))))
      (gobject:connect task :clicked (lambda (w) (declare (ignore w)) (select-files default-paths)))
      (gobject:connect cancel :clicked (lambda (w) (declare (ignore w)) (unless busy (gtk:window-close window))))
      (when continue
        (gobject:connect continue :clicked
          (lambda (w) (declare (ignore w))
            (when (and opening-state (not busy))
              (let ((state opening-state))
                (gtk:window-close window)
                (funcall on-continue state))))))
      (gobject:connect commit :clicked
        (lambda (w) (declare (ignore w))
          (when (and preview (not busy) (not *committing*))
            (let ((selected preview) (message (gtk:editable-get-text entry)))
              (setf busy t *committing* t)
              (incf generation)
              (gtk:widget-set-sensitive body nil)
              (say "Committing…")
              (in-background "commit selected files"
                (lambda ()
                  (if workflow (commit-workflow-changes workflow :preview selected :message message)
                      (let ((hash (commit-repository-preview selected message)))
                        (list :status :committed :commit hash))))
                (lambda (result why)
                  (setf busy nil *committing* nil)
                  (unless closed
                    (gtk:widget-set-sensitive body t)
                    (if (and (not why) (eq (getf result :status) :committed))
                        (progn (gtk:window-close window) (when on-committed (funcall on-committed result)))
                        (progn (setf preview nil) (gtk:widget-set-sensitive commit nil)
                               (say (or why (getf result :message) "Commit failed. Reopen the preview to retry.")))))))))))
      (gobject:connect window :close-request
        (lambda (w) (declare (ignore w)) (if busy t (progn (setf closed t) nil))))
      (refresh t)
      (gtk:window-present window)
      window)))
