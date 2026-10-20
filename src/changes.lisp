;;;; Read-only per-file snapshot diffs.
(in-package #:agent-duet)

(defun show-changes-window (directory start end &key (title "Task changes"))
  "Present a new, read-only window comparing START and END snapshots. Return its GTK window."
  (let* ((changes (snapshot-changes directory start end))
         (shown nil)
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
    (labels ((show-diff (change)
               (gtk:text-buffer-set-text buffer (if change (snapshot-change-diff change)
                                                   (if changes "No matching files" "No changes in this task")) -1)
               (when change
                 (let ((offset 0) (in-hunk nil))
                   (dolist (line (uiop:split-string (snapshot-change-diff change) :separator '(#\Newline)))
                     (let ((tag (cond ((uiop:string-prefix-p "@@" line)
                                       (setf in-hunk t) "diff-hunk")
                                      ((and in-hunk (uiop:string-prefix-p "+" line)) "diff-add")
                                      ((and in-hunk (uiop:string-prefix-p "-" line)) "diff-remove"))))
                       (when tag
                         (gtk:text-buffer-apply-tag-by-name buffer tag (iter-at buffer offset)
                                                           (iter-at buffer (+ offset (length line)))))
                       (incf offset (1+ (length line))))))))
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
      (refill))
    (gtk:window-present window)
    window))
