;;;; preview.lisp — the Documents window: Markdown in the folder, previewed
;;;;
;;;; A list of the folder's Markdown files on the left (filter it by typing),
;;;; the chosen one rendered on the right: headings, emphasis, code, lists
;;;; and task lists, quotes, tables, rules, local images and links. Read
;;;; only. It is drawn again when the file changes, so a design document an
;;;; agent is writing can be watched as it grows. Drawing is Cadre's preview
;;;; (ui/markdown.lisp), on markdown.lisp's blocks.

(in-package #:agent-integration)

(defstruct docs
  window list search view title
  (files '())                           ; the folder's documents, relative paths
  (shown '())                           ; those the filter lets through, in the list's order
  file                                  ; the document shown, a pathname
  (modified nil)                        ; its write date when drawn
  (links '())                           ; (start end url) in the preview
  (block-lines '())                     ; (source line . offset)
  (timer nil)
  (selecting nil))                      ; true while the list's selection follows the document

(defvar *docs* nil "The Documents window, once opened.")

;;; Tags

(defparameter *preview-heading-scales* #(1.9d0 1.55d0 1.3d0 1.15d0 1.0d0 0.9d0))

(defun preview-tag (gtk-buffer name &rest properties)
  "The tag NAME in GTK-BUFFER, made with PROPERTIES if it is new."
  (let ((table (gtk:text-buffer-get-tag-table gtk-buffer)))
    (or (gtk:text-tag-table-lookup table name)
        (let ((tag (make-instance 'gtk:text-tag :name name)))
          (loop for (key value) on properties by #'cddr
                do (setf (gobject:property tag key) value))
          (gtk:text-tag-table-add table tag)
          tag))))

(defun ensure-preview-tags (gtk-buffer)
  (unless (gtk:text-tag-table-lookup (gtk:text-buffer-get-tag-table gtk-buffer) "md-p-strong")
    ;; Made first, so every other tag (code's Monospace) wins over it.
    (preview-tag gtk-buffer "md-p-body" :family "Sans" :scale 1.08d0 :pixels-below-lines 2)
    (loop for level from 1 to 6
          do (preview-tag gtk-buffer (format nil "md-p-h~d" level)
                          :scale (aref *preview-heading-scales* (1- level)) :weight 700
                          :pixels-above-lines (if (<= level 2) 14 8) :pixels-below-lines 4))
    (preview-tag gtk-buffer "md-p-strong" :weight 700)
    (preview-tag gtk-buffer "md-p-em" :style :italic)
    (preview-tag gtk-buffer "md-p-strike" :strikethrough t)
    (preview-tag gtk-buffer "md-p-code" :family "Monospace" :background "rgba(128,128,128,0.18)")
    (preview-tag gtk-buffer "md-p-codeblock" :family "Monospace" :left-margin 28 :right-margin 16
                                             :paragraph-background "rgba(128,128,128,0.14)"
                                             :pixels-above-lines 0 :pixels-below-lines 0 :wrap-mode :char)
    (preview-tag gtk-buffer "md-p-link" :foreground "#4f8fe6" :underline :single)
    (preview-tag gtk-buffer "md-p-quote" :foreground "#8b8f96" :style :italic)
    (preview-tag gtk-buffer "md-p-rule" :foreground "#8b8f96")
    (preview-tag gtk-buffer "md-p-marker" :foreground "#8b8f96")
    (preview-tag gtk-buffer "md-p-table" :family "Monospace" :wrap-mode :none)
    (preview-tag gtk-buffer "md-p-table-header" :weight 700)
    (preview-tag gtk-buffer "md-p-gap" :scale 0.45d0)))

(defun margin-tag (gtk-buffer level &key hang)
  "A tag indenting paragraphs LEVEL steps; with HANG, the first line starts
further left, for a list item's marker."
  (preview-tag gtk-buffer (format nil "md-p-~:[margin~;hang~]-~d" hang level)
               :left-margin (+ 24 (* 28 level)) :indent (if hang -20 0)))

;;; Drawing

(defvar *preview* nil "While drawing: (gtk-buffer links lines base-directory).")

(defun iter-at (gtk-buffer offset)
  (gtk:text-buffer-get-iter-at-offset gtk-buffer offset))

(defun preview-end () (gtk:text-iter-get-offset (gtk:text-buffer-get-end-iter (first *preview*))))

(defun preview-insert (text tags)
  "Insert TEXT at the end of the preview being drawn, with TAGS (tags or names)."
  (let* ((gtk-buffer (first *preview*))
         (start (preview-end)))
    (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer) text -1)
    (let ((s (iter-at gtk-buffer start))
          (e (gtk:text-buffer-get-end-iter gtk-buffer)))
      (dolist (tag tags)
        (if (stringp tag)
            (gtk:text-buffer-apply-tag-by-name gtk-buffer tag s e)
            (gtk:text-buffer-apply-tag gtk-buffer tag s e))))
    start))

(defun preview-image (url alt tags)
  "Show the local image URL, or its ALT text when it can't be shown."
  (let* ((base (fourth *preview*))
         (path (and (not (search "://" url)) base
                    (merge-pathnames (string-left-trim "/" (substitute #\/ #\\ url)) base)))
         (texture (and path (probe-file path)
                       (ignore-errors
                        (gdk:texture-new-for-pixbuf
                         (gdk-pixbuf:pixbuf-new-from-file-at-scale (uiop:native-namestring path) 640 -1 t))))))
    (if texture
        (let ((gtk-buffer (first *preview*)))
          (gtk:text-buffer-insert-paintable gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer) texture))
        (let ((start (preview-insert (format nil "[~a]" (if (string= alt "") "image" alt)) (cons "md-p-link" tags))))
          (push (list start (preview-end) url) (second *preview*))))))

(defun preview-inlines (inlines tags)
  "Insert INLINES ((string . nodes)) with TAGS."
  (let ((string (car inlines)))
    (labels ((walk (node tags)
               (destructuring-bind (kind s e cs ce children &rest more) node
                 (declare (ignore e))
                 (case kind
                   ((:text :escape) (preview-insert (subseq string cs ce) tags))
                   (:code (preview-insert (code-span-text string cs ce) (cons "md-p-code" tags)))
                   (:strong (dolist (c children) (walk c (cons "md-p-strong" tags))))
                   (:emphasis (dolist (c children) (walk c (cons "md-p-em" tags))))
                   (:strike (dolist (c children) (walk c (cons "md-p-strike" tags))))
                   (:link (let ((start (preview-end)))
                            (dolist (c children) (walk c (cons "md-p-link" tags)))
                            (push (list start (preview-end) (third more)) (second *preview*))))
                   (:autolink (let ((start (preview-insert (subseq string cs ce) (cons "md-p-link" tags))))
                                (push (list start (preview-end) (first more)) (second *preview*))))
                   (:image (preview-image (third more) (subseq string cs ce) tags))
                   (t (preview-insert (subseq string s (fifth node)) tags))))))
      (dolist (node (cdr inlines)) (walk node tags)))))

(defun preview-code (text tags)
  (preview-insert (format nil "~%") (list "md-p-gap"))
  (dolist (line (split-text-lines text))
    (preview-insert (format nil "~a~%" line) (list* "md-p-codeblock" tags))))

(defun preview-table (aligns header rows tags)
  (let* ((cells (cons (mapcar #'inline-plain-text header)
                      (mapcar (lambda (row) (mapcar #'inline-plain-text row)) rows)))
         (columns (reduce #'max cells :key #'length :initial-value 0))
         (widths (loop for c below columns
                       collect (reduce #'max cells :key (lambda (row) (length (or (nth c row) ""))) :initial-value 1))))
    (flet ((row-text (row)
             (format nil "~{~a~^  │  ~}~%"
                     (loop for c below columns
                           for text = (or (nth c row) "")
                           for width = (nth c widths)
                           for pad = (- width (length text))
                           collect (pad-cell text pad (or (nth c aligns) :left))))))
      (preview-insert (row-text (first cells)) (list* "md-p-table" "md-p-table-header" tags))
      (preview-insert (format nil "~{~a~^──┼──~}~%" (mapcar (lambda (w) (make-string w :initial-element #\─)) widths))
                      (list* "md-p-table" "md-p-rule" tags))
      (dolist (row (rest cells))
        (preview-insert (row-text row) (list* "md-p-table" tags))))))

(defun preview-blocks (blocks level &key tight quote)
  "Insert BLOCKS indented LEVEL steps. TIGHT leaves out the space after
paragraphs (in a list item); QUOTE styles them as quoted."
  (let ((gtk-buffer (first *preview*)))
    (loop for (block . more) on blocks
          do (let ((tags (append (list (margin-tag gtk-buffer level)) (and quote (list "md-p-quote")))))
               (push (cons (car (last block)) (preview-end)) (third *preview*))
               (ecase (first block)
                 (:heading (preview-inlines (third block) (cons (format nil "md-p-h~d" (second block)) tags))
                  (preview-insert (format nil "~%") tags))
                 (:paragraph (preview-inlines (second block) tags)
                  (preview-insert (format nil "~%") tags))
                 (:code (preview-code (third block) (list (margin-tag gtk-buffer level))))
                 (:quote (preview-blocks (second block) (1+ level) :quote t))
                 (:rule (preview-insert (format nil "~a~%" (make-string 48 :initial-element #\─)) (cons "md-p-rule" tags)))
                 (:table (preview-table (second block) (third block) (fourth block) tags))
                 (:list
                  (destructuring-bind (ordered start items line) (rest block)
                    (declare (ignore line))
                    (loop for (task item-blocks) in items
                          for n from start
                          do (let ((marker (format nil "~:[~a~*~;~*~d.~]~@[ ~a~]  "
                                                   ordered (if (evenp level) "•" "◦") n
                                                   (case task (:open "☐") (:done "☑"))))
                                   (first (first item-blocks)))
                               (preview-insert marker (list (margin-tag gtk-buffer (1+ level) :hang t) "md-p-marker"))
                               (if (and first (eq (first first) :paragraph))
                                   (progn
                                     (preview-inlines (second first)
                                                      (append (list (margin-tag gtk-buffer (1+ level) :hang t))
                                                              (and quote (list "md-p-quote"))
                                                              (and (eq task :done) (list "md-p-strike"))))
                                     (preview-insert (format nil "~%") (list (margin-tag gtk-buffer (1+ level) :hang t)))
                                     (preview-blocks (rest item-blocks) (1+ level) :tight t :quote quote))
                                   (progn (preview-insert (format nil "~%") nil)
                                          (preview-blocks item-blocks (1+ level) :tight t :quote quote))))))))
               ;; Space between blocks, but not between a tight list's paragraphs.
               (unless (or (and tight (member (first block) '(:paragraph :list))) (eq (first block) :code)
                           (and (eq (first block) :list) more (eq (first (first more)) :list)))
                 (preview-insert (format nil "~%") (list "md-p-gap")))
               (when (eq (first block) :code)
                 (preview-insert (format nil "~%") (list "md-p-gap")))))))

(defun read-document (path)
  (handler-case (uiop:read-file-string path :external-format '(:utf-8 :replacement #\?))
    (error (e) (format nil "Couldn't read ~a: ~a" (namestring path) e))))

(defun render-document (docs &key keep-scroll)
  "Draw DOCS's file into its view, keeping the scroll position with KEEP-SCROLL."
  (let* ((view (docs-view docs))
         (gtk-buffer (gtk:text-view-get-buffer view))
         (path (docs-file docs))
         (adjustment (gtk:scrollable-get-vadjustment view))
         (scroll (and keep-scroll (gtk:adjustment-get-value adjustment))))
    (ensure-preview-tags gtk-buffer)
    (setf (docs-modified docs) (ignore-errors (file-write-date path)))
    (let ((*preview* (list gtk-buffer '() '() (uiop:pathname-directory-pathname path))))
      (gtk:text-buffer-set-text gtk-buffer "" -1)
      (handler-case (preview-blocks (markdown-blocks (read-document path)) 0)
        (error (e) (preview-insert (format nil "Couldn't show the preview: ~a~%" e) nil)))
      (gtk:text-buffer-apply-tag-by-name gtk-buffer "md-p-body" (gtk:text-buffer-get-start-iter gtk-buffer)
                                         (gtk:text-buffer-get-end-iter gtk-buffer))
      (setf (docs-links docs) (second *preview*)
            (docs-block-lines docs) (sort (third *preview*) #'< :key #'car)))
    (gtk:text-buffer-place-cursor gtk-buffer (gtk:text-buffer-get-start-iter gtk-buffer))
    (glib:idle-add glib:+priority-default-idle+
                   (lambda () (gtk:adjustment-set-value adjustment (or scroll 0d0)) nil))))

;;; Following links

(defun scroll-preview-to-offset (docs offset)
  (let* ((view (docs-view docs))
         (iter (iter-at (gtk:text-view-get-buffer view) offset)))
    (gtk:text-view-scroll-to-iter view iter 0d0 t 0d0 0d0)))

(defun follow-link (docs url)
  "Web links in the browser; #anchors to the heading; Markdown files in the
preview; other files with their default application."
  (let ((base (uiop:pathname-directory-pathname (docs-file docs))))
    (flet ((launch-uri (uri)
             (gio:async (gtk:uri-launcher-launch (gtk:uri-launcher-new uri) (docs-window docs))
                        (lambda (ok) (declare (ignore ok)))
                        :error (lambda (e) (declare (ignore e))))))
      (cond ((or (search "://" url) (string-equal "mailto:" url :end2 (min 7 (length url))))
             (launch-uri url))
            ((and (plusp (length url)) (char= (char url 0) #\#))
             (let* ((anchor (subseq url 1))
                    (heading (find anchor (markdown-headings (read-document (docs-file docs)))
                                   :key (lambda (h) (heading-anchor (second h))) :test #'string=))
                    (offset (and heading (cdr (assoc (third heading) (docs-block-lines docs))))))
               (when offset (scroll-preview-to-offset docs offset))))
            (t
             (let* ((file (subseq url 0 (or (position #\# url) (length url))))
                    (path (probe-file (merge-pathnames file base))))
               (cond ((null path))
                     ((markdown-file-p path) (show-document docs path))
                     (t (launch-uri (format nil "file://~a" (uiop:native-namestring path)))))))))))

(defun setup-link-clicks (docs)
  (let ((click (gtk:gesture-click-new))
        (view (docs-view docs)))
    (gtk:gesture-single-set-button click 1)
    (gobject:connect click :released
                     (lambda (gesture n x y)
                       (declare (ignore gesture n))
                       (multiple-value-bind (bx by) (gtk:text-view-window-to-buffer-coords view :widget (round x) (round y))
                         (multiple-value-bind (ok iter) (gtk:text-view-get-iter-at-location view bx by)
                           (when ok
                             (let* ((offset (gtk:text-iter-get-offset iter))
                                    (link (find-if (lambda (l) (<= (first l) offset (1- (second l)))) (docs-links docs))))
                               ;; Not when text was being selected.
                               (when (and link (not (gtk:text-buffer-get-has-selection (gtk:text-view-get-buffer view))))
                                 (follow-link docs (third link)))))))))
    (gtk:widget-add-controller view click)))

;;; The list of documents

(defun document-path (docs relative)
  (declare (ignore docs))
  (merge-pathnames relative (uiop:ensure-directory-pathname *directory*)))

(defun refill-document-list (docs)
  "Show the documents the filter lets through."
  (let* ((filter (string-trim " " (gtk:editable-get-text (docs-search docs))))
         (shown (if (zerop (length filter))
                    (docs-files docs)
                    (remove-if-not (lambda (f) (search filter f :test #'char-equal)) (docs-files docs))))
         (list (docs-list docs)))
    (setf (docs-shown docs) shown)
    (gtk:list-box-remove-all list)
    (dolist (file shown)
      (let ((slash (position #\/ file :from-end t)))
        (gtk:list-box-append list
                             (gtk:build
                               (gtk:box :orientation :vertical :margin-start 8 :margin-end 8 :margin-top 4 :margin-bottom 4
                                        :tooltip-text file
                                 (gtk:label :label (if slash (subseq file (1+ slash)) file) :xalign 0.0 :ellipsize :end)
                                 (gtk:label :label (if slash (subseq file 0 slash) "") :xalign 0.0 :ellipsize :start
                                            :visible (and slash t) :css-classes '("caption" "dim-label")))))))
    (select-shown-document docs)
    (when (null shown)
      (gtk:list-box-append list (make-instance 'gtk:label :label (if (docs-files docs) "No match" "No Markdown files here")
                                                          :margin-top 12 :css-classes '("dim-label"))))))

(defun refresh-documents (docs)
  "Find the folder's documents again."
  (setf (docs-files docs) (markdown-files *directory*))
  (refill-document-list docs)
  (gtk:window-set-title (docs-window docs) (format nil "Documents — ~a" (folder-label *directory*)))
  ;; The first, if nothing is shown or the shown one is gone (another folder).
  (unless (and (docs-file docs) (probe-file (docs-file docs))
               (uiop:subpathp (docs-file docs) (uiop:ensure-directory-pathname *directory*)))
    (let ((first (or (find-if (lambda (f) (string-equal "readme" (pathname-name f))) (docs-files docs))
                     (first (docs-files docs)))))
      (if first
          (show-document docs (document-path docs first))
          (progn (setf (docs-file docs) nil)
                 (gtk:label-set-text (docs-title docs) "")
                 (gtk:text-buffer-set-text (gtk:text-view-get-buffer (docs-view docs)) "" -1))))))

(defun select-shown-document (docs)
  "Highlight the shown document in the list, if the filter lets it through."
  (let* ((relative (and (docs-file docs)
                        (enough-namestring (docs-file docs) (uiop:ensure-directory-pathname *directory*))))
         (index (and relative (position relative (docs-shown docs) :test #'string=)))
         (row (and index (gtk:list-box-get-row-at-index (docs-list docs) index))))
    (setf (docs-selecting docs) t)
    (unwind-protect
         (if row
             (gtk:list-box-select-row (docs-list docs) row)
             (gtk:list-box-unselect-all (docs-list docs)))
      (setf (docs-selecting docs) nil))))

(defun show-document (docs path)
  (setf (docs-file docs) (pathname path))
  (select-shown-document docs)
  (gtk:label-set-text (docs-title docs) (enough-namestring path (uiop:ensure-directory-pathname *directory*)))
  (gtk:widget-set-tooltip-text (docs-title docs) (namestring path))
  (render-document docs))

(defun watch-document (docs)
  "Every second, while the window shows: draw the document again if it changed."
  (setf (docs-timer docs)
        (glib:timeout-add glib:+priority-default+ 1000
                          (lambda ()
                            (let ((file (docs-file docs)))
                              (when (and file (gtk:widget-get-visible (docs-window docs)))
                                (let ((date (ignore-errors (file-write-date file))))
                                  (when (and date (not (eql date (docs-modified docs))))
                                    (render-document docs :keep-scroll t)))))
                            t))))

;;; The window

(defun make-documents-window ()
  (let* ((window (make-instance 'gtk:window :title "Documents" :default-width 1100 :default-height 820
                                            :hide-on-close t))
         (search (make-instance 'gtk:search-entry :placeholder-text "Filter documents"
                                                  :margin-start 8 :margin-end 8 :margin-top 8 :margin-bottom 6))
         (list (make-instance 'gtk:list-box :selection-mode :single :css-classes '("navigation-sidebar")))
         (view (make-instance 'gtk:text-view :editable nil :cursor-visible nil :wrap-mode :word-char
                                             :left-margin 24 :right-margin 32 :top-margin 16 :bottom-margin 32))
         (title (make-instance 'gtk:label :xalign 0.0 :hexpand t :ellipsize :start :css-classes '("heading")))
         (docs (make-docs :window window :list list :search search :view view :title title)))
    (gobject:connect search :search-changed (lambda (e) (declare (ignore e)) (refill-document-list docs)))
    (gobject:connect list :row-activated
                     (lambda (l row) (declare (ignore l))
                       (let ((file (nth (gtk:list-box-row-get-index row) (docs-shown docs))))
                         (when file (show-document docs (document-path docs file))))))
    ;; Single click opens too.
    (gobject:connect list :row-selected
                     (lambda (l row) (declare (ignore l))
                       (when (and row (not (docs-selecting docs)))
                         (let ((file (nth (gtk:list-box-row-get-index row) (docs-shown docs))))
                           (when file (show-document docs (document-path docs file)))))))
    (setup-link-clicks docs)
    (let ((paned (make-instance 'gtk:paned :orientation :horizontal :position 280
                                           :shrink-start-child nil :shrink-end-child nil)))
      (gtk:paned-set-start-child paned
                                 (gtk:build
                                   (gtk:box :orientation :vertical
                                     search
                                     (gtk:scrolled-window :vexpand t :hscrollbar-policy :never :child list)
                                     (gtk:button :label "Refresh list" :css-classes '("flat") :margin-bottom 6
                                                 :tooltip-text "Look for Markdown files in the folder again"
                                                 :on-clicked (lambda (b) (declare (ignore b)) (refresh-documents docs))))))
      (gtk:paned-set-end-child paned
                               (gtk:build
                                 (gtk:box :orientation :vertical
                                   (gtk:box :spacing 8 :margin-start 16 :margin-end 12 :margin-top 8 :margin-bottom 8
                                     title
                                     (gtk:label :label "Preview · updates as the file changes" :css-classes '("dim-label")))
                                   (gtk:separator)
                                   (gtk:scrolled-window :vexpand t :hexpand t :hscrollbar-policy :never :child view))))
      (gtk:window-set-child window paned))
    (watch-document docs)
    docs))

(defun show-documents ()
  "Open the Documents window (or bring it forward), listing the folder's Markdown files."
  (unless *docs*
    (setf *docs* (make-documents-window)))
  (refresh-documents *docs*)
  (gtk:window-present (docs-window *docs*)))

(defun documents-folder-changed ()
  (when (and *docs* (gtk:widget-get-visible (docs-window *docs*)))
    (refresh-documents *docs*)))

(defun close-documents ()
  (when *docs*
    (glib:source-remove (docs-timer *docs*))
    (gtk:window-destroy (docs-window *docs*))
    (setf *docs* nil)))
