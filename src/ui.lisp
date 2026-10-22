;;;; ui.lisp — a shared conversation with live agent panels
;;;;
;;;; The agent panels show everything each agent does: the prompts it gets, its
;;;; replies as they stream, its tool calls, and an expandable direct composer.
;;;; The shared conversation holds messages and attention cards in every layout.
;;;; Its primary composer starts a workflow (workflow.lisp) when idle, or routes
;;;; direction to both agents or an individual agent while work continues.

(in-package #:agent-duet)

(defconstant +conversation-chat-head-size+ 28)
(defconstant +conversation-chat-head-spacing+ 10)
(defconstant +conversation-gutter+
  (+ +conversation-chat-head-size+ +conversation-chat-head-spacing+))

(defvar *window* nil)
(defvar *pane-search-scroller* nil "The scroller whose automatic following is paused for Find.")
(defvar *claude* nil)
(defvar *codex* nil)
(defvar *panes* '() "Agent id → AGENT-PANE.")
(defvar *middle* nil "The conversation's PANE.")
(defvar *checklist* nil "The task checklist panel's PANE.")
(defvar *checklist-findings* nil "The consolidated findings pane.")
(defvar *checklist-boxes* nil "Snapshot heading → card widgets and presentation data, one entry per loop.")
(defvar *request-preflight* nil)
(defvar *workflow* nil "The running or last workflow.")
(defvar *controls* nil "A plist of the middle pane's widgets.")
(defvar *directory* nil "Where the agents work.")
(defvar *permission* :ask)
(defvar *history* nil "The conversation being recorded (history.lisp); nil until something is said.")

(defun deliver (thunk)
  (glib:call-in-main-thread thunk))

(defun attention (title body &key sound)
  "Tell the user, with a desktop notification (notify.lisp), unless they're looking at the window."
  (unless (and *window* (gtk:window-is-active *window*))
    (notify-user title body :sound sound)))

(defun split-lines (string)
  "STRING's lines, without newlines. A final newline doesn't add an empty line."
  (let ((lines (uiop:split-string string :separator '(#\Newline))))
    (if (and (rest lines) (string= (car (last lines)) ""))
        (butlast lines)
        lines)))

(defun clear-box (box)
  (loop for child = (gtk:widget-get-first-child box)
        while child do (gtk:box-remove box child)))

(defun text-view-text (view)
  (let ((buffer (gtk:text-view-get-buffer view)))
    (gtk:text-buffer-get-text buffer (gtk:text-buffer-get-start-iter buffer)
                              (gtk:text-buffer-get-end-iter buffer) t)))

(defun clear-text-view (view)
  (gtk:text-buffer-set-text (gtk:text-view-get-buffer view) "" -1))

(defun modifier-list (state)
  (cond ((listp state) state)
        ((keywordp state) (list state))
        (t '())))

(defun on-return (view function)
  "Call FUNCTION when Return (without Shift) is pressed in VIEW."
  (let ((keys (gtk:event-controller-key-new)))
    (gobject:connect keys :key-pressed
                     (lambda (c keyval keycode state)
                       (declare (ignore c keycode))
                       (when (and (member (gdk:keyval-name keyval) '("Return" "KP_Enter") :test #'string=)
                                  (not (member :shift-mask (modifier-list state))))
                         (funcall function)
                         t)))
    (gtk:widget-add-controller view keys)))

;;; Images in replies: an agent's mockups, as thumbnails that open in the
;;; system's viewer (image-artifacts.lisp finds the files)

(defvar *image-directory* nil
  "Where relative image references resolve while a saved conversation is
replayed: its folder. Otherwise *DIRECTORY*.")

(defparameter *image-thumbnail-size* '(640 480) "The largest thumbnail, width and height.")

(defvar *image-thumbnails* (make-hash-table :test #'equal)
  "(namestring write-date) → texture, so a reply drawn again isn't decoded again.")

(defun markdown-image-reference (text node)
  "The destination of the :image NODE in TEXT. One in <…> may hold spaces,
where the inline parser stops."
  (destructuring-bind (kind s e cs ce children us ue url) node
    (declare (ignore kind s cs ce children ue))
    (let ((close (and (< us e) (char= (char text us) #\<) (position #\> text :start us :end e))))
      (if close (subseq text (1+ us) close) url))))

(defun image-node-alt (text node)
  (string-trim " " (subseq text (fourth node) (fifth node))))

(defun image-node-artifact (text node)
  "The existing local image file the :image NODE in TEXT refers to, or nil."
  (resolve-image-artifact (markdown-image-reference text node) (or *image-directory* *directory*)))

(defun image-fallback-markup (alt reference)
  "Pango markup for an image that can't be shown: a link to a web image,
otherwise its description and where it was expected."
  (flet ((escape (s) (glib:markup-escape-text s -1)))
    (let ((alt (if (string= alt "") "image" alt)))
      (if (or (uiop:string-prefix-p "http://" reference) (uiop:string-prefix-p "https://" reference))
          (format nil "<a href=\"~a\">🖼 ~a</a>" (escape reference) (escape alt))
          (format nil "🖼 ~a <span alpha=\"60%\">(~a not found)</span>" (escape alt) (escape reference))))))

(defun line-images (line)
  "The local images a line of Markdown shows, as (pathname alt), in order."
  (when (search "![" line)
    (let ((found '()))
      (labels ((walk (node)
                 (if (eq (first node) :image)
                     (let ((path (image-node-artifact line node)))
                       (when path (push (list path (image-node-alt line node)) found)))
                     (mapc #'walk (sixth node)))))
        (mapc #'walk (markdown-inlines line)))
      (nreverse found))))

(defparameter *image-preview-max-bytes* (* 20 1024 1024))
(defparameter *image-preview-max-pixels* 16000000)

(defun image-thumbnail-texture (path)
  "Return a bounded preview, caching failures too. Oversized files remain openable."
  (let ((key (list (namestring path) (ignore-errors (file-write-date path)))))
    (multiple-value-bind (cached present) (gethash key *image-thumbnails*)
      (when present (return-from image-thumbnail-texture (unless (eq cached :none) cached))))
    (let ((texture
            (ignore-errors
              (when (with-open-file (in path :element-type '(unsigned-byte 8))
                      (<= (file-length in) *image-preview-max-bytes*))
                (let ((name (uiop:native-namestring path)))
                  (multiple-value-bind (format width height) (gdk-pixbuf:pixbuf-get-file-info name)
                    (when (and format (plusp width) (plusp height)
                               (<= (* width height) *image-preview-max-pixels*))
                      (gdk:texture-new-for-pixbuf
                       (gdk-pixbuf:pixbuf-new-from-file-at-scale
                        name (min width (first *image-thumbnail-size*))
                        (min height (second *image-thumbnail-size*)) t)))))))))
      (when (>= (hash-table-count *image-thumbnails*) 64) (clrhash *image-thumbnails*))
      (setf (gethash key *image-thumbnails*) (or texture :none))
      texture)))

(defun open-image-artifact (path widget)
  "Open the image file PATH in the system's viewer."
  (gio:async (gtk:uri-launcher-launch (gtk:uri-launcher-new (image-artifact-uri path))
                                      (or (gtk:widget-get-root widget) *window*))
             (lambda (ok) (declare (ignore ok)))
             :error (lambda (e) (declare (ignore e)))))

(defun image-thumbnail (path alt)
  "A button showing PATH's image, captioned with ALT or its name, that opens it."
  (let* ((texture (image-thumbnail-texture path))
         (caption (if (string= alt "") (file-namestring path) alt))
         (button (make-instance 'gtk:button :css-classes '("flat" "duet-image") :halign :start
                                :tooltip-text (format nil "Open ~a" (uiop:native-namestring path))))
         (box (make-instance 'gtk:box :orientation :vertical :spacing 4)))
    (if texture
        (let* ((width (gdk:paintable-get-intrinsic-width texture))
               (height (gdk:paintable-get-intrinsic-height texture))
               (scale (min 1 (/ 240 (max width height)))))
          ;; A modest, aspect-preserving floor prevents collapse without making
          ;; the full texture width a minimum for the pane and window.
          (gtk:box-append box (make-instance 'gtk:picture :paintable texture :can-shrink t
                                             :width-request (max 1 (round (* width scale)))
                                             :height-request (max 1 (round (* height scale)))
                                             :content-fit :scale-down :halign :start
                                             :alternative-text caption))
          (gtk:box-append box (make-instance 'gtk:label :label caption :xalign 0.0 :ellipsize :middle
                                             :css-classes '("caption" "dim-label"))))
        (gtk:box-append box (make-instance 'gtk:label :label (format nil "🖼 ~a — no preview, click to open" caption)
                                           :xalign 0.0 :wrap t :wrap-mode :word-char)))
    (gtk:button-set-child button box)
    (gobject:connect button :clicked (lambda (b) (open-image-artifact path b)))
    button))

;;; Rendering Markdown, simply (from Cadre)

(defun code-span-text (string start end)
  (if (and (> (- end start) 2) (char= #\Space (char string start)) (char= #\Space (char string (1- end)))
           (find #\Space string :start start :end end :test-not #'char=))
      (subseq string (1+ start) (1- end))
      (subseq string start end)))

(defun inline-markup (text)
  "Pango markup for a line of Markdown."
  (flet ((escape (s) (glib:markup-escape-text s -1)))
    (with-output-to-string (out)
      (labels ((walk (node)
                 (destructuring-bind (kind s e cs ce children &rest more) node
                   (declare (ignore s e))
                   (flet ((wrap (open close)
                            (write-string open out) (mapc #'walk children) (write-string close out)))
                     (case kind
                       ((:text :escape) (write-string (escape (subseq text cs ce)) out))
                       (:directive
                        (destructuring-bind (caption &optional uri)
                            (visualization-link (subseq text cs ce) (or *image-directory* *directory*))
                          (if uri (format out "<a href=\"~a\">~a</a>" (escape uri) (escape caption))
                              (write-string (escape caption) out))))
                       (:code (format out "<tt>~a</tt>" (escape (code-span-text text cs ce))))
                       (:strong (wrap "<b>" "</b>"))
                       (:emphasis (wrap "<i>" "</i>"))
                       (:strike (wrap "<s>" "</s>"))
                       (:link (wrap (format nil "<a href=\"~a\">" (escape (third more))) "</a>"))
                       (:autolink (format out "<a href=\"~a\">~a</a>"
                                          (escape (let ((url (first more)))
                                                    (if (search "://" url) url (format nil "https://~a" url))))
                                          (escape (subseq text cs ce))))
                       ;; A local image is shown below its line instead.
                       (:image (unless (image-node-artifact text node)
                                 (write-string (image-fallback-markup (image-node-alt text node)
                                                                      (markdown-image-reference text node))
                                               out))))))))
        (mapc #'walk (markdown-inlines text))))))

(defun prose-markup (text)
  (with-output-to-string (out)
    (loop for (line . rest) on (split-lines text)
          do (let ((trimmed (string-left-trim " " line)))
               (cond ((and (plusp (length trimmed)) (char= (char trimmed 0) #\#))
                      (format out "<b>~a</b>" (inline-markup (string-left-trim "# " trimmed))))
                     ((and (> (length trimmed) 1) (member (char trimmed 0) '(#\- #\*)) (char= (char trimmed 1) #\Space))
                      (format out "  • ~a" (inline-markup (subseq trimmed 2))))
                     (t (write-string (inline-markup line) out))))
             (when rest (terpri out)))))

(defun markdown-segments (text)
  "TEXT split into (:prose string) and (:code string language) segments at ``` fences."
  (let ((segments '()) (lines '()) (in-code nil) (language nil))
    (flet ((flush (kind)
             (let ((s (format nil "~{~a~^~%~}" (reverse lines))))
               (when (or (eq kind :code) (plusp (length (string-trim '(#\Space #\Newline) s))))
                 (push (if (eq kind :code) (list :code s language) (list :prose s)) segments)))
             (setf lines '())))
      (dolist (line (split-lines text))
        (if (and (>= (length (string-left-trim " " line)) 3)
                 (string= "```" (string-left-trim " " line) :end2 3))
            (if in-code
                (progn (flush :code) (setf in-code nil))
                (progn (flush :prose) (setf in-code t language (string-trim " `" line))))
            (push line lines)))
      (flush (if in-code :code :prose)))
    (nreverse segments)))

(defun code-block-widget (code)
  (let ((copy (make-instance 'gtk:button :label "Copy" :css-classes '("flat"))))
    (gobject:connect copy :clicked (lambda (b)
                                     (gdk:clipboard-set-text (gtk:widget-get-clipboard b) code)))
    (gtk:build
     (gtk:box :orientation :vertical :css-classes '("duet-code")
              (gtk:label :label code :xalign 0.0 :selectable t :wrap t :wrap-mode :char :css-classes '("monospace"))
              (gtk:box :halign :end copy)))))

(defun markup-without-links (markup)
  "MARKUP with its links as spans, for checking: Pango can't parse <a>,
though labels show it. Text in markup is escaped, so each < starts a tag."
  (with-output-to-string (out)
    (loop with i = 0
          for start = (position #\< markup :start i)
          while start
          do (let* ((end (or (position #\> markup :start start) (1- (length markup))))
                    (tag (subseq markup start (1+ end))))
               (write-string markup out :start i :end start)
               (write-string (cond ((uiop:string-prefix-p "<a " tag) "<span>")
                                   ((string= tag "</a>") "</span>")
                                   (t tag))
                             out)
               (setf i (1+ end)))
          finally (write-string markup out :start i))))

(defparameter *local-document-extensions*
  (append *image-artifact-extensions*
          '("html" "htm" "pdf" "txt" "md" "markdown" "csv" "tsv"
            "json" "xml" "log" "doc" "docx" "odt" "rtf" "xls" "xlsx"
            "ods" "ppt" "pptx" "odp")))

(defun activate-prose-link (widget uri)
  "Handle local paths and file URIs, including refused links; defer web URIs to GTK."
  (let ((file-uri (and (>= (length uri) 5) (string-equal uri "file:" :end1 5))))
    (when (or file-uri (and (plusp (length uri)) (char= (char uri 0) #\/)))
      (let* ((path (resolve-image-artifact uri nil :extensions *local-document-extensions*))
             (original (and path
                            (if file-uri
                                (image-artifact-decode
                                 (subseq uri 5 (position-if (lambda (ch) (find ch "?#")) uri)))
                                uri))))
        ;; Check the displayed name and resolved target, plus every execute bit.
        ;; Consume refused file URIs too, so GTK cannot launch them itself.
        (when (and path
                   (ignore-errors
                     (and (member (pathname-type (uiop:parse-native-namestring original))
                                  *local-document-extensions* :test #'equalp)
                          (zerop (logand #o111 (sb-posix:stat-mode (sb-posix:stat path)))))))
          (open-image-artifact path widget)))
      t)))

(defun prose-label (text)
  "A label showing TEXT's prose, or nil when only shown images were in it."
  (let ((markup (prose-markup text)))
    (when (plusp (length (string-trim '(#\Space #\Newline #\•) markup)))
      (let ((label (make-instance 'gtk:label :xalign 0.0 :wrap t :selectable t :wrap-mode :word-char)))
        ;; Markup GTK can't parse would leave the label
        ;; empty: show the text as it is instead.
        (if (ignore-errors (pango:parse-markup (markup-without-links markup) -1 0) t)
            (gtk:label-set-markup label markup)
            (gtk:label-set-text label text))
        (gobject:connect label :activate-link #'activate-prose-link)
        label))))

(defun normalize-image-lines (text)
  "Keep wrapped images and artifact directives together while rendering prose."
  (let ((result (copy-seq text)))
    (labels ((walk (node)
               (if (member (first node) '(:image :directive))
                   (loop for i from (second node) below (third node)
                         when (char= (char result i) #\Newline)
                           do (setf (char result i) #\Space))
                   (mapc #'walk (sixth node)))))
      (mapc #'walk (markdown-inlines text)))
    result))

(defun render-prose (box text)
  "Append TEXT's prose to BOX. A line showing local images ends its label,
and the images follow it as thumbnails."
  (let ((lines '()))
    (flet ((flush ()
             (let ((label (and lines (prose-label (format nil "~{~a~^~%~}" (reverse lines))))))
               (when label (gtk:box-append box label)))
             (setf lines '())))
      (dolist (line (split-lines (normalize-image-lines text)))
        (push line lines)
        (let ((images (line-images line)))
          (when images
            (flush)
            (loop for (path alt) in images
                  do (gtk:box-append box (image-thumbnail path alt))))))
      (flush))))

(defun render-markdown (box text)
  "Fill BOX with TEXT, rendered."
  (dolist (segment (markdown-segments text))
    (if (eq (first segment) :code)
        (gtk:box-append box (code-block-widget (second segment)))
        (render-prose box (second segment)))))

(defun markdown-box (text &rest classes)
  (let ((box (make-instance 'gtk:box :orientation :vertical :spacing 6 :css-classes classes)))
    (render-markdown box text)
    box))

;;; A scrolling list of messages

(defstruct pane
  messages scroller (scroll-pending nil)
  status input send)

(defun make-message-list ()
  (let* ((messages (make-instance 'gtk:box :orientation :vertical :spacing 22
                                  :margin-start 12 :margin-end 12 :margin-top 8 :margin-bottom 8))
         (scroller (make-instance 'gtk:scrolled-window :child messages :vexpand t :hexpand t
                                  :hscrollbar-policy :never)))
    ;; The replies are selectable labels, so they can take the focus; the
    ;; viewport would then scroll to the one focused, often the first.
    (let ((viewport (gtk:scrolled-window-get-child scroller)))
      (when (typep viewport 'gtk:viewport)
        (gtk:viewport-set-scroll-to-focus viewport nil)))
    ;; Layout updates the range after idle callbacks may already have run.
    ;; Follow that range too, so streamed and rendered replies stay in view.
    ;; Not at once: the range changes while the viewport lays out its
    ;; content, and a new value then moves the scrollbar but not the
    ;; content, which stays where it was until the next scroll.
    (let ((pending nil))
      (gobject:connect (gtk:scrolled-window-get-vadjustment scroller) :changed
                       (lambda (adjustment)
                         (unless pending
                           (setf pending t)
                           (glib:idle-add glib:+priority-default-idle+
                                          (lambda ()
                                            (setf pending nil)
                                            (unless (eq scroller *pane-search-scroller*)
                                              (scroll-adjustment-to-end adjustment))
                                            nil))))))
    (values messages scroller)))

(defun scroll-adjustment-to-end (adjustment)
  (gtk:adjustment-set-value adjustment (max 0d0 (- (gtk:adjustment-get-upper adjustment)
                                                   (gtk:adjustment-get-page-size adjustment)))))

(defun pane-scroll-to-end (pane)
  (unless (pane-scroll-pending pane)
    (setf (pane-scroll-pending pane) t)
    (glib:idle-add glib:+priority-default-idle+
                   (lambda ()
                     (setf (pane-scroll-pending pane) nil)
                     (unless (eq (pane-scroller pane) *pane-search-scroller*)
                       (scroll-adjustment-to-end (gtk:scrolled-window-get-vadjustment (pane-scroller pane))))
                     nil))))

(defun pane-append (pane widget)
  (gtk:box-append (pane-messages pane) widget)
  (pane-scroll-to-end pane)
  widget)

(defun pane-note-label (text &rest classes)
  (make-instance 'gtk:label :label text :wrap t :xalign 0.0 :selectable t
                            :wrap-mode :word-char
                            :css-classes (or classes '("dim-label"))))

(defun pane-note (pane text &rest classes)
  (pane-append pane (apply #'pane-note-label text classes)))

(defun composer (input send)
  (gtk:build
   (gtk:box :orientation :vertical :css-classes '("duet-composer") :spacing 8
            :margin-start 8 :margin-end 8 :margin-top 6 :margin-bottom 8
            (gtk:frame
             (gtk:scrolled-window :max-content-height 180 :propagate-natural-height t
                                  :hscrollbar-policy :never :child input))
            (gtk:box :halign :end :spacing 8 send))))

(defun make-input ()
  (make-instance 'gtk:text-view :wrap-mode :word-char :top-margin 6 :bottom-margin 6
                 :left-margin 8 :right-margin 8 :accepts-tab nil))

;;; Files dropped on a message box

(defun dropped-paths (value)
  "The local paths of the files in a drop's VALUE: a GValue holding a GdkFileList."
  (let ((files (if (typep value 'gtk4.runtime:boxed)
                   (gtk4.runtime:gvalue-get (gtk4.runtime:object-pointer value))
                   value)))
    (remove nil (mapcar #'gio:file-get-path (gdk:file-list-get-files files)))))

(defun on-file-drop (widget function)
  "Call FUNCTION with the paths of the files dropped on WIDGET. It sees the
drop before WIDGET's children, so a text view doesn't take it as text."
  (let ((target (gtk:drop-target-new (gtk4.runtime:gtype-from-name "GdkFileList" "gdk_file_list_get_type")
                                     '(:copy))))
    (gtk:event-controller-set-propagation-phase target :capture)
    (gobject:connect target :drop
                     (lambda (target value x y)
                       (declare (ignore target x y))
                       (let ((paths (ignore-errors (dropped-paths value))))
                         (when paths (funcall function paths))
                         (and paths t))))
    (gtk:widget-add-controller widget target)))

(defun insert-paths (view paths)
  "Type PATHS into VIEW where its cursor is, quoted for a shell when they need it."
  (gtk:text-buffer-insert-at-cursor (gtk:text-view-get-buffer view)
                                    (format nil "~{~a~^ ~}" (mapcar #'shell-quote paths))
                                    -1))

;;; An agent's pane

(defstruct (agent-pane (:include pane) (:constructor new-agent-pane))
  agent stop
  (attachments '())                           ; the paths of the files dropped on its message box
  chips                                       ; the box showing them
  (stream-box nil) (stream-label nil) (stream-text "") (stream-timeout nil)
  (conversation-box nil) (conversation-label nil) (conversation-activity nil)
  (conversation-placeholder-p nil) ; waiting for the first reply text
  (conversation-replies nil) ; completed text/bubble pairs eligible for a handoff
  (tools (make-hash-table :test 'equal))
  (activity-text "No commands yet") (activity-id nil) (activity-title "") (activity-labels '())
  (approvals (make-hash-table :test 'equal))  ; approval id → its cards: (button row . widgets to disable)s
  (ready "Checking…")                         ; what the status says when the agent is idle
  (version nil)
  (model-dropdowns '()) (effort-dropdowns '()) ; in its pane and in the conversation-only view
  (models '())                 ; the model dropdown's choices: names (Claude) or model/list entries (Codex)
  (efforts '(nil))             ; the effort dropdown's choices, nil being the default
  (refreshing nil))            ; true while the dropdowns are being refilled

(defun agent-pane-of (agent) (cdr (assoc (agent-id agent) *panes*)))

(defun agent-setting-key (agent what)
  (intern (format nil "~a-~a" (agent-id agent) what) :keyword))

;;; Models and thinking levels

(defun model-choice-name (agent model)
  (if (eq (agent-id agent) :claude)
      (claude-model-label model)
      (or (jget model "displayName") (codex-model-id model))))

(defun model-choice-id (agent model)
  (if (eq (agent-id agent) :claude) (claude-model-id model) (codex-model-id model)))

;;; Each agent has a model and an effort dropdown in its pane, and another
;;; pair in the conversation-only view's activity strip; all show the same.

(defun pane-current-model-entry (pane)
  (let ((agent (agent-pane-agent pane)))
    (find (agent-model agent) (agent-pane-models pane)
          :key (lambda (m) (model-choice-id agent m)) :test #'equal)))

(defun pane-effort-choices (pane)
  (let ((agent (agent-pane-agent pane)))
    (cons nil (if (eq (agent-id agent) :claude)
                  *claude-efforts*
                  (let ((model (pane-current-model-entry pane)))
                    (and model (codex-model-efforts model)))))))

(defun effort-label (effort)
  (format nil "Thinking: ~a" (if effort (string-capitalize effort) "Default")))

(defun refresh-effort-dropdown (pane)
  "Fill the effort dropdowns with the current model's levels, keeping the
agent's effort when the model has it."
  (let* ((agent (agent-pane-agent pane))
         (choices (pane-effort-choices pane)))
    ;; Before Codex's asynchronous catalog arrives only Default is displayed;
    ;; that placeholder must not discard the effort loaded from preferences.
    (setf (agent-effort agent)
          (catalog-effort (agent-effort agent) choices
                          (or (eq (agent-id agent) :claude)
                              (not (null (agent-pane-models pane))))))
    (setf (agent-pane-refreshing pane) t)
    (unwind-protect
        (progn
          (setf (agent-pane-efforts pane) choices)
          (dolist (dropdown (agent-pane-effort-dropdowns pane))
            (gtk:drop-down-set-model dropdown (gtk:string-list-new (mapcar #'effort-label choices)))
            (gtk:drop-down-set-selected dropdown (or (position (agent-effort agent) choices :test #'equal) 0))
            (gtk:widget-set-sensitive dropdown (> (length choices) 1))))
      (setf (agent-pane-refreshing pane) nil))))

(defun show-model-choice (pane)
  "Select the agent's model in each of PANE's model dropdowns."
  (let ((i (or (position (pane-current-model-entry pane) (agent-pane-models pane)) 0)))
    (setf (agent-pane-refreshing pane) t)
    (unwind-protect
        (dolist (dropdown (agent-pane-model-dropdowns pane))
          (unless (= (gtk:drop-down-get-selected dropdown) i)
            (gtk:drop-down-set-selected dropdown i)))
      (setf (agent-pane-refreshing pane) nil))))

(defun show-effort-choice (pane)
  "Select the agent's effort in each of PANE's effort dropdowns."
  (let ((i (or (position (agent-effort (agent-pane-agent pane)) (agent-pane-efforts pane) :test #'equal) 0)))
    (setf (agent-pane-refreshing pane) t)
    (unwind-protect
        (dolist (dropdown (agent-pane-effort-dropdowns pane))
          (unless (= (gtk:drop-down-get-selected dropdown) i)
            (gtk:drop-down-set-selected dropdown i)))
      (setf (agent-pane-refreshing pane) nil))))

(defun set-model-choices (pane models)
  "Fill the model dropdowns with MODELS, selecting the agent's model (or,
for Codex, the one marked as the default)."
  (let ((agent (agent-pane-agent pane)))
    (setf (agent-pane-refreshing pane) t)
    (unwind-protect
        (progn
          (setf (agent-pane-models pane) models)
          (let ((i (or (position (agent-model agent) models
                                 :key (lambda (m) (model-choice-id agent m)) :test #'equal)
                       (and (eq (agent-id agent) :codex)
                            (position-if (lambda (m) (eq (jget m "isDefault") t)) models))
                       0)))
            (when models
              (setf (agent-model agent) (model-choice-id agent (nth i models))))
            (dolist (dropdown (agent-pane-model-dropdowns pane))
              (gtk:drop-down-set-model dropdown
                                       (gtk:string-list-new
                                        (or (mapcar (lambda (m) (model-choice-name agent m)) models)
                                            '("Default model"))))
              (gtk:drop-down-set-selected dropdown i)
              (gtk:widget-set-sensitive dropdown (not (null models))))))
      (setf (agent-pane-refreshing pane) nil))
    (refresh-effort-dropdown pane)))

(defun make-model-dropdowns (pane)
  "A model and an effort dropdown for PANE's agent, kept in step with its others."
  (let* ((agent (agent-pane-agent pane))
         (first (null (agent-pane-model-dropdowns pane)))
         (models (gtk:drop-down-new-from-strings '("Default model")))
         (efforts (gtk:drop-down-new-from-strings '("Thinking: Default"))))
    (setf (agent-pane-model-dropdowns pane) (append (agent-pane-model-dropdowns pane) (list models))
          (agent-pane-effort-dropdowns pane) (append (agent-pane-effort-dropdowns pane) (list efforts)))
    (gtk:widget-set-tooltip-text models (format nil "~a's model, from its next message" (agent-name agent)))
    (gtk:widget-set-tooltip-text efforts (format nil "How hard ~a thinks, from its next message" (agent-name agent)))
    (gobject:connect models "notify::selected"
                     (lambda (d pspec) (declare (ignore pspec))
                       (unless (agent-pane-refreshing pane)
                         (let ((model (nth (gtk:drop-down-get-selected d) (agent-pane-models pane))))
                           (when model
                             (setf (agent-model agent) (model-choice-id agent model)
                                   (setting (agent-setting-key agent "MODEL")) (agent-model agent))))
                         (show-model-choice pane)
                         (refresh-effort-dropdown pane)
                         (setf (setting (agent-setting-key agent "EFFORT")) (agent-effort agent)))))
    (gobject:connect efforts "notify::selected"
                     (lambda (d pspec) (declare (ignore pspec))
                       (unless (agent-pane-refreshing pane)
                         (setf (agent-effort agent) (nth (gtk:drop-down-get-selected d) (agent-pane-efforts pane))
                               (setting (agent-setting-key agent "EFFORT")) (agent-effort agent))
                         (show-effort-choice pane))))
    (if (and first (eq (agent-id agent) :claude))
        ;; The CLI catalog arrives asynchronously. Its IDs can include choices
        ;; absent from the built-in aliases (for example "default" or a full
        ;; versioned name). Keep the restored model until that catalog arrives.
        (set-model-choices pane (claude-startup-models (agent-model agent)))
        ;; A later pair starts with what the others show.
        (set-model-choices pane (agent-pane-models pane)))
    (values models efforts)))

;;; The pane

(defun wrapping-controls (&rest widgets)
  "Let controls wrap without forcing the pane to fit a single wide row."
  (let ((flow (make-instance 'gtk:flow-box :selection-mode :none
                             :homogeneous nil :min-children-per-line 1
                             :max-children-per-line 6 :row-spacing 4 :column-spacing 6
                             :margin-start 8 :margin-end 8 :margin-top 6 :margin-bottom 6)))
    (dolist (widget widgets)
      (gtk:flow-box-append flow widget))
    flow))

(defun make-workflow-activity-pill (agent key)
  "Register a workflow pill beside an agent's status and usage."
  (let ((label (make-instance 'gtk:label :xalign 0.0
                             :valign :center :css-classes '("caption"))))
    (setf (getf *controls* (agent-setting-key agent key)) label)
    label))

(defun make-agent-disclosure (agent name label child)
  "An inset disclosure with an agent-specific saved expansion preference."
  (let* ((key (agent-setting-key agent name))
         (expander (make-instance 'gtk:expander :label label :child child
                                  :expanded (setting key nil)
                                  :margin-start 12 :margin-end 12 :margin-top 4 :margin-bottom 4)))
    (setf (getf *controls* key) expander)
    (gobject:connect expander "notify::expanded"
                     (lambda (widget spec) (declare (ignore spec))
                       (setf (setting key) (gtk:expander-get-expanded widget))))
    expander))

(defun make-agent-pane (agent)
  (multiple-value-bind (messages scroller) (make-message-list)
    (let* ((status (make-instance 'gtk:label :xalign 0.0 :ellipsize :end :css-classes '("dim-label")
                                  :label "Checking…"))
           (input (make-input))
           (send (make-instance 'gtk:button :label "Send" :css-classes '("suggested-action")
                                :tooltip-text (format nil "Talk to ~a alone" (agent-name agent))))
           (stop (make-instance 'gtk:button :label "Stop" :visible nil
                                :css-classes '("destructive-action")
                                :tooltip-text (format nil "Stop ~a" (agent-name agent))))
           (chips (make-instance 'gtk:flow-box :selection-mode :none :visible nil :row-spacing 4
                                 :column-spacing 4 :margin-start 8 :margin-end 8 :margin-top 6))
           (pane (new-agent-pane :agent agent :stop stop :messages messages :scroller scroller
                                 :status status :input input :send send :chips chips)))
      (flet ((send-it ()
               (let ((text (string-trim '(#\Space #\Newline #\Tab) (text-view-text input)))
                     (attachments (agent-pane-attachments pane)))
                 (when (and (or (plusp (length text)) attachments) (not (committing-p pane)))
                   (handler-case
                       (progn (unless (handle-clear-context-command text attachments (list agent))
                                (agent-send-now agent (if (plusp (length text)) text "Take a look at these.")
                                                attachments))
                              (clear-text-view input)
                              (set-attachments pane '()))
                     (error (e) (pane-note pane (princ-to-string e) "error")))))))
        (gobject:connect send :clicked (lambda (b) (declare (ignore b)) (send-it)))
        (gobject:connect stop :clicked (lambda (b) (declare (ignore b))
                                         ;; In a task, only this agent's part pauses.
                                         (if (and (workflow-running-p) (workflow-agent-p *workflow* agent))
                                             (pause-workflow-agent *workflow* agent)
                                             (agent-cancel agent))))
        (on-return input #'send-it))
      (setf (agent-listener agent) #'agent-pane-event)
      (multiple-value-bind (models efforts) (make-model-dropdowns pane)
        (values pane
                (gtk:build
                 (gtk:box :orientation :vertical :width-request 300 :css-classes (list "duet-pane" (if (eq (agent-id agent) :claude) "duet-panel-claude" "duet-panel-codex"))
                          (gtk:box :spacing 8 :margin-start 12 :margin-end 6 :margin-top 6
                                   (gtk:label :label (agent-name agent) :css-classes (list "heading" (agent-css-class agent)))
                                   status
                                   (make-workflow-activity-pill agent "WORKFLOW-ACTIVITY")
                                   (gtk:box :hexpand t)
                                   (gtk:button :label "Clear context" :css-classes '("flat")
                                               :tooltip-text (format nil "Clear ~a's context (/clear); starts a new conversation" (agent-name agent))
                                               :on-clicked (lambda (b) (declare (ignore b)) (new-conversation agent))))
                          (make-agent-disclosure agent "MODEL-EXPANDED" "Model & thinking" (wrapping-controls models efforts))
                          (make-agent-activity-label pane)
                          (gtk:separator)
                          (make-transcript agent scroller)
                          (gtk:separator)
                          (let ((box (gtk:build
                                      (gtk:box :orientation :vertical
                                               chips
                                               (composer input
                                                         (gtk:build (gtk:box :spacing 8 stop send)))))))
                            (gtk:widget-set-tooltip-text
                             input (format nil "Drop files or images here to attach them to the message to ~a"
                                           (agent-name agent)))
                            (on-file-drop box (lambda (paths)
                                                (set-attachments pane (append (agent-pane-attachments pane)
                                                                              (set-difference paths (agent-pane-attachments pane)
                                                                                              :test #'equal)))))
                            (make-agent-disclosure agent "COMPOSER-EXPANDED"
                                                   (format nil "Message ~a directly" (agent-name agent)) box)))))))))

(defun set-attachments (pane paths)
  "Attach PATHS to the next message from PANE's box, each shown as a removable chip."
  (let ((chips (agent-pane-chips pane)))
    (setf (agent-pane-attachments pane) paths)
    (gtk:flow-box-remove-all chips)
    (dolist (path paths)
      (let* ((path path)
             (remove (make-instance 'gtk:button :label "×" :css-classes '("flat" "circular")
                                                :tooltip-text "Don't attach it")))
        (gobject:connect remove :clicked
                         (lambda (b) (declare (ignore b))
                           (set-attachments pane (remove path (agent-pane-attachments pane) :test #'equal))))
        (gtk:flow-box-append chips
                             (gtk:build
                              (gtk:box :spacing 2 :css-classes '("duet-chip") :tooltip-text path
                                       (gtk:label :label (format nil "~:[📄~;🖼~] ~a" (image-media-type path)
                                                                 (file-namestring path))
                                                  :ellipsize :middle :max-width-chars 28 :margin-start 6)
                                       remove)))))
    (gtk:widget-set-visible chips (and paths t))))

(defun agent-css-class (agent)
  (if (eq (agent-id agent) :claude) "duet-claude" "duet-codex"))

(defun pane-update-status (pane)
  "The status line: what the agent is doing, and how much usage it has left
(orange when low or out)."
  (let* ((agent (agent-pane-agent pane))
         (waiting (plusp (hash-table-count (agent-pane-approvals pane))))
         (windows (agent-usage-windows agent))
         (usage (usage-summary windows))
         (out (agent-out-until agent))
         (out-now (or (agent-out-of-usage-p agent) (eq out :unknown))))
    (gtk:label-set-text (pane-status pane)
                        (format nil "~a~:[~@[ · ~a left~]~;~*~] · ~a"
                                (cond (waiting "Waiting for you")
                                      ((agent-busy agent) "Working…")
                                      (out-now (if (integerp out)
                                                   (format nil "Out of usage until ~a" (format-reset-time out))
                                                   "Out of usage"))
                                      (t (agent-pane-ready pane)))
                                out-now usage (agent-context-summary agent)))
    (gtk:widget-set-css-classes (pane-status pane)
                                (if (or out-now (usage-low-p windows)) '("warning") '("dim-label")))
    (gtk:widget-set-tooltip-text (pane-status pane)
                                 (format nil "~@[~a~]~@[~%Remaining usage: ~a~]~{~%~a~}"
                                         (agent-pane-version pane) usage
                                         (loop for w in windows
                                               when (window-resets-at w)
                                               collect (format nil "~a resets ~a" (window-label w)
                                                               (format-reset-time (window-resets-at w)))))))
  (refresh-activity))

(defun pane-set-busy (pane busy)
  (let ((agent (agent-pane-agent pane)))
    (gtk:button-set-label (pane-send pane) (if busy "Send now" "Send"))
    (gtk:widget-set-visible (agent-pane-stop pane) busy)
    (let ((in-task (and (workflow-running-p) (workflow-agent-p *workflow* agent))))
      (gtk:button-set-label (agent-pane-stop pane) (if in-task "Pause" "Stop"))
      (gtk:widget-set-tooltip-text (agent-pane-stop pane)
                                   (if in-task
                                       (format nil "Pause ~a's part of the task; work that doesn't depend on it carries on"
                                               (agent-name agent))
                                       (format nil "Stop ~a" (agent-name agent)))))
    (gtk:widget-set-tooltip-text (pane-send pane)
                                 (if busy (format nil "Send to ~a while it keeps working" (agent-name agent))
                                     (format nil "Talk to ~a alone" (agent-name agent))))
    (pane-update-status pane)))

;;; Approvals

(defun decision-text (decision)
  (if (consp decision)
      (format nil "Answered: ~{~a~^; ~}" (mapcar #'cdr (cdr decision)))
      (ecase decision
        (:allow "Allowed")
        (:always "Allowed for the rest of this conversation")
        (:deny "Denied"))))

(defun approval-buttons (agent id)
  "Deny, Always allow and Allow, answering AGENT's approval request ID."
  (let ((buttons (make-instance 'gtk:box :spacing 6 :halign :end)))
    (flet ((choice (label decision classes tooltip)
             (let ((button (make-instance 'gtk:button :label label :css-classes classes :tooltip-text tooltip)))
               (gobject:connect button :clicked
                                (lambda (b) (declare (ignore b)) (agent-answer-approval agent id decision)))
               (gtk:box-append buttons button))))
      (choice "Deny" :deny '("destructive-action") "Don't run it; the agent carries on without it")
      (choice "Always allow" :always '() "Allow this, and things like it, for the rest of this conversation")
      (choice "Allow" :allow '("suggested-action") "Run it this once"))
    buttons))

(defun approval-heading (agent text)
  "A card's heading, saying which agent it's for (in its color, in the conversation)."
  (make-instance 'gtk:label :label text :xalign 0.0 :css-classes (list "heading" (agent-css-class agent))))

(defun pane-approval (pane id title text)
  "Show synchronized approval cards in the transcript and every conversation layout."
  (reveal-agent-transcript pane)
  (let ((agent (agent-pane-agent pane)))
    (flet ((card ()
             (let ((buttons (approval-buttons agent id)))
               (push (list buttons) (gethash id (agent-pane-approvals pane)))
               (gtk:build
                (gtk:box :orientation :vertical :spacing 6 :css-classes '("duet-approval")
                         (approval-heading agent (format nil "~a asks to run:" (agent-name agent)))
                         (gtk:label :label title :xalign 0.0 :wrap t :wrap-mode :char :selectable t
                                    :css-classes '("monospace"))
                         (gtk:label :label (or text "") :xalign 0.0 :wrap t :visible (and text (plusp (length text)) t)
                                    :css-classes '("dim-label"))
                         buttons)))))
      (pane-finish-stream pane)
      (pane-append pane (card))
      (pane-append *middle* (card))
      (pane-update-status pane)
      (attention (format nil "~a needs your approval" (agent-name agent)) title :sound t))))

(defun question-choices (question)
  "Widgets for one of AskUserQuestion's QUESTIONS, and a function giving the
answer chosen (nil when none): the chosen labels, or what was typed in Other."
  (let* ((multiple (jtrue-p (jget question "multiSelect")))
         (box (make-instance 'gtk:box :orientation :vertical :spacing 4))
         (checks '())
         (group nil))
    (flet ((check (label)
             (let ((check (make-instance 'gtk:check-button :label label)))
               (unless multiple
                 (if group (gtk:check-button-set-group check group) (setf group check)))
               check)))
      (let ((header (jget question "header")))
        (when (and (stringp header) (plusp (length header)))
          (gtk:box-append box (make-instance 'gtk:label :label header :xalign 0.0 :css-classes '("caption-heading" "dim-label")))))
      (gtk:box-append box (make-instance 'gtk:label :label (or (jget question "question") "") :xalign 0.0 :wrap t
                                         :wrap-mode :word-char :selectable t))
      (dolist (option (jlist (jget question "options")))
        (let ((check (check (jget option "label")))
              (description (jget option "description")))
          (push (cons check (jget option "label")) checks)
          (gtk:box-append box check)
          (when (and (stringp description) (plusp (length description)))
            (gtk:box-append box (make-instance 'gtk:label :label description :xalign 0.0 :wrap t :wrap-mode :word-char
                                               :margin-start 28 :css-classes '("dim-label"))))))
      (let ((other (check "Other:"))
            (entry (make-instance 'gtk:entry :hexpand t :placeholder-text "Your own answer")))
        (gobject:connect entry :changed
                         (lambda (e) (when (plusp (length (gtk:editable-get-text e)))
                                       (gtk:check-button-set-active other t))))
        (gtk:box-append box (gtk:build (gtk:box :spacing 6 other entry)))
        (setf checks (nreverse checks))
        (values box
                (lambda ()
                  (let ((chosen (loop for (check . label) in checks
                                      when (gtk:check-button-get-active check) collect label))
                        (typed (string-trim " " (gtk:editable-get-text entry))))
                    (when (and (gtk:check-button-get-active other) (plusp (length typed)))
                      (setf chosen (append chosen (list typed))))
                    (and chosen (format nil "~{~a~^, ~}" chosen))))
                (cons other (mapcar #'car checks))
                entry)))))

(defun question-card (pane id questions)
  "A card for AGENT's QUESTIONS, registered with PANE's approvals so the
answer settles it, wherever it was given."
  (let* ((agent (agent-pane-agent pane))
         (buttons (make-instance 'gtk:box :spacing 6 :halign :end))
         (submit (make-instance 'gtk:button :label "Submit" :css-classes '("suggested-action") :sensitive nil))
         (skip (make-instance 'gtk:button :label "Skip" :tooltip-text "Don't answer; the agent carries on without it"))
         (card (gtk:build (gtk:box :orientation :vertical :spacing 10 :css-classes '("duet-approval")
                                   (approval-heading agent (format nil "~a has ~:[a question~;questions~]:"
                                                                   (agent-name agent) (rest questions))))))
         (answers '())
         (boxes '()))
    (labels ((answers ()
               ;; Every question's answer, or nil while one is unanswered.
               (loop for (question . answer) in answers
                     for text = (funcall answer)
                     unless text return nil
                     collect (cons question text)))
             (refresh (&rest ignore)
               (declare (ignore ignore))
               (gtk:widget-set-sensitive submit (and (answers) t)))
             (finish (decision)
               (agent-answer-approval agent id decision)))
      (dolist (question questions)
        (multiple-value-bind (box answer checks entry) (question-choices question)
          (dolist (check checks) (gobject:connect check :toggled #'refresh))
          (gobject:connect entry :changed #'refresh)
          (push (cons (or (jget question "question") "") answer) answers)
          (push box boxes)
          (gtk:box-append card box)))
      (setf answers (nreverse answers))
      (gobject:connect submit :clicked
                       (lambda (b) (declare (ignore b))
                         (let ((chosen (answers)))
                           (when chosen (finish (cons :answers chosen))))))
      (gobject:connect skip :clicked (lambda (b) (declare (ignore b)) (finish :deny))))
    (gtk:box-append buttons skip)
    (gtk:box-append buttons submit)
    (gtk:box-append card buttons)
    (push (cons buttons boxes) (gethash id (agent-pane-approvals pane)))
    card))

(defun pane-question (pane id questions)
  (reveal-agent-transcript pane)
  (let ((agent (agent-pane-agent pane)))
    (pane-finish-stream pane)
    (pane-append pane (question-card pane id questions))
    (pane-append *middle* (question-card pane id questions))
    (pane-update-status pane)
    (attention (format nil "~a has a question" (agent-name agent)) "It's waiting for your answer." :sound t)))

(defun pane-approval-answered (pane id decision)
  "Settle every card for approval ID: the buttons give way to the answer."
  (let ((cards (gethash id (agent-pane-approvals pane))))
    (when cards
      (remhash id (agent-pane-approvals pane))
      (loop for (buttons . boxes) in cards
            do (dolist (box boxes) (gtk:widget-set-sensitive box nil))
               (clear-box buttons)
               (gtk:box-append buttons (make-instance 'gtk:label :label (decision-text decision)
                                                      :css-classes (if (eq decision :deny) '("error") '("dim-label")))))
      (pane-update-status pane))))

(defvar *replaying-conversation* nil "Replay must not record new history entries.")

(defun ensure-conversation-reply (pane)
  "Give this reply its own conversation bubble and command line."
  (when (and *middle* (not (agent-pane-conversation-box pane)))
    (let* ((agent (agent-pane-agent pane))
           (bubble (middle-bubble (agent-id agent) (agent-name agent) ""))
           (body (gtk:widget-get-last-child bubble))
           (label (make-instance 'gtk:label :label (format nil "~a is working…" (agent-name agent))
                                          :xalign 0.0 :wrap t :selectable t
                                          :wrap-mode :word-char)))
      ;; Only the newest message follows live commands; older messages keep
      ;; the command that accompanied them.
      (setf (agent-pane-activity-labels pane)
            (remove (agent-pane-conversation-activity pane)
                    (agent-pane-activity-labels pane)))
      (gtk:box-append body label)
      (let ((activity (make-agent-activity-label pane)))
        (setf (agent-pane-activity-id pane) nil
              (agent-pane-activity-title pane) "")
        (gtk:label-set-text activity "")
        (gtk:widget-set-visible activity nil)
        (gtk:widget-set-margin-start activity +conversation-gutter+)
        (gtk:box-append bubble activity)
        (setf (agent-pane-conversation-activity pane) activity))
      (setf (agent-pane-conversation-box pane) body
            (agent-pane-conversation-label pane) label
            (agent-pane-conversation-placeholder-p pane) t))))

(defun retire-conversation-activity (pane)
  (setf (agent-pane-activity-labels pane)
        (remove (agent-pane-conversation-activity pane) (agent-pane-activity-labels pane))
        (agent-pane-conversation-activity pane) nil))

(defun clear-conversation-placeholder (pane)
  "Stop claiming work is in progress when a pending reply ends without text."
  (when (agent-pane-conversation-placeholder-p pane)
    (when (agent-pane-conversation-label pane)
      (gtk:label-set-text (agent-pane-conversation-label pane) ""))
    (setf (agent-pane-conversation-placeholder-p pane) nil)))

(defun begin-conversation-turn (pane)
  ;; A prompt can also be an injected message during a live text block.
  (unless (agent-pane-stream-box pane)
    (pane-finish-stream pane nil)
    (retire-conversation-activity pane))
  (setf (agent-pane-conversation-replies pane) nil))

(defun pane-finish-stream (pane &optional (preserve-pending t))
  "End streamed text, keeping a command-only bubble waiting for its reply.
Finished text, a new turn and clearing the pane explicitly discard that pending state."
  (flush-pane-stream pane)
  (unless (and preserve-pending
               (agent-pane-conversation-label pane)
               (agent-pane-conversation-placeholder-p pane))
    (clear-conversation-placeholder pane)
    (setf (agent-pane-conversation-box pane) nil
          (agent-pane-conversation-label pane) nil
          (agent-pane-conversation-placeholder-p pane) nil))
  (setf (agent-pane-stream-box pane) nil (agent-pane-stream-label pane) nil (agent-pane-stream-text pane) ""))

(defun flush-pane-stream (pane)
  "Publish pending deltas before their stream is retired."
  (when (agent-pane-stream-timeout pane)
    (glib:source-remove (agent-pane-stream-timeout pane))
    (setf (agent-pane-stream-timeout pane) nil)
    (when (agent-pane-stream-label pane)
      (gtk:label-set-text (agent-pane-stream-label pane) (agent-pane-stream-text pane))
      (when (agent-pane-conversation-label pane)
        (gtk:label-set-text (agent-pane-conversation-label pane) (agent-pane-stream-text pane))
        (pane-scroll-to-end *middle*))
      (pane-scroll-to-end pane))))

(defun pane-delta (pane text)
  (when (zerop (length text)) (return-from pane-delta))
  (ensure-conversation-reply pane)
  (unless (agent-pane-stream-box pane)
    (let ((box (make-instance 'gtk:box :orientation :vertical :spacing 6))
          (label (make-instance 'gtk:label :xalign 0.0 :wrap t :selectable t :wrap-mode :word-char)))
      (gtk:box-append box label)
      (setf (agent-pane-stream-box pane) (pane-append pane box)
            (agent-pane-stream-label pane) label
            (agent-pane-stream-text pane) "")))
  (setf (agent-pane-stream-text pane) (append-stream-text (agent-pane-stream-text pane) text))
  (when (agent-pane-conversation-label pane)
    (setf (agent-pane-conversation-placeholder-p pane) nil))
  ;; Bound GTK layout work to twenty updates per second. Completion renders
  ;; the full reply directly and cancels this source in PANE-FINISH-STREAM.
  (unless (agent-pane-stream-timeout pane)
    (setf (agent-pane-stream-timeout pane)
          (glib:timeout-add glib:+priority-default+ 50
            (lambda ()
              (flush-pane-stream pane)
              nil)))))

(defun pane-text (pane text)
  "A finished block of a reply: rendered in place of what was streamed."
  ;; Flush before replacing either label; finishing must not touch removed widgets.
  (flush-pane-stream pane)
  (let ((*image-directory* (or *image-directory*
                              (agent-directory (agent-pane-agent pane))
                              *directory*)))
    (ensure-conversation-reply pane)
    (when (agent-pane-conversation-box pane)
      (setf (agent-pane-conversation-placeholder-p pane) nil)
      (clear-box (agent-pane-conversation-box pane))
      (render-markdown (agent-pane-conversation-box pane) text)
      (push (cons text (gtk:widget-get-parent (agent-pane-conversation-box pane)))
            (agent-pane-conversation-replies pane))
      (pane-scroll-to-end *middle*))
    (let ((box (or (agent-pane-stream-box pane)
                   (pane-append pane (make-instance 'gtk:box :orientation :vertical :spacing 6)))))
      (clear-box box)
      (render-markdown box text)
      (pane-finish-stream pane nil)
      (pane-scroll-to-end pane))))

(defun set-agent-activity (pane text &optional error)
  "Keep the latest command/edit visible without adding transcript rows."
  (let ((line (substitute #\Space #\Return (substitute #\Space #\Newline text))))
    (setf (agent-pane-activity-text pane) line)
    (dolist (label (agent-pane-activity-labels pane))
      (gtk:label-set-text label line)
      (gtk:widget-set-visible label t)
      (gtk:widget-set-tooltip-text label text)
      (gtk:widget-set-css-classes label
        (list "duet-live-activity" (if error "error" (agent-css-class (agent-pane-agent pane))))))))

(defun make-agent-activity-label (pane)
  (let ((label (make-instance 'gtk:label :label (agent-pane-activity-text pane)
                             :xalign 0.0 :ellipsize :end :max-width-chars 48
                             :margin-start 12 :margin-end 12 :margin-bottom 6
                             :css-classes (list "duet-live-activity" (agent-css-class (agent-pane-agent pane))))))
    (gtk:widget-set-tooltip-text label (agent-pane-activity-text pane))
    (push label (agent-pane-activity-labels pane))
    label))

(defun pane-tool (pane id title)
  (unless (agent-pane-conversation-activity pane)
    (ensure-conversation-reply pane))
  ;; The streaming tool name may later be refined to its full command.
  ;; Keep that refinement, but do not save repeated identical notifications.
  (unless (or *replaying-conversation*
              (and (equal id (agent-pane-activity-id pane))
                   (equal title (agent-pane-activity-title pane))))
    (record (agent-id (agent-pane-agent pane)) :activity (format nil "Running · ~a" title)))
  (setf (agent-pane-activity-id pane) id
        (agent-pane-activity-title pane) title)
  (set-agent-activity pane (format nil "Running · ~a" title))
  (let ((entry (gethash id (agent-pane-tools pane)))
        (label (format nil "⚙ ~a" title)))
    (if entry
        (progn (gtk:label-set-text (gtk:expander-get-label-widget (car entry)) label)
          (gtk:widget-set-tooltip-text (car entry) label))
        (let* ((result (make-instance 'gtk:label :xalign 0.0 :wrap t :selectable t :wrap-mode :char
                                      :css-classes '("monospace" "dim-label")))
               ;; Ellipsized, so a long command doesn't widen the pane.
               (title-label (make-instance 'gtk:label :label label :xalign 0.0 :ellipsize :end :hexpand t))
               (expander (make-instance 'gtk:expander :label-widget title-label :child result
                                        :css-classes '("duet-tool") :tooltip-text label)))
          (pane-finish-stream pane)
          (setf (gethash id (agent-pane-tools pane)) (cons expander result))
          (pane-append pane expander)))))

(defun pane-tool-result (pane id text error)
  (let ((entry (gethash id (agent-pane-tools pane))))
    (when entry
      ;; An older parallel tool finishing must not replace the newest command.
      (when (equal id (agent-pane-activity-id pane))
        (unless *replaying-conversation*
          (record (agent-id (agent-pane-agent pane)) (if error :activity-error :activity)
                  (format nil "~a · ~a" (if error "Failed" "Finished")
                          (agent-pane-activity-title pane))))
        (set-agent-activity pane
          (format nil "~a · ~a" (if error "Failed" "Finished")
                  (agent-pane-activity-title pane)) error))
      (gtk:label-set-text (cdr entry) (if (> (length text) 4000) (concatenate 'string (subseq text 0 4000) "…") text))
      (when error
        (let ((title (gtk:expander-get-label-widget (car entry))))
          (gtk:label-set-text title (format nil "~a — failed" (gtk:label-get-text title))))))))

(defun pane-thinking (pane text)
  (let ((text (string-trim '(#\Space #\Newline) (or text ""))))
    (when (plusp (length text))
      (let* ((first-line (subseq text 0 (or (position #\Newline text) (length text))))
             (title (make-instance 'gtk:label :label (format nil "💭 ~a" first-line)
                                   :xalign 0.0 :ellipsize :end :hexpand t)))
        (pane-finish-stream pane)
        (pane-append pane (make-instance 'gtk:expander :label-widget title
                                         :child (markdown-box text "dim-label")
                                         :css-classes '("duet-tool")))))))

(defparameter *prompt-preview-length* 2000
  "Prompts longer than this many characters (reviews quote whole diffs) show
their beginning until expanded: GTK rewraps every label on each resize, and a
few such prompts made resizing take a large part of a second.")

(defun prompt-preview (text)
  "TEXT's beginning, cut at a line break near *PROMPT-PREVIEW-LENGTH* if there's one."
  (let* ((limit (min (length text) *prompt-preview-length*))
         (break (position #\Newline text :end limit :from-end t)))
    (subseq text 0 (if (and break (> break (floor limit 2))) break limit))))

(defun pane-prompt (pane text)
  ;; A side-pane message can arrive halfway through an assistant block.
  ;; Keep its widget so the remaining deltas and final text update it.
  (let ((label (make-instance 'gtk:label :label text :wrap t :xalign 0.0 :selectable t :wrap-mode :word-char))
        (box (make-instance 'gtk:box :orientation :vertical :spacing 4 :halign :end
                                     :css-classes '("duet-prompt"))))
    (gtk:box-append box label)
    (when (> (length text) *prompt-preview-length*)
      (let ((preview (concatenate 'string (prompt-preview text) "…"))
            (more (format nil "Show all (~:d characters)" (length text)))
            (button (make-instance 'gtk:button :halign :start :css-classes '("flat")))
            (all nil))
        (flet ((show ()
                 (gtk:label-set-text label (if all text preview))
                 (gtk:button-set-label button (if all "Show less" more))))
          (show)
          (gobject:connect button :clicked
                           (lambda (b) (declare (ignore b))
                             (setf all (not all))
                             (show))))
        (gtk:box-append box button)))
    (pane-append pane box)))

(defun agent-pane-event (agent kind &key text title id error busy decision questions)
  (let ((pane (agent-pane-of agent)))
    (when pane
      (record-agent-event pane kind text id error)
      (reconcile-stall-cards agent)
      (ecase kind
        (:busy (pane-set-busy pane busy) (update-controls))
        (:prompt
         (begin-conversation-turn pane)
         (pane-prompt pane text))
        (:delta (pane-delta pane text))
        (:text (pane-text pane text))
        (:thinking (pane-thinking pane text))
        (:tool (pane-tool pane id title))
        (:tool-result (pane-tool-result pane id text error))
        (:approval (pane-approval pane id title text))
        (:question (pane-question pane id questions))
        (:approval-answered (pane-approval-answered pane id decision))
        (:usage (pane-update-status pane) (refresh-pause-cards agent))
        (:note (pane-note pane text "error"))
        (:turn-done
         ;; A turn that streamed but never finished its block.
         (when (agent-pane-stream-box pane)
           (pane-text pane (copy-seq (agent-pane-stream-text pane))))
         (clear-conversation-placeholder pane)
         (retire-conversation-activity pane)
         (when error (pane-note pane (or text "The turn failed.") "error")))))))

(defun clear-agent-pane (pane)
  (flush-pane-stream pane)
  (setf (agent-pane-activity-labels pane)
        (remove (agent-pane-conversation-activity pane) (agent-pane-activity-labels pane))
        (agent-pane-conversation-activity pane) nil
        (agent-pane-conversation-replies pane) nil)
  (clear-box (pane-messages pane))
  (setf (agent-pane-activity-id pane) nil
        (agent-pane-activity-title pane) "")
  (set-agent-activity pane "No commands yet")
  (clrhash (agent-pane-tools pane))
  (clrhash (agent-pane-approvals pane))
  (pane-finish-stream pane nil)
  (pane-update-status pane))

(defun handle-clear-context-command (text attachments agents)
  "Consume /clear locally, using the same reset and history path as the button."
  (when (string= text "/clear")
    (when attachments (error "/clear does not accept attachments."))
    ;; Check all recipients before resetting any of them.
    (when (some (lambda (agent)
                  (and (workflow-running-p) (workflow-agent-p *workflow* agent))) agents)
      (error "Stop the task before clearing this agent's context."))
    (dolist (agent agents) (new-conversation agent))
    t))

(defun new-conversation (agent)
  "A new conversation with AGENT alone. The history goes on, AGENT's part
of it now carried on from here."
  (when (and (workflow-running-p) (workflow-agent-p *workflow* agent))
    (pane-note (agent-pane-of agent) "Stop the task before clearing this agent's context.")
    (return-from new-conversation nil))
  (agent-cancel agent)
  (agent-new-conversation agent)
  (let ((pane (agent-pane-of agent)))
    (clear-agent-pane pane)
    (pane-note pane "New conversation."))
  (when *history*
    (record (agent-id agent) :note "New conversation.")
    (save-current-history))
  t)

;;; History: each conversation kept, to look back on and carry on (history.lisp)

(defun record (who kind text)
  "Add to the conversation's history. A prompt starts one if need be; what
comes without one (a turn stopped as the folder changes) isn't kept."
  (when (and *directory* (stringp text) (plusp (length text)))
    (when (and (null *history*) (eq kind :prompt))
      (setf *history* (make-history *directory*)))
    (when *history*
      (history-add-entry *history* who kind text))))

(defun record-agent-event (pane kind text id error)
  "Keep what an agent pane shows (but deltas, thinking and approvals)."
  (let ((who (agent-id (agent-pane-agent pane))))
    (case kind
      (:prompt (record who :prompt text) (save-current-history))
      (:text (record who :text text))
      (:tool-result
       ;; Recorded when it ends, by then with its full title.
       (let ((entry (gethash id (agent-pane-tools pane))))
         (when entry
           (record who :tool (format nil "~a~:[~; — failed~]"
                                     (string-left-trim "⚙ " (gtk:label-get-text (gtk:expander-get-label-widget (car entry))))
                                     error)))))
      (:note (record who :note text))
      (:turn-done
       ;; A reply that streamed but never finished its block.
       (when (agent-pane-stream-box pane)
         (record who :text (copy-seq (agent-pane-stream-text pane))))
       (when error (record who :error (if (plusp (length text)) text "The turn failed.")))
       (save-current-history)))))

(defun save-current-history (&key synchronous)
  "Save the history, with what resumes each agent's conversation."
  (when *history*
    (dolist (agent (list *claude* *codex*))
      (setf (history-handle *history* (agent-id agent)) (agent-conversation-handle agent)))
    (if synchronous (save-history *history*) (queue-history-save *history*))))

(defun finish-history ()
  "Save the history; what is said next starts another."
  (save-current-history)
  (setf *history* nil))

(defun agents-idle-p ()
  (not (or (workflow-running-p) (agent-busy *claude*) (agent-busy *codex*))))

(defun new-duet-conversation ()
  "New conversations with both, and an empty history."
  (if (not (agents-idle-p))
      (pane-note *middle* "Stop the agents before starting a new conversation." "error")
      (progn
        (finish-history)
        (setf *workflow* nil
              *recovery-card* nil)
        (update-controls)
        (dolist (agent (list *claude* *codex*))
          (agent-new-conversation agent)
          (clear-agent-pane (agent-pane-of agent)))
        (clear-box (pane-messages *middle*))
        (clear-checklist)
        (pane-note *middle* "New conversation."))))

(defun replay-history (history)
  "Show HISTORY's entries in the panes, as they were."
  (let ((*replaying-conversation* t)
        (*image-directory* (history-folder history)))
    (dolist (entry (history-entries history))
      (destructuring-bind (&key who kind text &allow-other-keys) entry
        (case who
          ((:claude :codex)
           (let ((pane (cdr (assoc who *panes*))))
             (case kind
               (:prompt (begin-conversation-turn pane) (pane-prompt pane text))
               (:text (pane-text pane text))
               ((:activity :activity-error)
                (unless (agent-pane-conversation-activity pane)
                  (ensure-conversation-reply pane))
                (set-agent-activity pane text (eq kind :activity-error)))
               (:tool
                (pane-note pane (format nil "⚙ ~a" text) "dim-label" "duet-tool")
                ;; Older histories have completion titles but no live activity events.
                (unless (some (lambda (saved)
                                (and (eq who (getf saved :who))
                                     (member (getf saved :kind) '(:activity :activity-error))))
                              (history-entries history))
                  (unless (agent-pane-conversation-activity pane)
                    (ensure-conversation-reply pane))
                  (set-agent-activity pane (format nil "Finished · ~a" text))))
               (:error (pane-note pane text "error"))
               (t (pane-note pane text)))))
          (:user (middle-bubble :user "You  ·  request" text))
          (:middle
           (case kind
             (:text
              ;; The header, as MIDDLE-MESSAGE wrote it, says whose it was.
              (let* ((end (or (search (format nil "~%~%") text) (length text)))
                     (header (subseq text 0 end))
                     (body (string-left-trim '(#\Newline) (subseq text end))))
                (cond
                  ((string= header "Check") (middle-check body))
                  ;; Earlier conversations saved checklists as "Review checklist".
                  ((member header '("Task checklist" "Review checklist") :test #'string=)
                   (show-checklist body :notify nil))
                  (t (cond ((uiop:string-prefix-p (format nil "~a →" (agent-name *claude*)) header)
                            (middle-agent-message *claude* header body))
                           ((uiop:string-prefix-p (format nil "~a →" (agent-name *codex*)) header)
                            (middle-agent-message *codex* header body))
                           (t (middle-bubble :user header body)))))))
             (:error (pane-note *middle* text "error"))
             (t (pane-note *middle* text "dim-label" "duet-note")))))))
    ;; Saved activity cannot indicate a live turn after reopening history.
    (dolist (entry *panes*)
      (clear-conversation-placeholder (cdr entry)))))

(defun format-history-time (time)
  (multiple-value-bind (s minute hour day month year) (decode-universal-time time)
    (declare (ignore s))
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day hour minute)))

(defun open-history (id)
  "Show conversation ID again; the agents carry it on from their next message."
  ;; It may be this one, with entries not yet saved.
  (save-current-history :synchronous t)
  (let ((history (load-history id)))
    (cond
     ((not (agents-idle-p))
      (pane-note *middle* "Stop the agents before opening another conversation." "error"))
     ((null history)
      (pane-note *middle* "That conversation couldn't be read." "error"))
     ((not (uiop:directory-exists-p (history-directory history)))
      (pane-note *middle* (format nil "That conversation's folder, ~a, is gone." (history-directory history)) "error"))
     (t
      (finish-history)
      (set-directory (history-directory history))
      (dolist (agent (list *claude* *codex*))
        (agent-resume-conversation agent (history-handle history (agent-id agent)))
        (clear-agent-pane (agent-pane-of agent)))
      (clear-box (pane-messages *middle*))
      (clear-checklist)
      (replay-history history)
      (setf *history* history
            *workflow* nil
            *recovery-card* nil)
      (pane-note *middle* (format nil "Carrying on the conversation from ~a. Each agent picks it up at its next message."
                                  (format-history-time (history-updated history)))
                 "dim-label" "duet-note")
      ;; A task that stopped partway: shown as it was left, nothing run.
      (when (history-recoverable-p history)
        (offer-recovery history))
      (update-controls)))))

(defun fill-history-list (box popover &optional (query ""))
  (clear-box box)
  (flet ((item (child tooltip action)
           (let ((b (make-instance 'gtk:button :css-classes '("flat") :child child :tooltip-text tooltip)))
             (gobject:connect b :clicked (lambda (b) (declare (ignore b))
                                           (gtk:popover-popdown popover)
                                           (funcall action)))
             (gtk:box-append box b))))
    (item (make-instance 'gtk:label :label "New conversation" :xalign 0.0)
          "Start new conversations with both agents" #'new-duet-conversation)
    (gtk:box-append box (make-instance 'gtk:separator))
    (gtk:box-append box (make-instance 'gtk:label :label (format nil "Earlier in ~a" (folder-label *directory*))
                                       :xalign 0.0 :margin-start 6 :ellipsize :start :max-width-chars 24
                                       :css-classes '("caption-heading" "dim-label")))
    (let ((summaries (list-histories :directory *directory* :limit (if (zerop (length query)) 30 nil) :query query)))
      (unless summaries
        (gtk:box-append box (make-instance 'gtk:label :label (if (zerop (length query)) "Nothing yet" "No matching conversations") :xalign 0.0 :margin-start 6
                                           :css-classes '("dim-label"))))
      (dolist (summary summaries)
        (let ((id (getf summary :id))
              (title (or (getf summary :title) "Untitled")))
          (item (gtk:build
                 (gtk:box :orientation :vertical
                          (gtk:label :label (format nil "~:[~;• ~]~a" (and *history* (equal id (history-id *history*))) title)
                                     :xalign 0.0 :ellipsize :end :max-width-chars 24)
                          (gtk:label :label (format nil "~a  ·  ~d entries" (format-history-time (getf summary :updated))
                                                    (getf summary :entry-count))
                                     :xalign 0.0 :css-classes '("caption" "dim-label"))))
                title (lambda () (open-history id))))))))

(defun make-history-button ()
  (let* ((button (make-instance 'gtk:menu-button :label "History"
                                :tooltip-text "Start a new conversation, or carry on an earlier one in this folder"))
         (popover (make-instance 'gtk:popover))
         (entry (make-instance 'gtk:entry :placeholder-text "Search conversations"))
         (content (make-instance 'gtk:box :orientation :vertical :spacing 6))
         (box (make-instance 'gtk:box :orientation :vertical :spacing 2
                             :margin-start 4 :margin-end 4 :margin-top 4 :margin-bottom 4)))
    (gtk:box-append content entry)
    (gtk:box-append content (make-instance 'gtk:scrolled-window :child box :hscrollbar-policy :never
                                             :max-content-height 480 :propagate-natural-height t))
    (gtk:popover-set-child popover content)
    (gobject:connect entry :changed
                     (lambda (e) (fill-history-list box popover (gtk:editable-get-text e))))
    ;; Read the saved conversations when the menu opens, not before.
    (gobject:connect popover :show (lambda (p) (declare (ignore p)) (fill-history-list box popover (gtk:editable-get-text entry))))
    (gtk:menu-button-set-popover button popover)
    button))

;;; The conversation between them

(defun participant-name (who)
  (case who
    (:user "You")
    (:system nil)
    (t (agent-name who))))

(defun kind-label (kind)
  (case kind
    (:request "request")
    (:proposal "proposal")
    (:plan "plan")
    (:work "work done")
    (:review "review")
    (t nil)))

(defun refresh-checklist-summary ()
  (let ((button (control :checklist-summary)))
    (when button
      (gtk:widget-set-visible button (and *checklist-boxes* (not (setting :checklist-visible t))))
      (gtk:label-set-text (gtk:button-get-child button)
                          (if *checklist-boxes*
                              (checklist-summary (mapcar (lambda (entry) (getf (cdr entry) :data))
                                                        (reverse *checklist-boxes*)))
                              "")))))

(defun make-checklist-summary ()
  (let ((button (make-instance 'gtk:button :visible nil :css-classes '("flat")
                               :margin-start 12 :margin-end 12 :margin-bottom 6
                               :tooltip-text "Open task cards and review findings"
                               :child (make-instance 'gtk:label :wrap t :xalign 0.0))))
    (setf (getf *controls* :checklist-summary) button)
    (gobject:connect button :clicked
                     (lambda (widget) (declare (ignore widget))
                       (gtk:check-button-set-active (control :checklist-toggle) t)))
    button))

(defun clear-checklist ()
  "Reset the cards and summary when changing conversations or folders."
  (setf *checklist-boxes* nil)
  (when *checklist*
    (clear-box (pane-messages *checklist*))
    (pane-note *checklist* "Tasks will appear here once assigned." "dim-label"))
  (refresh-checklist-summary))

(defun render-checklist-card (box data &optional request-expanded resolved-expanded)
  (clear-box box)
  (if (getf data :legacy)
      (render-markdown box (getf data :legacy))
      (progn
        (gtk:box-append box (make-instance 'gtk:label :label (getf data :title) :wrap t :xalign 0.0
                                          :css-classes '("heading")))
        (gtk:box-append box (make-instance 'gtk:label :label (format nil "Assigned to ~a" (getf data :author))
                                          :wrap t :xalign 0.0 :css-classes '("dim-label")))
        (gtk:box-append box (make-instance 'gtk:label :label (getf data :state) :wrap t :xalign 0.0
                                          :halign :start
                                          :css-classes (list "duet-pill" (format nil "duet-pill-~a" (getf data :tone)))))
        (let ((request (make-instance 'gtk:expander :label "Show request" :expanded request-expanded
                                      :child (markdown-box (getf data :request)))))
          (gtk:box-append box request)
          (when (getf data :open)
            (gtk:box-append box (markdown-box (getf data :open))))
          (let* ((count (+ (getf data :fixed-count) (getf data :dismissed-count)))
                 (resolved (when (plusp count)
                             (make-instance 'gtk:expander :expanded resolved-expanded
                                            :label (format nil "~d fixed · ~d dismissed"
                                                           (getf data :fixed-count) (getf data :dismissed-count))
                                            :child (markdown-box (format nil "~a~%~%~a"
                                                                         (or (getf data :fixed) "")
                                                                         (or (getf data :dismissed) "")))))))
            (when resolved (gtk:box-append box resolved))
            (values request resolved))))))

(defun show-checklist (text &key (notify t))
  "Update a task card in place, preserving expanded details and the reader's position."
  (declare (ignore notify))
  (when *checklist*
    (let* ((data (checklist-card-data text))
           (key (getf data :key))
           (entry (assoc key *checklist-boxes* :test #'string=))
           (old (cdr entry))
           (box (or (getf old :box) (make-instance 'gtk:box :orientation :vertical :spacing 8
                                                               :css-classes '("duet-checklist")))))
      (multiple-value-bind (request resolved)
          (render-checklist-card box data
                                 (and (getf old :request) (gtk:expander-get-expanded (getf old :request)))
                                 (and (getf old :resolved) (gtk:expander-get-expanded (getf old :resolved))))
        (let ((value (list :box box :data data :request request :resolved resolved)))
          (if entry
              (setf (cdr entry) value)
              (progn
                (unless *checklist-boxes* (clear-box (pane-messages *checklist*)))
                (push (cons key value) *checklist-boxes*)
                (pane-append *checklist* box)))))
      (refresh-checklist-summary))))

(defun remember-checklist-width ()
  "Persist only a visible, allocated checklist after restoration has finished."
  (let ((panel (control :checklist-panel)))
    (when (and panel (gtk:widget-get-visible panel)
               (not (control :restore-checklist-width)))
      (let ((width (gtk:widget-get-width panel)))
        (when (and (plusp width) (not (eql width (saved-checklist-width))))
          (setf (setting :checklist-width) width))))))

(defun restore-checklist-width ()
  "Restore the saved width once the visible panel has an allocation."
  (let ((width (control :restore-checklist-width))
        (panel (control :checklist-panel))
        (split (control :checklist-split)))
    (when (and width panel split (gtk:widget-get-visible panel)
               (plusp (gtk:widget-get-width panel)))
      (let* ((position (gtk:paned-get-position split))
             (target (max 0 (+ position (- (gtk:widget-get-width panel) width)))))
        (remf *controls* :restore-checklist-width)
        (unless (= position target)
          (gtk:paned-set-position split target)
          t)))))

(defun set-checklist-visible (visible)
  "Keep the checklist at a useful width, preserving the wide conversation layout."
  (when (and (control :sidebar-toggle)
             (not (eql (not (null visible))
                       (gtk:toggle-button-get-active (control :sidebar-toggle)))))
    (gtk:toggle-button-set-active (control :sidebar-toggle) (not (null visible))))
  (let ((panel (control :checklist-panel))
        (content (control :checklist-split)))
    (when (and panel content (not (eql (not (null visible)) (gtk:widget-get-visible panel))))
      (remf *controls* :checklist-outer-restored)
      (when visible
        (remember-layout)
        (setf (getf *controls* :pre-checklist-positions)
              (unless (or (control :narrow-layout) (control :equalize-agent-panels))
                (list (gtk:paned-get-position (control :agent-split))
                      (gtk:paned-get-position (control :conversation-split))))))
      (unless visible
        (setf (getf *controls* :restore-checklist-positions)
              (control :pre-checklist-positions))
        (remf *controls* :pre-checklist-positions))
      (unless visible
        (remember-checklist-width)
        (remf *controls* :restore-checklist-width))
      (gtk:widget-set-visible panel (not (null visible)))
      (when visible
        (setf (getf *controls* :restore-checklist-width)
              (or (saved-checklist-width) 300))
        (gtk:widget-queue-allocate content)))))

(defun make-checklist-panel (&key embedded)
  (multiple-value-bind (messages scroller) (make-message-list)
    (setf *checklist* (make-pane :messages messages :scroller scroller)
          *checklist-findings* nil)
    (clear-checklist)
    (let ((panel (gtk:build
                  (gtk:box :orientation :vertical :width-request 260
                           (gtk:label :visible (not embedded) :label "Tasks" :xalign 0.0
                                      :margin-start 12 :margin-top 8 :margin-bottom 8
                                      :css-classes '("title-4"))
                           (gtk:separator :visible (not embedded))
                           scroller))))
      (setf (getf *controls* :checklist-panel) panel)
      (gtk:widget-set-visible panel (setting :checklist-visible t))
      panel)))

(defun middle-check (text)
  "The check command's progress or result, in the conversation."
  (pane-append *middle* (gtk:build
                         (gtk:box :orientation :vertical :spacing 4 :css-classes '("duet-checklist")
                                  (gtk:label :label "Check" :xalign 0.0 :css-classes '("caption-heading"))
                                  (markdown-box text)))))

(defun conversation-heading-label (bubble)
  "The speaker label beside BUBBLE's chat head."
  (gtk:widget-get-last-child (gtk:widget-get-first-child bubble)))

(defun middle-bubble (side header text)
  "A message with an original initial badge and a compact left gutter."
  (let* ((identity (ecase side
                     (:user "duet-from-user")
                     (:claude "duet-from-claude")
                     (:codex "duet-from-codex")))
         ;; Plain letters and CSS circles: no downloaded artwork or brand logos.
         (head (make-instance 'gtk:label
                             :label (ecase side (:user "Y") (:claude "C") (:codex "G"))
                             :width-request +conversation-chat-head-size+
                             :height-request +conversation-chat-head-size+
                             ;; Decorative initial; the adjacent heading names the speaker.
                             :accessible-role :none
                             :halign :center :valign :start
                             :tooltip-text (ecase side (:user "You") (:claude "Claude") (:codex "ChatGPT"))
                             :css-classes '("duet-chat-head")))
         (body (markdown-box text)))
    (gtk:widget-set-margin-start body +conversation-gutter+)
    (pane-append *middle*
                 (gtk:build
                  (gtk:box :orientation :vertical :spacing 8 :hexpand t :halign :fill
                           :css-classes (list "duet-message" identity)
                           (gtk:box :orientation :horizontal :spacing +conversation-chat-head-spacing+
                                    head
                                    (gtk:label :label header :xalign 0.0 :wrap t
                                               :wrap-mode :word-char :hexpand t
                                               :css-classes '("caption-heading")))
                           body)))))

(defun middle-agent-message (agent header text)
  "Promote an existing live reply to a handoff instead of repeating it."
  (let* ((pane (agent-pane-of agent))
         (replies (and pane
                       (remove-if-not
                        (lambda (entry)
                          (eq (gtk:widget-get-parent (cdr entry)) (pane-messages *middle*)))
                        (agent-pane-conversation-replies pane))))
         ;; Claude's result is separate from its streamed text blocks. Prefer
         ;; the matching block, then the newest reply from this prompt.
         (entry (or (find text replies :key #'car
                          :test (lambda (a b)
                                  (string= (string-trim '(#\Space #\Tab #\Return #\Newline) a)
                                           (string-trim '(#\Space #\Tab #\Return #\Newline) b))))
                    (first replies)))
         (bubble (cdr entry)))
    (when pane (clear-conversation-placeholder pane))
    (if (and bubble (eq (gtk:widget-get-parent bubble) (pane-messages *middle*)))
        (progn
          (gtk:label-set-text (conversation-heading-label bubble) header)
          ;; A fallback may contain only the last block of a longer result.
          ;; Show the authoritative handoff without disturbing its command.
          (let ((body (gtk:widget-get-next-sibling (gtk:widget-get-first-child bubble))))
            (clear-box body)
            (render-markdown body text))
          (dolist (other (reverse replies))
            (when (and (not (eq other entry)) (plusp (length (car other)))
                       (search (car other) text))
              (let* ((old (cdr other))
                     (activity (gtk:widget-get-last-child old)))
                ;; Keep the command beneath the combined reply instead of
                ;; leaving a separate, textless message behind.
                (when (gtk:widget-get-visible activity)
                  (gtk:box-remove old activity)
                  (gtk:widget-insert-before activity bubble (gtk:widget-get-last-child bubble)))
                (gtk:box-remove (pane-messages *middle*) old))))
          (pane-scroll-to-end *middle*)
          (setf (agent-pane-conversation-replies pane) nil)
          bubble)
        (middle-bubble (agent-id agent) header text))))

(defun middle-message (from to text kind)
  (cond
    ((eq kind :findings)
     ;; A snapshot of the loop's tasks and findings; replay also accepts
     ;; earlier conversations' "Review checklist".
     (show-checklist text)
     ;; A loop's state has changed: working, checking, reviewing…
     (refresh-activity)
     (record :middle :text (format nil "Task checklist~%~%~a" text))
     (save-current-history))
    ((eq kind :check)
     (middle-check text)
     (record :middle :text (format nil "Check~%~%~a" text))
     (save-current-history))
    ((eq from :system)
     (pane-note *middle* text "dim-label" "duet-note")
     (record :middle :note text))
    (t
      (let ((header (format nil "~a → ~a~@[  ·  ~a~]" (participant-name from) (participant-name to)
                            (let ((label (kind-label kind)))
                              (if (eq kind :review)
                                  (format nil "~a: ~(~a~)" label
                                          (if (eq (review-verdict text) :approved) "approved" "changes requested"))
                                  label)))))
        (if (eq from :user)
            (middle-bubble :user header text)
            (middle-agent-message from header text))
        ;; The request is the conversation's title; the rest keep their header.
        (if (eq from :user)
            (record :user :prompt text)
            (record :middle :text (format nil "~a~%~%~a" header text)))
        (save-current-history)))))

(defun control (name) (getf *controls* name))

(defun selected-lead ()
  (if (= 1 (gtk:drop-down-get-selected (control :lead))) *codex* *claude*))

(defun selected-mode ()
  (case (gtk:drop-down-get-selected (control :mode))
    (1 :divide)
    (2 :compare)
    (3 :review-only)
    (t :review)))

(defun workflow-running-p ()
  (and *workflow* (eq (workflow-state *workflow*) :running)))

(defun update-controls ()
  (when *controls*
    (update-workflow-summary)
    (when (control :routing-note)
      (gtk:label-set-text (control :routing-note)
                          (if (and (eq (composer-recipient) :both) (workflow-running-p))
                              "To both: direction goes only to task agents with a live turn."
                              "")))
    (let ((running (workflow-running-p)))
      (gtk:button-set-label (control :send)
                            (if (and (eq (composer-recipient) :both) (not running)
                                     (not (agent-busy *claude*)) (not (agent-busy *codex*)))
                                "Start collaboration" "Send"))
      (gtk:widget-set-sensitive (control :send) t)
      (when (control :stop)
        (gtk:widget-set-visible (control :stop) running))
      (dolist (name '(:mode :lead :rounds :check :folder :permission :review-target :review-base :review-branch :review-count :review-agents))
        (gtk:widget-set-sensitive (control name) (not running)))
      ;; Only the modes that make a plan wait for one.
      (dolist (name '(:approve-plan :worktrees))
        (gtk:widget-set-sensitive (control name) (and (not running) (member (selected-mode) '(:divide :compare)))))
      (gtk:widget-set-visible (control :review-options) (eq (selected-mode) :review-only))
      ;; Mode and lead only matter when the message goes to both agents.
      (dolist (name '(:mode :lead))
        (let ((widget (control name)))
          (gtk:widget-set-visible (or (gtk:widget-get-parent widget) widget)
                                  (eq (composer-recipient) :both))))
      (let ((branch (member (gtk:drop-down-get-selected (control :review-target)) '(1 2))))
        (dolist (name '(:review-base :review-branch))
          (gtk:widget-set-sensitive (control name) (and (not running) branch)))
        (gtk:widget-set-sensitive (control :review-count)
                                  (and (not running) (= 3 (gtk:drop-down-get-selected (control :review-target))))))
      (dolist (name '(:rounds :check))
        (gtk:widget-set-sensitive (control name) (and (not running) (not (eq (selected-mode) :review-only)))))
      (gtk:label-set-text (pane-status *middle*)
                          (cond ((not running) "")
                                ((workflow-pending-plan *workflow*) "Waiting for you to approve the plan")
                                ((workflow-paused *workflow*)
                                 (format nil "Paused: ~a" (pause-summary *workflow*)))
                                (t (format nil "Working · ~{~a~^, ~}"
                                           (or (loop for a in (list *claude* *codex*)
                                                     when (agent-busy a) collect (agent-name a))
                                               '("waiting")))))))
    (refresh-activity)))

;;; Conversation with a sidebar, the original three panes, or conversation alone

(defvar *view* :conversation
  ":sidebar places stacked agents to the right; :agents-left places them to the left.
:panes retains the original layout.
:conversation
shows the conversation alone, with the agents' approvals and questions in it,
and a line saying what each is doing.")

(defparameter *views* '((:sidebar "Agents on right") (:agents-left "Agents on left") (:panes "Three panes") (:conversation "Conversation only")))

(defvar *view-only* '() "(widget . view)s: shown only in that view.")

(defun visible-in-view-p (only view)
  (or (eq only view) (and (eq only :panes) (sidebar-view-p view))))

(defun view-only (view widget)
  "Show WIDGET only in VIEW. Returns WIDGET."
  ;; Widgets gone from their pane (a new conversation) are forgotten.
  (setf *view-only* (remove-if-not #'gtk:widget-get-parent *view-only* :key #'car))
  (push (cons widget view) *view-only*)
  (gtk:widget-set-visible widget (visible-in-view-p view *view*))
  widget)

(defun remember-window-geometry (window)
  "Persist the normal window size and maximized state only when changed."
  (let ((size (multiple-value-list (gtk:window-get-default-size window)))
        (maximized (gtk:window-is-maximized window))
        (changed nil))
    ;; GTK retains the normal size while maximized.
    (when (and (every #'plusp size) (not (equal size (setting :window-size))))
      (setf (getf *settings* :window-size) size changed t))
    (unless (eql maximized (setting :window-maximized))
      (setf (getf *settings* :window-maximized) maximized changed t))
    (when changed (save-settings))))

(defun remember-layout ()
  "Remember divider positions for the current layout, independently of other views."
  (when (and (control :agent-split) (not (control :narrow-layout))
             (not (control :equalize-agent-panels)) (not (eq *view* :conversation)))
    (let ((positions
            (agent-layout-preference
             (or (control :restore-checklist-positions)
                 (control :pre-checklist-positions)
                 (control :pending-layout-positions)
                 (list (gtk:paned-get-position (control :agent-split))
                       (gtk:paned-get-position (control :conversation-split))))
             (agent-layout-custom-p))))
      (unless (equal positions (layout-positions *view*))
        (setf (setting (layout-position-key *view*)) positions)))))

(defun arrange-conversation (view)
  "Reuse the live widgets: changing layout never restarts an agent or a stream."
  (let ((outer (control :agent-split))
        (inner (control :conversation-split))
        (middle (control :middle-widget))
        (claude (control :claude-widget))
        (codex (control :codex-widget)))
    (when outer
      (migrate-agent-panel-positions)
      (setf (getf *controls* :custom-agent-split)
            (not (null (second (layout-positions view)))))
      (remf *controls* :agent-layout-size)
      (setf (getf *controls* :narrow-layout) nil)
      (remf *controls* :last-wide-positions)
      (remf *controls* :pending-layout-positions)
      (remf *controls* :layout-outer-restored)
      (remf *controls* :equalize-agent-panels)
      (gtk:orientable-set-orientation outer :horizontal)
      ;; The retained three-pane layout must respect each pane's minimum
      ;; size; otherwise dragging a divider can hide approvals and questions.
      (dolist (split (list outer inner))
        (gtk:paned-set-shrink-start-child split nil)
        (gtk:paned-set-shrink-end-child split nil))
      ;; Detach before reparenting; the Lisp widget references keep them alive.
      (gtk:paned-set-start-child outer nil)
      (gtk:paned-set-end-child outer nil)
      (gtk:paned-set-start-child inner nil)
      (gtk:paned-set-end-child inner nil)
      (gtk:orientable-set-orientation inner (if (eq view :panes) :horizontal :vertical))
      (gtk:widget-set-visible claude (not (eq view :conversation)))
      (gtk:widget-set-visible codex (not (eq view :conversation)))
      (if (eq view :panes)
          (progn
            (gtk:paned-set-start-child inner middle)
            (gtk:paned-set-end-child inner codex)
            (gtk:paned-set-start-child outer claude)
            (gtk:paned-set-end-child outer inner))
          (progn
            (gtk:paned-set-start-child inner claude)
            (gtk:paned-set-end-child inner codex)
            (gtk:paned-set-start-child outer (if (eq view :agents-left) inner middle))
            (unless (eq view :conversation)
              (gtk:paned-set-end-child outer (if (eq view :agents-left) middle inner)))))
      (unless (eq view :conversation)
        (let* ((saved (layout-positions view))
               (width (let ((width (gtk:widget-get-width outer))) (if (plusp width) width 1440))))
          ;; Apply after allocation: the inner pane still has its old axis and
          ;; extent here. Its default divider is calculated in the next frame.
          (setf (getf *controls* :pending-layout-positions)
                (list (or (first saved) (round (* width (case view (:sidebar 0.66) (:agents-left 0.34) (t 0.30)))))
                      (or (second saved)
                          (if (sidebar-view-p view)
                              (equal-agent-divider-position view
                                (let ((height (gtk:widget-get-height outer)))
                                  (if (plusp height) height 720)))
                              (equal-agent-divider-position view
                                (round (* width 0.70)) (round (* width 0.30)))))))
          (unless (second saved)
            (setf (getf *controls* :equalize-agent-panels) :pending)))))))

(defun update-agent-toggle ()
  "Keep the header shortcut and layout selector in sync with the current view."
  (when (control :agent-toggle)
    (if (eq *view* :conversation)
        (gtk:widget-remove-css-class (control :agent-toggle) "suggested-action")
        (gtk:widget-add-css-class (control :agent-toggle) "suggested-action")))
  (when (control :view)
    (let ((selected (position *view* *views* :key #'first)))
      (unless (= selected (gtk:drop-down-get-selected (control :view)))
        (gtk:drop-down-set-selected (control :view) selected)))))

(defun toggle-agents ()
  "Hide agents or restore the last selected agent layout."
  (set-view (if (eq *view* :conversation)
                (let ((saved (setting :last-agent-view :sidebar)))
                  (if (and (assoc saved *views*) (not (eq saved :conversation)))
                      saved :sidebar))
                :conversation)))

(defun set-view (view)
  "Show VIEW and persist the layout and its divider positions."
  (let ((view (if (assoc view *views*) view :sidebar)))
    (unless (eq view *view*)
      (remember-layout)
      (unless (eq *view* :conversation)
        (setf (getf *settings* :last-agent-view) *view*))
      ;; Checklist snapshots belong to the view in which they were captured.
      (remf *controls* :pre-checklist-positions)
      (remf *controls* :restore-checklist-positions)
      (remf *controls* :checklist-outer-restored)
      (setf *view* view)
      (arrange-conversation view))
    (unless (eq view :conversation)
      (setf (getf *settings* :last-agent-view) view))
    (setf (setting :view) view)
    (update-agent-toggle)
    (setf *view-only* (remove-if-not #'gtk:widget-get-parent *view-only* :key #'car))
    (loop for (widget . only) in *view-only*
          do (gtk:widget-set-visible widget (visible-in-view-p only view)))
    (refresh-activity)))

(defun agent-activity (agent)
  "Workflow detail, remaining usage and context beside the conversation header pill."
  (when (or (null *workflow*) (not (workflow-agent-member-p *workflow* agent)))
    (return-from agent-activity (gtk:label-get-text (pane-status (agent-pane-of agent)))))
  (let* ((presentation (workflow-agent-presentation *workflow* agent))
         (usage (usage-summary (agent-usage-windows agent))))
    (format nil "~{~a~^ · ~}"
            (remove nil
                    (list (getf presentation :detail)
                          (and (not (agent-out-of-usage-p agent))
                               (not (eq (agent-out-until agent) :unknown)) usage
                               (format nil "~a left" usage))
                          (agent-context-summary agent))))))

(defun refresh-activity ()
  "Bring the shared conversation's agent status lines up to date."
  (refresh-workflow-presentation)
  ;; The shared conversation has its own copy of each header pill.
  (dolist (agent (list *claude* *codex*))
    (when agent
      (let ((source (control (agent-setting-key agent "WORKFLOW-ACTIVITY")))
            (target (control (agent-setting-key agent "WORKFLOW-STATUS-PILL"))))
        (when (and source target)
          (gtk:label-set-text target (gtk:label-get-text source))
          (gtk:widget-set-tooltip-text target (gtk:widget-get-tooltip-text source))
          (gtk:widget-set-css-classes target (gtk:widget-get-css-classes source))))))
  (when (and (control :claude-activity) *panes*)
    (loop for (agent . name) in (list (cons *claude* :claude-activity) (cons *codex* :codex-activity))
          for label = (control name)
          for text = (agent-activity agent)
          do (gtk:label-set-text label text)
             (gtk:widget-set-tooltip-text label (gtk:widget-get-tooltip-text (pane-status (agent-pane-of agent))))
             (gtk:widget-set-css-classes label (if (or (search "Waiting for you" text) (search "usage" text)
                                                       (usage-low-p (agent-usage-windows agent)))
                                                   '("warning") '("dim-label"))))))

(defun make-activity-strip ()
  "Keep workflow pills and usage beside each name.
Model controls are available when the agent panels are hidden."
  (let ((claude (make-instance 'gtk:label :xalign 0.0 :ellipsize :end :css-classes '("dim-label")))
        (codex (make-instance 'gtk:label :xalign 0.0 :ellipsize :end :css-classes '("dim-label"))))
    (flet ((agent-column (agent label)
             (multiple-value-bind (models efforts) (make-model-dropdowns (agent-pane-of agent))
               (gtk:build
                (gtk:box :orientation :vertical :spacing 4
                         (gtk:box :spacing 6
                                  (gtk:label :label (agent-name agent) :css-classes (list "caption-heading" (agent-css-class agent)))
                                  label
                                  (make-workflow-activity-pill agent "WORKFLOW-STATUS-PILL"))
                         (let ((controls (wrapping-controls models efforts)))
                           ;; Lined up with the agent's name, as the strip has its own margins.
                           (gtk:widget-set-margin-start controls 0)
                           (gtk:widget-set-margin-end controls 0)
                           (gtk:widget-set-margin-top controls 0)
                           (gtk:widget-set-margin-bottom controls 0)
                           (view-only :conversation controls)))))))
    (let ((strip (gtk:build
                  (gtk:box :spacing 8 :margin-start 12 :margin-end 12 :margin-bottom 6 :homogeneous t
                           (agent-column *claude* claude)
                           (agent-column *codex* codex)))))
      (setf *controls* (list* :activity strip :claude-activity claude :codex-activity codex *controls*))
      strip))))

;;; A step paused: its agent ran out of usage, failed, or was paused by the
;;; user. Whatever is chosen goes through RECOVER-WORKFLOW (workflow.lisp),
;;; as recovering a reopened task does (below).

(defvar *pause-timers* '() "(step . glib source id): steps set to resume when usage resets.")
(defvar *pause-cards* '()
  "(step refresh settle) for the pause cards still waiting on a choice:
REFRESH follows the agent's usage, SETTLE takes the buttons away with a word.")
(defvar *closing* nil "True once the window is closing: no more cards.")

(defparameter *shutdown-wait-seconds* 30
  "How long a hand-off or resume waits for the stopped turn, and its editing
processes, to be confirmed gone before the card says they haven't.")

(defun clear-pause-timer (step)
  (let ((entry (assoc step *pause-timers*)))
    (when entry
      (ignore-errors (glib:source-remove (cdr entry)))
      (setf *pause-timers* (remove entry *pause-timers*)))))

(defun clear-pause-timers (&optional (why "Stopped"))
  "Stop waiting for usage to reset, and settle the pause cards with WHY."
  (dolist (entry *pause-timers*) (ignore-errors (glib:source-remove (cdr entry))))
  (let ((cards *pause-cards*))
    (setf *pause-timers* '()
          *pause-cards* '())
    (dolist (entry cards) (ignore-errors (funcall (third entry) why)))))

(defun later (thunk)
  "Call THUNK from the main loop once what's under way now has finished."
  (glib:timeout-add-seconds glib:+priority-default+ 0 (lambda () (unless *closing* (funcall thunk)) nil)))

(defun attempt-recovery (workflow action report &key step acknowledge-changes (while (constantly t)))
  "RECOVER-WORKFLOW, telling REPORT the outcome: :started, :stopped,
:abandoned, :repository-changed, :blocked, :unavailable, or (:error text).
Waiting for an agent's turn and editing processes to stop, nothing starts:
it is tried again every 2 seconds, REPORT told :pending-shutdown each time,
until they have stopped, or *SHUTDOWN-WAIT-SECONDS* pass and REPORT is told
:shutdown-unconfirmed, or WHILE returns false (the user chose otherwise)."
  (let ((started (get-universal-time)))
    (labels ((current-p () (and (eq workflow *workflow*) (funcall while)))
             (try ()
               (when (current-p)
                 ;; Inspect on a worker; all workflow and GTK mutations stay on
                 ;; the main loop. Each retry gets a fresh inspection.
                 (in-background
                  "recovery preflight"
                  (lambda () (inspect-workflow-recovery workflow))
                  (lambda (inspection why)
                    (when (current-p)
                      (let ((result (if why (list :error why)
                                        (handler-case
                                            (recover-workflow workflow action :step step
                                                              :inspection inspection
                                                              :acknowledge-changes acknowledge-changes)
                                          (error (e) (list :error (princ-to-string e)))))))
                        (cond ((not (eq result :pending-shutdown)) (funcall report result))
                              ((> (- (get-universal-time) started) *shutdown-wait-seconds*)
                               (funcall report :shutdown-unconfirmed))
                              (t (funcall report :pending-shutdown)
                                 (glib:timeout-add-seconds glib:+priority-default+ 2
                                                           (lambda () (try) nil)))))))))))
      (try))
    (update-controls)))

(defun recovery-outcome-text (result agent)
  "What RESULT (ATTEMPT-RECOVERY's) means for the user; AGENT the one it was for."
  (let ((name (if agent (agent-name agent) "The agent")))
    (cond ((eq result :pending-shutdown)
           (format nil "Waiting for ~a's stopped turn to finish stopping. Nothing starts until it has." name))
          ((eq result :shutdown-unconfirmed)
           (format nil "~a's stopped turn, or a process it started, still hasn't stopped, so nothing has been ~
started: the same files could be edited twice. Try again in a moment, or stop the task." name))
          ((eq result :repository-changed)
           "The folder or a worktree has changed since this stopped. Look at what's there; Carry on anyway goes on from it as it is now.")
          ((eq result :blocked)
           "It can't carry on yet: a folder or worktree is missing or in the middle of a merge, or work this depends on isn't finished.")
          ((eq result :unavailable) (format nil "~a is out of usage." name))
          ((and (consp result) (eq (first result) :error)) (format nil "That didn't work: ~a" (second result)))
          (t nil))))

(defun resume-when-reset (step reset fire)
  "Call FIRE, the card's own Resume, a minute after RESET (a universal time),
so what comes of it shows on STEP's card."
  (let ((seconds (max 5 (+ 60 (- reset (get-universal-time))))))
    (clear-pause-timer step)
    (push (cons step (glib:timeout-add-seconds glib:+priority-default+ seconds
                                               (lambda ()
                                                 (setf *pause-timers* (remove step *pause-timers* :key #'car))
                                                 (funcall fire)
                                                 nil)))
          *pause-timers*)))

(defun refresh-pause-cards (agent)
  "AGENT's usage changed: its cards may now know when it resets."
  (dolist (entry *pause-cards*)
    (when (eq (step-agent (car entry)) agent)
      (funcall (second entry)))))

(defun show-paused-step (workflow step)
  "A step paused (ON-PAUSE). Stopping the task pauses every step under way:
then the task's recovery card stands for them all, so wait to see."
  (later (lambda ()
           (when (and (eq workflow *workflow*) (workflow-live-p workflow)
                      (member step (workflow-paused workflow)))
             (show-paused-step-card workflow step)))))

(defun show-paused-step-card (workflow step)
  "A card in the conversation: the step that paused, why, and what to do
about it. Out of usage, it follows the agent's usage: a reset time learned
later (Codex reads its usage after the turn) brings the Wait button."
  (let* ((agent (step-agent step))
         (other (other-agent workflow agent))
         (reason (step-reason step))
         (usage (eq reason :out-of-usage))
         (role (step-role step))
         (heading (make-instance 'gtk:label :xalign 0.0 :css-classes '("heading")))
         (buttons (make-instance 'gtk:box :spacing 6 :halign :end))
         (status (make-instance 'gtk:label :xalign 0.0 :wrap t :css-classes '("dim-label") :visible nil))
         (wait-button nil)
         (waiting-for nil)                      ; the reset being waited for, once Wait is chosen
         (attempt 0))                           ; which choice a pending wait belongs to
    (attention (format nil "~a: ~a" (agent-name agent) (step-reason-text reason))
               (format nil "~a is paused." (string-capitalize (step-description step) :end 1)))
    (labels ((reset () (and (integerp (agent-out-until agent)) (agent-out-until agent)))
             (settled (text)
               (setf *pause-cards* (remove step *pause-cards* :key #'car))
               (clear-pause-timer step)
               (incf attempt)
               (clear-box buttons)
               (gtk:box-append buttons (make-instance 'gtk:label :label text :css-classes '("dim-label")))
               (gtk:widget-set-visible status nil)
               (update-controls))
             (say (text &optional (classes '("dim-label")))
               (gtk:label-set-text status text)
               (gtk:widget-set-css-classes status classes)
               (gtk:widget-set-visible status t))
             (choose (action done &key acknowledge-changes)
               (let ((mine (incf attempt)))
                 (attempt-recovery workflow action
                                   (lambda (result)
                                     (case result
                                       ((:started :stopped) (settled done))
                                       (t (say (recovery-outcome-text result (if (eq action :hand-off) other agent))
                                               (if (eq result :pending-shutdown) '("dim-label") '("warning")))
                                        (when (eq result :repository-changed) (offer-anyway action done)))))
                                   :step step :acknowledge-changes acknowledge-changes
                                   :while (lambda () (= attempt mine)))))
             (offer-anyway (action done)
               (unless (find "Carry on anyway" (gtk-children buttons) :key #'button-label-or-nil :test #'equal)
                 (choice "Carry on anyway" "Go on from the folder and worktrees as they are now" '()
                         (lambda () (choose action done :acknowledge-changes t)))))
             (choice (label tooltip classes action)
               (let ((b (make-instance 'gtk:button :label label :tooltip-text tooltip :css-classes classes)))
                 (gobject:connect b :clicked (lambda (b) (declare (ignore b)) (funcall action)))
                 (gtk:box-append buttons b)
                 b))
             (wait ()
               (let ((reset (reset)))
                 (when reset
                   (setf waiting-for reset)
                   ;; As choosing Resume would: a folder changed meanwhile asks
                   ;; first (Carry on anyway), and the card says why it didn't.
                   (resume-when-reset step reset (lambda () (choose :resume "Carried on")))
                   (say (format nil "Waiting: ~a will carry on at about ~a. Leave the app open."
                                (agent-name agent) (format-reset-time (+ reset 60)))))))
             (refresh ()
               (let ((reset (and usage (reset))))
                 (gtk:label-set-text heading
                                     (if usage
                                         (format nil "~a is out of usage~@[ until ~a~]" (agent-name agent)
                                                 (and reset (format-reset-time reset)))
                                         (format nil "~a: ~a" (string-capitalize (step-description step) :end 1)
                                                 (if (eq reason :disagreement)
                                                     "your decision is needed"
                                                     (step-reason-text reason)))))
                 (when reset
                   (unless wait-button
                     (setf wait-button (choice "" (format nil "Send it to ~a again once its usage resets" (agent-name agent))
                                               '("suggested-action") #'wait)))
                   (gtk:button-set-label wait-button (format nil "Wait until ~a" (format-reset-time reset)))
                   ;; Already waiting, for a reset that has moved: wait for the new one.
                   (when (and waiting-for (/= waiting-for reset))
                     (wait))))))
      (choice "Stop task" "Stop both agents; the task can be recovered later" '("destructive-action")
              (lambda () (incf attempt) (stop-workflow workflow) (settled "Stopped")))
      ;; The author keeps disputing findings the reviewer keeps open: the user decides.
      (when (eq reason :disagreement)
        (flet ((decide (decision done)
                 (incf attempt)
                 (if (decide-disagreement workflow step decision)
                     (settled done)
                     (say (format nil "~a is still busy; try again in a moment." (agent-name agent)) '("warning")))))
          (choice "Dismiss them" "Dismiss the disputed findings; with no blocker left open, the work is approved" '()
                  (lambda () (decide :dismiss "Dismissed")))
          (choice "They stand" (format nil "~a addresses the disputed findings, told they stand" (agent-name agent))
                  '("suggested-action") (lambda () (decide :keep "Kept")))))
      (unless (eq reason :disagreement)
        (let ((hand-off (choice (case role
                                  (:review "Finish unreviewed")
                                  ((:propose :answer) "Go without it")
                                  (t (format nil "Hand off to ~a" (agent-name other))))
                                (case role
                                  (:review "Finish this part without a review, and say so")
                                  (:propose (format nil "Carry on with ~a's proposal alone" (agent-name other)))
                                  (:answer (format nil "~a's held work goes on without the answers" (agent-name other)))
                                  (t (format nil "~a carries on from what ~a has done so far; its work won't be reviewed"
                                             (agent-name other) (agent-name agent))))
                                '()
                                (lambda () (choose :hand-off (case role
                                                               (:review "Finished unreviewed")
                                                               ((:propose :answer) "Went without it")
                                                               (t (format nil "Handed to ~a" (agent-name other)))))))))
          ;; Only taking work over needs the other agent's usage.
          (when (or (eq role :check)
                    (and (not (member role '(:review :propose :answer))) (agent-out-of-usage-p other)))
            (gtk:widget-set-sensitive hand-off nil)
            (gtk:widget-set-tooltip-text hand-off (if (eq role :check) "A check is run again, not handed off"
                                                      (format nil "~a is out of usage too" (agent-name other))))))
        (choice (if usage "Try again" "Retry step")
                (format nil "Give ~a the step again, as it was first given" (agent-name agent)) '()
                (lambda () (choose :retry "Tried again")))
        (unless usage
          (choice "Resume" (format nil "~a carries on from where it stopped, shown what's already done first"
                                   (agent-name agent))
                  '("suggested-action") (lambda () (choose :resume "Resumed")))))
      (refresh)
      (push (list step #'refresh #'settled) *pause-cards*)
      (pane-append *middle*
                   (gtk:build
                    (gtk:box :orientation :vertical :spacing 6 :css-classes '("duet-approval")
                             heading
                             (gtk:label :label (if (eq reason :disagreement)
                                                   (format nil "~a disputes these findings, and ~a keeps them open:~%~a"
                                                           (agent-name agent) (agent-name other)
                                                           (findings-lines (disputed-findings (step-loop step))))
                                                   (format nil "~a is paused~:[~; while ~a carries on with work that doesn't depend on it~]. ~
Nothing done so far is lost~@[: what it had written is kept~]."
                                                           (string-capitalize (step-description step) :end 1)
                                                           (agent-busy other) (agent-name other)
                                                           (step-progress step)))
                                        :xalign 0.0 :wrap t)
                             status
                             buttons))))
    (update-controls)))

(defun gtk-children (box)
  (loop for child = (gtk:widget-get-first-child box) then (gtk:widget-get-next-sibling child)
        while child collect child))

(defun button-label-or-nil (widget)
  (and (typep widget 'gtk:button) (gtk:button-get-label widget)))

(defun pause-summary (workflow)
  "Who is paused, and why: \"Claude out of usage, ChatGPT paused or stopped by you\"."
  (format nil "~{~a~^, ~}"
          (remove-duplicates (mapcar (lambda (s) (format nil "~a ~a" (agent-name (step-agent s))
                                                         (step-reason-text (step-reason s))))
                                     (workflow-paused workflow))
                             :test #'string=)))

;;; Recovering a task that stopped: reopened after a restart, or stopped by
;;; the user. Its checkpoint is saved with the history whenever it changes;
;;; reopened, it is restored (RESTORE-WORKFLOW) but nothing runs until the
;;; user chooses (docs/recovery-api.md).

(defvar *checkpoint-failed* nil "The workflow whose checkpoint last failed to save, so it's said once.")
(defvar *recovery-card* nil "(workflow . refresh function) for the recovery card still offering choices.")

(defun save-workflow-checkpoint (workflow)
  "WORKFLOW's state changed (ON-CHECKPOINT): keep its checkpoint with the
history, saved at once, so a restart finds where the task had got to."
  (when (and (eq workflow *workflow*) *directory* (not *closing*))
    (unless *history* (setf *history* (make-history *directory*)))
    (handler-case
        (progn (record-history-checkpoint *history* (workflow-checkpoint workflow))
               ;; Dispatch relies on this checkpoint already being durable.
               (save-current-history :synchronous t))
      (error (e)
        (unless (eq *checkpoint-failed* workflow)
          (setf *checkpoint-failed* workflow)
          (pane-note *middle* (format nil "The task's progress couldn't be saved for recovery: ~a" e) "error"))))))

(defun workflow-callbacks ()
  "Keep presentation current after every workflow callback, new or restored."
  (flet ((present-after (callback)
           (lambda (&rest arguments)
             (multiple-value-prog1 (apply callback arguments)
               (refresh-activity)))))
    (list :on-message (present-after #'middle-message)
          :on-pause (present-after #'show-paused-step)
          :on-plan (present-after #'show-plan-card)
          :on-finish (present-after #'workflow-finished)
          :on-checkpoint (present-after #'save-workflow-checkpoint))))

(defun restore-history-workflow (history)
  "HISTORY's task, restored with this app's agents: lead first, as saved.
Nothing runs. Returns nil, and why, if it can't be."
  (let* ((checkpoint (history-checkpoint history))
         (lead-id (jget checkpoint "agents" 0 "id"))
         (lead (if (equal lead-id "codex") *codex* *claude*)))
    (handler-case (values (apply #'restore-workflow checkpoint
                                 :lead lead :partner (if (eq lead *claude*) *codex* *claude*)
                                 (workflow-callbacks))
                          nil)
      (error (e) (values nil (princ-to-string e))))))

(defun truncated (text limit)
  (if (> (length text) limit) (format nil "~a…" (subseq text 0 limit)) text))

(defun recovery-detail-lines (summary inspection workflow)
  "The recovery card's lines: (text . css classes), from the checkpoint's
SUMMARY and, once it's read, INSPECTION's of the repository now."
  (let ((lines '()))
    (flet ((line (classes fmt &rest args) (push (cons (apply #'format nil fmt args) classes) lines)))
      (let ((step (getf summary :step))
            (integration (getf summary :integration)))
        (when (getf summary :request)
          (line '("dim-label") "The request: ~a" (truncated (getf summary :request) 200)))
        (cond (step
               (line '() "Stopped at ~a: ~a." (getf step :description) (format nil "~a~@[ (~a)~]" (step-reason-text (getf step :reason))
                               (or (getf (find (getf step :id) (getf inspection :steps)
                                              :key (lambda (s) (getf s :id)) :test #'equal) :waiting-for)
                                   (getf step :waiting-for))))
               (when (getf step :progress)
                 (line '("dim-label") "What ~a had written by then: ~a" (or (getf step :agent) "it")
                       (truncated (getf step :progress) 300)))
               (let ((others (remove step (getf summary :steps))))
                 (when others
                   (line '() "Also waiting: ~{~a~^; ~}." (mapcar (lambda (s)
                                      (format nil "~a~@[: ~a~]" (getf s :description) (getf s :waiting-for)))
                                    others)))))
              ((getf summary :pending-plan)
               (line '() "Stopped while the plan was waiting for your approval."))
              ((getf integration :state)
               (line '() "Stopped while bringing the parts together (~a)." (getf integration :state)))
              (t (line '() "No step was under way.")))
        (when (getf integration :committed)
          (line '("dim-label") "Committed on their branches: ~{~a~^; ~}." (getf integration :committed)))
        (when (getf integration :merged)
          (line '("dim-label") "Already merged, not to be repeated: ~{~a~^; ~}." (getf integration :merged)))
        (dolist (l (or (getf inspection :loops) (getf summary :loops)))
          (line (if (getf l :unreviewed) '("warning") '("dim-label"))
                "~a: ~a~@[, ~d review~:p done~]~:[~;, not reviewed~]"
                (or (getf l :label) (format nil "~@[~a's ~]work" (getf l :author)))
                (or (and (not (member (getf l :state) '("approved" "unreviewed" "finished" "done") :test #'equal))
                         (getf l :waiting-for))
                    (getf l :state) "unknown") (getf l :round) (getf l :unreviewed)))
        (when (getf summary :unreviewed)
          (line '("warning") "Work no one has reviewed: ~{~a~^, ~}. It stays marked unreviewed."
                (getf summary :unreviewed)))
        ;; The agents: from the inspection once there, else as saved.
        (let ((agents (or (getf inspection :agents) (getf summary :agents))))
          (when agents
            (line '() "~{~a~^ · ~}"
                  (mapcar (lambda (a)
                            (let* ((agent (find (getf a :agent) (list *claude* *codex*) :key #'agent-name :test #'equal))
                                   (until (or (and agent (integerp (agent-out-until agent)) (agent-out-until agent))
                                              (getf a :unavailable-until)))
                                   (out (and until (> until (get-universal-time)))))
                              (format nil "~a: ~:[~:[available~;still working~]~;out of usage until ~a~]"
                                      (getf a :agent) out (if out (format-reset-time until) (getf a :busy)))))
                          agents))))
        (dolist (tree (getf summary :worktrees))
          (let ((now (find (getf tree :path) (getf inspection :repositories) :key (lambda (r) (getf r :path))
                                                                                :test #'equal)))
            (line (if (and now (not (getf now :available))) '("warning") '("dim-label"))
                  "~@[~a's ~]worktree, ~a~@[ (~a)~]~:[~;: missing~]" (getf tree :agent) (getf tree :path)
                  (getf tree :branch) (and now (not (getf now :available))))))
        (cond
          ((null inspection) (line '("dim-label") "Looking at the folder and worktrees as they are now…"))
          (t
           (when (getf inspection :changed)
             (line '("warning") "~:[The folder or a worktree has changed since the task stopped.~;~
The folder or a worktree differs from the last snapshot saved. The app closed before a newer one was ~
taken, so this may include the agents' own last edits.~]"
                   (getf inspection :approximate)))
           (dolist (r (getf inspection :repositories))
             (cond
               ((not (getf r :available))
                (line '("warning") "~a: ~a" (getf r :path) (or (getf r :error) "not there")))
               (t
                (line '() "~a~@[ on ~a~]~@[ at ~a~]: ~:[nothing uncommitted~;~:*~d changed file~:p~@[: ~{~a~^, ~}~]~]"
                      (getf r :path) (getf r :branch) (getf r :head)
                      (let ((files (getf r :files))) (and files (length files)))
                      (let ((files (getf r :files)))
                        (append (mapcar #'cdr (subseq files 0 (min 8 (length files))))
                                (and (> (length files) 8) (list "…")))))
                (when (getf r :new-commits) (line '("warning") "  New commits since it stopped."))
                (when (getf r :branch-was) (line '("warning") "  The branch was ~a then." (getf r :branch-was)))
                (when (getf r :operation)
                  (line '("warning") "  A merge (or rebase, cherry-pick…) is in progress there.")))))
           (when (getf inspection :recovery-refs)
             (line '("dim-label") "Recovery refs kept for: ~{~a~^, ~}." (getf inspection :recovery-refs))))))
      (unless workflow
        (line '("warning") "This task can't be carried on here, only looked at or abandoned.")))
    (nreverse lines)))

(defun step-agent-other (step)
  "The agent that would take over STEP (a CHECKPOINT-SUMMARY step)."
  (let ((name (getf step :agent)))
    (cond ((equal name (agent-name *claude*)) *codex*)
          ((equal name (agent-name *codex*)) *claude*))))

(defun show-recovery-card (workflow history &key restore-error)
  "A card for a task that stopped before it finished: where it stopped, its
progress, the agents, what the folder and worktrees look like now, and the
choices. WORKFLOW is the task (restored, or stopped here), or nil when it
couldn't be restored (RESTORE-ERROR says why): then it can only be looked at
or abandoned. Nothing is done until a choice is made."
  (let* ((card (make-instance 'gtk:box :orientation :vertical :spacing 6 :css-classes '("duet-approval")))
         (heading (make-instance 'gtk:label :xalign 0.0 :css-classes '("heading")))
         (details (make-instance 'gtk:box :orientation :vertical :spacing 2))
         (status (make-instance 'gtk:label :xalign 0.0 :wrap t :visible nil))
         (buttons (make-instance 'gtk:box :spacing 6 :halign :end))
         (inspection nil)
         (inspecting nil)
         (attempt 0)
         (confirm-abandon nil)
         (anyway nil)                   ; the action to offer carrying on with, the folder having changed
         (chosen nil)                   ; the id of the step chosen, when more than one waits
         (steps (make-instance 'gtk:box :spacing 6 :visible nil))
         (done nil))
    (labels ((checkpoint ()
               ;; As saved: strings where the workflow has keywords.
               (or (and workflow (ignore-errors (json-copy (workflow-checkpoint workflow))))
                   (and history (history-checkpoint history))))
             (current-p () (or (null workflow) (eq workflow *workflow*)))
             (say (text &optional (classes '("dim-label")))
               (gtk:label-set-text status (or text ""))
               (gtk:widget-set-css-classes status classes)
               (gtk:widget-set-visible status (and text t)))
             (settled (text)
               (setf done t)
               (incf attempt)
               (say nil)
               (when (eq (car *recovery-card*) workflow) (setf *recovery-card* nil))
               (clear-box buttons)
               (gtk:box-append buttons (make-instance 'gtk:label :label text :css-classes '("dim-label")))
               (update-controls))
             (look ()
               (when (and workflow (not inspecting))
                 (setf inspecting t)
                 (progn
                   (in-background "duet-recovery-inspection"
                                  (lambda () (json-copy (recover-workflow workflow :review)))
                                  (lambda (value why)
                                    (setf inspecting nil)
                                    (cond (why (say (format nil "Couldn't look at the folder: ~a" why) '("warning")))
                                          (t (setf inspection (inspection-summary value (checkpoint)))))
                                    (render))))))
             (choose (action &key acknowledge-changes)
               (let* ((mine (incf attempt))
                      (summary (summary))
                      (step (getf (getf summary :step) :id)))
                 (setf anyway nil)
                 (attempt-recovery
                  workflow action
                  (lambda (result)
                    (case result
                      (:started
                       (let ((what (case action
                                     (:resume "Resumed")
                                     (:retry "Redoing the step")
                                     (:revise-order "Revising the order")
                                     (:run-independently "Running independently")
                                     (t "Handed off"))))
                         ;; Other steps still waiting keep the card, to choose for them too.
                         (if (and workflow (workflow-paused workflow))
                             (progn (setf chosen nil inspection nil)
                                    (say (format nil "~a. Still waiting: ~{~a~^; ~}." what
                                                 (mapcar #'step-description (workflow-paused workflow))))
                                    (render) (look))
                             (settled what))))
                      (:awaiting-plan (settled "The plan is shown again below"))
                      (:restored (setf inspection nil) (say "Restored the missing worktrees from their recovery refs. Uncommitted edits made in them couldn't be recovered.")
                       (render) (look))
                      (:stopped (setf inspection nil) (say "Stopped. The task can still be recovered here, or when its conversation is reopened.")
                       (render) (look))
                      (t (say (recovery-outcome-text
                               result (let ((agent (find (getf (getf summary :step) :agent)
                                                         (list *claude* *codex*) :key #'agent-name :test #'equal)))
                                        (if (and agent (eq action :hand-off)) (other-agent workflow agent) agent)))
                              (if (eq result :pending-shutdown) '("dim-label") '("warning")))
                       (when (eq result :repository-changed)
                         (setf inspection nil
                               anyway action)
                         (render)
                         (look)))))
                  :step step :acknowledge-changes acknowledge-changes
                  :while (lambda () (and (not done) (= attempt mine))))))
             (summary ()
               ;; The steps waiting are those the workflow still has paused (a
               ;; resumed one is running); the chosen one, if still waiting.
               (let ((summary (if workflow
                                  (checkpoint-summary (checkpoint)
                                                      :waiting (mapcar #'step-id (workflow-paused workflow)))
                                  (checkpoint-summary (checkpoint)))))
                 (unless (find chosen (getf summary :steps) :key (lambda (s) (getf s :id)) :test #'equal)
                   (setf chosen nil))
                 (select-summary-step summary chosen)))
             (show-steps (summary)
               ;; More than one step waiting: choose which the buttons are for.
               (let ((waiting (getf summary :steps)))
                 (clear-box steps)
                 (gtk:widget-set-visible steps (and (rest waiting) t))
                 (when (rest waiting)
                   (let ((menu (gtk:drop-down-new-from-strings (mapcar (lambda (s) (getf s :description)) waiting))))
                     (gtk:drop-down-set-selected menu (or (position (getf (getf summary :step) :id) waiting
                                                                    :key (lambda (s) (getf s :id)) :test #'equal)
                                                          0))
                     (gobject:connect menu "notify::selected"
                                      (lambda (m p) (declare (ignore p))
                                        (setf chosen (getf (nth (gtk:drop-down-get-selected m) waiting) :id))
                                        ;; Not while the menu that changed is being replaced.
                                        (later #'render)))
                     (gtk:box-append steps (make-instance 'gtk:label :label "For:" :css-classes '("dim-label")))
                     (gtk:box-append steps menu)))))
             (abandon ()
               (cond
                 ((not confirm-abandon)
                  (setf confirm-abandon t)
                  (say "Abandon this task? It won't be offered for recovery again. Nothing is deleted: the folder, the worktrees and recovery refs stay as they are. Choose Abandon again to confirm."
                       '("warning"))
                  (render))
                 (t
                  (when workflow
                    (incf attempt)
                    (handler-case (recover-workflow workflow :abandon)
                      (error (e) (pane-note *middle* (format nil "Abandoning: ~a" e) "error"))))
                  (when history
                    (abandon-history-task history)
                    (if (eq history *history*) (save-current-history) (save-history history)))
                  (settled "Abandoned. Nothing was deleted."))))
             (button (label tooltip classes action)
               (let ((b (make-instance 'gtk:button :label label :tooltip-text tooltip :css-classes classes)))
                 (gobject:connect b :clicked (lambda (b) (declare (ignore b))
                                               (cond ((not (current-p)) (settled "A newer task has started"))
                                                     ((committing-p *middle*))
                                                     (t (funcall action)))))
                 (gtk:box-append buttons b)
                 b))
             (render ()
               (unless done
                 (let* ((summary (summary))
                        (step (getf summary :step))
                        (state (getf summary :state))
                        (actions (if workflow (recovery-actions summary inspection) '(:abandon))))
                   (gtk:label-set-text heading
                                       (cond ((equal state "stopped") "The task is stopped")
                                             ((member state *interrupted-states* :test #'equal)
                                              "This task was interrupted")
                                             (t "The task stopped before it finished")))
                   (show-steps summary)
                   (clear-box details)
                   (dolist (line (recovery-detail-lines summary inspection workflow))
                     (gtk:box-append details (make-instance 'gtk:label :label (car line) :xalign 0.0 :wrap t
                                                                       :selectable t :css-classes (cdr line))))
                   (clear-box buttons)
                   (loop for (action label tooltip) in (recovery-choices actions :step-role (getf step :role)
                                                                         :plan (getf summary :pending-plan)
                                                                         :integration (getf (getf summary :integration) :state))
                         for b = (button (cond ((and (eq action :abandon) confirm-abandon) "Abandon — confirm")
                                               ((and (eq action :hand-off) (equal label "Hand off") (step-agent-other step))
                                                (format nil "Hand off to ~a" (agent-name (step-agent-other step))))
                                               (t label))
                                         tooltip
                                         (case action
                                           (:abandon '("destructive-action"))
                                           (:resume '("suggested-action"))
                                           (t '()))
                                         (let ((action action))
                                           (case action
                                             (:review (lambda () (setf inspection nil) (render) (look)))
                                             (:abandon #'abandon)
                                             (:stop (lambda () (choose :stop)))
                                             (t (lambda () (choose action))))))
                         ;; Carrying on waits until it's known what the folder looks like now.
                         do (when (and (member action '(:resume :retry :hand-off :restore-worktrees :revise-order :run-independently)) (null inspection))
                              (gtk:widget-set-sensitive b nil)
                              (gtk:widget-set-tooltip-text b "Looking at the folder and worktrees first…")))
                   (when anyway
                     (let ((action anyway))
                       (gtk:widget-set-sensitive
                        (button "Carry on anyway" "Go on from the folder and worktrees as they are now" '()
                                (lambda () (choose action :acknowledge-changes t)))
                        (and inspection t))))))))
      (when restore-error
        (say (format nil "It can't be carried on here: ~a" restore-error) '("warning")))
      (setf *recovery-card* (cons workflow (lambda () (unless done (setf inspection nil) (render) (look)))))
      (render)
      (look)
      (gtk:box-append card heading)
      (gtk:box-append card details)
      (gtk:box-append card steps)
      (gtk:box-append card status)
      (gtk:box-append card buttons)
      (pane-append *middle* card)
      (update-controls))))

(defun offer-recovery (history)
  "HISTORY was reopened with a task that stopped before it finished: restore
it, without running anything, and show its recovery card."
  (multiple-value-bind (workflow why) (restore-history-workflow history)
    (when workflow (setf *workflow* workflow))
    (show-recovery-card workflow history :restore-error why)
    (when (history-interrupted-p history)
      (attention "A task was interrupted" "It's waiting for you to choose how to recover it."))))

;;; A turn gone quiet

(defparameter *stall-grace-seconds* 20
  "How long an interrupted quiet turn has to end before the card says it hasn't.")

(defvar *stall-cards* '()
  "(agent . function) for the stall cards still waiting on a choice; the
function settles the card if its turn has ended or shown signs of life.")

(defun stall-minutes ()
  "Minutes a turn may go without an event before a card asks about it
(the :stall-minutes setting); nil when that's off (0)."
  (let ((minutes (setting :stall-minutes *stall-minutes*)))
    (and (realp minutes) (plusp minutes) minutes)))

(defun check-stalls ()
  (reconcile-stall-cards)
  (let ((minutes (stall-minutes)))
    (when minutes
      (dolist (agent (list *claude* *codex*))
        (when (agent-check-stall agent (round (* 60 minutes)))
          (show-stall-card agent minutes))))))

(defun reconcile-stall-cards (&optional agent)
  "Settle the stall cards (AGENT's, or all) whose turn has ended or come
back to life: anything that counts as activity (AGENT-TOUCH, a subagent's
event or Codex's stderr too) clears the stall, so a card can't outlive it."
  (dolist (entry *stall-cards*)
    (when (or (null agent) (eq (car entry) agent))
      (funcall (cdr entry)))))

(defun show-stall-card (agent minutes)
  "A card for AGENT's turn gone MINUTES without an event: Keep waiting,
Interrupt and retry, or Stop. In the conversation during a task, in the
agent's pane otherwise."
  (let* ((workflow (and (workflow-agent-p *workflow* agent) *workflow*))
         (pane (if (or workflow (eq *view* :conversation)) *middle* (agent-pane-of agent)))
         (token (agent-turn-token agent))
         (name (agent-name agent))
         (buttons (make-instance 'gtk:box :spacing 6 :halign :end))
         (status (make-instance 'gtk:label :xalign 0.0 :wrap t :css-classes '("dim-label") :visible nil))
         (retrying nil)
         (entry nil))
    (labels ((current-p ()
               ;; Still the turn the card is about (being interrupted, if retrying).
               (and (agent-busy agent) (eq token (agent-turn-token agent))))
             (say (text)
               (gtk:label-set-text status text)
               (gtk:widget-set-visible status t))
             (settled (text)
               (setf *stall-cards* (remove entry *stall-cards*))
               (clear-box buttons)
               (gtk:box-append buttons (make-instance 'gtk:label :label text :css-classes '("dim-label")))
               (update-controls))
             (choice (label tooltip classes action)
               (let ((b (make-instance 'gtk:button :label label :tooltip-text tooltip :css-classes classes)))
                 (gobject:connect b :clicked (lambda (b) (declare (ignore b)) (funcall action)))
                 (gtk:box-append buttons b)))
             (stop ()
               (if (current-p)
                   (progn (stop-stalled-turn workflow agent) (settled "Stopped"))
                   (settled "The turn had already ended")))
             (stop-button ()
               (choice "Stop" (format nil "End ~a's turn and its process~:[~;, and stop the task~]" name workflow)
                       '("destructive-action") #'stop))
             (keep-waiting ()
               (if (current-p)
                   (progn (agent-keep-waiting agent) (settled "Kept waiting"))
                   (settled "The turn had already ended")))
             (watch-retry ()
               (let ((started (clock-seconds)) (warned nil))
                 (glib:timeout-add-seconds
                  glib:+priority-default+ 1
                  (lambda ()
                    (cond ((not (member entry *stall-cards*)) nil)
                          ((eq (agent-retry agent) :sent) (settled "Interrupted and sent again") nil)
                          ((not (agent-retry-pending-p agent)) (settled "Stopped") nil)
                          (t (when (and (not warned) (>= (- (clock-seconds) started) *stall-grace-seconds*))
                               (setf warned t)
                               (say (format nil "~a's turn still hasn't ended, ~d seconds after the interrupt, so ~
the prompt hasn't been sent again. Stop ends the turn and ~a's process~:[~;, and the task~]."
                                            name *stall-grace-seconds* name workflow))
                               (stop-button))
                             t))))))
             (retry ()
               (cond ((not (current-p)) (settled "The turn had already ended"))
                     ((retry-stalled-turn workflow agent)
                      (setf retrying t)
                      (clear-box buttons)
                      (say (format nil "Interrupting ~a. The same prompt is sent again once its turn has ended." name))
                      (watch-retry))
                     (t (settled "The turn is already stopping")))))
      (setf entry (cons agent (lambda (&optional superseded)
                                (unless retrying
                                  (cond ((not (current-p)) (settled "The turn ended"))
                                        ((not (agent-stall-noticed agent)) (settled "It's working again"))
                                        (superseded (settled "Still quiet: see the newer card")))))))
      ;; One open card for an agent: an older one is settled first.
      (dolist (old (remove agent *stall-cards* :key #'car :test-not #'eq))
        (funcall (cdr old) t))
      (push entry *stall-cards*)
      (choice "Stop" (format nil "End ~a's turn~:[~;, and stop the task~]" name workflow)
              '("destructive-action") #'stop)
      (choice "Interrupt and retry"
              (format nil "Interrupt ~a's turn, and send it the same prompt again once that turn has ended" name)
              '() #'retry)
      (choice "Keep waiting" (format nil "Give ~a another ~d minute~:p" name minutes) '("suggested-action")
              #'keep-waiting)
      (pane-append pane
                   (gtk:build
                    (gtk:box :orientation :vertical :spacing 6 :css-classes '("duet-approval")
                             (gtk:label :label (format nil "~a has been quiet for ~d minute~:p" name minutes)
                                        :xalign 0.0 :css-classes '("heading"))
                             (gtk:label :label (format nil "Nothing has come from ~a's turn~:[~; (~a pane)~] in that time. ~
It may be stuck, or busy with something long, such as a command that prints nothing until it ends."
                                                       name workflow
                                                       (if (eq (agent-id agent) :claude) "left" "right"))
                                        :xalign 0.0 :wrap t)
                             status
                             buttons)))
      (attention (format nil "~a has gone quiet" name)
                 (format nil "No events for ~d minute~:p." minutes)))))

;;; What the latest task changed (TASK-CHANGES; changes.lisp)

(defvar *task-changes* nil "The latest task's TASK-CHANGES, or nil.")

(defun snapshot-failed (why)
  (pane-note *middle* (format nil "Couldn't record the folder's state: ~a" why) "error"))

(defun snapshot-task-start (workflow ready)
  "Capture the starting state off the UI thread, then call READY on it."
  (let ((directory *directory*) (previous *task-changes*))
    (setf *request-preflight* t)
    (in-background "capture task starting snapshot"
      (lambda ()
        (let ((previous-error nil))
          (when previous
            (multiple-value-bind (result why) (finish-task-changes previous)
              (when (eq result :failed) (setf previous-error why))))
          (multiple-value-bind (task why) (begin-task-changes workflow directory)
            (list task why previous-error
                  (ignore-errors
                    (repository-start-state directory (task-changes-start task)))))))
      (lambda (result why)
        (setf *request-preflight* nil)
        (when (and (eq workflow *workflow*) (equal directory *directory*))
          (if why
              (snapshot-failed why)
              (destructuring-bind (task problem previous-error repository-start) result
                (setf *task-changes* task)
                (refresh-workspace-sidebar)
                (when previous-error (snapshot-failed previous-error))
                (when problem (snapshot-failed problem))
                (funcall ready repository-start))))))))

(defun finish-task-end (task)
  "Record TASK's end if it's time; true while it's still waiting."
  (multiple-value-bind (result why) (finish-task-changes task)
    (when (and result (eq task *task-changes*))
      (refresh-workspace-sidebar))
    (when (eq result :failed)
      (snapshot-failed (format nil "~a. The task's changes can't be shown." why))))
  (eq (task-changes-final task) :pending))

(defun snapshot-task-end (workflow)
  "WORKFLOW has ended: record the folder once both agents have stopped
(an agent stopped mid-turn may still be writing)."
  (let ((task *task-changes*))
    (when (and task (eq workflow (task-changes-workflow task)))
      (end-task-changes task)
      (when (finish-task-end task)
        (glib:timeout-add-seconds glib:+priority-default+ 1 (lambda () (finish-task-end task)))))))

(defun show-task-changes (&optional (task *task-changes*))
  "The Changes window for TASK (TASK-CHANGES), by default the latest: from
its start to its end, or to now while it runs (or is still stopping)."
  (if (null task)
      (pane-note *middle* "No task has run yet. Changes shows what the latest task changed in the folder."
                 "dim-label" "duet-note")
      (let ((directory (task-changes-directory task)))
        (multiple-value-bind (kind start end) (task-changes-range task)
          (ecase kind
            (:unrecorded
             (pane-note *middle* "The latest task's changes weren't recorded: its folder isn't a git repository with a commit, or recording failed."
                        "dim-label" "duet-note"))
            (:failed
             (pane-note *middle* "The latest task's final state couldn't be recorded, so its changes can't be shown."
                        "dim-label" "duet-note"))
            (:final
             (show-changes-window directory start end :title (format nil "Changes · ~a" (folder-label directory))))
            (:live
             ;; Recorded off the GTK thread; the window has read what it
             ;; shows once it's closed.
             (show-changes-window directory start
                                  (lambda () (task-snapshot task (format nil "now-~a" (subseq (make-uuid) 0 8))))
                                  :title (format nil "Changes so far · ~a" (folder-label directory))
                                  :on-close (lambda (now)
                                              (when now
                                                (ignore-errors (funcall *task-snapshot-delete* directory now)))))))))))

;;; The summary card: how a task ended, and Commit (workflow.lisp's
;;; WORKFLOW-COMPLETION and COMMIT-WORKFLOW-CHANGES)

(defparameter *summary-expanded-length* 1500
  "A summary longer than this many characters starts folded.")

(defun outcome-text (outcome)
  (case outcome
    (:approved "Approved")
    (:unresolved "Not approved")
    (:unreviewed "Done without review")
    (:stopped "Stopped")
    (t "Failed")))

(defun check-status-text (status)
  (case status
    (:passed "Check passed")
    (:failed "Check failed")
    (t nil)))

(defvar *committing* nil
  "True while a summary card's commit runs: no agent work starts meanwhile,
so none of it can get into the commit.")

(defun committing-p (pane)
  "True, saying so in PANE, while a commit runs."
  (when *committing*
    (pane-note pane "Wait for the commit to finish first." "error")
    t))

(defun in-background (name work done)
  "Call WORK off the GTK thread (it runs git), then DONE with its value, or
nil and the error's message, back on it."
  (sb-thread:make-thread
   (lambda ()
     (multiple-value-bind (value why)
         (handler-case (values (funcall work) nil)
           (error (e) (values nil (princ-to-string e))))
       (deliver (lambda () (funcall done value why)))))
   :name name))

(defun show-summary-card (workflow)
  "A card in the conversation saying how WORKFLOW ended: its outcome, check
and summary, with Changes and, when the work can be committed, Commit changes."
  (let* ((heading (make-instance 'gtk:label :label "Work finished" :xalign 0.0 :css-classes '("heading")))
         (facts (make-instance 'gtk:label :xalign 0.0 :wrap t :css-classes '("dim-label") :label "Checking the repository…"))
         (body (make-instance 'gtk:box :orientation :vertical :spacing 6))
         (files-label (make-instance 'gtk:label :xalign 0.0 :wrap t))
         (status (make-instance 'gtk:label :xalign 0.0 :wrap t :wrap-mode :word-char :selectable t :visible nil))
         (commit (make-instance 'gtk:button :label "Commit changes" :sensitive nil :css-classes '("suggested-action")))
         (changes (make-instance 'gtk:button :label "Changes" :tooltip-text "What this task changed, file by file"))
         ;; This task's, not whichever is latest when the button is clicked.
         (task (and *task-changes* (eq (task-changes-workflow *task-changes*) workflow) *task-changes*))
         (history *history*)
         (buttons (gtk:build (gtk:box :spacing 6 :halign :end changes commit))))
    (labels ((say (text &optional (class "dim-label"))
               (gtk:label-set-text status text)
               (gtk:widget-set-css-classes status (list class))
               (gtk:widget-set-visible status (plusp (length text))))
             (show (completion keep-status)
               ;; The card as COMPLETION has it; nil once the task has carried on.
               ;; KEEP-STATUS: leave what the card says (a commit's error) as it is.
               (if (null completion)
                   (progn (gtk:widget-set-sensitive commit nil)
                          (say "This task has carried on since; see the newer summary."))
                   (destructuring-bind (&key outcome summary check-status repository can-commit commit-reason
                                          commit-hash &allow-other-keys)
                       completion
                     (gtk:label-set-text heading (if (eq outcome :approved)
                                                     "Work complete"
                                                     (format nil "Work finished: ~(~a~)" (outcome-text outcome))))
                     (gtk:label-set-text facts (format nil "~a~@[  ·  ~a~]  ·  ~a"
                                                       (outcome-text outcome) (check-status-text check-status)
                                                       (if repository (folder-label repository) "Not a git repository")))
                     (clear-box body)
                     (dolist (line (task-summary-lines completion))
                       (gtk:box-append body (make-instance 'gtk:label :label line :xalign 0.0 :wrap t
                                                                 :selectable t :css-classes '("dim-label"))))
                     (update-files (getf completion :files) (getf completion :files-known))
                     (gtk:box-append body files-label)
                     (let ((summary (string-trim '(#\Space #\Newline) (or summary ""))))
                       (when (plusp (length summary))
                         (gtk:box-append body (make-instance 'gtk:expander :label "Summary" :child (markdown-box summary)
                                                                           :expanded (<= (length summary) *summary-expanded-length*)))))
                     (gtk:widget-set-sensitive commit (and can-commit t))
                     (gtk:widget-set-tooltip-text commit (if can-commit
                                                             (format nil "Select files and preview a commit in ~a"
                                                                     (folder-label repository))
                                                             commit-reason))
                     (cond (commit-hash
                            (gtk:widget-set-visible commit nil)
                              (say (format nil "Committed as ~a." (subseq commit-hash 0 (min 12 (length commit-hash))))))
                           (keep-status)
                           ((and (not can-commit) commit-reason (eq outcome :approved))
                            (say commit-reason))
                           (t (say ""))))))
             (update-files (files known)
               (gtk:button-set-label changes (if known (format nil "Files changed (~d)" (length files)) "Changes"))
               (gtk:label-set-text files-label (summary-files-text files known)))
             (poll-files ()
               ;; Waiting for writers needs no Git call and must not rebuild
               ;; the summary, expander, or selectable metric labels.
               (when (and task (workflow-completion-data workflow))
                 (if (eq :pending (task-changes-final task))
                     (glib:timeout-add-seconds glib:+priority-default+ 1
                                              (lambda () (poll-files) nil))
                     (in-background "task completion files"
                                    (lambda ()
                                      (when (and (eq :taken (task-changes-final task))
                                                 (task-changes-start task))
                                        (list :known t
                                              :files (mapcar #'snapshot-change-path
                                                             (snapshot-changes (task-changes-directory task)
                                                                               (task-changes-start task)
                                                                               (task-changes-end task))))))
                                    (lambda (result why)
                                      (when (workflow-completion-data workflow)
                                        (update-files (getf result :files) (and (not why) (getf result :known)))))))))
             (refresh (&optional keep-status)
               (in-background "task completion" (lambda () (workflow-completion workflow))
                              (lambda (completion why)
                                (if why
                                    (progn (gtk:label-set-text facts "Couldn't read the task's outcome.")
                                           (say why "error"))
                                    (progn (show completion keep-status)
                                           (when completion (poll-files)))))))
             (commit-it ()
               (cond (*committing* (say "Another commit is running." "error"))
                     ((workflow-running-p) (say "Wait for the running task to end." "error"))
                     (t (show-commit-preview
                         (getf (workflow-completion-state workflow) :root)
                         :start (workflow-repository-start workflow) :workflow workflow
                         :message (default-commit-message workflow)
                         :on-committed (lambda (result)
                                         (when history
                                           (history-add-entry history :middle :note (getf result :message))
                                           (save-history history))
                                         (refresh)))))))
      (gobject:connect commit :clicked (lambda (b) (declare (ignore b)) (commit-it)))
      (gobject:connect changes :clicked
                       (lambda (b) (declare (ignore b))
                         (if task
                             (show-task-changes task)
                             (say "This task's changes weren't recorded."))))
      (pane-append *middle* (gtk:build
                             (gtk:box :orientation :vertical :spacing 6 :css-classes '("duet-summary")
                                      heading facts body status buttons)))
      (refresh))))

;;; A workflow stopped at its last round, unapproved

(defun workflow-finished (workflow)
  (clear-pause-timers (if (eq (workflow-state workflow) :finished) "Finished" "Stopped"))
  (snapshot-task-end workflow)
  ;; Stopped, it can be carried on: the recovery card says how. Abandoning
  ;; stops it first, so wait to see whether it's still only stopped.
  (when (eq (workflow-state workflow) :stopped)
    (later (lambda ()
             (when (and (eq workflow *workflow*) (eq (workflow-state workflow) :stopped))
               (if (eq (car *recovery-card*) workflow)
                   (funcall (cdr *recovery-card*))
                   (show-recovery-card workflow *history*))))))
  (when (eq (workflow-state workflow) :finished)
    (attention "Both agents have finished"
               (format nil "~{~a~^; ~}"
                       (or (loop for l in (workflow-loops workflow)
                                 collect (format nil "~@[~a: ~]~a" (loop-label l) (loop-outcome l)))
                           '("It stopped before any work was done")))))
  (when (eq (workflow-state workflow) :finished)
    (show-summary-card workflow))
  (when (and (eq (workflow-state workflow) :finished)
             (eq (workflow-mode workflow) :review-only)
             (some (lambda (l) (plusp (count-findings (loop-findings l) :open))) (workflow-loops workflow)))
    (let ((button (make-instance 'gtk:button :label "Fix these" :halign :start :css-classes '("suggested-action"))))
      (gobject:connect button :clicked
                       (lambda (b)
                         (when (and (eq workflow *workflow*) (not (workflow-running-p))
                                    (not (agent-busy *claude*)) (not (agent-busy *codex*)))
                           (handler-case
                               (let ((request (review-fix-request workflow)))
                                 (when request
                                   (gtk:drop-down-set-selected (control :mode) 0)
                                   (gtk:text-buffer-set-text (gtk:text-view-get-buffer (control :input)) request -1)
                                   (gtk:widget-set-sensitive b nil)
                                   (start-request)))
                             (error (e) (pane-note *middle* (princ-to-string e) "error"))))))
      (pane-append *middle* button)))
  (when (workflow-continuable-p workflow)
    (show-continue-card workflow))
  (update-controls))

;;; A plan waiting for the user's approval

(defun show-plan-card (workflow lead-task partner-task)
  "A card with the plan's two parts, to edit, and Start or Stop."
  (let ((buttons (make-instance 'gtk:box :spacing 6 :halign :end))
        (lead (workflow-lead workflow))
        (partner (workflow-partner workflow)))
    (flet ((part (text)
             (let ((view (make-input)))
               (gtk:text-buffer-set-text (gtk:text-view-get-buffer view) (or text "") -1)
               view))
           (heading (text)
             (make-instance 'gtk:label :label text :xalign 0.0 :css-classes '("caption-heading"))))
      (let ((lead-view (part lead-task))
            (partner-view (part partner-task))
            (order (gtk:drop-down-new-from-strings
                    (list "Independently" (format nil "~a's part first" (agent-name lead))
                          (format nil "~a's part first" (agent-name partner)))))
            (plan-error (and (fboundp 'pending-plan-error) (ignore-errors (pending-plan-error workflow))))
            (error-label (make-instance 'gtk:label :xalign 0.0 :wrap t :css-classes '("error")))
            (start-button nil))
        (gtk:drop-down-set-selected order
                                    (or (position (and (fboundp 'pending-plan-order)
                                                       (ignore-errors (pending-plan-order workflow)))
                                                  '(:parallel :lead-first :partner-first)) 0))
        (gtk:label-set-text error-label (or plan-error ""))
        (gtk:widget-set-visible error-label (not (null plan-error)))
        (labels ((edited ()
                   (setf plan-error nil)
                   (gtk:widget-set-visible error-label nil)
                   (when start-button (gtk:widget-set-sensitive start-button t)))
                 (settled (text)
                   (clear-box buttons)
                   (gtk:box-append buttons (make-instance 'gtk:label :label text :css-classes '("dim-label")))
                   (gtk:widget-set-sensitive lead-view nil)
                   (gtk:widget-set-sensitive partner-view nil)
                   (gtk:widget-set-sensitive order nil)
                   (update-controls))
                 (start ()
                   (let ((previous-error (and (fboundp 'pending-plan-error)
                                              (ignore-errors (pending-plan-error workflow)))))
                     (cond ((not (workflow-live-p workflow)) (settled "Stopped"))
                         ((and (every (lambda (view)
                                          (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                                      (text-view-text view)))))
                                      (list lead-view partner-view)))
                          (setf plan-error "Give at least one of them a part.")
                          (gtk:label-set-text error-label plan-error)
                          (gtk:widget-set-visible error-label t)
                          (gtk:widget-set-sensitive start-button nil))
                         ((approve-plan workflow (text-view-text lead-view) (text-view-text partner-view)
                                        :order (nth (gtk:drop-down-get-selected order)
                                                    '(:parallel :lead-first :partner-first)))
                          (settled "Started"))
                         (t (setf plan-error
                                  (let ((current-error (and (fboundp 'pending-plan-error)
                                                            (ignore-errors (pending-plan-error workflow)))))
                                    (if (and current-error (not (equal current-error previous-error)))
                                        current-error
                                        "The plan could not be started. Edit a part or choose another order.")))
                            (gtk:label-set-text error-label plan-error)
                            (gtk:widget-set-visible error-label t)
                            (gtk:widget-set-sensitive start-button nil)))))
                 (button (label classes tooltip action)
                   (let ((b (make-instance 'gtk:button :label label :css-classes classes :tooltip-text tooltip)))
                     (gobject:connect b :clicked (lambda (b) (declare (ignore b)) (funcall action)))
                     (gtk:box-append buttons b)
                     b)))
          (gobject:connect order "notify::selected"
                           (lambda (d pspec) (declare (ignore d pspec)) (edited)))
          (dolist (view (list lead-view partner-view))
            (gobject:connect (gtk:text-view-get-buffer view) :changed
                             (lambda (buffer) (declare (ignore buffer)) (edited))))
          (button "Stop" '("destructive-action") "Stop the whole task here"
                  (lambda () (stop-workflow workflow) (settled "Stopped")))
          (setf start-button
                (button "Start" '("suggested-action") "Start the work on the plan, as it is here" #'start))
          ;; Every choice in this menu is acyclic, including the preset
          ;; Independently choice used for a rejected circular plan.
          (gtk:widget-set-sensitive start-button t)
          (pane-append *middle*
                       (gtk:build
                        (gtk:box :orientation :vertical :spacing 6 :css-classes '("duet-approval")
                                 (gtk:label :label "Approve the plan" :xalign 0.0 :css-classes '("heading"))
                                 (gtk:label :label "Edit either part if you like, then start. Leave a part empty to have the other agent do its part alone."
                                            :xalign 0.0 :wrap t)
                                 (heading (format nil "~a's part" (agent-name lead)))
                                 (gtk:frame lead-view)
                                 (heading (format nil "~a's part" (agent-name partner)))
                                 (gtk:frame partner-view)
                                 order
                                 error-label
                                 buttons)))
          (attention "The plan is ready" "It's waiting for your approval before the work starts.")
          (update-controls))))))

(defun show-continue-card (workflow)
  "A card offering to carry WORKFLOW on for more rounds, where it stopped."
  (let ((buttons (make-instance 'gtk:box :spacing 6 :halign :end))
        (rounds (gtk:spin-button-new-with-range 1d0 10d0 1d0)))
    (gtk:spin-button-set-value rounds (coerce (gtk:spin-button-get-value-as-int (control :rounds)) 'double-float))
    (gtk:widget-set-tooltip-text rounds "Most further review rounds")
    (labels ((settled (text)
               (clear-box buttons)
               (gtk:box-append buttons (make-instance 'gtk:label :label text :css-classes '("dim-label"))))
             (carry-on ()
               (cond ((not (eq workflow *workflow*))
                      (settled "A newer task has started"))
                     ((committing-p *middle*))
                     ((or (agent-busy *claude*) (agent-busy *codex*))
                      (pane-note *middle* "Wait for both agents to finish, or stop them, before continuing." "error"))
                     ((continue-workflow workflow (gtk:spin-button-get-value-as-int rounds))
                      (settled "Continued")
                      (update-controls))
                     (t (settled "Can't continue: the folder or a conversation has changed since")))))
      (gtk:box-append buttons (make-instance 'gtk:label :label "More rounds" :css-classes '("dim-label")))
      (gtk:box-append buttons rounds)
      (let ((b (make-instance 'gtk:button :label "Continue" :css-classes '("suggested-action")
                                          :tooltip-text "The author addresses the last review, and the reviewing goes on")))
        (gobject:connect b :clicked (lambda (b) (declare (ignore b)) (carry-on)))
        (gtk:box-append buttons b))
      (pane-append *middle*
                   (gtk:build
                    (gtk:box :orientation :vertical :spacing 6 :css-classes '("duet-approval")
                             (gtk:label :label "Not approved yet" :xalign 0.0 :css-classes '("heading"))
                             (gtk:label :label "The review wasn't approved: the rounds ran out, or findings were left open. Continue to carry on from the last review."
                                        :xalign 0.0 :wrap t)
                             buttons))))))

(defun composer-recipient ()
  (case (if (control :recipient) (gtk:drop-down-get-selected (control :recipient)) 0)
    (1 :claude) (2 :codex) (t :both)))

(defun send-composer-message ()
  "Start a task, or deliver direction, remembering partial delivery for retries."
  (when *request-preflight* (return-from send-composer-message))
  (let* ((recipient (composer-recipient))
         (holder (control :composer-attachments))
         (attachments (copy-list (and holder (agent-pane-attachments holder))))
         (text (string-trim '(#\Space #\Newline #\Tab) (text-view-text (control :input))))
         (key (list recipient text attachments *workflow* *directory*))
         (pending (control :composer-delivery)))
    (when (and (string= text "/clear") (not (committing-p *middle*)))
      (handler-case
          (when (handle-clear-context-command
                 text attachments (case recipient
                                    (:claude (list *claude*)) (:codex (list *codex*))
                                    (t (list *claude* *codex*))))
            (clear-text-view (control :input))
            (setf (getf *controls* :composer-delivery) nil))
        (error (e) (pane-note *middle* (princ-to-string e) "error")))
      (return-from send-composer-message))
    (unless (equal key (getf pending :key))
      (setf pending nil (getf *controls* :composer-delivery) nil))
    (unless (committing-p *middle*)
      (if (and (null pending) (eq recipient :both) (not (workflow-running-p))
               (not (agent-busy *claude*)) (not (agent-busy *codex*)))
          (progn
            (start-request attachments)
            (when (and holder (workflow-running-p)) (set-attachments holder nil)))
          (when (or (plusp (length text)) attachments)
            (unless pending
              (let ((targets (case recipient
                               (:claude (list *claude*)) (:codex (list *codex*))
                               (t (if (workflow-running-p)
                                      (remove-if-not
                                       (lambda (agent) (and (agent-busy agent)
                                                            (workflow-agent-member-p *workflow* agent)))
                                       (list *claude* *codex*))
                                      (list *claude* *codex*))))))
                (unless targets
                  (pane-note *middle* "No task agents have a live turn. Your message is kept; send when an agent is working." "dim-label")
                  (return-from send-composer-message))
                (setf pending (list :key key :remaining targets :contract-recorded nil)
                      (getf *controls* :composer-delivery) pending)))
            (handler-case
                (progn
                  (loop while (getf pending :remaining)
                        for agent = (first (getf pending :remaining)) do
                    ;; A task agent may have finished since a partial delivery.
                    ;; A retry must never start an unrelated idle turn.
                    (when (and (eq recipient :both) (workflow-running-p) (not (agent-busy agent)))
                      (error "~a has no live turn. The remaining message is kept for retry." (agent-name agent)))
                    (agent-send-now agent (if (plusp (length text)) text "Take a look at these.") attachments)
                    ;; Mark delivery first: checkpoint/history errors must not
                    ;; resend direction to an agent that already received it.
                    (pop (getf pending :remaining))
                    ;; Keep shared direction once on partial delivery/retry.
                    ;; Messages addressed to one agent stay private.
                    (when (and (eq recipient :both) (workflow-running-p)
                               (not (getf pending :contract-recorded)))
                      (setf (getf pending :contract-recorded) t)
                      (record-task-direction *workflow* (prompt-with-attachments text attachments)))
                    (let ((header (format nil "You → ~a" (agent-name agent)))
                          (body (prompt-with-attachments text attachments)))
                      (middle-bubble :user header body)
                      (record :middle :text (format nil "~a~%~%~a" header body))
                      (save-current-history)))
                  (setf (getf *controls* :composer-delivery) nil)
                  (clear-text-view (control :input))
                  (when holder (set-attachments holder nil)))
              (error (e) (pane-note *middle* (format nil "~a~%Retry sends only to recipients who have not received this message."
                                                   e) "error"))))))))

(defun update-workflow-summary ()
  (when (control :workflow-summary)
    (gtk:label-set-text
     (control :workflow-summary)
     (format nil "~a · ~a leads~a"
             (nth (gtk:drop-down-get-selected (control :mode))
                  '("Write & review" "Split the work" "Compare proposals" "Review only"))
             (agent-name (selected-lead))
             (if (eq (selected-mode) :review-only) ""
                 (format nil " · ~d rounds" (gtk:spin-button-get-value-as-int (control :rounds))))))))


(defun start-request (&optional attachments)
  (cond ((workflow-running-p) (stop-workflow *workflow*))
        (*request-preflight* nil)
        ((committing-p *middle*) nil)
        ((and (not attachments) (not (eq (selected-mode) :review-only))
              (null (blank-to-nil (text-view-text (control :input))))) nil)
        ((or (eq (selected-mode) :review-only)
             (not (git-repository-p *directory*)))
         (start-request-ready attachments))
        (t
         (let ((directory *directory*)
               (text (text-view-text (control :input)))
               (mode (selected-mode)))
           (setf *request-preflight* t)
           (labels ((still-current-p ()
                      (and (equal directory *directory*) (eq mode (selected-mode))
                           (equal text (text-view-text (control :input)))
                           (not (workflow-running-p))))
                    (proceed (state)
                      (in-background "verify task starting state"
                        (lambda () (equal state (repository-state directory)))
                        (lambda (current why)
                          (if (and current (not why) (still-current-p))
                              (start-request-ready attachments)
                              (pane-note *middle* "The folder or request changed. Start again to review its current state." "error"))))))
             (in-background "inspect task starting state"
               (lambda () (repository-state directory))
               (lambda (state why)
                 (setf *request-preflight* nil)
                 (cond (why (pane-note *middle* (format nil "Cannot inspect the repository: ~a" why) "error"))
                       ((not (still-current-p)) nil)
                       ((zerop (length (getf state :status))) (start-request-ready attachments))
                       (t (show-commit-preview directory
                            :opening-state state
                            :on-continue #'proceed
                            :on-committed (lambda (result) (declare (ignore result))
                                            ;; Recheck cleanliness and offer the prompt again if needed.
                                            (when (still-current-p) (start-request attachments)))))))))))))

(defun start-request-ready (&optional attachments)

  (if (workflow-running-p)
      (stop-workflow *workflow*)
      (let ((text (prompt-with-attachments
                   (string-trim '(#\Space #\Newline #\Tab) (text-view-text (control :input))) attachments))
            (problem nil))
        (when (or (plusp (length text)) (eq (selected-mode) :review-only))
          (cond
            ((committing-p *middle*))
            ((or (agent-busy *claude*) (agent-busy *codex*))
             (pane-note *middle* "Wait for both agents to finish, or stop them, before starting." "error"))
            ((and (worktrees-wanted-p) (setf problem (worktrees-problem *directory*)))
             (pane-note *middle* (format nil "Can't give each part its own worktree: ~a." problem) "error"))
            ((heavy-context-agents)
             (show-fresh-context-card (heavy-context-agents) attachments))
            (t
             (setf *context-card* nil)
             ;; A task that can still be recovered keeps its conversation, and
             ;; checkpoint: this one starts another.
             (when (and *history* (history-recoverable-p *history*))
               (finish-history)
               (setf *recovery-card* nil)
               (pane-note *middle* "The earlier task can still be recovered from its conversation in History; this one is kept separately."
                          "dim-label" "duet-note"))
             (let* ((lead (selected-lead))
                    (partner (if (eq lead *claude*) *codex* *claude*)))
               (clear-text-view (control :input))
               (setf *workflow* (apply #'make-instance 'workflow
                                               :mode (selected-mode) :lead lead :partner partner
                                               :request (if (plusp (length text)) text "Review for bugs, regressions, and missed requirements.")
                                               :review-target (nth (gtk:drop-down-get-selected (control :review-target))
                                                                   '(:uncommitted :branch :branch-tips :commits))
                                               :review-base (gtk:editable-get-text (control :review-base))
                                               :review-branch (gtk:editable-get-text (control :review-branch))
                                               :review-count (gtk:spin-button-get-value-as-int (control :review-count))
                                               :review-agents (nth (gtk:drop-down-get-selected (control :review-agents)) '(:both :lead :partner))
                                               :max-rounds (gtk:spin-button-get-value-as-int (control :rounds))
                                               :approve-plan (and (member (selected-mode) '(:divide :compare))
                                                                  (gtk:check-button-get-active (control :approve-plan)))
                                               :worktrees (worktrees-wanted-p)
                                               :check-command (folder-check-command *directory*)
                                               (workflow-callbacks)))
               (let ((workflow *workflow*))
                 (snapshot-task-start workflow
                   (lambda (repository-start)
                     (start-workflow workflow :repository-start repository-start)
                     (let ((holder (control :composer-attachments)))
                       (when (and holder
                                  (equal attachments (agent-pane-attachments holder)))
                         (set-attachments holder nil)))
                     (update-controls))))
               (update-controls))))))))

;;; A long conversation is reread on every turn: before a task, one whose
;;; context is well filled is offered a new start, once per conversation.

(defvar *context-offered* '() "(agent conversation)s already offered a new conversation.")

(defvar *context-card* nil "The latest offer's token; older cards, and any once a task starts, are inert.")

(defun heavy-context-agents ()
  "The agents with a heavily filled context not yet offered a new conversation."
  (remove-if-not (lambda (agent)
                   (and (agent-context-heavy-p agent)
                        (not (member (list agent (agent-conversation-handle agent)) *context-offered*
                                     :test #'equal))))
                 (list *claude* *codex*)))

(defun show-fresh-context-card (agents attachments)
  "Offer AGENTS new conversations before the task starts; either choice starts
it, while the request, folder, task and conversations are those it was offered for."
  (dolist (agent agents)
    (push (list agent (agent-conversation-handle agent)) *context-offered*))
  (let* ((buttons (make-instance 'gtk:box :spacing 6 :halign :end))
         (token (list :context-card))
         (state (lambda ()
                  (let ((holder (control :composer-attachments)))
                    (list *directory* *workflow* (selected-mode) (text-view-text (control :input))
                          (copy-list (and holder (agent-pane-attachments holder)))
                          (mapcar #'agent-conversation-handle (list *claude* *codex*))))))
         (offered (funcall state)))
    (setf *context-card* token)
    (labels ((settled (text)
               (clear-box buttons)
               (gtk:box-append buttons (make-instance 'gtk:label :label text :css-classes '("dim-label"))))
             (go-on (fresh)
               (cond ((or (not (eq token *context-card*)) (workflow-running-p)
                          (not (equal offered (funcall state))))
                      (when (eq token *context-card*) (setf *context-card* nil))
                      (settled "Out of date: press Start again"))
                     ((some #'agent-busy (list *claude* *codex*))
                      (pane-note *middle* "Wait for both agents to finish, or stop them, before starting." "error"))
                     (t
                      (setf *context-card* nil)
                      (when fresh (dolist (agent agents) (new-conversation agent)))
                      (settled (if fresh "Started in new conversations" "Kept the conversations"))
                      (start-request attachments)
                      (let ((holder (control :composer-attachments)))
                        (when (and holder (workflow-running-p)) (set-attachments holder nil))))))
             (button (label fresh &rest options)
               (let ((b (apply #'make-instance 'gtk:button :label label options)))
                 (gobject:connect b :clicked (lambda (b) (declare (ignore b)) (go-on fresh)))
                 (gtk:box-append buttons b))))
      (button "Keep and start" nil)
      (button "New conversation and start" t :css-classes '("suggested-action")
              :tooltip-text "The new conversation is given the request, the task contract and the project notes")
      (pane-append *middle*
                   (gtk:build
                    (gtk:box :orientation :vertical :spacing 6 :css-classes '("duet-approval")
                             (gtk:label :label "Start in a new conversation?" :xalign 0.0 :css-classes '("heading"))
                             (gtk:label :label (format nil "~{~a~^ ~} Every turn rereads the conversation, so a new one makes ~
this task's turns cheaper."
                                                       (mapcar (lambda (agent)
                                                                 (format nil "~a's context is about ~d% full (~:d of ~:d tokens)."
                                                                         (agent-name agent)
                                                                         (round (* 100 (agent-context-fraction agent)))
                                                                         (round (agent-context-tokens agent))
                                                                         (round (agent-context-limit agent))))
                                                               agents))
                                        :xalign 0.0 :wrap t)
                             buttons))))))

(defun worktrees-wanted-p ()
  (and (member (selected-mode) '(:divide :compare)) (gtk:check-button-get-active (control :worktrees))))

(defun make-middle-pane ()
  (multiple-value-bind (messages scroller) (make-message-list)
    (let* ((status (make-instance 'gtk:label :xalign 1.0 :hexpand t :ellipsize :end :css-classes '("dim-label")))
           (input (make-input))
           (recipient (gtk:drop-down-new-from-strings '("To both agents" "To Claude" "To ChatGPT")))
           (chips (make-instance 'gtk:flow-box :selection-mode :none :visible nil
                                 :row-spacing 4 :column-spacing 4 :margin-start 12 :margin-end 12))
           (attachments (new-agent-pane :chips chips))
           (send (make-instance 'gtk:button :label "Start collaboration" :css-classes '("suggested-action")))
           (stop (make-instance 'gtk:button :label "Stop collaboration" :visible nil
                                :css-classes '("destructive-action")))
           (mode (gtk:drop-down-new-from-strings '("Write & review" "Split the work" "Compare proposals" "Review only")))
           (review-target (gtk:drop-down-new-from-strings '("Uncommitted changes" "Branch (merge base)" "Branch (compare tips)" "Last N commits")))
           (review-base (make-instance 'gtk:entry :text "HEAD" :placeholder-text "Base branch" :width-chars 10))
           (review-branch (make-instance 'gtk:entry :placeholder-text "Branch to review" :width-chars 12))
           (review-count (gtk:spin-button-new-with-range 1d0 1000d0 1d0))
           (review-agents (gtk:drop-down-new-from-strings '("Both review" "Lead reviews" "Partner reviews")))
           (review-options nil)
           (approve-plan (make-instance 'gtk:check-button :label "Approve plan" :active (setting :approve-plan nil)))
           (worktrees (make-instance 'gtk:check-button :label "Own worktrees" :active (setting :worktrees nil)))
           (lead (gtk:drop-down-new-from-strings '("Claude leads" "ChatGPT leads")))
           (rounds (gtk:spin-button-new-with-range 1d0 10d0 1d0))
           (check (make-instance 'gtk:entry :placeholder-text "Check command" :width-chars 14 :hexpand t)))
      (setf review-options (wrapping-controls review-target review-base review-branch review-count review-agents))
      (gtk:widget-set-visible review-options nil)
      (gtk:widget-set-tooltip-text review-base "Base for branch review (HEAD by default)")
      (gtk:widget-set-tooltip-text review-count "Number of commits for Last N commits")
      (setf *middle* (make-pane :messages messages :scroller scroller :status status :input input :send send))
      (gtk:drop-down-set-selected mode (saved-choice-index :workflow-mode 4))
      (gtk:drop-down-set-selected lead (saved-choice-index :workflow-lead 2))
      (let ((saved (setting :workflow-rounds *default-max-rounds*)))
        (gtk:spin-button-set-value rounds (coerce (if (and (integerp saved) (<= 1 saved 10))
                                                                     saved *default-max-rounds*) 'double-float)))
      (gobject:connect lead "notify::selected"
                       (lambda (widget spec) (declare (ignore spec))
                         (setf (setting :workflow-lead) (gtk:drop-down-get-selected widget))
                         (update-workflow-summary)))
      (gobject:connect rounds :value-changed
                       (lambda (widget) (setf (setting :workflow-rounds) (gtk:spin-button-get-value-as-int widget))
                         (update-workflow-summary)))
      (gtk:widget-set-tooltip-text mode "Write & review: the lead does the work and the other reviews it, back and forth until approved. Split the work: the lead divides the task, both work at once, then each reviews the other's part. Compare proposals: both propose an approach on their own, the lead reconciles them into a plan, and then the work is split. Review only: selected agents inspect existing changes with enforced read-only access; Fix these starts a writing task.")
      (gtk:widget-set-tooltip-text approve-plan "Wait for you to look over (and edit) the plan before any work starts")
      (gobject:connect review-target "notify::selected" (lambda (d pspec) (declare (ignore d pspec)) (update-controls)))
      (gobject:connect mode "notify::selected"
                       (lambda (d pspec) (declare (ignore pspec))
                         (setf (setting :workflow-mode) (gtk:drop-down-get-selected d))
                         (update-controls)))
      (gobject:connect approve-plan :toggled
                       (lambda (b) (setf (setting :approve-plan) (gtk:check-button-get-active b))))
      (gtk:widget-set-tooltip-text worktrees "Each agent does its part in its own git worktree, in a new conversation; then the parts are committed and merged into the current branch. Both final replies are combined and reviewed with any file changes. Worktrees and recovery refs are kept until approval. Needs a git repository with nothing uncommitted.")
      (gobject:connect worktrees :toggled
                       (lambda (b) (setf (setting :worktrees) (gtk:check-button-get-active b))))
      (gtk:widget-set-tooltip-text lead "Who does the work (or splits it)")
      (gtk:widget-set-tooltip-text rounds "Most review rounds before stopping")
      (gtk:widget-set-tooltip-text check "A shell command run in the folder after each round of work, such as `make test`. If it fails, the author gets its output to fix before the review; the reviewer is told how it went. Kept for this folder; leave it empty for none.")
      (gobject:connect check :changed
                       (lambda (e) (when *directory*
                                     (setf (folder-check-command *directory*) (gtk:editable-get-text e)))))
      (gtk:widget-set-tooltip-text input "Add direction or ask a question. Drop files or images to attach them. Return sends; Shift+Return adds a line.")
      (on-file-drop input (lambda (paths)
                            (set-attachments attachments
                                             (union (agent-pane-attachments attachments) paths :test #'equal))))
      (setf *controls* (list* :recipient recipient :composer-attachments attachments :input input :send send :stop stop :mode mode :lead lead :rounds rounds :check check
                              :approve-plan approve-plan :worktrees worktrees
                              :review-options review-options :review-target review-target :review-base review-base
                              :review-branch review-branch :review-count review-count :review-agents review-agents *controls*))
      (gobject:connect send :clicked (lambda (b) (declare (ignore b))
                                      (send-composer-message)))
      (gobject:connect stop :clicked (lambda (b) (declare (ignore b))
                                      (when (workflow-running-p) (stop-workflow *workflow*))))
      (gtk:widget-set-size-request input -1 100)
      (on-return input #'send-composer-message)
      (gobject:connect recipient "notify::selected" (lambda (d p) (declare (ignore d p)) (update-controls)))
      (pane-note *middle* (format nil "Describe what you want done. The agents pass work and reviews back and forth here; the agent panels show their full transcripts.~%~%Choose both agents to start collaborating, or route a message to one agent. During a task, send direction here. Return sends; Shift+Return adds a line."))
      (gtk:build
       (gtk:box :orientation :vertical :width-request 320 :css-classes '("duet-pane" "duet-middle")
                (gtk:box :spacing 8 :margin-start 12 :margin-end 12 :margin-top 6 :margin-bottom 6
                         (gtk:label :label "Conversation" :css-classes '("heading"))
                         status)
                (let ((progress (make-instance 'gtk:label :xalign 0.0 :wrap t
                                                :margin-start 12 :margin-end 12 :margin-bottom 6)))
                  (setf (getf *controls* :progress) progress)
                  progress)
                (make-checklist-summary)
                (make-progress-details)
                (make-activity-strip)
                (gtk:separator)
                scroller
                (gtk:separator)
                (let ((summary (make-instance 'gtk:label :xalign 0.0 :wrap t
                                               :margin-start 12 :margin-end 12 :margin-top 6
                                               :css-classes '("caption-heading"))))
                  (setf (getf *controls* :workflow-summary) summary)
                  (update-workflow-summary)
                  summary)
                (make-workflow-settings
                 (gtk:build
                  (gtk:box :orientation :vertical :spacing 8
                           (wrapping-controls
                            (gtk:build (gtk:box :spacing 6 (gtk:label :label "Rounds") rounds)))
                           (wrapping-controls approve-plan worktrees)
                           (wrapping-controls (make-instance 'gtk:label :label "Check command") check)
                           review-options)))
                (wrapping-controls recipient mode lead)
                (let ((note (make-instance 'gtk:label :xalign 0.0 :wrap t
                                            :margin-start 12 :margin-end 12 :css-classes '("dim-label" "caption"))))
                  (setf (getf *controls* :routing-note) note)
                  note)
                chips
                (composer input (gtk:build (gtk:box :spacing 8 stop send))))))))


;;; The folder, what the agents may do, and the look

(defun folder-label (directory)
  (let ((home (namestring (user-homedir-pathname)))
        (path (string-right-trim "/" (namestring directory))))
    (cond ((string= path (string-right-trim "/" home)) "~")
          ((and (> (length path) (length home)) (string= home path :end2 (length home)))
           (format nil "~~/~a" (subseq path (length home))))
          (t path))))

(defun set-directory (directory)
  "Have both agents work in DIRECTORY, in new conversations."
  (let ((directory (uiop:ensure-directory-pathname directory))
        (first-time (null *directory*)))
    (unless (equal directory *directory*)
      ;; Stop the agents while the history is still theirs, so what they
      ;; were saying is kept, with what resumes it; then put it away.
      (dolist (agent (list *claude* *codex*))
        (when agent
          (agent-cancel agent)
          (agent-shutdown agent)))
      (finish-history)
      (clear-checklist)
      (setf *directory* directory)
      (note-recent-folder directory)
      (setf (setting :directory) (namestring directory))
      (dolist (agent (list *claude* *codex*))
        (when agent
          (agent-new-conversation agent)
          (setf (agent-directory agent) directory)
          (unless first-time
            (pane-note (agent-pane-of agent) (format nil "Now working in ~a: a new conversation." (namestring directory))))))
      (when *middle*
        (pane-note *middle* (format nil "Working in ~a" (namestring directory)) "dim-label" "duet-note"))
      (refresh-folder-menu)
      (refresh-check-command)
      (documents-folder-changed)
      (refresh-workspace-sidebar))))

(defun refresh-check-command ()
  "Show the current folder's check command."
  (let ((entry (control :check)))
    (when entry
      (gtk:editable-set-text entry (or (folder-check-command *directory*) "")))))

(defun choose-directory ()
  (let ((dialog (gtk:file-dialog-new)))
    (gtk:file-dialog-set-title dialog "Choose the folder the agents work in")
    (when *directory*
      (gtk:file-dialog-set-initial-folder dialog (gio:file-new-for-path (namestring *directory*))))
    (gio:async (gtk:file-dialog-select-folder dialog *window*)
               (lambda (file) (set-directory (gio:file-get-path file)))
               :error (lambda (e) (declare (ignore e))))))

(defun refresh-folder-menu ()
  "Show the current folder on the folder button, and the recent ones in its menu."
  (let ((button (control :folder)))
    (when button
      (gtk:label-set-text (control :folder-label) (folder-label *directory*))
      (gtk:widget-set-tooltip-text button (format nil "Claude and ChatGPT work in ~a.~%Click to switch folders."
                                                  (namestring *directory*)))
      (let* ((popover (make-instance 'gtk:popover))
             (list (make-instance 'gtk:box :orientation :vertical :spacing 2
                                  :margin-start 4 :margin-end 4 :margin-top 4 :margin-bottom 4)))
        (flet ((item (label tooltip action)
                 (let ((b (make-instance 'gtk:button :css-classes '("flat")
                                         :child (make-instance 'gtk:label :label label :xalign 0.0
                                                               :ellipsize :start
                                                               :max-width-chars 60)
                                         :tooltip-text tooltip)))
                   (gobject:connect b :clicked (lambda (b) (declare (ignore b))
                                                 (gtk:popover-popdown popover)
                                                 (funcall action)))
                   (gtk:box-append list b))))
          (gtk:box-append list (make-instance 'gtk:label :label "Recent folders" :xalign 0.0
                                              :margin-start 6 :css-classes '("caption-heading" "dim-label")))
          (dolist (path (setting :recent-folders))
            (unless (equal path (namestring *directory*))
              (let ((path path))
                (when (uiop:directory-exists-p path)
                  (item (folder-label path) path (lambda () (set-directory path)))))))
          (gtk:box-append list (make-instance 'gtk:separator))
          (item "Choose folder…" "Pick any folder" #'choose-directory))
        (gtk:popover-set-child popover list)
        (gtk:menu-button-set-popover button popover)))))

(defparameter *permission-choices*
  '((:ask "Ask before commands"
     "Both may edit files in the folder. Before running a command (tests, builds, scripts) they ask you: Allow, Always allow, or Deny.")
    (:edits "Edits only, no prompts"
     "Both may edit files in the folder without asking. Claude runs no commands but read-only git; ChatGPT runs commands in Codex's sandbox (no network) without asking.")
    (:full "Full access"
     "Neither is restricted or asks. Only in a folder you're happy for them to change.")))

(defun set-permission (permission)
  (setf *permission* permission
        (setting :permission) permission)
  ;; Each picks it up at its next message.
  (dolist (agent (list *claude* *codex*))
    (setf (agent-permission agent) permission)))

(defparameter *themes* '((:dark "Dark" :force-dark) (:light "Light" :force-light) (:system "System" :default)))

(defun apply-theme (theme)
  (setf (setting :theme) theme)
  (setf (adw:color-scheme) (third (assoc theme *themes*)))
  (restyle-terminals))

(defun panel-shortcut-label (name)
  (let ((text (panel-shortcut-text name)))
    (if (eq (parse-panel-shortcut text) :disabled) "Disabled" text)))

(defun update-shortcut-tooltips ()
  (dolist (spec '((:agent-toggle :agents-shortcut "Toggle agents")
                  (:checklist-toggle :checklist-shortcut "Show or hide the sidebar")
                  (:sidebar-toggle :checklist-shortcut "Toggle sidebar")
                  (:terminal :terminal-shortcut "Show or hide the terminal")
                  (:terminal-button :terminal-shortcut "Show or hide the terminal")))
    (destructuring-bind (control-name shortcut title) spec
      (when (control control-name)
        (gtk:widget-set-tooltip-text
         (control control-name)
         (format nil "~a (~a~a)" title (panel-shortcut-label shortcut)
                 (if (eq shortcut :terminal-shortcut) "; Ctrl+` also works" "")))))))

(defun show-shortcut-settings ()
  (let* ((window (make-instance 'gtk:window :title "Keyboard shortcuts"
                                :transient-for *window* :modal t :default-width 480))
         (box (make-instance 'gtk:box :orientation :vertical :spacing 12
                             :margin-start 18 :margin-end 18 :margin-top 18 :margin-bottom 18))
         (help (make-instance 'gtk:label :wrap t :xalign 0.0
                             :label "Type a shortcut such as Alt+A or Ctrl+Shift+F1. Modifiers: Cmd, Option, Ctrl, Alt, Shift, Super. Leave blank to disable. Desktop shortcuts may take priority; editing and terminal shortcuts can also conflict."))
         (message (make-instance 'gtk:label :wrap t :xalign 0.0))
         (entries nil)
         (buttons (make-instance 'gtk:box :spacing 8))
         (reset (make-instance 'gtk:button :label "Restore defaults"))
         (cancel (make-instance 'gtk:button :label "Cancel"))
         (apply (make-instance 'gtk:button :label "Apply" :css-classes '("suggested-action"))))
    (gtk:window-set-child window box)
    (gtk:box-append box help)
    (dolist (spec *panel-shortcuts*)
      (let* ((name (first spec))
             (entry (make-instance 'gtk:entry :hexpand t))
             (row (make-instance 'gtk:box :spacing 12)))
        (gtk:editable-set-text entry (panel-shortcut-text name))
        (push (cons name entry) entries)
        (gtk:box-append row (make-instance 'gtk:label :label (second spec) :xalign 0.0 :width-chars 10))
        (gtk:box-append row entry)
        (gtk:box-append box row)))
    (setf entries (nreverse entries))
    (gtk:box-append box (make-instance 'gtk:label :xalign 0.0 :label "Ctrl+` remains an additional terminal shortcut."))
    (gtk:box-append box message)
    (dolist (button (list reset cancel apply)) (gtk:box-append buttons button))
    (gtk:box-append box buttons)
    (flet ((show-message (text errorp)
             (gtk:label-set-text message text)
             (if errorp
                 (gtk:widget-add-css-class message "error")
                 (gtk:widget-remove-css-class message "error"))))
      (gobject:connect cancel :clicked (lambda (b) (declare (ignore b)) (gtk:window-close window)))
      (gobject:connect reset :clicked
                       (lambda (b)
                         (declare (ignore b))
                         (dolist (entry entries)
                           (gtk:editable-set-text (cdr entry) (default-panel-shortcut (car entry))))
                         (show-message "Press Apply to save the defaults." nil)))
      (gobject:connect apply :clicked
                       (lambda (b)
                         (declare (ignore b))
                         (let* ((texts (mapcar (lambda (entry) (gtk:editable-get-text (cdr entry))) entries))
                                (error (panel-shortcut-error texts)))
                           (if error
                               (show-message error t)
                               (progn
                                 (loop for entry in entries for text in texts
                                       do (setf (getf *settings* (car entry)) (string-trim '(#\Space #\Tab) text)))
                                 (save-settings)
                                 (update-shortcut-tooltips)
                                 (gtk:window-close window)))))))
    (gtk:window-present window)
    window))

(defun run-panel-shortcut (action)
  "Toggle the panel for ACTION, a name from *PANEL-SHORTCUTS*; true if it did."
  (case action
    (:agents-shortcut (toggle-agents))
    (:terminal-shortcut (toggle-terminal))
    (:checklist-shortcut
     (let ((checklist (control :checklist-toggle)))
       (gtk:check-button-set-active checklist (not (gtk:check-button-get-active checklist))))))
  (not (null action)))

(defun event-panel-shortcut (event keyval modifiers)
  ;; GDK compares modifiers itself, ignoring those the layout consumed to
  ;; produce the key (such as Shift for digits on AZERTY). A signal emitted
  ;; without an event can only be compared directly.
  (panel-shortcut-action
   (lambda (key required)
     (cond (event (gdk:key-event-matches event (gdk:keyval-from-name key) required))
           ((and (= (gdk:keyval-to-lower keyval) (gdk:keyval-from-name key))
                 (null (set-exclusive-or
                        required
                        (intersection modifiers '(:control-mask :alt-mask :shift-mask
                                                  :meta-mask :super-mask)))))
            :exact)
           (t :none)))))

(defun make-panel-icon (kind)
  "Draw a theme-colored vector icon without relying on installed icon themes."
  (let ((icon (make-instance 'gtk:drawing-area
                             :content-width 16 :content-height 16
                             :halign :center :valign :center)))
    (gtk:drawing-area-set-draw-func
     icon
     (lambda (area cr width height)
       (cairo:save cr)
       (cairo:scale cr (/ width 16.0d0) (/ height 16.0d0))
       (gdk:cairo-set-source-rgba cr (gtk:widget-get-color area))
       (cairo:set-line-width cr 1.5d0)
       (ecase kind
         (:agents
          ;; Two heads and shoulders, readable at the header's 16px size.
          (dolist (x '(5.0d0 11.0d0))
            (cairo:new-sub-path cr)
            (cairo:arc cr x 4.5d0 2.0d0 0.0d0 (* 2.0d0 pi)))
          (cairo:stroke cr)
          (dolist (x '(5.0d0 11.0d0))
            (cairo:new-sub-path cr)
            (cairo:arc cr x 12.5d0 3.25d0 pi (* 2.0d0 pi)))
          (cairo:stroke cr))
         (:sidebar
          ;; A window frame with a filled right-hand sidebar.
          (cairo:rectangle cr 1.75d0 2.75d0 12.5d0 10.5d0)
          (cairo:stroke cr)
          (cairo:rectangle cr 9.5d0 3.0d0 4.5d0 10.0d0)
          (cairo:fill cr)))
       (cairo:restore cr)))
    icon))

(defun make-header ()
  (let* ((header (make-instance 'gtk:header-bar))
         (folder-label (make-instance 'gtk:label :ellipsize :start :max-width-chars 24))
         (folder (make-instance 'gtk:menu-button
                                :child (gtk:build (gtk:box :spacing 6 folder-label (gtk:label :label "▾")))))
         (permission (gtk:drop-down-new-from-strings (mapcar #'second *permission-choices*)))
         (theme (gtk:drop-down-new-from-strings (mapcar #'second *themes*)))
         (view (gtk:drop-down-new-from-strings (mapcar #'second *views*)))
         (agent-toggle (make-instance 'gtk:button :child (make-panel-icon :agents)
                                      :css-classes '("image-button")
                                      :tooltip-text "Toggle agents"))
         (sidebar-toggle (make-instance 'gtk:toggle-button :child (make-panel-icon :sidebar)
                                        :css-classes '("image-button")
                                        :active (setting :checklist-visible t)
                                        :tooltip-text "Toggle sidebar"))
         (terminal-shortcut (make-instance 'gtk:button :label ">_"
                                           :css-classes '("duet-terminal-shortcut" "monospace")
                                           :tooltip-text "Show or hide the terminal"))
         (tools (make-instance 'gtk:menu-button :label "Workspace"))
         (box (make-instance 'gtk:box :orientation :vertical :spacing 10
                             :margin-start 12 :margin-end 12 :margin-top 12 :margin-bottom 12))
         (popover (make-instance 'gtk:popover :child box)))
    (gtk:widget-set-tooltip-text folder "Choose a workspace folder or reopen a recent folder")
    (gtk:menu-button-set-popover tools popover)
    (setf *controls* (list* :folder folder :folder-label folder-label :permission permission
                                 :view view :agent-toggle agent-toggle :sidebar-toggle sidebar-toggle :terminal-button terminal-shortcut *controls*))
    (flet ((row (label widget)
             (gtk:box-append box
                             (gtk:build (gtk:box :spacing 8
                                                 (gtk:label :label label :xalign 0.0 :hexpand t) widget))))
           (action (label callback)
             (let ((button (make-instance 'gtk:button :label label)))
               (gobject:connect button :clicked
                                (lambda (b) (declare (ignore b))
                                  (gtk:popover-popdown popover) (funcall callback)))
               (gtk:box-append box button))))
      (let ((terminal (make-instance 'gtk:toggle-button :label "Terminal"
                                     :tooltip-text "Show or hide the terminal"))
            (checklist (make-instance 'gtk:check-button :label "Sidebar"
                                      :active (setting :checklist-visible t)
                                      :tooltip-text "Show or hide the sidebar"))
            (notes (make-instance 'gtk:check-button :label "Share project instructions" :active *shared-notes*
                                  :tooltip-text "Give each agent the other agent's project instructions too.")))
        (setf (getf *controls* :terminal) terminal
              (getf *controls* :checklist-toggle) checklist)
        (gobject:connect terminal :toggled
                         (lambda (b)
                           (let ((visible (gtk:toggle-button-get-active b)))
                             (unless (eql visible (not (null (terminal-visible-p))))
                               (set-terminal-visible visible)))))
        (gobject:connect checklist :toggled
                         (lambda (b)
                           (let ((visible (gtk:check-button-get-active b)))
                             (setf (setting :checklist-visible) visible)
                             (set-checklist-visible visible)
                             (refresh-checklist-summary))))
        (gobject:connect notes :toggled
                         (lambda (b) (setf *shared-notes* (gtk:check-button-get-active b)
                                           (setting :shared-notes) *shared-notes*)))
        (gtk:box-append box terminal)
        (action "Keyboard shortcuts…" #'show-shortcut-settings)
        (action "Find in pane…" #'show-pane-search)
        (action "Documents" #'show-documents)
        (action "Changes" #'show-task-changes)
        (gtk:box-append box checklist)
        (gtk:box-append box (make-instance 'gtk:separator))
        (row "Layout" view)
        (row "Permissions" permission)
        (row "Appearance" theme)
        (gtk:box-append box notes)))
    (gtk:drop-down-set-selected permission (or (position *permission* *permission-choices* :key #'first) 0))
    (gtk:widget-set-tooltip-text permission
                                 (format nil "~{~a~^~%~%~}"
                                         (mapcar (lambda (choice)
                                                   (format nil "~a: ~a" (second choice) (third choice)))
                                                 *permission-choices*)))
    (gobject:connect permission "notify::selected"
                     (lambda (d spec) (declare (ignore spec))
                       (set-permission (first (nth (gtk:drop-down-get-selected d) *permission-choices*)))))
    (gtk:widget-set-tooltip-text theme "Choose Dark, Light, or System appearance.")
    (gtk:drop-down-set-selected theme (or (position (setting :theme :dark) *themes* :key #'first) 0))
    (gobject:connect theme "notify::selected"
                     (lambda (d spec) (declare (ignore spec))
                       (apply-theme (first (nth (gtk:drop-down-get-selected d) *themes*)))))
    (gtk:drop-down-set-selected view (or (position *view* *views* :key #'first) 0))
    (gobject:connect view "notify::selected"
                     (lambda (d spec) (declare (ignore spec))
                       (let ((selected (first (nth (gtk:drop-down-get-selected d) *views*))))
                         (unless (eq selected *view*) (set-view selected)))))
    (update-agent-toggle)
    (update-shortcut-tooltips)
    (gobject:connect agent-toggle :clicked
                     (lambda (button) (declare (ignore button)) (toggle-agents)))
    (gtk:header-bar-pack-start header folder)
    (gtk:header-bar-pack-start header (make-history-button))
    (gobject:connect terminal-shortcut :clicked
                     (lambda (button) (declare (ignore button)) (toggle-terminal)))
    (gtk:header-bar-pack-end header tools)
    (gtk:header-bar-pack-end header terminal-shortcut)
    (gobject:connect sidebar-toggle :toggled
                     (lambda (button)
                       (gtk:check-button-set-active (control :checklist-toggle)
                                                    (gtk:toggle-button-get-active button))))
    (gtk:header-bar-pack-end header sidebar-toggle)
    (gtk:header-bar-pack-end header agent-toggle)
    header))

;;; Checking the CLIs

(defun check-agents ()
  (sb-thread:make-thread
   (lambda ()
     (let ((status (check-claude)))
       (deliver (lambda () (show-claude-status status)))
       (when (claude-status-program status)
         (let ((models (ignore-errors (claude-list-models (claude-status-program status)))))
           (when models
             (deliver (lambda () (set-model-choices (agent-pane-of *claude*) models))))))))
   :name "check claude")
  (let ((agent *codex*))
    (sb-thread:make-thread
     (lambda ()
       (let ((status (check-codex)))
         (deliver (lambda () (show-codex-status status)))
         (when (and (codex-status-program status) (codex-status-logged-in status))
           (multiple-value-bind (models usage)
               (ignore-errors (codex-agent-load-catalog agent (codex-status-program status)))
             (deliver (lambda () (show-codex-catalog models usage)))))))
     :name "check codex")))

(defun set-pane-ready (pane ready &optional version)
  (setf (agent-pane-ready pane) ready
        (agent-pane-version pane) version)
  (pane-update-status pane))

(defun show-claude-status (status)
  (let ((pane (agent-pane-of *claude*)))
    (cond ((claude-status-error status)
           (set-pane-ready pane "Not available")
           (pane-note pane (format nil "~a~%~%Install Claude Code (https://claude.com/claude-code), then restart."
                                   (claude-status-error status)) "error"))
          ((not (claude-status-logged-in status))
           (set-pane-ready pane "Not signed in")
           (pane-note pane "Claude Code is not signed in. Run `claude auth login` in a terminal, then send a message." "error"))
          (t (set-pane-ready pane "Ready" (claude-status-version status))))))

(defun show-codex-status (status)
  (let ((pane (agent-pane-of *codex*)))
    (when (codex-status-program status)
      (setf (codex-agent-program *codex*) (codex-status-program status)))
    (cond ((null (codex-status-program status))
           (set-pane-ready pane "Not available")
           (pane-note pane (format nil "~a~%~%Install the Codex CLI or the ChatGPT desktop app, then restart."
                                   (codex-status-error status)) "error"))
          ((not (codex-status-logged-in status))
           (set-pane-ready pane "Not signed in")
           (pane-note pane (format nil "Codex is not signed in~@[ (~a)~]. Run `codex login` in a terminal, then send a message."
                                   (codex-status-error status)) "error"))
          (t (set-pane-ready pane "Ready" (codex-status-version status))))))

(defun show-codex-catalog (models usage)
  (let ((pane (agent-pane-of *codex*)))
    (when usage
      (setf (agent-usage *codex*) usage))
    (when models
      (setf (codex-agent-models *codex*) models)
      (set-model-choices pane models))
    (pane-update-status pane)))

;;; The window

(defparameter *css*
  '((".duet-panel-claude" :border-top "3px solid alpha(#d97757, 0.8)")
    (".duet-panel-codex" :border-top "3px solid alpha(#14b8a6, 0.8)")
    (".duet-live-activity" :font-size "smaller" :background-color "alpha(@view_fg_color, 0.05)"
     :border-radius "6px" :padding ("5px" "8px"))
    (".duet-terminal-shortcut" :color "@accent_color" :background-color "alpha(@accent_bg_color, 0.12)")
    (".duet-middle" :background-color "alpha(@view_fg_color, 0.02)")
    (".duet-pane" :background-color "@view_bg_color")
    (".duet-prompt" :background-color "alpha(@accent_bg_color, 0.18)" :border-radius "8px" :padding ("6px" "10px"))
    (".duet-image" :padding "4px")
    (".duet-code" :background-color "alpha(@view_fg_color, 0.07)" :border-radius "6px" :padding "6px")
    (".duet-tool" :opacity "1.0")
    (".duet-approval" :background-color "alpha(@warning_bg_color, 0.22)" :border-radius "8px" :padding "10px"
     :border "1px solid alpha(@warning_color, 0.5)")
    (".duet-message" :border-radius "10px" :padding ("8px" "12px"))
    (".duet-from-user" :background-color "alpha(@accent_bg_color, 0.18)")
    (".duet-from-claude" :border "1px solid alpha(#d97757, 0.24)" :background-color "alpha(#d97757, 0.16)" :margin-right "12px")
    (".duet-from-codex" :border "1px solid alpha(#14b8a6, 0.24)" :background-color "alpha(#14b8a6, 0.16)" :margin-left "12px")
    (".duet-claude" :color "mix(@view_fg_color, #e8875f, 0.7)")
    (".duet-codex" :color "mix(@view_fg_color, #2dd4bf, 0.7)")
    (".duet-note" :font-size "smaller")
    (".duet-progress > flowboxchild" :background-color "transparent" :box-shadow "none"
     :border "none" :padding "0" :outline "none")
    (".duet-stage" :background-color "alpha(@view_fg_color, 0.06)" :border-radius "8px" :padding "10px"
     :border "1px solid alpha(@view_fg_color, 0.12)")
    (".duet-stage-active" :background-color "alpha(@accent_bg_color, 0.16)"
     :border-color "alpha(@accent_color, 0.55)")
    (".duet-stage-complete" :background-color "alpha(@success_bg_color, 0.16)"
     :border-color "alpha(@success_color, 0.5)")
    (".duet-stage-attention" :background-color "alpha(@warning_bg_color, 0.18)"
     :border-color "alpha(@warning_color, 0.55)")
    (".duet-stage-error" :background-color "alpha(@error_bg_color, 0.16)"
     :border-color "alpha(@error_color, 0.55)")
    (".duet-stage-skipped" :color "alpha(@view_fg_color, 0.6)" :border-style "dashed")
    (".duet-chip" :background-color "alpha(@view_fg_color, 0.08)" :border-radius "12px")
    (".duet-summary" :background-color "alpha(@success_bg_color, 0.14)" :border-radius "8px" :padding "10px"
     :border "1px solid alpha(@success_color, 0.45)")
    (".duet-checklist" :background-color "alpha(@view_fg_color, 0.05)" :border-radius "8px" :padding ("8px" "12px"))
    (".duet-terminal-panel" :background-color "@view_bg_color")
    (".duet-terminal" :padding ("2px" "6px") :font-family "Menlo, Monaco, monospace")))

(defun setup-window-keys (window)
  "Handle window shortcuts wherever the focus is, including the Terminal."
  (let ((keys (gtk:event-controller-key-new)))
    (gtk:event-controller-set-propagation-phase keys :capture)
    (gobject:connect keys :key-pressed
                     (lambda (c keyval keycode state)
                       (declare (ignore keycode))
                       (let ((key (gdk:keyval-name keyval))
                             (modifiers (modifier-list state)))
                         (cond
                          ((quit-shortcut-p key modifiers)
                           ;; Closing the window runs the normal cleanup for
                           ;; agents, terminals and document previews before quit.
                           (gtk:window-close window)
                           t)
                          ((run-panel-shortcut
                            (event-panel-shortcut (gtk:event-controller-get-current-event c)
                                                  keyval modifiers)))
                          ((find-shortcut-p key modifiers :terminal-focused (terminal-focus-p window))
                           (show-pane-search)
                           t)
                          ((terminal-grave-shortcut-p key modifiers)
                           (toggle-terminal)
                           t)))))
    (gtk:widget-add-controller window keys)
    keys))

(defun apply-settings-to (agent)
  (setf (agent-permission agent) *permission*)
  (let ((model (setting (agent-setting-key agent "MODEL")))
        (effort (setting (agent-setting-key agent "EFFORT"))))
    (when model (setf (agent-model agent) model))
    (setf (agent-effort agent) effort)))

(defun make-main-content ()
  "The agent panes, shared conversation, checklist and Terminal panel."
  (multiple-value-bind (claude-pane claude-widget) (make-agent-pane *claude*)
    (multiple-value-bind (codex-pane codex-widget) (make-agent-pane *codex*)
      (setf *panes* (list (cons :claude claude-pane) (cons :codex codex-pane)))
      ;; Controls wrap within each pane, so the dividers can respect their
      ;; actual minimum widths without forcing all options onto one row.
      (let* ((middle (make-middle-pane))
             (inner (make-instance 'gtk:paned :orientation :horizontal
                                   :position (if (setting :checklist-visible t) 470 600)
                                   :shrink-start-child nil :shrink-end-child nil))
             (outer (make-instance 'gtk:paned :orientation :horizontal
                                   :position (if (setting :checklist-visible t) 336 420)
                                   :shrink-start-child nil :shrink-end-child nil)))
        (gtk:paned-set-start-child inner middle)
        (gtk:paned-set-end-child inner codex-widget)
        (gtk:paned-set-start-child outer claude-widget)
        (gtk:paned-set-end-child outer inner)
        ;; A separate, resizable checklist to the right of the agent panes.
        (let ((content (make-instance 'gtk:paned :orientation :horizontal :position 1120
                                      :shrink-start-child nil :shrink-end-child nil
                                      :resize-end-child nil)))
          (setf (getf *controls* :checklist-split) content
                (getf *controls* :agent-split) outer
                (getf *controls* :conversation-split) inner
                (getf *controls* :middle-widget) middle
                (getf *controls* :claude-widget) claude-widget
                (getf *controls* :codex-widget) codex-widget)
          (arrange-conversation *view*)
          (gtk:paned-set-start-child content outer)
          (gtk:paned-set-end-child content (make-workspace-sidebar))
          (when (setting :checklist-visible t)
            (setf (getf *controls* :restore-checklist-width)
                  (or (saved-checklist-width) 300)))
          ;; The Terminal panel below the panes, hidden until asked for.
          (let ((vertical (make-instance 'gtk:paned :orientation :vertical :position 560
                                         :shrink-start-child nil :shrink-end-child nil
                                         :resize-end-child nil)))
            (setf (getf *controls* :terminal-split) vertical)
            (gtk:paned-set-start-child vertical content)
            (gtk:paned-set-end-child vertical (make-terminal-panel))
            vertical))))))

(defun activate (app)
  "Open the main window."
  (gtk:add-css *css*)
  (apply-theme (setting :theme :dark))
  (setf *controls* '()
        *permission* (or (setting :permission) :ask)
        *shared-notes* (setting :shared-notes t)
        *view* (if (assoc (setting :view) *views*) (setting :view) :conversation)
        *view-only* '())
  (let ((window (destructuring-bind (width height) (saved-window-size)
                  (make-instance 'gtk:application-window
                               :application app :title "Agent Duet"
                               :default-width width :default-height height))))
    (setf *window* window
          *claude* (make-instance 'claude-agent :deliver #'deliver)
          *codex* (make-instance 'codex-agent :deliver #'deliver))
    (apply-settings-to *claude*)
    (apply-settings-to *codex*)
    (gtk:window-set-titlebar window (make-header))
    (gtk:window-set-child window (make-main-content))
    (setup-window-keys window)
    (let ((directory (or *directory*
                         (let ((saved (setting :directory)))
                           (and saved (uiop:directory-exists-p saved)))
                         ;; Started from the Finder, the current directory is /.
                         (let ((cwd (uiop:getcwd)))
                           (if (equal (namestring cwd) "/") (user-homedir-pathname) cwd)))))
      (setf *directory* nil)
      (set-directory directory))
    (when (eq (setting :terminal-visible) t)
      (set-terminal-visible t :focus nil))
    (update-controls)
    (prune-histories)
    (check-agents)
    ;; Usage resets with nothing else happening: keep the status lines current.
    (glib:timeout-add-seconds glib:+priority-default+ 30
                              (lambda ()
                                (dolist (entry *panes*) (pane-update-status (cdr entry)))
                                t))
    ;; Turns gone quiet.
    (glib:timeout-add-seconds glib:+priority-default+ 15 (lambda () (check-stalls) t))
    (gobject:connect window :close-request
                     (lambda (w) (declare (ignore w))
                       ;; The last checkpoint saved stands: a task under way
                       ;; now is found interrupted when reopened.
                       (remember-layout)
                       (remember-window-geometry window)
                       (remember-terminal-layout)
                       (remember-checklist-width)
                       (when (control :layout-save-timeout)
                         (glib:source-remove (control :layout-save-timeout))
                         (remf *controls* :layout-save-timeout))
                       (setf *closing* t)
                       (dolist (agent (list *claude* *codex*))
                         (ignore-errors (agent-cancel agent))
                         (ignore-errors (agent-shutdown agent)))
                       (ignore-errors (finish-history))
                       (ignore-errors (flush-history-saves))
                       (ignore-errors (kill-all-terminals))
                       (ignore-errors (close-documents))
                       nil))
    (watch-conversation-allocation (control :agent-split))
    (when (eq (setting :window-maximized) t)
      (gtk:window-maximize window))
    (gtk:window-present window)))

(defun main (&optional directory)
  "Run the application; returns when its window closes. DIRECTORY is where
the agents work (by default, the last folder used, or else the current
directory). On macOS it must run on the first thread: in Cadre, use Run GTK App."
  (when (member "--review-hook" (rest sb-ext:*posix-argv*) :test #'string=)
    (run-review-hook))
  (load-settings)
  (setf *directory* (and directory (uiop:ensure-directory-pathname directory)))
  (let ((app (adw:application-new "org.example.agent_duet" '(:default-flags))))
    (gobject:connect app :window-removed
                     (lambda (application window)
                       (declare (ignore window))
                       (when (null (gtk:application-get-windows application))
                         (gio:application-quit application))))
    (gobject:connect app :activate
                     (lambda (app)
                       ;; Errors here would otherwise leave no window and no word why.
                       (handler-bind ((error (lambda (e)
                                               (format *error-output* "~&Agent Duet couldn't start: ~a~%" e)
                                               (finish-output *error-output*))))
                         (activate app))))
    (gio:application-run app nil)))
