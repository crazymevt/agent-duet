;;;; ui.lisp — three panes: Claude, the conversation between them, ChatGPT
;;;;
;;;; The side panes show everything each agent does: the prompts it gets, its
;;;; replies as they stream, its tool calls. Each has its own box for talking
;;;; to that agent alone. The middle pane is the conversation between them:
;;;; the user's request and each finished reply passed from one to the other.
;;;; Its box starts a workflow (workflow.lisp) with the chosen mode and lead.

(in-package #:agent-duet)

(defvar *window* nil)
(defvar *claude* nil)
(defvar *codex* nil)
(defvar *panes* '() "Agent id → AGENT-PANE.")
(defvar *middle* nil "The conversation's PANE.")
(defvar *checklist* nil "The review checklist panel's PANE.")
(defvar *checklist-boxes* nil "Checklist heading → current Markdown box, one per review loop.")
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
                       (:code (format out "<tt>~a</tt>" (escape (code-span-text text cs ce))))
                       (:strong (wrap "<b>" "</b>"))
                       (:emphasis (wrap "<i>" "</i>"))
                       (:strike (wrap "<s>" "</s>"))
                       (:link (wrap (format nil "<a href=\"~a\">" (escape (third more))) "</a>"))
                       (:autolink (format out "<a href=\"~a\">~a</a>"
                                          (escape (let ((url (first more)))
                                                    (if (search "://" url) url (format nil "https://~a" url))))
                                          (escape (subseq text cs ce))))
                       (:image (write-string (escape (subseq text cs ce)) out)))))))
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

(defun render-markdown (box text)
  "Fill BOX with TEXT, rendered."
  (dolist (segment (markdown-segments text))
    (gtk:box-append box (if (eq (first segment) :code)
                            (code-block-widget (second segment))
                            (let ((label (make-instance 'gtk:label :xalign 0.0 :wrap t :selectable t
                                                        :wrap-mode :word-char)))
                              ;; Markup GTK can't parse would leave the label
                              ;; empty: show the text as it is instead.
                              (let ((markup (prose-markup (second segment))))
                                (if (ignore-errors (pango:parse-markup markup -1 0) t)
                                    (gtk:label-set-markup label markup)
                                    (gtk:label-set-text label (second segment))))
                              label)))))

(defun markdown-box (text &rest classes)
  (let ((box (make-instance 'gtk:box :orientation :vertical :spacing 6 :css-classes classes)))
    (render-markdown box text)
    box))

;;; A scrolling list of messages

(defstruct pane
  messages scroller (scroll-pending nil)
  status input send)

(defun make-message-list ()
  (let* ((messages (make-instance 'gtk:box :orientation :vertical :spacing 10
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
                                            (scroll-adjustment-to-end adjustment)
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
                     (scroll-adjustment-to-end (gtk:scrolled-window-get-vadjustment (pane-scroller pane)))
                     nil))))

(defun pane-append (pane widget)
  (gtk:box-append (pane-messages pane) widget)
  (pane-scroll-to-end pane)
  widget)

(defun pane-note (pane text &rest classes)
  (pane-append pane (make-instance 'gtk:label :label text :wrap t :xalign 0.0 :selectable t
                                   :wrap-mode :word-char
                                   :css-classes (or classes '("dim-label")))))

(defun composer (input send)
  (gtk:build
   (gtk:box :spacing 6 :margin-start 8 :margin-end 8 :margin-top 6 :margin-bottom 8
            (gtk:frame :hexpand t
                       (gtk:scrolled-window :max-content-height 180 :propagate-natural-height t
                                            :hscrollbar-policy :never :child input))
            send)))

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
  (stream-box nil) (stream-label nil) (stream-text "")
  (tools (make-hash-table :test 'equal))
  (approvals (make-hash-table :test 'equal))  ; approval id → its card's button row
  (ready "Checking…")                         ; what the status says when the agent is idle
  (version nil)
  model-dropdown effort-dropdown
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

(defun pane-current-model-entry (pane)
  (nth (gtk:drop-down-get-selected (agent-pane-model-dropdown pane)) (agent-pane-models pane)))

(defun pane-effort-choices (pane)
  (let ((agent (agent-pane-agent pane)))
    (cons nil (if (eq (agent-id agent) :claude)
                  *claude-efforts*
                  (let ((model (pane-current-model-entry pane)))
                    (and model (codex-model-efforts model)))))))

(defun effort-label (effort)
  (format nil "Thinking: ~a" (if effort (string-capitalize effort) "Default")))

(defun refresh-effort-dropdown (pane)
  "Fill the effort dropdown with the current model's levels, keeping the
agent's effort when the model has it."
  (let* ((agent (agent-pane-agent pane))
         (choices (pane-effort-choices pane))
         (dropdown (agent-pane-effort-dropdown pane)))
    (unless (member (agent-effort agent) choices :test #'equal)
      (setf (agent-effort agent) nil))
    (setf (agent-pane-refreshing pane) t)
    (unwind-protect
        (progn
          (setf (agent-pane-efforts pane) choices)
          (gtk:drop-down-set-model dropdown (gtk:string-list-new (mapcar #'effort-label choices)))
          (gtk:drop-down-set-selected dropdown (or (position (agent-effort agent) choices :test #'equal) 0))
          (gtk:widget-set-sensitive dropdown (> (length choices) 1)))
      (setf (agent-pane-refreshing pane) nil))))

(defun set-model-choices (pane models)
  "Fill the model dropdown with MODELS, selecting the agent's model (or,
for Codex, the one marked as the default)."
  (let* ((agent (agent-pane-agent pane))
         (dropdown (agent-pane-model-dropdown pane)))
    (setf (agent-pane-refreshing pane) t)
    (unwind-protect
        (progn
          (setf (agent-pane-models pane) models)
          (gtk:drop-down-set-model dropdown
                                   (gtk:string-list-new
                                    (or (mapcar (lambda (m) (model-choice-name agent m)) models)
                                        '("Default model"))))
          (let ((i (or (position (agent-model agent) models
                                 :key (lambda (m) (model-choice-id agent m)) :test #'equal)
                       (and (eq (agent-id agent) :codex)
                            (position-if (lambda (m) (eq (jget m "isDefault") t)) models))
                       0)))
            (gtk:drop-down-set-selected dropdown i)
            (when models
              (setf (agent-model agent) (model-choice-id agent (nth i models)))))
          (gtk:widget-set-sensitive dropdown (not (null models))))
      (setf (agent-pane-refreshing pane) nil))
    (refresh-effort-dropdown pane)))

(defun make-model-dropdowns (pane)
  (let ((agent (agent-pane-agent pane))
        (models (gtk:drop-down-new-from-strings '("Default model")))
        (efforts (gtk:drop-down-new-from-strings '("Thinking: Default"))))
    (setf (agent-pane-model-dropdown pane) models
          (agent-pane-effort-dropdown pane) efforts)
    (gtk:widget-set-tooltip-text models (format nil "~a's model, from its next message" (agent-name agent)))
    (gtk:widget-set-tooltip-text efforts (format nil "How hard ~a thinks, from its next message" (agent-name agent)))
    (gobject:connect models "notify::selected"
                     (lambda (d pspec) (declare (ignore d pspec))
                       (unless (agent-pane-refreshing pane)
                         (let ((model (pane-current-model-entry pane)))
                           (when model
                             (setf (agent-model agent) (model-choice-id agent model)
                                   (setting (agent-setting-key agent "MODEL")) (agent-model agent))))
                         (refresh-effort-dropdown pane)
                         (setf (setting (agent-setting-key agent "EFFORT")) (agent-effort agent)))))
    (gobject:connect efforts "notify::selected"
                     (lambda (d pspec) (declare (ignore pspec))
                       (unless (agent-pane-refreshing pane)
                         (setf (agent-effort agent) (nth (gtk:drop-down-get-selected d) (agent-pane-efforts pane))
                               (setting (agent-setting-key agent "EFFORT")) (agent-effort agent)))))
    (when (eq (agent-id agent) :claude)
      ;; The CLI catalog arrives asynchronously. Its IDs can include choices
      ;; absent from the built-in aliases (for example "default" or a full
      ;; versioned name). Keep the restored model until that catalog arrives.
      (set-model-choices pane (claude-startup-models (agent-model agent))))
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

(defun make-agent-pane (agent)
  (multiple-value-bind (messages scroller) (make-message-list)
    (let* ((status (make-instance 'gtk:label :xalign 0.0 :hexpand t :ellipsize :end :css-classes '("dim-label")
                                  :label "Checking…"))
           (input (make-input))
           (send (make-instance 'gtk:button :label "Send" :valign :end :css-classes '("suggested-action")
                                :tooltip-text (format nil "Talk to ~a alone" (agent-name agent))))
           (stop (make-instance 'gtk:button :label "Stop" :valign :end :visible nil
                                :css-classes '("destructive-action")
                                :tooltip-text (format nil "Stop ~a" (agent-name agent))))
           (chips (make-instance 'gtk:flow-box :selection-mode :none :visible nil :row-spacing 4
                                 :column-spacing 4 :margin-start 8 :margin-end 8 :margin-top 6))
           (pane (new-agent-pane :agent agent :stop stop :messages messages :scroller scroller
                                 :status status :input input :send send :chips chips)))
      (flet ((send-it ()
               (let ((text (string-trim '(#\Space #\Newline #\Tab) (text-view-text input)))
                     (attachments (agent-pane-attachments pane)))
                 (when (or (plusp (length text)) attachments)
                   (handler-case
                       (progn (agent-send-now agent (if (plusp (length text)) text "Take a look at these.")
                                              attachments)
                              (clear-text-view input)
                              (set-attachments pane '()))
                     (error (e) (pane-note pane (princ-to-string e) "error")))))))
        (gobject:connect send :clicked (lambda (b) (declare (ignore b)) (send-it)))
        (gobject:connect stop :clicked (lambda (b) (declare (ignore b)) (agent-cancel agent)))
        (on-return input #'send-it))
      (setf (agent-listener agent) #'agent-pane-event)
      (multiple-value-bind (models efforts) (make-model-dropdowns pane)
        (values pane
                (gtk:build
                 (gtk:box :orientation :vertical :css-classes '("duet-pane")
                          (gtk:box :spacing 8 :margin-start 12 :margin-end 6 :margin-top 6
                                   (gtk:label :label (agent-name agent) :css-classes (list "heading" (agent-css-class agent)))
                                   status
                                   (gtk:button :label "New" :css-classes '("flat")
                                               :tooltip-text (format nil "Start a new conversation with ~a" (agent-name agent))
                                               :on-clicked (lambda (b) (declare (ignore b)) (new-conversation agent))))
                          (wrapping-controls models efforts)
                          (gtk:separator)
                          scroller
                          (gtk:separator)
                          (let ((box (gtk:build
                                      (gtk:box :orientation :vertical
                                               chips
                                               (composer input
                                                         (gtk:build (gtk:box :orientation :vertical :spacing 6
                                                                             :valign :end send stop)))))))
                            (gtk:widget-set-tooltip-text
                             input (format nil "Drop files or images here to attach them to the message to ~a"
                                           (agent-name agent)))
                            (on-file-drop box (lambda (paths)
                                                (set-attachments pane (append (agent-pane-attachments pane)
                                                                              (set-difference paths (agent-pane-attachments pane)
                                                                                              :test #'equal)))))
                            box))))))))

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
                        (format nil "~a~:[~@[ · ~a left~]~;~*~]"
                                (cond (waiting "Waiting for you")
                                      ((agent-busy agent) "Working…")
                                      (out-now (if (integerp out)
                                                   (format nil "Out of usage until ~a" (format-reset-time out))
                                                   "Out of usage"))
                                      (t (agent-pane-ready pane)))
                                out-now usage))
    (gtk:widget-set-css-classes (pane-status pane)
                                (if (or out-now (usage-low-p windows)) '("warning") '("dim-label")))
    (gtk:widget-set-tooltip-text (pane-status pane)
                                 (format nil "~@[~a~]~@[~%Remaining usage: ~a~]~{~%~a~}"
                                         (agent-pane-version pane) usage
                                         (loop for w in windows
                                               when (window-resets-at w)
                                               collect (format nil "~a resets ~a" (window-label w)
                                                               (format-reset-time (window-resets-at w))))))))

(defun pane-set-busy (pane busy)
  (let ((agent (agent-pane-agent pane)))
    (gtk:button-set-label (pane-send pane) (if busy "Send now" "Send"))
    (gtk:widget-set-visible (agent-pane-stop pane) busy)
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

(defun pane-approval (pane id title text)
  (let* ((agent (agent-pane-agent pane))
         (buttons (make-instance 'gtk:box :spacing 6 :halign :end)))
    (flet ((choice (label decision classes tooltip)
             (let ((button (make-instance 'gtk:button :label label :css-classes classes :tooltip-text tooltip)))
               (gobject:connect button :clicked
                                (lambda (b) (declare (ignore b)) (agent-answer-approval agent id decision)))
               (gtk:box-append buttons button))))
      (choice "Deny" :deny '("destructive-action") "Don't run it; the agent carries on without it")
      (choice "Always allow" :always '() "Allow this, and things like it, for the rest of this conversation")
      (choice "Allow" :allow '("suggested-action") "Run it this once"))
    (pane-finish-stream pane)
    (setf (gethash id (agent-pane-approvals pane)) buttons)
    (pane-append pane (gtk:build
                       (gtk:box :orientation :vertical :spacing 6 :css-classes '("duet-approval")
                                (gtk:label :label (format nil "~a asks to run:" (agent-name agent))
                                           :xalign 0.0 :css-classes '("heading"))
                                (gtk:label :label title :xalign 0.0 :wrap t :wrap-mode :char :selectable t
                                           :css-classes '("monospace"))
                                (gtk:label :label (or text "") :xalign 0.0 :wrap t :visible (and text (plusp (length text)) t)
                                           :css-classes '("dim-label"))
                                buttons)))
    (pane-update-status pane)
    (attention (format nil "~a needs your approval" (agent-name agent)) title :sound t)
    (when (workflow-running-p)
      (pane-note *middle* (format nil "~a is waiting for you to approve a command (~a pane)." (agent-name agent)
                                  (if (eq (agent-id agent) :claude) "left" "right"))
                 "warning" "duet-note"))))

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

(defun pane-question (pane id questions)
  (let* ((agent (agent-pane-agent pane))
         (buttons (make-instance 'gtk:box :spacing 6 :halign :end))
         (submit (make-instance 'gtk:button :label "Submit" :css-classes '("suggested-action") :sensitive nil))
         (skip (make-instance 'gtk:button :label "Skip" :tooltip-text "Don't answer; the agent carries on without it"))
         (card (gtk:build (gtk:box :orientation :vertical :spacing 10 :css-classes '("duet-approval")
                                   (gtk:label :label (format nil "~a has ~:[a question~;questions~]:" (agent-name agent)
                                                             (rest questions))
                                              :xalign 0.0 :css-classes '("heading")))))
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
               (dolist (box boxes) (gtk:widget-set-sensitive box nil))
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
    (pane-finish-stream pane)
    (setf (gethash id (agent-pane-approvals pane)) buttons)
    (pane-append pane card)
    (pane-update-status pane)
    (attention (format nil "~a has a question" (agent-name agent)) "It's waiting for your answer." :sound t)
    (when (workflow-running-p)
      (pane-note *middle* (format nil "~a is waiting for you to answer a question (~a pane)." (agent-name agent)
                                  (if (eq (agent-id agent) :claude) "left" "right"))
                 "warning" "duet-note"))))

(defun pane-approval-answered (pane id decision)
  (let ((buttons (gethash id (agent-pane-approvals pane))))
    (when buttons
      (remhash id (agent-pane-approvals pane))
      (clear-box buttons)
      (gtk:box-append buttons (make-instance 'gtk:label :label (decision-text decision)
                                             :css-classes (if (eq decision :deny) '("error") '("dim-label"))))
      (pane-update-status pane))))

(defun pane-finish-stream (pane)
  (setf (agent-pane-stream-box pane) nil (agent-pane-stream-label pane) nil (agent-pane-stream-text pane) ""))

(defun pane-delta (pane text)
  (unless (agent-pane-stream-box pane)
    (let ((box (make-instance 'gtk:box :orientation :vertical :spacing 6))
          (label (make-instance 'gtk:label :xalign 0.0 :wrap t :selectable t :wrap-mode :word-char)))
      (gtk:box-append box label)
      (setf (agent-pane-stream-box pane) (pane-append pane box)
            (agent-pane-stream-label pane) label
            (agent-pane-stream-text pane) "")))
  (setf (agent-pane-stream-text pane) (concatenate 'string (agent-pane-stream-text pane) text))
  (gtk:label-set-text (agent-pane-stream-label pane) (agent-pane-stream-text pane))
  (pane-scroll-to-end pane))

(defun pane-text (pane text)
  "A finished block of a reply: rendered in place of what was streamed."
  (let ((box (or (agent-pane-stream-box pane)
                 (pane-append pane (make-instance 'gtk:box :orientation :vertical :spacing 6)))))
    (clear-box box)
    (render-markdown box text)
    (pane-finish-stream pane)
    (pane-scroll-to-end pane)))

(defun pane-tool (pane id title)
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

(defun pane-prompt (pane text)
  ;; A side-pane message can arrive halfway through an assistant block.
  ;; Keep its widget so the remaining deltas and final text update it.
  (pane-append pane (gtk:build
                     (gtk:box :halign :end :css-classes '("duet-prompt")
                              (gtk:label :label text :wrap t :xalign 0.0 :selectable t :wrap-mode :word-char)))))

(defun agent-pane-event (agent kind &key text title id error busy decision questions)
  (let ((pane (agent-pane-of agent)))
    (when pane
      (record-agent-event pane kind text id error)
      (reconcile-stall-cards agent)
      (ecase kind
        (:busy (pane-set-busy pane busy) (update-controls))
        (:prompt (pane-prompt pane text))
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
           (pane-text pane (agent-pane-stream-text pane)))
         (when error (pane-note pane (or text "The turn failed.") "error")))))))

(defun clear-agent-pane (pane)
  (clear-box (pane-messages pane))
  (clrhash (agent-pane-tools pane))
  (clrhash (agent-pane-approvals pane))
  (pane-finish-stream pane)
  (pane-update-status pane))

(defun new-conversation (agent)
  "A new conversation with AGENT alone. The history goes on, AGENT's part
of it now carried on from here."
  (agent-cancel agent)
  (agent-new-conversation agent)
  (let ((pane (agent-pane-of agent)))
    (clear-agent-pane pane)
    (pane-note pane "New conversation."))
  (when *history*
    (record (agent-id agent) :note "New conversation.")
    (save-current-history)))

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
         (record who :text (agent-pane-stream-text pane)))
       (when error (record who :error (if (plusp (length text)) text "The turn failed.")))
       (save-current-history)))))

(defun save-current-history ()
  "Save the history, with what resumes each agent's conversation."
  (when *history*
    (dolist (agent (list *claude* *codex*))
      (setf (history-handle *history* (agent-id agent)) (agent-conversation-handle agent)))
    (save-history *history*)))

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
        (dolist (agent (list *claude* *codex*))
          (agent-new-conversation agent)
          (clear-agent-pane (agent-pane-of agent)))
        (clear-box (pane-messages *middle*))
        (clear-checklist)
        (pane-note *middle* "New conversation."))))

(defun replay-history (history)
  "Show HISTORY's entries in the panes, as they were."
  (dolist (entry (history-entries history))
    (destructuring-bind (&key who kind text &allow-other-keys) entry
      (case who
        ((:claude :codex)
         (let ((pane (cdr (assoc who *panes*))))
           (case kind
             (:prompt (pane-prompt pane text))
             (:text (pane-text pane text))
             (:tool (pane-note pane (format nil "⚙ ~a" text) "dim-label" "duet-tool"))
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
                ((string= header "Review checklist") (show-checklist body))
                (t (middle-bubble (cond ((uiop:string-prefix-p (format nil "~a →" (agent-name *claude*)) header) :claude)
                                        ((uiop:string-prefix-p (format nil "~a →" (agent-name *codex*)) header) :codex)
                                        (t :user))
                                  header body)))))
           (:error (pane-note *middle* text "error"))
           (t (pane-note *middle* text "dim-label" "duet-note"))))))))

(defun format-history-time (time)
  (multiple-value-bind (s minute hour day month year) (decode-universal-time time)
    (declare (ignore s))
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day hour minute)))

(defun open-history (id)
  "Show conversation ID again; the agents carry it on from their next message."
  ;; It may be this one, with entries not yet saved.
  (save-current-history)
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
      (setf *history* history)
      (pane-note *middle* (format nil "Carrying on the conversation from ~a. Each agent picks it up at its next message."
                                  (format-history-time (history-updated history)))
                 "dim-label" "duet-note")))))

(defun fill-history-list (box popover)
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
                                       :xalign 0.0 :margin-start 6 :ellipsize :start :max-width-chars 50
                                       :css-classes '("caption-heading" "dim-label")))
    (let ((summaries (list-histories :directory *directory* :limit 30)))
      (unless summaries
        (gtk:box-append box (make-instance 'gtk:label :label "Nothing yet" :xalign 0.0 :margin-start 6
                                           :css-classes '("dim-label"))))
      (dolist (summary summaries)
        (let ((id (getf summary :id))
              (title (or (getf summary :title) "Untitled")))
          (item (gtk:build
                 (gtk:box :orientation :vertical
                          (gtk:label :label (format nil "~:[~;• ~]~a" (and *history* (equal id (history-id *history*))) title)
                                     :xalign 0.0 :ellipsize :end :max-width-chars 50)
                          (gtk:label :label (format nil "~a  ·  ~d entries" (format-history-time (getf summary :updated))
                                                    (getf summary :entry-count))
                                     :xalign 0.0 :css-classes '("caption" "dim-label"))))
                title (lambda () (open-history id))))))))

(defun make-history-button ()
  (let* ((button (make-instance 'gtk:menu-button :label "History"
                                :tooltip-text "Start a new conversation, or carry on an earlier one in this folder"))
         (popover (make-instance 'gtk:popover))
         (box (make-instance 'gtk:box :orientation :vertical :spacing 2
                             :margin-start 4 :margin-end 4 :margin-top 4 :margin-bottom 4)))
    (gtk:popover-set-child popover (make-instance 'gtk:scrolled-window :child box :hscrollbar-policy :never
                                                  :max-content-height 480 :propagate-natural-height t))
    ;; Read the saved conversations when the menu opens, not before.
    (gobject:connect popover :show (lambda (p) (declare (ignore p)) (fill-history-list box popover)))
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

(defun clear-checklist ()
  "Reset the checklist when changing conversations or folders."
  (setf *checklist-boxes* nil)
  (when *checklist*
    (clear-box (pane-messages *checklist*))
    (pane-note *checklist* "Review findings will appear here after a review." "dim-label")))

(defun show-checklist (text)
  "Update this loop's checklist in place, including when replaying history."
  (when *checklist*
    ;; Only the bold heading identifies the loop. Counts after its closing
    ;; ** change between reviews and must not become part of the key.
    (let* ((heading-end (search "**" text :start2 2))
           (heading (subseq text 0 (if heading-end (+ heading-end 2)
                                      (position #\Newline text))))
           (box (cdr (assoc heading *checklist-boxes* :test #'string=))))
      (if box
          (progn (clear-box box) (render-markdown box text)
                 (pane-scroll-to-end *checklist*))
          (progn
            ;; Clear the placeholder only when the first loop arrives.
            (unless *checklist-boxes* (clear-box (pane-messages *checklist*)))
            (setf box (markdown-box text "duet-checklist"))
            (push (cons heading box) *checklist-boxes*)
            (pane-append *checklist* box))))
    (unless (gtk:widget-get-visible (control :checklist-panel))
      (pane-note *middle* "Review checklist updated. Open Checklist to see the findings."
                 "dim-label" "duet-note"))))

(defun set-checklist-visible (visible)
  "Show the checklist temporarily, restoring the conversation dividers on close."
  (let ((panel (control :checklist-panel))
        (content (control :checklist-split))
        (visible (if visible t nil)))
    (when (and panel content
               (not (eql visible (gtk:widget-get-visible panel))))
      (let* ((width (gtk:widget-get-width content))
             (agent-split (control :agent-split))
             (conversation-split (control :conversation-split))
             (saved (control :pre-checklist-positions))
             ;; Keep the user's checklist width when reopening the panel.
             (panel-width (if (gtk:widget-get-visible panel)
                              (max 260 (gtk:widget-get-width panel))
                              (- width (gtk:paned-get-position content))))
             (available (if visible (- width (max 260 panel-width)) width)))
        (when visible
          (setf (getf *controls* :pre-checklist-positions)
                (list (gtk:paned-get-position agent-split)
                      (gtk:paned-get-position conversation-split))))
        ;; Explicit positions must survive the allocation caused by this toggle.
        ;; Restore proportional resizing after that frame's layout and paint,
        ;; so window resizing and manual checklist drags still share the width.
        (let ((clock (gtk:widget-get-frame-clock content))
              (handler nil))
          (when clock
            (dolist (split (list agent-split conversation-split))
              (gtk:paned-set-resize-start-child split nil))
            (setf handler
                  (gobject:connect clock :after-paint
                    (lambda (clock)
                      (gobject:disconnect clock handler)
                      (dolist (split (list agent-split conversation-split))
                        (gtk:paned-set-resize-start-child split t)))))))
        (gtk:widget-set-visible panel visible)
        (when (plusp available)
          (when visible (gtk:paned-set-position content available))
          ;; A window that starts with the checklist open has no saved layout.
          (gtk:paned-set-position agent-split
                                  (if (and (not visible) saved)
                                      (first saved) (round (* available 0.30))))
          (gtk:paned-set-position conversation-split
                                  (if (and (not visible) saved)
                                      (second saved) (round (* available 0.42)))))
        (unless visible
          (remf *controls* :pre-checklist-positions))))))

(defun make-checklist-panel ()
  (multiple-value-bind (messages scroller) (make-message-list)
    (setf *checklist* (make-pane :messages messages :scroller scroller))
    (clear-checklist)
    (let ((panel (gtk:build
                  (gtk:box :orientation :vertical :width-request 260
                           (gtk:label :label "Review checklists" :xalign 0.0
                                      :margin-start 12 :margin-top 8 :margin-bottom 8
                                      :css-classes '("title-4"))
                           (gtk:separator)
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

(defun middle-bubble (side header text)
  "A message in the conversation, from SIDE: :user, :claude or :codex."
  (pane-append *middle*
               (gtk:build
                (gtk:box :orientation :vertical :spacing 4
                         :css-classes (list "duet-message" (ecase side
                                                             (:user "duet-from-user")
                                                             (:claude "duet-from-claude")
                                                             (:codex "duet-from-codex")))
                         :halign (ecase side (:user :center) (:claude :start) (:codex :end))
                         (gtk:label :label header :xalign 0.0 :css-classes '("caption-heading"))
                         (markdown-box text)))))

(defun middle-message (from to text kind)
  (cond
    ((eq kind :findings)
     ;; Keep the history format compatible with earlier conversations.
     (show-checklist text)
     (record :middle :text (format nil "Review checklist~%~%~a" text))
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
        (middle-bubble (if (eq from :user) :user (agent-id from)) header text)
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
    (t :review)))

(defun workflow-running-p ()
  (and *workflow* (eq (workflow-state *workflow*) :running)))

(defun update-controls ()
  (when *controls*
    (let ((running (workflow-running-p)))
      (gtk:button-set-label (control :send) (if running "Stop" "Start"))
      (gtk:widget-set-css-classes (control :send) (if running '("destructive-action") '("suggested-action")))
      (dolist (name '(:mode :lead :rounds :check :folder :permission))
        (gtk:widget-set-sensitive (control name) (not running)))
      ;; Only the modes that make a plan wait for one.
      (dolist (name '(:approve-plan :worktrees))
        (gtk:widget-set-sensitive (control name) (and (not running) (not (eq (selected-mode) :review)))))
      (gtk:label-set-text (pane-status *middle*)
                          (cond ((not running) "")
                                ((workflow-pending-plan *workflow*) "Waiting for you to approve the plan")
                                ((workflow-paused *workflow*)
                                 (format nil "Paused: ~a out of usage"
                                         (format nil "~{~a~^ and ~}"
                                                 (remove-duplicates (mapcar (lambda (s) (agent-name (step-agent s)))
                                                                            (workflow-paused *workflow*))
                                                                    :test #'string=))))
                                (t (format nil "Working · ~{~a~^, ~}"
                                           (or (loop for a in (list *claude* *codex*)
                                                     when (agent-busy a) collect (agent-name a))
                                               '("waiting")))))))))

;;; A step paused because its agent ran out of usage

(defvar *pause-timers* '() "(step . glib source id): steps set to resume when usage resets.")
(defvar *pause-cards* '() "(step . refresh function) for the pause cards still waiting on a choice.")

(defun clear-pause-timer (step)
  (let ((entry (assoc step *pause-timers*)))
    (when entry
      (ignore-errors (glib:source-remove (cdr entry)))
      (setf *pause-timers* (remove entry *pause-timers*)))))

(defun clear-pause-timers ()
  (dolist (entry *pause-timers*) (ignore-errors (glib:source-remove (cdr entry))))
  (setf *pause-timers* '()
        *pause-cards* '()))

(defun resume-when-reset (workflow step reset)
  "Give STEP to its agent again a minute after RESET (a universal time)."
  (let ((seconds (max 5 (+ 60 (- reset (get-universal-time))))))
    (clear-pause-timer step)
    (push (cons step (glib:timeout-add-seconds glib:+priority-default+ seconds
                                               (lambda ()
                                                 (setf *pause-timers* (remove step *pause-timers* :key #'car))
                                                 (resume-step workflow step)
                                                 (update-controls)
                                                 nil)))
          *pause-timers*)))

(defun refresh-pause-cards (agent)
  "AGENT's usage changed: its cards may now know when it resets."
  (dolist (entry *pause-cards*)
    (when (eq (step-agent (car entry)) agent)
      (funcall (cdr entry)))))

(defun show-paused-step (workflow step)
  "A card in the conversation: the step that paused, and what to do about it.
It follows the agent's usage: a reset time learned later (Codex reads its
usage after the turn) brings the Wait button."
  (let* ((agent (step-agent step))
         (other (other-agent workflow agent))
         (heading (make-instance 'gtk:label :xalign 0.0 :css-classes '("heading")))
         (buttons (make-instance 'gtk:box :spacing 6 :halign :end))
         (status (make-instance 'gtk:label :xalign 0.0 :wrap t :css-classes '("dim-label") :visible nil))
         (wait-button nil)
         (waiting-for nil))                     ; the reset being waited for, once Wait is chosen
    (attention (format nil "~a is out of usage" (agent-name agent))
               (format nil "~a is paused." (string-capitalize (step-description step) :end 1)))
    (labels ((reset () (and (integerp (agent-out-until agent)) (agent-out-until agent)))
             (settled (text)
               (setf *pause-cards* (remove step *pause-cards* :key #'car))
               (clear-pause-timer step)
               (clear-box buttons)
               (gtk:box-append buttons (make-instance 'gtk:label :label text :css-classes '("dim-label")))
               (gtk:widget-set-visible status nil)
               (update-controls))
             (choice (label tooltip classes action)
               (let ((b (make-instance 'gtk:button :label label :tooltip-text tooltip :css-classes classes)))
                 (gobject:connect b :clicked (lambda (b) (declare (ignore b)) (funcall action)))
                 (gtk:box-append buttons b)
                 b))
             (wait ()
               (let ((reset (reset)))
                 (when reset
                   (setf waiting-for reset)
                   (resume-when-reset workflow step reset)
                   (gtk:label-set-text status (format nil "Waiting: ~a will carry on at about ~a. Leave the app open."
                                                      (agent-name agent) (format-reset-time (+ reset 60))))
                   (gtk:widget-set-visible status t))))
             (refresh ()
               (let ((reset (reset)))
                 (gtk:label-set-text heading (format nil "~a is out of usage~@[ until ~a~]" (agent-name agent)
                                                     (and reset (format-reset-time reset))))
                 (when reset
                   (unless wait-button
                     (setf wait-button (choice "" (format nil "Send it to ~a again once its usage resets" (agent-name agent))
                                               '("suggested-action") #'wait)))
                   (gtk:button-set-label wait-button (format nil "Wait until ~a" (format-reset-time reset)))
                   ;; Already waiting, for a reset that has moved: wait for the new one.
                   (when (and waiting-for (/= waiting-for reset))
                     (wait))))))
      (choice "Stop" "Stop the whole task here" '("destructive-action")
              (lambda () (stop-workflow workflow) (settled "Stopped")))
      (let ((hand-off (choice (case (step-role step)
                                (:review "Finish unreviewed")
                                (:propose "Go without it")
                                (t (format nil "Let ~a take over" (agent-name other))))
                              (case (step-role step)
                                (:review "Finish this part without a review, and say so")
                                (:propose (format nil "Carry on with ~a's proposal alone" (agent-name other)))
                                (t (format nil "~a carries on from where ~a stopped; its work won't be reviewed"
                                           (agent-name other) (agent-name agent))))
                              '()
                              (lambda () (settled (case (step-role step)
                                                    (:review "Finished unreviewed")
                                                    (:propose "Went without it")
                                                    (t (format nil "Handed to ~a" (agent-name other)))))
                                (hand-off-step workflow step)))))
        ;; Only taking work over needs the other agent's usage.
        (unless (or (member (step-role step) '(:review :propose)) (can-hand-off-p workflow step))
          (gtk:widget-set-sensitive hand-off nil)
          (gtk:widget-set-tooltip-text hand-off (format nil "~a is out of usage too" (agent-name other)))))
      (choice "Try again" (format nil "Send it to ~a again now" (agent-name agent)) '()
              (lambda () (settled "Tried again") (resume-step workflow step)))
      (refresh)
      (push (cons step #'refresh) *pause-cards*)
      (pane-append *middle*
                   (gtk:build
                    (gtk:box :orientation :vertical :spacing 6 :css-classes '("duet-approval")
                             heading
                             (gtk:label :label (format nil "~a is paused. Nothing done so far is lost."
                                                       (string-capitalize (step-description step) :end 1))
                                        :xalign 0.0 :wrap t)
                             status
                             buttons))))
    (update-controls)))

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
         (pane (if workflow *middle* (agent-pane-of agent)))
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

(defun snapshot-task-start (workflow)
  "Record the folder as WORKFLOW starts; first, the last task's end, if
that's still waiting (its agents have stopped by now)."
  (when *task-changes* (finish-task-end *task-changes*))
  (multiple-value-bind (task why) (begin-task-changes workflow *directory*)
    (setf *task-changes* task)
    (when why (snapshot-failed why))))

(defun finish-task-end (task)
  "Record TASK's end if it's time; true while it's still waiting."
  (multiple-value-bind (result why) (finish-task-changes task)
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

(defun show-task-changes ()
  "The Changes window for the latest task: from its start to its end, or
to now while it runs (or is still stopping)."
  (let ((task *task-changes*))
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
               (multiple-value-bind (now why) (task-snapshot task (format nil "now-~a" (subseq (make-uuid) 0 8)))
                 (if (null now)
                     (snapshot-failed (or why "the folder is no longer a git repository with a commit"))
                     (unwind-protect
                          (show-changes-window directory start now
                                               :title (format nil "Changes so far · ~a" (folder-label directory)))
                       ;; The window has read what it shows.
                       (ignore-errors (funcall *task-snapshot-delete* directory now))))))))))))

;;; A workflow stopped at its last round, unapproved

(defun workflow-finished (workflow)
  (clear-pause-timers)
  (snapshot-task-end workflow)
  (when (eq (workflow-state workflow) :finished)
    (attention "Both agents have finished"
               (format nil "~{~a~^; ~}"
                       (or (loop for l in (workflow-loops workflow)
                                 collect (format nil "~@[~a: ~]~a" (loop-label l) (loop-outcome l)))
                           '("It stopped before any work was done")))))
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
            (partner-view (part partner-task)))
        (labels ((settled (text)
                   (clear-box buttons)
                   (gtk:box-append buttons (make-instance 'gtk:label :label text :css-classes '("dim-label")))
                   (gtk:widget-set-sensitive lead-view nil)
                   (gtk:widget-set-sensitive partner-view nil)
                   (update-controls))
                 (start ()
                   (cond ((not (workflow-live-p workflow)) (settled "Stopped"))
                         ((approve-plan workflow (text-view-text lead-view) (text-view-text partner-view))
                          (settled "Started"))
                         (t (pane-note *middle* "Give at least one of them a part." "error"))))
                 (button (label classes tooltip action)
                   (let ((b (make-instance 'gtk:button :label label :css-classes classes :tooltip-text tooltip)))
                     (gobject:connect b :clicked (lambda (b) (declare (ignore b)) (funcall action)))
                     (gtk:box-append buttons b))))
          (button "Stop" '("destructive-action") "Stop the whole task here"
                  (lambda () (stop-workflow workflow) (settled "Stopped")))
          (button "Start" '("suggested-action") "Start the work on the plan, as it is here" #'start)
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

(defun start-request ()
  (if (workflow-running-p)
      (stop-workflow *workflow*)
      (let ((text (string-trim '(#\Space #\Newline #\Tab) (text-view-text (control :input))))
            (problem nil))
        (when (plusp (length text))
          (cond
            ((or (agent-busy *claude*) (agent-busy *codex*))
             (pane-note *middle* "Wait for both agents to finish, or stop them, before starting." "error"))
            ((and (worktrees-wanted-p) (setf problem (worktrees-problem *directory*)))
             (pane-note *middle* (format nil "Can't give each part its own worktree: ~a." problem) "error"))
            (t
             (let* ((lead (selected-lead))
                    (partner (if (eq lead *claude*) *codex* *claude*)))
               (clear-text-view (control :input))
               (setf *workflow* (make-instance 'workflow
                                               :mode (selected-mode) :lead lead :partner partner
                                               :request text
                                               :max-rounds (gtk:spin-button-get-value-as-int (control :rounds))
                                               :on-message #'middle-message
                                               :on-pause #'show-paused-step
                                               :approve-plan (and (not (eq (selected-mode) :review))
                                                                  (gtk:check-button-get-active (control :approve-plan)))
                                               :on-plan #'show-plan-card
                                               :worktrees (worktrees-wanted-p)
                                               :check-command (folder-check-command *directory*)
                                               :on-finish #'workflow-finished))
               (snapshot-task-start *workflow*)
               (start-workflow *workflow*)
               (update-controls))))))))

(defun worktrees-wanted-p ()
  (and (not (eq (selected-mode) :review)) (gtk:check-button-get-active (control :worktrees))))

(defun make-middle-pane ()
  (multiple-value-bind (messages scroller) (make-message-list)
    (let* ((status (make-instance 'gtk:label :xalign 1.0 :hexpand t :ellipsize :end :css-classes '("dim-label")))
           (input (make-input))
           (send (make-instance 'gtk:button :label "Start" :valign :end :css-classes '("suggested-action")))
           (mode (gtk:drop-down-new-from-strings '("Write & review" "Split the work" "Compare proposals")))
           (approve-plan (make-instance 'gtk:check-button :label "Approve plan" :active (setting :approve-plan nil)))
           (worktrees (make-instance 'gtk:check-button :label "Own worktrees" :active (setting :worktrees nil)))
           (lead (gtk:drop-down-new-from-strings '("Claude leads" "ChatGPT leads")))
           (rounds (gtk:spin-button-new-with-range 1d0 10d0 1d0))
           (check (make-instance 'gtk:entry :placeholder-text "Check command" :width-chars 14 :hexpand t)))
      (setf *middle* (make-pane :messages messages :scroller scroller :status status :input input :send send))
      (gtk:spin-button-set-value rounds (coerce *default-max-rounds* 'double-float))
      (gtk:widget-set-tooltip-text mode "Write & review: the lead does the work and the other reviews it, back and forth until approved. Split the work: the lead divides the task, both work at once, then each reviews the other's part. Compare proposals: both propose an approach on their own, the lead reconciles them into a plan, and then the work is split.")
      (gtk:widget-set-tooltip-text approve-plan "Wait for you to look over (and edit) the plan before any work starts")
      (gobject:connect mode "notify::selected" (lambda (d pspec) (declare (ignore d pspec)) (update-controls)))
      (gobject:connect approve-plan :toggled
                       (lambda (b) (setf (setting :approve-plan) (gtk:check-button-get-active b))))
      (gtk:widget-set-tooltip-text worktrees "Each agent does its part in its own git worktree, in a new conversation; then the parts are committed, merged into the current branch, and the merged result reviewed. Needs a git repository with nothing uncommitted.")
      (gobject:connect worktrees :toggled
                       (lambda (b) (setf (setting :worktrees) (gtk:check-button-get-active b))))
      (gtk:widget-set-tooltip-text lead "Who does the work (or splits it)")
      (gtk:widget-set-tooltip-text rounds "Most review rounds before stopping")
      (gtk:widget-set-tooltip-text check "A shell command run in the folder after each round of work, such as `make test`. If it fails, the author gets its output to fix before the review; the reviewer is told how it went. Kept for this folder; leave it empty for none.")
      (gobject:connect check :changed
                       (lambda (e) (when *directory*
                                     (setf (folder-check-command *directory*) (gtk:editable-get-text e)))))
      (gtk:widget-set-tooltip-text input "Drop files here to put their paths in the request")
      (on-file-drop input (lambda (paths) (insert-paths input paths)))
      (setf *controls* (list* :input input :send send :mode mode :lead lead :rounds rounds :check check
                              :approve-plan approve-plan :worktrees worktrees *controls*))
      (gobject:connect send :clicked (lambda (b) (declare (ignore b)) (start-request)))
      (on-return input (lambda () (unless (workflow-running-p) (start-request))))
      (pane-note *middle* (format nil "Describe what you want done. The agents pass work and reviews back and forth here; each side shows that agent's own work in full.~%~%Return starts; Shift+Return starts a new line."))
      (gtk:build
       (gtk:box :orientation :vertical :css-classes '("duet-pane" "duet-middle")
                (gtk:box :spacing 8 :margin-start 12 :margin-end 12 :margin-top 6 :margin-bottom 6
                         (gtk:label :label "Conversation" :css-classes '("heading"))
                         status)
                (gtk:separator)
                scroller
                (gtk:separator)
                (wrapping-controls
                 mode lead
                 (gtk:build
                  (gtk:box :spacing 6
                           (gtk:label :label "Rounds" :css-classes '("dim-label")) rounds))
                 approve-plan worktrees
                 (gtk:build
                  (gtk:box :spacing 6
                           (gtk:label :label "Check" :css-classes '("dim-label")) check)))
                (composer input send))))))

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
      (documents-folder-changed))))

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

(defun make-header ()
  (let* ((header (make-instance 'gtk:header-bar))
         (folder-label (make-instance 'gtk:label :ellipsize :start :max-width-chars 50))
         (folder (make-instance 'gtk:menu-button
                                :child (gtk:build
                                        (gtk:box :spacing 6
                                                 (gtk:label :label "Folder:" :css-classes '("dim-label"))
                                                 folder-label
                                                 (gtk:label :label "▾" :css-classes '("dim-label"))))))
         (permission (gtk:drop-down-new-from-strings (mapcar #'second *permission-choices*)))
         (theme (gtk:drop-down-new-from-strings (mapcar #'second *themes*))))
    (gtk:drop-down-set-selected permission (or (position *permission* *permission-choices* :key #'first) 0))
    (gtk:widget-set-tooltip-text permission
                                 (format nil "~{~a~^~%~%~}"
                                         (mapcar (lambda (c) (format nil "~a: ~a" (second c) (third c)))
                                                 *permission-choices*)))
    (gobject:connect permission "notify::selected"
                     (lambda (d pspec) (declare (ignore pspec))
                       (set-permission (first (nth (gtk:drop-down-get-selected d) *permission-choices*)))))
    (gtk:drop-down-set-selected theme (or (position (setting :theme :dark) *themes* :key #'first) 0))
    (gtk:widget-set-tooltip-text theme "Light or dark")
    (gobject:connect theme "notify::selected"
                     (lambda (d pspec) (declare (ignore pspec))
                       (apply-theme (first (nth (gtk:drop-down-get-selected d) *themes*)))))
    (setf *controls* (list* :folder folder :folder-label folder-label :permission permission *controls*))
    (let ((notes (make-instance 'gtk:check-button :label "Shared notes" :active *shared-notes*
                                :tooltip-text "Give Claude the folder's AGENTS.md, and ChatGPT its CLAUDE.md, with their instructions (each reads its own already), so both follow the project's conventions. Read again in a new conversation.")))
      (gobject:connect notes :toggled
                       (lambda (b) (setf *shared-notes* (gtk:check-button-get-active b)
                                         (setting :shared-notes) *shared-notes*)))
      (gtk:header-bar-pack-end header notes))
    (gtk:header-bar-pack-start header folder)
    (let ((terminal (make-instance 'gtk:toggle-button :label "Terminal"
                                   :tooltip-text "Show or hide the terminal panel (Ctrl+`)"))
          (documents (make-instance 'gtk:button :label "Documents"
                                    :tooltip-text "Preview the folder's Markdown files"))
          (changes (make-instance 'gtk:button :label "Changes"
                                  :tooltip-text "What the latest task changed in the folder, file by file (a git repository's)")))
      (gobject:connect terminal :toggled
                       (lambda (b)
                         (let ((active (gtk:toggle-button-get-active b)))
                           (unless (eq active (terminal-visible-p))
                             (set-terminal-visible active)))))
      (gobject:connect documents :clicked (lambda (b) (declare (ignore b)) (show-documents)))
      (gobject:connect changes :clicked (lambda (b) (declare (ignore b)) (show-task-changes)))
      (setf *controls* (list* :terminal terminal *controls*))
      (gtk:header-bar-pack-start header terminal)
      (gtk:header-bar-pack-start header documents)
      (gtk:header-bar-pack-start header changes)
      (gtk:header-bar-pack-start header (make-history-button)))
    (let ((checklist (make-instance 'gtk:toggle-button :label "Checklist"
                                    :active (setting :checklist-visible t)
                                    :tooltip-text "Show or hide review checklists on the right")))
      (gobject:connect checklist :toggled
                       (lambda (b)
                         (let ((visible (gtk:toggle-button-get-active b)))
                           (setf (setting :checklist-visible) visible)
                           (set-checklist-visible visible))))
      (gtk:header-bar-pack-start header checklist))
    (gtk:header-bar-pack-end header theme)
    (gtk:header-bar-pack-end header permission)
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
  '((".duet-middle" :background-color "alpha(@view_fg_color, 0.02)")
    (".duet-pane" :background-color "@view_bg_color")
    (".duet-prompt" :background-color "alpha(@accent_bg_color, 0.18)" :border-radius "8px" :padding ("6px" "10px"))
    (".duet-code" :background-color "alpha(@view_fg_color, 0.07)" :border-radius "6px" :padding "6px")
    (".duet-tool" :opacity "0.8")
    (".duet-approval" :background-color "alpha(@warning_bg_color, 0.22)" :border-radius "8px" :padding "10px"
     :border "1px solid alpha(@warning_color, 0.5)")
    (".duet-message" :border-radius "10px" :padding ("8px" "12px"))
    (".duet-from-user" :background-color "alpha(@accent_bg_color, 0.18)")
    (".duet-from-claude" :background-color "alpha(#d97757, 0.16)" :margin-right "40px")
    (".duet-from-codex" :background-color "alpha(#10a37f, 0.16)" :margin-left "40px")
    (".duet-claude" :color "#e08a6a")
    (".duet-codex" :color "#19b98f")
    (".duet-note" :font-size "smaller")
    (".duet-chip" :background-color "alpha(@view_fg_color, 0.08)" :border-radius "12px")
    (".duet-checklist" :background-color "alpha(@view_fg_color, 0.05)" :border-radius "8px" :padding ("8px" "12px"))
    (".duet-terminal-panel" :background-color "@view_bg_color")
    (".duet-terminal" :padding ("2px" "6px") :font-family "Menlo, Monaco, monospace")))

(defun setup-window-keys (window)
  "Handle window shortcuts wherever the focus is, including the Terminal."
  (let ((keys (gtk:event-controller-key-new)))
    (gtk:event-controller-set-propagation-phase keys :capture)
    (gobject:connect keys :key-pressed
                     (lambda (c keyval keycode state)
                       (declare (ignore c keycode))
                       (let ((key (gdk:keyval-name keyval))
                             (modifiers (modifier-list state)))
                         (cond
                          ((and (string= key "q")
                                (member :meta-mask modifiers)
                                (not (intersection '(:control-mask :alt-mask :shift-mask) modifiers)))
                           ;; Closing the window runs the normal cleanup for
                           ;; agents, terminals and document previews before quit.
                           (gtk:window-close window)
                           t)
                          ((and (member key '("grave" "dead_grave") :test #'string=)
                                (member :control-mask modifiers))
                           (toggle-terminal)
                           t)))))
    (gtk:widget-add-controller window keys)))

(defun apply-settings-to (agent)
  (setf (agent-permission agent) *permission*)
  (let ((model (setting (agent-setting-key agent "MODEL")))
        (effort (setting (agent-setting-key agent "EFFORT"))))
    (when model (setf (agent-model agent) model))
    (setf (agent-effort agent) effort)))

(defun activate (app)
  "Open the main window."
  (gtk:add-css *css*)
  (apply-theme (setting :theme :dark))
  (setf *controls* '()
        *permission* (or (setting :permission) :ask)
        *shared-notes* (setting :shared-notes t))
  (let ((window (make-instance 'gtk:application-window
                               :application app :title "Agent Duet"
                               :default-width 1440 :default-height 900)))
    (setf *window* window
          *claude* (make-instance 'claude-agent :deliver #'deliver)
          *codex* (make-instance 'codex-agent :deliver #'deliver))
    (apply-settings-to *claude*)
    (apply-settings-to *codex*)
    (gtk:window-set-titlebar window (make-header))
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
                  (getf *controls* :conversation-split) inner)
            (gtk:paned-set-start-child content outer)
            (gtk:paned-set-end-child content (make-checklist-panel))
            ;; The Terminal panel below the panes, hidden until asked for.
            (let ((vertical (make-instance 'gtk:paned :orientation :vertical :position 560
                                           :shrink-start-child nil :shrink-end-child nil
                                           :resize-end-child nil)))
              (gtk:paned-set-start-child vertical content)
              (gtk:paned-set-end-child vertical (make-terminal-panel))
              (gtk:window-set-child window vertical))))))
    (setup-window-keys window)
    (let ((directory (or *directory*
                         (let ((saved (setting :directory)))
                           (and saved (uiop:directory-exists-p saved)))
                         ;; Started from the Finder, the current directory is /.
                         (let ((cwd (uiop:getcwd)))
                           (if (equal (namestring cwd) "/") (user-homedir-pathname) cwd)))))
      (setf *directory* nil)
      (set-directory directory))
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
                       (dolist (agent (list *claude* *codex*))
                         (ignore-errors (agent-cancel agent))
                         (ignore-errors (agent-shutdown agent)))
                       (ignore-errors (finish-history))
                       (ignore-errors (kill-all-terminals))
                       (ignore-errors (close-documents))
                       nil))
    (gtk:window-present window)))

(defun main (&optional directory)
  "Run the application; returns when its window closes. DIRECTORY is where
the agents work (by default, the last folder used, or else the current
directory). On macOS it must run on the first thread: in Cadre, use Run GTK App."
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
