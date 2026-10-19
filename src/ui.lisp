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
(defvar *workflow* nil "The running or last workflow.")
(defvar *controls* nil "A plist of the middle pane's widgets.")
(defvar *directory* nil "Where the agents work.")
(defvar *permission* :ask)

(defun deliver (thunk)
  (glib:call-in-main-thread thunk))

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
    (gobject:connect (gtk:scrolled-window-get-vadjustment scroller) :changed
                     (lambda (adjustment)
                       (gtk:adjustment-set-value adjustment
                                                 (max 0d0 (- (gtk:adjustment-get-upper adjustment)
                                                             (gtk:adjustment-get-page-size adjustment))))))
    (values messages scroller)))

(defun pane-scroll-to-end (pane)
  (unless (pane-scroll-pending pane)
    (setf (pane-scroll-pending pane) t)
    (glib:idle-add glib:+priority-default-idle+
                   (lambda ()
                     (setf (pane-scroll-pending pane) nil)
                     (let ((adj (gtk:scrolled-window-get-vadjustment (pane-scroller pane))))
                       (gtk:adjustment-set-value adj (- (gtk:adjustment-get-upper adj)
                                                        (gtk:adjustment-get-page-size adj))))
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

;;; An agent's pane

(defstruct (agent-pane (:include pane) (:constructor new-agent-pane))
  agent stop
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
           (pane (new-agent-pane :agent agent :stop stop :messages messages :scroller scroller
                                 :status status :input input :send send)))
      (flet ((send-it ()
               (let ((text (string-trim '(#\Space #\Newline #\Tab) (text-view-text input))))
                 (when (plusp (length text))
                   (handler-case
                       (progn (agent-send-now agent text) (clear-text-view input))
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
                          (gtk:box :spacing 6 :margin-start 10 :margin-end 10 :margin-top 4 :margin-bottom 6
                                   models efforts)
                          (gtk:separator)
                          scroller
                          (gtk:separator)
                          (composer input
                                    (gtk:build (gtk:box :orientation :vertical :spacing 6
                                                       :valign :end send stop))))))))))

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

(defun new-conversation (agent)
  (agent-cancel agent)
  (agent-new-conversation agent)
  (let ((pane (agent-pane-of agent)))
    (clear-box (pane-messages pane))
    (clrhash (agent-pane-tools pane))
    (clrhash (agent-pane-approvals pane))
    (pane-finish-stream pane)
    (pane-note pane "New conversation.")
    (pane-update-status pane)))

;;; The conversation between them

(defun participant-name (who)
  (case who
    (:user "You")
    (:system nil)
    (t (agent-name who))))

(defun kind-label (kind)
  (case kind
    (:request "request")
    (:plan "plan")
    (:work "work done")
    (:review "review")
    (t nil)))

(defun middle-message (from to text kind)
  (if (eq from :system)
      (pane-note *middle* text "dim-label" "duet-note")
      (let* ((header (format nil "~a → ~a~@[  ·  ~a~]" (participant-name from) (participant-name to)
                             (let ((label (kind-label kind)))
                               (if (eq kind :review)
                                   (format nil "~a: ~(~a~)" label
                                           (if (eq (review-verdict text) :approved) "approved" "changes requested"))
                                   label))))
             (classes (list "duet-message"
                            (cond ((eq from :user) "duet-from-user")
                                  ((eq (agent-id from) :claude) "duet-from-claude")
                                  (t "duet-from-codex")))))
        (pane-append *middle*
                     (gtk:build
                      (gtk:box :orientation :vertical :spacing 4 :css-classes classes
                               :halign (cond ((eq from :user) :center)
                                             ((eq (agent-id from) :claude) :start)
                                             (t :end))
                               (gtk:label :label header :xalign 0.0 :css-classes '("caption-heading"))
                               (markdown-box text)))))))

(defun control (name) (getf *controls* name))

(defun selected-lead ()
  (if (= 1 (gtk:drop-down-get-selected (control :lead))) *codex* *claude*))

(defun selected-mode ()
  (if (= 1 (gtk:drop-down-get-selected (control :mode))) :divide :review))

(defun workflow-running-p ()
  (and *workflow* (eq (workflow-state *workflow*) :running)))

(defun update-controls ()
  (when *controls*
    (let ((running (workflow-running-p)))
      (gtk:button-set-label (control :send) (if running "Stop" "Start"))
      (gtk:widget-set-css-classes (control :send) (if running '("destructive-action") '("suggested-action")))
      (dolist (name '(:mode :lead :rounds :folder :permission))
        (gtk:widget-set-sensitive (control name) (not running)))
      (gtk:label-set-text (pane-status *middle*)
                          (cond ((not running) "")
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
      (let ((hand-off (choice (if (eq (step-role step) :review)
                                  "Finish unreviewed"
                                  (format nil "Let ~a take over" (agent-name other)))
                              (if (eq (step-role step) :review)
                                  "Finish this part without a review, and say so"
                                  (format nil "~a carries on from where ~a stopped; its work won't be reviewed"
                                          (agent-name other) (agent-name agent)))
                              '()
                              (lambda () (settled (if (eq (step-role step) :review)
                                                      "Finished unreviewed"
                                                      (format nil "Handed to ~a" (agent-name other))))
                                (hand-off-step workflow step)))))
        (unless (or (eq (step-role step) :review) (can-hand-off-p workflow step))
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

(defun start-request ()
  (if (workflow-running-p)
      (stop-workflow *workflow*)
      (let ((text (string-trim '(#\Space #\Newline #\Tab) (text-view-text (control :input)))))
        (when (plusp (length text))
          (if (or (agent-busy *claude*) (agent-busy *codex*))
              (pane-note *middle* "Wait for both agents to finish, or stop them, before starting." "error")
              (let* ((lead (selected-lead))
                     (partner (if (eq lead *claude*) *codex* *claude*)))
                (clear-text-view (control :input))
                (setf *workflow* (make-instance 'workflow
                                                :mode (selected-mode) :lead lead :partner partner
                                                :request text
                                                :max-rounds (gtk:spin-button-get-value-as-int (control :rounds))
                                                :on-message #'middle-message
                                                :on-pause #'show-paused-step
                                                :on-finish (lambda (w) (declare (ignore w))
                                                             (clear-pause-timers)
                                                             (update-controls))))
                (start-workflow *workflow*)
                (update-controls)))))))

(defun make-middle-pane ()
  (multiple-value-bind (messages scroller) (make-message-list)
    (let* ((status (make-instance 'gtk:label :xalign 1.0 :hexpand t :ellipsize :end :css-classes '("dim-label")))
           (input (make-input))
           (send (make-instance 'gtk:button :label "Start" :valign :end :css-classes '("suggested-action")))
           (mode (gtk:drop-down-new-from-strings '("Write & review" "Split the work")))
           (lead (gtk:drop-down-new-from-strings '("Claude leads" "ChatGPT leads")))
           (rounds (gtk:spin-button-new-with-range 1d0 10d0 1d0)))
      (setf *middle* (make-pane :messages messages :scroller scroller :status status :input input :send send))
      (gtk:spin-button-set-value rounds (coerce *default-max-rounds* 'double-float))
      (gtk:widget-set-tooltip-text mode "Write & review: the lead does the work and the other reviews it, back and forth until approved. Split the work: the lead divides the task, both work at once, then each reviews the other's part.")
      (gtk:widget-set-tooltip-text lead "Who does the work (or splits it)")
      (gtk:widget-set-tooltip-text rounds "Most review rounds before stopping")
      (setf *controls* (list* :input input :send send :mode mode :lead lead :rounds rounds *controls*))
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
                (gtk:box :spacing 6 :margin-start 8 :margin-end 8 :margin-top 6
                         mode lead
                         (gtk:label :label "Rounds" :css-classes '("dim-label") :margin-start 6)
                         rounds)
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
      (setf *directory* directory)
      (note-recent-folder directory)
      (setf (setting :directory) (namestring directory))
      (dolist (agent (list *claude* *codex*))
        (when agent
          (agent-cancel agent)
          (agent-new-conversation agent)
          (setf (agent-directory agent) directory)
          (unless first-time
            (pane-note (agent-pane-of agent) (format nil "Now working in ~a: a new conversation." (namestring directory))))))
      (when *middle*
        (pane-note *middle* (format nil "Working in ~a" (namestring directory)) "dim-label" "duet-note"))
      (refresh-folder-menu)
      (documents-folder-changed))))

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
    (gtk:header-bar-pack-start header folder)
    (let ((terminal (make-instance 'gtk:toggle-button :label "Terminal"
                                   :tooltip-text "Show or hide the terminal panel (Ctrl+`)"))
          (documents (make-instance 'gtk:button :label "Documents"
                                    :tooltip-text "Preview the folder's Markdown files")))
      (gobject:connect terminal :toggled
                       (lambda (b)
                         (let ((active (gtk:toggle-button-get-active b)))
                           (unless (eq active (terminal-visible-p))
                             (set-terminal-visible active)))))
      (gobject:connect documents :clicked (lambda (b) (declare (ignore b)) (show-documents)))
      (setf *controls* (list* :terminal terminal *controls*))
      (gtk:header-bar-pack-start header terminal)
      (gtk:header-bar-pack-start header documents))
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
        *permission* (or (setting :permission) :ask))
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
        (let* ((middle (make-middle-pane))
               (inner (make-instance 'gtk:paned :orientation :horizontal :position 600
                                     :shrink-start-child nil :shrink-end-child nil))
               (outer (make-instance 'gtk:paned :orientation :horizontal :position 420
                                     :shrink-start-child nil :shrink-end-child nil)))
          (gtk:paned-set-start-child inner middle)
          (gtk:paned-set-end-child inner codex-widget)
          (gtk:paned-set-start-child outer claude-widget)
          (gtk:paned-set-end-child outer inner)
          ;; The Terminal panel below the three panes, hidden until asked for.
          (let ((vertical (make-instance 'gtk:paned :orientation :vertical :position 560
                                         :shrink-start-child nil :shrink-end-child nil
                                         :resize-end-child nil)))
            (gtk:paned-set-start-child vertical outer)
            (gtk:paned-set-end-child vertical (make-terminal-panel))
            (gtk:window-set-child window vertical)))))
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
    (check-agents)
    ;; Usage resets with nothing else happening: keep the status lines current.
    (glib:timeout-add-seconds glib:+priority-default+ 30
                              (lambda ()
                                (dolist (entry *panes*) (pane-update-status (cdr entry)))
                                t))
    (gobject:connect window :close-request
                     (lambda (w) (declare (ignore w))
                       (dolist (agent (list *claude* *codex*))
                         (ignore-errors (agent-cancel agent))
                         (ignore-errors (agent-shutdown agent)))
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
