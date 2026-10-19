;;;; ui.lisp — three panes: Claude, the conversation between them, ChatGPT
;;;;
;;;; The side panes show everything each agent does: the prompts it gets, its
;;;; replies as they stream, its tool calls. Each has its own box for talking
;;;; to that agent alone. The middle pane is the conversation between them:
;;;; the user's request and each finished reply passed from one to the other.
;;;; Its box starts a workflow (workflow.lisp) with the chosen mode and lead.

(in-package #:agent-integration)

(defvar *window* nil)
(defvar *claude* nil)
(defvar *codex* nil)
(defvar *panes* '() "Agent id → AGENT-PANE.")
(defvar *middle* nil "The conversation's PANE.")
(defvar *workflow* nil "The running or last workflow.")
(defvar *controls* nil "A plist of the middle pane's widgets.")
(defvar *directory* nil "Where the agents work.")
(defvar *permission* :edits)

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
  agent
  (stream-box nil) (stream-label nil) (stream-text "")
  (tools (make-hash-table :test 'equal)))

(defun agent-pane-of (agent) (cdr (assoc (agent-id agent) *panes*)))

(defun make-agent-pane (agent)
  (multiple-value-bind (messages scroller) (make-message-list)
    (let* ((status (make-instance 'gtk:label :xalign 0.0 :hexpand t :ellipsize :end :css-classes '("dim-label")
                                             :label "Checking…"))
           (input (make-input))
           (send (make-instance 'gtk:button :label "Send" :valign :end :css-classes '("suggested-action")
                                            :tooltip-text (format nil "Talk to ~a alone" (agent-name agent))))
           (pane (new-agent-pane :agent agent :messages messages :scroller scroller
                                  :status status :input input :send send)))
      (flet ((send-it ()
               (if (agent-busy agent)
                   (agent-cancel agent)
                   (let ((text (string-trim '(#\Space #\Newline #\Tab) (text-view-text input))))
                     (when (plusp (length text))
                       (clear-text-view input)
                       (agent-submit agent text))))))
        (gobject:connect send :clicked (lambda (b) (declare (ignore b)) (send-it)))
        (on-return input (lambda () (unless (agent-busy agent) (send-it)))))
      (let ((header (gtk:build
                      (gtk:box :spacing 8 :margin-start 12 :margin-end 6 :margin-top 6 :margin-bottom 6
                        (gtk:label :label (agent-name agent) :css-classes (list "heading" (agent-css-class agent)))
                        status
                        (gtk:button :icon-name "edit-clear-all-symbolic" :css-classes '("flat")
                                    :tooltip-text "New conversation: forget this one"
                                    :on-clicked (lambda (b) (declare (ignore b)) (new-conversation agent)))))))
        (when (eq (agent-id agent) :claude)
          (let ((models (gtk:drop-down-new-from-strings *claude-models*)))
            (gtk:drop-down-set-selected models (or (position (claude-agent-model agent) *claude-models*
                                                             :test #'equal)
                                                   0))
            (gtk:widget-set-tooltip-text models "Claude's model (from the next message)")
            (gobject:connect models "notify::selected"
                             (lambda (d pspec) (declare (ignore pspec))
                               (setf (claude-agent-model agent) (nth (gtk:drop-down-get-selected d) *claude-models*))
                               (unless (agent-busy agent) (agent-shutdown agent))))
            (gtk:box-insert-child-after header models (gtk:widget-get-first-child header))))
        (setf (agent-listener agent) #'agent-pane-event)
        (values pane
                (gtk:build
                  (gtk:box :orientation :vertical :css-classes '("duet-pane")
                    header
                    (gtk:separator)
                    scroller
                    (gtk:separator)
                    (composer input send))))))))

(defun agent-css-class (agent)
  (if (eq (agent-id agent) :claude) "duet-claude" "duet-codex"))

(defun pane-set-busy (pane busy)
  (let ((agent (agent-pane-agent pane)))
    (gtk:label-set-text (pane-status pane) (if busy "Working…" "Ready"))
    (gtk:button-set-label (pane-send pane) (if busy "Stop" "Send"))
    (gtk:widget-set-css-classes (pane-send pane) (if busy '("destructive-action") '("suggested-action")))
    (gtk:widget-set-tooltip-text (pane-send pane)
                                 (if busy (format nil "Stop ~a" (agent-name agent))
                                     (format nil "Talk to ~a alone" (agent-name agent))))))

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
  (pane-finish-stream pane)
  (pane-append pane (gtk:build
                      (gtk:box :halign :end :css-classes '("duet-prompt")
                        (gtk:label :label text :wrap t :xalign 0.0 :selectable t :wrap-mode :word-char)))))

(defun agent-pane-event (agent kind &key text title id error busy)
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
        (:note (pane-note pane text "error"))
        (:turn-done
         ;; A turn that streamed but never finished its block.
         (when (agent-pane-stream-box pane)
           (pane-text pane (agent-pane-stream-text pane)))
         (when error (pane-note pane (or text "The turn failed.") "error")))))))

(defun new-conversation (agent)
  (agent-cancel agent)
  (if (typep agent 'claude-agent)
      (claude-agent-new-conversation agent)
      (codex-agent-new-conversation agent))
  (let ((pane (agent-pane-of agent)))
    (clear-box (pane-messages pane))
    (clrhash (agent-pane-tools pane))
    (pane-finish-stream pane)
    (pane-note pane "New conversation.")))

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
                          (if running
                              (format nil "Working · ~{~a~^, ~}"
                                      (or (loop for a in (list *claude* *codex*)
                                                when (agent-busy a) collect (agent-name a))
                                          '("waiting")))
                              "")))))

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
                                                :on-finish (lambda (w) (declare (ignore w)) (update-controls))))
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

;;; Settings: the folder and what the agents may do

(defun folder-label (directory)
  (let ((home (namestring (user-homedir-pathname)))
        (path (namestring directory)))
    (if (and (> (length path) (length home)) (string= home path :end2 (length home)))
        (format nil "~~/~a" (subseq path (length home)))
        path)))

(defun set-directory (directory)
  (setf *directory* (uiop:ensure-directory-pathname directory))
  (dolist (agent (list *claude* *codex*))
    (when agent
      (agent-cancel agent)
      ;; A new place, a new conversation.
      (if (typep agent 'claude-agent)
          (claude-agent-new-conversation agent)
          (codex-agent-new-conversation agent))
      (setf (agent-directory agent) *directory*)))
  (when (control :folder)
    (gtk:label-set-text (gtk:button-get-child (control :folder)) (folder-label *directory*))
    (gtk:widget-set-tooltip-text (control :folder) (format nil "The agents work in ~a. Click to choose another folder."
                                                           (namestring *directory*))))
  (when *middle*
    (pane-note *middle* (format nil "Working in ~a" (namestring *directory*)))))

(defun choose-directory ()
  (let ((dialog (gtk:file-dialog-new)))
    (gtk:file-dialog-set-initial-folder dialog (gio:file-new-for-path (namestring *directory*)))
    (gio:async (gtk:file-dialog-select-folder dialog *window*)
               (lambda (file) (set-directory (gio:file-get-path file)))
               :error (lambda (e) (declare (ignore e))))))

(defun set-permission (permission)
  (setf *permission* permission)
  (dolist (agent (list *claude* *codex*))
    (setf (agent-permission agent) permission)
    ;; The next message restarts each, resuming its conversation.
    (unless (agent-busy agent) (agent-shutdown agent))))

(defun make-header ()
  (let ((header (make-instance 'gtk:header-bar))
        (folder (make-instance 'gtk:button :css-classes '("flat")
                                           :child (make-instance 'gtk:label :ellipsize :start :max-width-chars 50)))
        (permission (gtk:drop-down-new-from-strings '("Edit files in folder" "Full access"))))
    (gtk:widget-set-tooltip-text permission "Edit files in folder: Claude may edit files (and run git to review); ChatGPT runs in Codex's workspace sandbox. Full access: neither is restricted.")
    (gobject:connect permission "notify::selected"
                     (lambda (d pspec) (declare (ignore pspec))
                       (set-permission (if (= 1 (gtk:drop-down-get-selected d)) :full :edits))))
    (gobject:connect folder :clicked (lambda (b) (declare (ignore b)) (choose-directory)))
    (setf *controls* (list* :folder folder :permission permission *controls*))
    (gtk:header-bar-pack-start header folder)
    (gtk:header-bar-pack-end header permission)
    header))

;;; Checking the CLIs

(defun check-agents ()
  (sb-thread:make-thread
   (lambda ()
     (let ((status (check-claude)))
       (deliver (lambda () (show-claude-status status)))))
   :name "check claude")
  (sb-thread:make-thread
   (lambda ()
     (let ((status (check-codex)))
       (deliver (lambda () (show-codex-status status)))))
   :name "check codex"))

(defun show-claude-status (status)
  (let ((pane (agent-pane-of *claude*)))
    (cond ((claude-status-error status)
           (gtk:label-set-text (pane-status pane) "Not available")
           (pane-note pane (format nil "~a~%~%Install Claude Code (https://claude.com/claude-code), then restart."
                                   (claude-status-error status)) "error"))
          ((not (claude-status-logged-in status))
           (gtk:label-set-text (pane-status pane) "Not signed in")
           (pane-note pane "Claude Code is not signed in. Run `claude auth login` in a terminal, then send a message." "error"))
          (t (unless (agent-busy *claude*)
               (gtk:label-set-text (pane-status pane) (format nil "Ready · ~a" (claude-status-version status))))))))

(defun show-codex-status (status)
  (let ((pane (agent-pane-of *codex*)))
    (when (codex-status-program status)
      (setf (codex-agent-program *codex*) (codex-status-program status)))
    (cond ((null (codex-status-program status))
           (gtk:label-set-text (pane-status pane) "Not available")
           (pane-note pane (format nil "~a~%~%Install the Codex CLI or the ChatGPT desktop app, then restart."
                                   (codex-status-error status)) "error"))
          ((not (codex-status-logged-in status))
           (gtk:label-set-text (pane-status pane) "Not signed in")
           (pane-note pane (format nil "Codex is not signed in~@[ (~a)~]. Run `codex login` in a terminal, then send a message."
                                   (codex-status-error status)) "error"))
          (t (unless (agent-busy *codex*)
               (gtk:label-set-text (pane-status pane) (format nil "Ready · ~a" (codex-status-version status))))))))

;;; The window

(defparameter *css*
  '((".duet-pane" :background-color "@view_bg_color")
    (".duet-middle" :background-color "alpha(@view_fg_color, 0.02)")
    (".duet-prompt" :background-color "alpha(@accent_bg_color, 0.15)" :border-radius "8px" :padding ("6px" "10px"))
    (".duet-code" :background-color "alpha(@view_fg_color, 0.06)" :border-radius "6px" :padding "6px")
    (".duet-tool" :opacity "0.8")
    (".duet-message" :border-radius "10px" :padding ("8px" "12px"))
    (".duet-from-user" :background-color "alpha(@accent_bg_color, 0.15)")
    (".duet-from-claude" :background-color "alpha(#d97757, 0.14)" :margin-right "40px")
    (".duet-from-codex" :background-color "alpha(#10a37f, 0.14)" :margin-left "40px")
    (".duet-claude" :color "#d97757")
    (".duet-codex" :color "#10a37f")
    (".duet-note" :font-size "smaller")))

(defun activate (app)
  "Open the main window."
  (gtk:add-css *css*)
  (setf *controls* '())
  (let ((window (make-instance 'gtk:application-window
                               :application app :title "Agent Duet"
                               :default-width 1440 :default-height 900)))
    (setf *window* window
          *claude* (make-instance 'claude-agent :deliver #'deliver :permission *permission*)
          *codex* (make-instance 'codex-agent :deliver #'deliver :permission *permission*))
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
          (gtk:window-set-child window outer))))
    (set-directory (or *directory* (uiop:getcwd)))
    (update-controls)
    (check-agents)
    (gobject:connect window :close-request
                     (lambda (w) (declare (ignore w))
                       (dolist (agent (list *claude* *codex*))
                         (ignore-errors (agent-cancel agent))
                         (ignore-errors (agent-shutdown agent)))
                       nil))
    (gtk:window-present window)))

(defun main (&optional directory)
  "Run the application; returns when its window closes. DIRECTORY is where
the agents work (by default, the current directory). On macOS it must run
on the first thread: in Cadre, use Run GTK App."
  (when directory (setf *directory* (uiop:ensure-directory-pathname directory)))
  (let ((app (gtk:application-new "org.example.agent_integration" '(:default-flags))))
    (gobject:connect app :window-removed
                     (lambda (application window)
                       (declare (ignore window))
                       (when (null (gtk:application-get-windows application))
                         (gio:application-quit application))))
    (gobject:connect app :activate 'activate)
    (gio:application-run app nil)))
