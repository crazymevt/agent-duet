;;;; ui-presentation.lisp — presentation widgets; delivery stays in ui.lisp.
(in-package #:agent-duet)

;;; The look of the conversation, its cards and composers, and the agents'
;;; panels: peach Claude, teal ChatGPT; blue review, green done, amber for
;;; the user's turn, red errors. Text colors mix the theme's foreground with
;;; each accent, keeping at least 4.5:1 contrast on their tints in light and
;;; dark themes. The classes come from ui.lisp. This provider sits one step
;;; above the window's stylesheet, so its rules win where both set a property.

(defparameter *presentation-css*
  '("@define-color duet_claude #d97757;"
    "@define-color duet_codex #14b8a6;"
    "@define-color duet_claude_text mix(@view_fg_color, #d97757, 0.55);"
    "@define-color duet_codex_text mix(@view_fg_color, #14b8a6, 0.45);"
    ;; Agent identity
    (".duet-claude" :color "@duet_claude_text")
    (".duet-codex" :color "@duet_codex_text")
    (".duet-panel-claude" :border-top "3px solid alpha(@duet_claude, 0.85)")
    (".duet-panel-codex" :border-top "3px solid alpha(@duet_codex, 0.85)")
    ;; Conversation messages and original letter badges in the left gutter
    (".duet-message" :border-radius "10px" :padding ("10px" "14px")
     :margin-top "3px" :margin-bottom "3px")
    (".duet-message > box > label.caption-heading" :font-weight "700")
    (".duet-from-user" :background-color "alpha(@view_fg_color, 0.06)"
     :border "1px solid alpha(@view_fg_color, 0.10)")
    (".duet-from-claude" :background-color "alpha(@duet_claude, 0.07)"
     :border "1px solid alpha(@duet_claude, 0.22)" :border-left "3px solid alpha(@duet_claude, 0.85)")
    (".duet-from-codex" :background-color "alpha(@duet_codex, 0.07)"
     :border "1px solid alpha(@duet_codex, 0.22)" :border-left "3px solid alpha(@duet_codex, 0.85)")
    (".duet-from-claude > box > label.caption-heading" :color "@duet_claude_text")
    (".duet-from-codex > box > label.caption-heading" :color "@duet_codex_text")
    (".duet-chat-head" :border-radius "999px" :font-weight "700"
     :font-size "12px" :background-color "alpha(@view_fg_color, 0.10)"
     :color "@view_fg_color")
    (".duet-from-claude .duet-chat-head" :background-color "alpha(@duet_claude, 0.20)"
     :color "@duet_claude_text")
    (".duet-from-codex .duet-chat-head" :background-color "alpha(@duet_codex, 0.20)"
     :color "@duet_codex_text")
    (".duet-prompt" :background-color "alpha(@view_fg_color, 0.06)" :border-radius "10px"
     :border "1px solid alpha(@view_fg_color, 0.10)" :padding ("7px" "11px"))
    (".duet-checklist" :background-color "alpha(@view_fg_color, 0.05)" :border-radius "9px"
     :padding ("8px" "12px"))
    ;; Approvals and questions stand out from the thread
    (".duet-approval" :background-color "alpha(@accent_bg_color, 0.08)" :border-radius "9px"
     :border "1px solid alpha(@view_fg_color, 0.14)" :border-left "3px solid @accent_bg_color"
     :padding ("12px" "14px"))
    (".duet-summary" :background-color "alpha(@success_bg_color, 0.10)" :border-radius "9px"
     :border "1px solid alpha(@success_color, 0.35)" :border-left "3px solid @success_color"
     :padding ("12px" "14px"))
    ;; Tool calls and thinking: compact disclosures, muted until opened
    ("expander-widget.duet-tool" :background-color "alpha(@view_fg_color, 0.035)"
     :border "1px solid alpha(@view_fg_color, 0.10)" :border-radius "8px" :padding ("2px" "8px"))
    ("expander-widget.duet-tool > box > title" :color "alpha(@view_fg_color, 0.75)" :padding ("3px" "0"))
    ("expander-widget.duet-tool label.monospace" :font-size "smaller")
    (".duet-code" :background-color "alpha(@view_fg_color, 0.07)" :border-radius "6px" :padding "6px")
    ;; Each agent's latest action, in its panel and the conversation
    (".duet-live-activity" :font-size "smaller" :background-color "alpha(@view_fg_color, 0.05)"
     :border-radius "6px" :padding ("5px" "9px") :border-left "3px solid alpha(@view_fg_color, 0.2)")
    (".duet-live-activity.duet-claude" :border-left-color "alpha(@duet_claude, 0.8)")
    (".duet-live-activity.duet-codex" :border-left-color "alpha(@duet_codex, 0.8)")
    (".duet-live-activity.error" :border-left-color "@error_color")
    ("expander-widget.duet-transcript > box > title" :font-weight "600" :padding ("4px" "12px"))
    ("expander-widget.duet-workflow-settings > box > title" :color "alpha(@view_fg_color, 0.75)")
    ;; Composers: one rounded field, outlined in the accent while typing.
    ;; A composer is a frame in a .duet-composer box, or (unmarked) directly in a
    ;; pane or in a box there. Only the field gets a border; the action row does not.
    (".duet-composer > frame, .duet-pane > box > frame, .duet-pane > box > box > frame"
     :border-radius "10px" :border "1px solid alpha(@view_fg_color, 0.16)"
     :background-color "alpha(@view_fg_color, 0.03)")
    (".duet-composer > frame:focus-within, .duet-pane > box > frame:focus-within, .duet-pane > box > box > frame:focus-within"
     :border-color "alpha(@accent_color, 0.8)")
    (".duet-composer > frame textview, .duet-composer > frame textview > text, .duet-pane > box > frame textview, .duet-pane > box > frame textview > text, .duet-pane > box > box > frame textview, .duet-pane > box > box > frame textview > text"
     :background-color "transparent")
    ;; The task's stages: ✓ Plan — ● Working — ○ Done
    (".duet-progress-label" :font-weight "600")
    (".duet-sequence > flowboxchild, .duet-sequence > flowboxchild:hover, .duet-sequence > flowboxchild:active"
     :background-color "transparent" :box-shadow "none"
     :border "none" :padding "0" :outline "none")
    (".duet-step-connector" :color "alpha(@view_fg_color, 0.35)")
    (".duet-step-marker" :font-weight "700")
    (".duet-step-detail" :font-size "smaller" :color "alpha(@view_fg_color, 0.72)")
    (".duet-step-group" :border-left "2px solid alpha(@view_fg_color, 0.15)" :padding-left "6px")
    (".duet-step-pending .duet-step-label, .duet-step-pending .duet-step-marker, .duet-step-skipped .duet-step-label, .duet-step-skipped .duet-step-marker, .duet-step-row.duet-step-pending, .duet-step-row.duet-step-skipped"
     :color "alpha(@view_fg_color, 0.70)")
    (".duet-step-active .duet-step-label" :font-weight "700")
    (".duet-step-active .duet-step-marker" :color "@accent_color")
    (".duet-step-complete .duet-step-marker" :color "@success_color")
    (".duet-step-attention .duet-step-marker, .duet-step-attention .duet-step-label, .duet-step-row.duet-step-attention"
     :color "@warning_color")
    (".duet-step-error .duet-step-marker, .duet-step-error .duet-step-label, .duet-step-row.duet-step-error"
     :color "@error_color")
    ;; Agent status pills
    (".duet-pill" :border-radius "999px" :padding ("2px" "10px") :font-weight "600"
     :border "1px solid transparent")
    (".duet-pill-idle" :background-color "alpha(@view_fg_color, 0.08)"
     :color "alpha(@view_fg_color, 0.75)" :border-color "alpha(@view_fg_color, 0.18)")
    (".duet-pill-working" :background-color "alpha(@accent_bg_color, 0.18)"
     :color "mix(@view_fg_color, @accent_color, 0.7)" :border-color "alpha(@accent_color, 0.6)")
    (".duet-pill-working.duet-pill-claude" :background-color "alpha(@duet_claude, 0.18)"
     :color "@duet_claude_text" :border-color "alpha(@duet_claude, 0.65)")
    (".duet-pill-working.duet-pill-codex" :background-color "alpha(@duet_codex, 0.18)"
     :color "@duet_codex_text" :border-color "alpha(@duet_codex, 0.65)")
    (".duet-pill-review" :background-color "alpha(#3b82f6, 0.18)"
     :color "mix(@view_fg_color, #3b82f6, 0.55)" :border-color "alpha(#3b82f6, 0.65)")
    (".duet-pill-success" :background-color "alpha(#22c55e, 0.16)"
     :color "mix(@view_fg_color, #22c55e, 0.45)" :border-color "alpha(#22c55e, 0.6)")
    (".duet-pill-attention" :background-color "alpha(#f59e0b, 0.2)"
     :color "mix(@view_fg_color, #f59e0b, 0.45)" :border-color "alpha(#f59e0b, 0.7)")
    (".duet-pill-error" :background-color "alpha(#ef4444, 0.18)"
     :color "mix(@view_fg_color, #ef4444, 0.55)" :border-color "alpha(#ef4444, 0.7)")))

(defvar *presentation-css-provider* nil)

(defun ensure-presentation-css ()
  "Install the presentation stylesheet once, for the default display."
  (unless *presentation-css-provider*
    (setf *presentation-css-provider*
          (gtk:add-css *presentation-css* :priority (1+ gtk:+style-provider-priority-application+)))))

(defun ensure-pill-css ()
  "The pills are part of the presentation stylesheet."
  (ensure-presentation-css))

(defun make-workflow-settings (child)
  (ensure-presentation-css)
  (let ((expander (make-instance 'gtk:expander :label "Workflow settings" :child child
                                 :expanded (setting :workflow-settings-expanded nil)
                                 :css-classes '("duet-workflow-settings")
                                 :margin-start 12 :margin-end 12 :margin-top 4)))
    (gobject:connect expander "notify::expanded"
                     (lambda (widget spec) (declare (ignore spec))
                       (setf (setting :workflow-settings-expanded) (gtk:expander-get-expanded widget))))
    expander))

(defun make-transcript (agent scroller)
  (ensure-presentation-css)
  (let* ((key (agent-setting-key agent "TRANSCRIPT-EXPANDED"))
         (expander (make-instance 'gtk:expander :label "Transcript" :child scroller
                                  :css-classes '("duet-transcript")
                                  :expanded (setting key t) :vexpand t)))
    (setf (getf *controls* (agent-setting-key agent "TRANSCRIPT")) expander)
    (gobject:connect expander "notify::expanded"
                     (lambda (widget spec) (declare (ignore spec))
                       (let ((expanded (gtk:expander-get-expanded widget)))
                         (gtk:widget-set-vexpand widget expanded)
                         (setf (setting key) expanded))))
    (gtk:widget-set-vexpand expander (setting key t))
    expander))

(defun progress-text (presentation)
  "The progress line: where the task is, and the round when one part is current."
  (format nil "~a~@[ · Round ~d~]~@[ of ~d~]"
          (getf presentation :label) (getf presentation :round)
          (and (getf presentation :round) (getf presentation :max-rounds))))

(defun refresh-workflow-presentation ()
  "Render the shared presentation model alongside CLI readiness and usage."
  (ensure-presentation-css)
  (let* ((presentation (workflow-progress-presentation *workflow*))
         (stages (getf presentation :stages)))
    (when (control :progress)
      (gtk:label-set-text (control :progress) (progress-text presentation))
      (gtk:widget-add-css-class (control :progress) "duet-progress-label")
      (gtk:widget-set-tooltip-text
       (control :progress) (format nil "~{~a~^~%~}" (mapcar #'progress-stage-summary stages))))
    (when (and (control :progress-details) (not (equal presentation (control :last-progress))))
      (setf (getf *controls* :last-progress) presentation)
      (clear-box (control :progress-details))
      (when stages
        (gtk:box-append (control :progress-details) (make-progress-stages stages)))
      (when (control :progress-stage-details)
        (clear-box (control :progress-stage-details))
        (dolist (stage stages)
          (gtk:box-append (control :progress-stage-details) (make-progress-stage-row stage))))
      (when (control :progress-stage-expander)
        (gtk:widget-set-visible (control :progress-stage-expander) (and stages t)))))
  (dolist (agent (list *claude* *codex*))
    (let* ((pane (and agent (agent-pane-of agent)))
           (label (and agent (control (agent-setting-key agent "WORKFLOW-ACTIVITY"))))
           (presentation (and pane (workflow-agent-presentation *workflow* agent))))
      (when label
        (gtk:label-set-text label (getf presentation :label))
        (gtk:widget-set-halign label :start)
        (gtk:widget-set-tooltip-text label
                                     (format nil "~a~@[~%~a~]" (getf presentation :label) (getf presentation :detail)))
        (gtk:widget-set-css-classes label (activity-pill-classes agent presentation))))))

(defun activity-pill-classes (agent presentation)
  (list "caption" "duet-pill"
        (if (eq (agent-id agent) :claude) "duet-pill-claude" "duet-pill-codex")
        (format nil "duet-pill-~(~a~)" (workflow-presentation-tone presentation))
        (format nil "duet-state-~(~a~)" (getf presentation :state))))

(defun adapt-conversation-width ()
  "Stack the conversation above the agents when the sidebar would be too narrow."
  (when (and *window* (control :agent-split) (sidebar-view-p *view*))
    (let* ((outer (control :agent-split))
           (inner (control :conversation-split))
           (width (gtk:widget-get-width outer))
           (narrow (and (plusp width) (< width 940))))
      (unless (or narrow (control :narrow-layout)
                  (control :pre-checklist-positions)
                  (control :restore-checklist-positions)
                  (control :pending-layout-positions)
                  (control :equalize-agent-panels))
        (setf (getf *controls* :last-wide-positions)
              (list (gtk:paned-get-position outer) (gtk:paned-get-position inner))))
      (when (and narrow (control :narrow-layout)
                 (not (eq (gtk:orientable-get-orientation inner)
                          (if (>= width 680) :horizontal :vertical))))
        (gtk:orientable-set-orientation inner (if (>= width 680) :horizontal :vertical))
        (remf *controls* :layout-outer-restored)
        (setf (getf *controls* :equalize-agent-panels) :pending)
        (setf (getf *controls* :pending-layout-positions)
              (list (round (* (gtk:widget-get-height outer) (if (>= width 680) 0.60 0.40)))
                    (if (>= width 680) (round (/ width 2))
                        (round (* (gtk:widget-get-height outer) 0.30))))))
      (unless (eql narrow (control :narrow-layout))
        (remf *controls* :layout-outer-restored)
        (when (and narrow (control :last-wide-positions)
                   (not (control :pre-checklist-positions))
                   (not (control :restore-checklist-positions))
                   (not (control :pending-layout-positions))
                   (not (control :equalize-agent-panels)))
          (setf (setting (layout-position-key *view*))
                (agent-layout-preference (control :last-wide-positions)
                                         (control :custom-agent-split))))
        (setf (getf *controls* :narrow-layout) narrow)
        ;; In either sidebar view, narrow windows put the conversation first.
        ;; Reparent only the existing widgets; streams and cards stay live.
        (when (eq *view* :agents-left)
          (gtk:paned-set-start-child outer nil)
          (gtk:paned-set-end-child outer nil)
          (gtk:paned-set-start-child outer (control (if narrow :middle-widget :conversation-split)))
          (gtk:paned-set-end-child outer (control (if narrow :conversation-split :middle-widget))))
        (gtk:orientable-set-orientation outer (if narrow :vertical :horizontal))
        (gtk:orientable-set-orientation inner (if (and narrow (>= width 680)) :horizontal :vertical))
        (setf (getf *controls* :equalize-agent-panels)
              (when (or narrow (not (or (control :restore-checklist-positions)
                                        (second (layout-positions *view*)))))
                :pending))
        (if narrow
            (setf (getf *controls* :pending-layout-positions)
                  (list (round (* (gtk:widget-get-height outer) (if (>= width 680) 0.60 0.40)))
                        (if (>= width 680) (round (/ width 2))
                            (round (* (gtk:widget-get-height outer) 0.30)))))
            (let ((saved (or (control :restore-checklist-positions)
                             (layout-positions *view*))))
              (setf (getf *controls* :pending-layout-positions)
                    (list (or (first saved) (round (* width (if (eq *view* :agents-left) 0.34 0.66))))
                          (or (second saved) 360)))))))))

(defun reveal-agent-transcript (pane)
  "An approval or question should never arrive behind a collapsed transcript."
  (let ((expander (control (agent-setting-key (agent-pane-agent pane) "TRANSCRIPT"))))
    (when expander (gtk:expander-set-expanded expander t))))

(defun make-progress-details ()
  "The stage sequence under the progress line, then each stage's detail."
  (ensure-presentation-css)
  (let* ((sequence (make-instance 'gtk:box :orientation :vertical))
         (rows (make-instance 'gtk:box :orientation :vertical :spacing 4 :margin-top 4 :margin-start 4))
         (expander (make-instance 'gtk:expander :label "Stage details" :child rows :visible nil
                                  :expanded (setting :workflow-progress-expanded nil)
                                  :css-classes '("duet-workflow-settings")
                                  :margin-start 12 :margin-end 12)))
    (setf (getf *controls* :progress-details) sequence
          (getf *controls* :progress-stage-details) rows
          (getf *controls* :progress-stage-expander) expander)
    (gobject:connect expander "notify::expanded"
                     (lambda (widget param)
                       (declare (ignore param))
                       (setf (setting :workflow-progress-expanded) (gtk:expander-get-expanded widget))))
    (gtk:build (gtk:box :orientation :vertical :spacing 2 :margin-bottom 6 sequence expander))))

(defparameter *parallel-stage-ids* '(:lead-part :partner-part :lead-review :partner-review)
  "Stages that run side by side: a split plan's parts, or both reviews.")

(defun progress-stage-tone (stage)
  "STAGE's look: an error, or something the user is to do, outranks its state."
  (cond ((eq (getf stage :state) :error) :error)
        ((getf stage :attention-p) :attention)
        (t (case (getf stage :state)
             ((:active :complete :skipped) (getf stage :state))
             (otherwise :pending)))))

(defun progress-stage-marker (stage)
  "A symbol for each state, so the sequence reads without color."
  (ecase (progress-stage-tone stage)
    (:error "✕") (:attention "!") (:active "●") (:complete "✓") (:skipped "–") (:pending "○")))

(defun progress-stage-summary (stage)
  "Plan: done, or Claude's part: in progress — Working · round 2."
  (format nil "~a: ~a~@[ — ~a~]" (getf stage :label)
          (ecase (progress-stage-tone stage)
            (:error "failed") (:attention "needs you") (:active "in progress")
            (:complete "done") (:skipped "skipped") (:pending "not started"))
          (getf stage :detail)))

(defun progress-stage-groups (stages)
  "STAGES in order, with consecutive parallel stages in one group."
  (let ((groups '()))
    (dolist (stage stages)
      (if (and groups (member (getf stage :id) *parallel-stage-ids*)
               (member (getf (first (first groups)) :id) *parallel-stage-ids*))
          (push stage (first groups))
          (push (list stage) groups)))
    (mapcar #'reverse (nreverse groups))))

(defun make-progress-stages (stages)
  "One wrapping row: ✓ Plan — ● Working — ○ Done. Parts that run side by side
are stacked in one step, so the row doesn't imply an order between them.
The row is status, not controls: its children take no focus or clicks, but
stay under the pointer so each stage's tooltip can be read."
  (let ((flow (apply #'wrapping-controls
                     (loop for group in (progress-stage-groups stages)
                           for first = t then nil
                           collect (let ((step (make-instance 'gtk:box :spacing 8 :css-classes '("duet-sequence-step"))))
                                     (unless first
                                       (gtk:box-append step (make-instance 'gtk:label :label "—" :valign :start
                                                                           :css-classes '("duet-step-connector"))))
                                     (gtk:box-append
                                      step
                                      (if (rest group)
                                          (let ((box (make-instance 'gtk:box :orientation :vertical :spacing 4
                                                                    :css-classes '("duet-step-group"))))
                                            (dolist (stage group box)
                                              (gtk:box-append box (make-progress-stage stage))))
                                          (make-progress-stage (first group))))
                                     step)))))
    (gtk:widget-add-css-class flow "duet-sequence")
    (gtk:flow-box-set-column-spacing flow 8)
    (gtk:flow-box-set-max-children-per-line flow 12)
    (gtk:widget-set-margin-start flow 12)
    (gtk:widget-set-margin-end flow 12)
    (gtk:widget-set-margin-top flow 0)
    (gtk:widget-set-margin-bottom flow 2)
    (gtk:flow-box-set-activate-on-single-click flow nil)
    (gtk:widget-set-can-focus flow nil)
    (loop for child = (gtk:widget-get-first-child flow) then (gtk:widget-get-next-sibling child)
          while child do (gtk:widget-set-focusable child nil))
    flow))

(defun make-progress-stage (stage)
  "A marker and name; beneath, what a current, blocked or failed stage is doing."
  (let* ((tone (progress-stage-tone stage))
         (detail (and (member tone '(:active :attention :error)) (getf stage :detail)))
         (box (make-instance 'gtk:box :orientation :vertical
                                      :tooltip-text (progress-stage-summary stage)
                                      :css-classes (list "duet-step" (format nil "duet-step-~(~a~)" tone)))))
    (gtk:box-append box (gtk:build
                         (gtk:box :spacing 5
                                  (gtk:label :label (progress-stage-marker stage) :css-classes '("duet-step-marker"))
                                  (gtk:label :label (getf stage :label) :xalign 0.0
                                             :css-classes '("duet-step-label")))))
    (when detail
      (gtk:box-append box (make-instance 'gtk:label :label detail :xalign 0.0 :wrap t
                                                    :max-width-chars 30 :margin-start 16
                                                    :css-classes '("duet-step-detail"))))
    box))

(defun make-progress-stage-row (stage)
  "STAGE's state and full detail, for Stage details."
  (make-instance 'gtk:label :xalign 0.0 :wrap t :wrap-mode :word-char :selectable t
                            :label (format nil "~a ~a" (progress-stage-marker stage) (progress-stage-summary stage))
                            :css-classes (list "duet-step-row"
                                               (format nil "duet-step-~(~a~)" (progress-stage-tone stage)))))

(defun agent-layout-size ()
  (let ((outer (control :agent-split)))
    (list (gtk:widget-get-width outer) (gtk:widget-get-height outer) *view*)))

(defun equal-agent-position ()
  "Default position from allocated child space, excluding the handle."
  (let* ((inner (control :conversation-split))
         (claude (control :claude-widget))
         (codex (control :codex-widget))
         (horizontal (eq (gtk:orientable-get-orientation inner) :horizontal))
         (extent (if horizontal #'gtk:widget-get-width #'gtk:widget-get-height))
         (start (if (eq *view* :panes) (control :middle-widget) claude))
         (available (+ (funcall extent start) (funcall extent codex))))
    (equal-agent-divider-position *view* available (funcall extent claude))))

(defun equalize-agent-panels ()
  (gtk:paned-set-position (control :conversation-split) (equal-agent-position)))

(defun agent-layout-custom-p ()
  "Compare geometry only while the window's allocation is stable."
  (or (control :custom-agent-split)
      (and (not (control :pending-layout-positions))
           (not (control :equalize-agent-panels))
           (agent-divider-custom-p
            (gtk:paned-get-position (control :conversation-split))
            (equal-agent-position) (control :agent-layout-size) (agent-layout-size)))))

(defun observe-agent-layout-size ()
  "Keep defaults equal through window resizing, without interpreting it as a drag."
  (let ((size (agent-layout-size)))
    (unless (or (eq *view* :conversation) (control :narrow-layout)
                (control :pre-checklist-positions) (control :restore-checklist-positions)
                (control :pending-layout-positions) (control :equalize-agent-panels))
      (if (equal size (control :agent-layout-size))
          (setf (getf *controls* :custom-agent-split) (agent-layout-custom-p))
          (unless (control :custom-agent-split)
            (setf (getf *controls* :equalize-agent-panels) :ready)
            (gtk:widget-queue-allocate (control :agent-split)))))
    (setf (getf *controls* :agent-layout-size) size)))

(defun window-layout-snapshot ()
  "Geometry and restoration state used to debounce persistence."
  (list (terminal-visible-p)
        (and (control :terminal-split)
             (gtk:widget-get-height (gtk:paned-get-end-child (control :terminal-split))))
        (control :restore-terminal-height)
        (control :restore-checklist-width)
        (and (control :checklist-panel)
             (gtk:widget-get-visible (control :checklist-panel))
             (gtk:widget-get-width (control :checklist-panel)))
        *view* (agent-layout-size)
        (gtk:paned-get-position (control :agent-split))
        (gtk:paned-get-position (control :conversation-split))
        (multiple-value-list (gtk:window-get-default-size *window*))
        (gtk:window-is-maximized *window*)
        (mapcar #'control '(:narrow-layout :pre-checklist-positions
                            :restore-checklist-positions :pending-layout-positions
                            :equalize-agent-panels))))

(defun schedule-window-layout-save ()
  "Save only after geometry has stopped changing for 300 milliseconds."
  (let ((snapshot (window-layout-snapshot)))
    (unless (equal snapshot (control :layout-save-snapshot))
      (when (control :layout-save-timeout)
        (glib:source-remove (control :layout-save-timeout)))
      (setf (getf *controls* :layout-save-snapshot) snapshot)
      (let ((window *window*) (outer (control :agent-split)))
        (setf (getf *controls* :layout-save-timeout)
              (glib:timeout-add glib:+priority-default+ 300
                (lambda ()
                  ;; An old window's callback must not save a replacement window.
                  (when (and (eq window *window*) (eq outer (control :agent-split)) (not *closing*))
                    (remf *controls* :layout-save-timeout)
                    (when (equal snapshot (window-layout-snapshot))
                      (unless (some #'control '(:narrow-layout :pre-checklist-positions
                                               :restore-checklist-positions :pending-layout-positions
                                               :equalize-agent-panels))
                        (remember-layout))
                      (remember-window-geometry window)
                      (remember-terminal-layout)
                      (remember-checklist-width)))
                  nil)))))))

(defun watch-conversation-allocation (widget)
  "Observe rendered allocations; queue another only for staged restoration."
  (dolist (split (list widget (control :conversation-split)))
    (gobject:connect split "notify::position"
      (lambda (split spec)
        (declare (ignore split spec))
        (when (and (sidebar-view-p *view*) (not (control :narrow-layout))
                   (>= (gtk:widget-get-width widget) 940)
                   (not (control :pre-checklist-positions))
                   (not (control :restore-checklist-positions))
                   (not (control :pending-layout-positions))
                   (not (control :equalize-agent-panels)))
          (setf (getf *controls* :last-wide-positions)
                (list (gtk:paned-get-position widget)
                      (gtk:paned-get-position (control :conversation-split))))))))
  (gobject:connect widget :realize
    (lambda (widget)
      (let ((size nil))
        (gobject:connect (gtk:widget-get-frame-clock widget) :after-paint
          (lambda (clock)
            (declare (ignore clock))
            ;; A terminal divider change needs another allocation before agent
            ;; dividers can be restored against the new available height.
            (unless (or *closing* (restore-checklist-width)
                        (control :restore-checklist-width)
                        (restore-terminal-height)
                        (control :restore-terminal-height))
              (observe-agent-layout-size)
              (schedule-window-layout-save)
              ;; Orientation changes queue another allocation. Set positions
              ;; only after that allocation, or GtkPaned scales them using the
              ;; previous axis and loses the user's wide divider positions.
              ;; The outer divider needs one allocation before the inner
              ;; divider can use the actual available agent space. Only new
              ;; layouts are equalized; subsequent user drags stay untouched.
              (when (eq (control :equalize-agent-panels) :ready)
                (equalize-agent-panels)
                (remf *controls* :equalize-agent-panels)
                (unless (control :narrow-layout)
                  (setf (getf *controls* :last-wide-positions)
                        (list (gtk:paned-get-position widget)
                              (gtk:paned-get-position (control :conversation-split))))))
              (let ((pending (control :pending-layout-positions)))
                (when pending
                  ;; Allocate the outer split first. Otherwise GTK scales the
                  ;; saved inner position against its previous allocation.
                  (if (control :layout-outer-restored)
                      (progn
                        (gtk:paned-set-position (control :conversation-split) (second pending))
                        (remf *controls* :pending-layout-positions)
                        (remf *controls* :layout-outer-restored)
                        (unless (or (control :equalize-agent-panels) (control :narrow-layout))
                          (setf (getf *controls* :last-wide-positions) pending))
                        (when (eq (control :equalize-agent-panels) :pending)
                          (setf (getf *controls* :equalize-agent-panels) :ready)
                          (gtk:widget-queue-allocate widget)))
                      (progn
                        (gtk:paned-set-position widget (first pending))
                        (setf (getf *controls* :layout-outer-restored) t)
                        (gtk:widget-queue-allocate widget)))))
              (let ((current (list (gtk:widget-get-width widget)
                                   (gtk:widget-get-height widget) *view*)))
                (unless (equal size current)
                  (setf size current)
                  (adapt-conversation-width))
                ;; Restore after GTK has allocated the newly available space,
                ;; including Three panes and unchanged-size visibility toggles.
                (let ((saved (control :restore-checklist-positions)))
                  (when (and saved (not (control :narrow-layout)))
                    ;; Restoring the outer split changes the inner split's
                    ;; allocation. Setting both in this frame lets GtkPaned
                    ;; rescale the inner request against its old width (most
                    ;; visible when the checklist grew a Three panes window).
                    (if (control :checklist-outer-restored)
                        (progn
                          (remf *controls* :restore-checklist-positions)
                          (remf *controls* :checklist-outer-restored)
                          (gtk:paned-set-position (control :conversation-split) (second saved)))
                        (progn
                          (gtk:paned-set-position widget (first saved))
                          (setf (getf *controls* :checklist-outer-restored) t)
                          (gtk:widget-queue-allocate widget)))))))))))))
