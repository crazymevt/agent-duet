;;;; ui-presentation.lisp — presentation widgets; delivery stays in ui.lisp.
(in-package #:agent-duet)

(defun make-workflow-settings (child)
  (let ((expander (make-instance 'gtk:expander :label "Workflow settings" :child child
                                 :expanded (setting :workflow-settings-expanded nil)
                                 :margin-start 12 :margin-end 12 :margin-top 4)))
    (gobject:connect expander "notify::expanded"
                     (lambda (widget spec) (declare (ignore spec))
                       (setf (setting :workflow-settings-expanded) (gtk:expander-get-expanded widget))))
    expander))

(defun make-transcript (agent scroller)
  (let* ((key (agent-setting-key agent "TRANSCRIPT-EXPANDED"))
         (expander (make-instance 'gtk:expander :label "Transcript" :child scroller
                                  :expanded (setting key t) :vexpand t)))
    (setf (getf *controls* (agent-setting-key agent "TRANSCRIPT")) expander)
    (gobject:connect expander "notify::expanded"
                     (lambda (widget spec) (declare (ignore spec))
                       (let ((expanded (gtk:expander-get-expanded widget)))
                         (gtk:widget-set-vexpand widget expanded)
                         (setf (setting key) expanded))))
    (gtk:widget-set-vexpand expander (setting key t))
    expander))

(defun refresh-workflow-presentation ()
  "Render the shared presentation model alongside CLI readiness and usage."
  (let* ((presentation (workflow-progress-presentation *workflow*))
           (stages (getf presentation :stages)))
      (when (control :progress)
        (gtk:label-set-text (control :progress)
                            (format nil "~a~@[ · round ~d~]~@[ of ~d~]"
                                    (getf presentation :label) (getf presentation :round)
                                    (and (getf presentation :round) (getf presentation :max-rounds))))
        (gtk:widget-set-tooltip-text
         (control :progress)
         (format nil "~{~a~^~%~}"
                 (mapcar (lambda (stage)
                           (format nil "~a: ~(~a~)" (getf stage :label) (getf stage :state))) stages))))
      (when (and (control :progress-details) (not (equal presentation (control :last-progress))))
        (setf (getf *controls* :last-progress) presentation)
        (clear-box (control :progress-details))
        (gtk:box-append (control :progress-details)
                        (make-progress-stages stages))))
  (dolist (agent (list *claude* *codex*))
      (let* ((pane (and agent (agent-pane-of agent)))
             (label (and agent (control (agent-setting-key agent "WORKFLOW-ACTIVITY"))))
             (presentation (and pane (workflow-agent-presentation *workflow* agent))))
        (when label
          (ensure-pill-css)
          (gtk:label-set-text label (getf presentation :label))
          (gtk:widget-set-halign label :start)
          (gtk:widget-set-tooltip-text label
                                       (format nil "~a~@[~%~a~]" (getf presentation :label) (getf presentation :detail)))
          (gtk:widget-set-css-classes label (activity-pill-classes agent presentation))))))

;;; Agent status pills: orange Claude, teal ChatGPT; blue review, green done,
;;; amber for the user's turn, red errors. A provider of their own, so the
;;; window's stylesheet needn't know them.

(defparameter *pill-css*
  '((".duet-pill" :border-radius "999px" :padding ("2px" "10px") :font-weight "600"
     :border "1px solid transparent")
    (".duet-pill-idle" :background-color "alpha(@view_fg_color, 0.08)"
     :color "alpha(@view_fg_color, 0.75)" :border-color "alpha(@view_fg_color, 0.18)")
    (".duet-pill-working" :background-color "alpha(@accent_bg_color, 0.18)"
     :color "mix(@view_fg_color, @accent_color, 0.7)" :border-color "alpha(@accent_color, 0.6)")
    (".duet-pill-working.duet-pill-claude" :background-color "alpha(#d97757, 0.18)"
     :color "mix(@view_fg_color, #e8875f, 0.7)" :border-color "alpha(#d97757, 0.65)")
    (".duet-pill-working.duet-pill-codex" :background-color "alpha(#14b8a6, 0.18)"
     :color "mix(@view_fg_color, #2dd4bf, 0.7)" :border-color "alpha(#14b8a6, 0.65)")
    (".duet-pill-review" :background-color "alpha(#3b82f6, 0.18)"
     :color "mix(@view_fg_color, #60a5fa, 0.7)" :border-color "alpha(#3b82f6, 0.65)")
    (".duet-pill-success" :background-color "alpha(#22c55e, 0.16)"
     :color "mix(@view_fg_color, #4ade80, 0.65)" :border-color "alpha(#22c55e, 0.6)")
    (".duet-pill-attention" :background-color "alpha(#f59e0b, 0.2)"
     :color "mix(@view_fg_color, #fbbf24, 0.7)" :border-color "alpha(#f59e0b, 0.7)")
    (".duet-pill-error" :background-color "alpha(#ef4444, 0.18)"
     :color "mix(@view_fg_color, #f87171, 0.7)" :border-color "alpha(#ef4444, 0.7)")))

(defvar *pill-css-provider* nil)

(defun ensure-pill-css ()
  "Install the pill stylesheet once, for the default display."
  (unless *pill-css-provider*
    (setf *pill-css-provider* (gtk:add-css *pill-css*))))

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
        (setf (getf *controls* :equalize-agent-panels) :pending)
        (setf (getf *controls* :pending-layout-positions)
              (list (round (* (gtk:widget-get-height outer) (if (>= width 680) 0.60 0.40)))
                    (if (>= width 680) (round (/ width 2))
                        (round (* (gtk:widget-get-height outer) 0.30))))))
      (unless (eql narrow (control :narrow-layout))
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
  (let ((box (make-instance 'gtk:box :orientation :vertical :spacing 6)))
    (setf (getf *controls* :progress-details) box)
    (make-instance 'gtk:expander :label "Workflow progress" :child box
                   :margin-start 12 :margin-end 12 :margin-bottom 6)))

(defun make-progress-stages (stages)
  "Wrap status cards without FlowBox's otherwise actionable child wrappers."
  (let ((flow (apply #'wrapping-controls (mapcar #'make-progress-stage stages))))
    (gtk:widget-add-css-class flow "duet-progress")
    (gtk:widget-set-can-target flow nil)
    (gtk:widget-set-can-focus flow nil)
    (loop for child = (gtk:widget-get-first-child flow) then (gtk:widget-get-next-sibling child)
          while child do (gtk:widget-set-focusable child nil))
    flow))

(defun make-progress-stage (stage)
  "Independent cards also describe concurrent parts without implying an order."
  (make-instance 'gtk:label :xalign 0.0 :wrap t :max-width-chars 34
                 :label (format nil "~a · ~(~a~)~@[~%~a~]"
                                (getf stage :label) (getf stage :state) (getf stage :detail))
                 :css-classes
                 (list "duet-stage"
                       (cond ((eq (getf stage :state) :error) "duet-stage-error")
                             ((getf stage :attention-p) "duet-stage-attention")
                             (t (case (getf stage :state)
                                  (:active "duet-stage-active")
                                  (:complete "duet-stage-complete")
                                  (:skipped "duet-stage-skipped")
                                  (otherwise "duet-stage-pending")))))))

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
            (unless *closing*
              (observe-agent-layout-size)
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
                  (gtk:paned-set-position widget (first pending))
                  (gtk:paned-set-position (control :conversation-split) (second pending))
                  (remf *controls* :pending-layout-positions)
                  (unless (or (control :equalize-agent-panels) (control :narrow-layout))
                    (setf (getf *controls* :last-wide-positions) pending))
                  (when (eq (control :equalize-agent-panels) :pending)
                    (setf (getf *controls* :equalize-agent-panels) :ready)
                    (gtk:widget-queue-allocate widget))))
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
