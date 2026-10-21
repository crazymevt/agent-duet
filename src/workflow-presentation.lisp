;;;; workflow-presentation.lisp — what a workflow and its agents are doing, for the UI
;;;;
;;;; Pure readers of WORKFLOW, its REVIEW-LOOPs and STEPs and the AGENTs: no
;;;; GTK, nothing changed. They say only what the state shows: a plan isn't
;;;; approved until its parts exist after waiting for approval, and a check
;;;; isn't said to pass at all (its failing is).
;;;;
;;;; (WORKFLOW-AGENT-PRESENTATION workflow agent), WORKFLOW nil when there is
;;;; none, is a plist:
;;;;   :state       :idle :working :planning :proposing :reviewing :fixing
;;;;                :checking :resolving :waiting :awaiting-approval
;;;;                :needs-input :paused :out-of-usage :complete :error :stopped
;;;;   :label       a few words: "Reviewing"
;;;;   :detail      a string or nil: "Claude's part · round 2 of 3"
;;;;   :attention-p true when the user is to do something
;;;; (WORKFLOW-PRESENTATION-TONE presentation) groups its :state for color:
;;;;   :working :review :success :attention :error or :idle.
;;;;
;;;; (WORKFLOW-PROGRESS-PRESENTATION workflow) is a plist:
;;;;   :label       the task as a whole: "2 parts in progress"
;;;;   :round       the round in progress (or the last), when one piece of
;;;;                work is under review; nil when there are several at once
;;;;                or rounds don't apply (review only)
;;;;   :max-rounds  nil when rounds don't apply
;;;;   :concurrent-p true while more than one part is active
;;;;   :stages      plists, (:id :label :state) and :detail and :attention-p;
;;;;                :state :pending :active :complete :skipped or :error. A
;;;;                split plan has a stage for each part, side by side, not
;;;;                in a line: each has its own round and state.

(in-package #:agent-duet)

(defun presentation-terminal-p (workflow)
  (member (workflow-state workflow) '(:finished :stopped)))

(defun presentation-failed-p (workflow)
  (and (eq (workflow-state workflow) :finished)
       (or (workflow-completion-error workflow)
           (find :failed (workflow-loops workflow) :key #'loop-state))
       t))

(defun presentation-running-steps (workflow &optional agent)
  "WORKFLOW's running steps (AGENT's, if given), oldest first."
  (reverse (remove-if-not (lambda (s) (and (eq (step-state s) :running)
                                           (or (null agent) (eq (step-agent s) agent))))
                          (workflow-steps workflow))))

(defun presentation-paused-steps (workflow &optional agent)
  (remove-if-not (lambda (s) (or (null agent) (eq (step-agent s) agent)))
                 (workflow-paused workflow)))

(defun presentation-user-pause-p (step)
  "Whether paused STEP waits for the user: not just for a prerequisite."
  (not (waiting-step-p step)))

(defun presentation-review-only-p (workflow)
  (eq (workflow-mode workflow) :review-only))

(defun presentation-loop-round (workflow loop)
  "The round LOOP is in (reviews done, and one more while it goes on), or nil
without rounds. A confirming review is no new round, running or paused."
  (unless (presentation-review-only-p workflow)
    (let ((confirming (find-if (lambda (s) (and (eq (step-loop s) loop) (step-confirming s)
                                                (eq (step-role s) :review)))
                               (append (presentation-running-steps workflow) (workflow-paused workflow)))))
      (if (or (loop-done-p loop) confirming (eq (loop-state loop) :assigned))
          (max 1 (loop-round loop))
          (min (1+ (loop-round loop)) (max 1 (workflow-max-rounds workflow)))))))

(defun presentation-round-text (workflow loop)
  (let ((round (presentation-loop-round workflow loop)))
    (and round (format nil "round ~d of ~d" round (workflow-max-rounds workflow)))))

(defun presentation-join (&rest parts)
  (let ((parts (remove nil parts)))
    (and parts (format nil "~{~a~^ · ~}" parts))))

(defun presentation-capitalize (text)
  (string-capitalize text :end (min 1 (length text))))

(defun presentation-pause-reason (step)
  (case (step-reason step)
    (:out-of-usage (format nil "~a is out of usage" (agent-name (step-agent step))))
    (:interrupted "interrupted")
    (:disconnected "disconnected")
    (:failed "the turn failed")
    (:continuation-failed "couldn't carry on from the reply")
    (:integration-unresolved "the merge isn't resolved")
    (:deadlock "the parts wait on each other")
    (t (string-downcase (princ-to-string (step-reason step))))))

(defun presentation-waiting-for (step)
  "What a step waiting in turn waits for: \"Claude's part\"."
  (let ((loop (step-loop step)))
    (and loop
         (let ((deps (remove-if #'loop-ready-p (loop-dependencies loop))))
           (format nil "~{~a~^ and ~}" (mapcar #'part-name (or deps (loop-dependencies loop))))))))

;;; An agent

(defun presentation-step (workflow step)
  "The plist for an agent running STEP."
  (let* ((loop (step-loop step))
         (part (and loop (part-name loop)))
         (round (and loop (presentation-round-text workflow loop))))
    (ecase (step-role step)
      (:propose (list :state :proposing :label "Proposing an approach" :detail nil))
      ((:plan :replan) (list :state :planning
                             :label (if (eq (step-role step) :replan) "Revising the plan" "Planning")
                             :detail nil))
      (:resolve (list :state :resolving :label "Resolving merge conflicts" :detail nil))
      (:check (list :state :checking :label "Running check" :detail part))
      (:review (list :state :reviewing
                     :label (if (step-confirming step) "Settling open findings" "Reviewing")
                     :detail (if (presentation-review-only-p workflow)
                                 "Read-only review"
                                 (presentation-join part round))))
      (:work (cond ((plusp (step-check-fixes step))
                    (list :state :fixing :label "Fixing the check" :detail (presentation-join part round)))
                   ((and loop (plusp (loop-round loop)) (not (loop-done-p loop)))
                    (list :state :fixing :label "Addressing the review" :detail (presentation-join part round)))
                   ((and loop (loop-combined-result loop))
                    (list :state :working :label "Combining the parts" :detail round))
                   (t (list :state :working :label "Working" :detail (presentation-join part round))))))))

(defun presentation-idle-in-workflow (workflow agent)
  "What AGENT, with no step of its own, waits for in running WORKFLOW."
  (let ((loops (remove-if #'loop-done-p (workflow-loops workflow))))
    (flet ((authored (state) (find-if (lambda (l) (and (eq (loop-author l) agent) (eq (loop-state l) state))) loops))
           (reviewing (state) (find-if (lambda (l) (and (eq (loop-reviewer l) agent) (eq (loop-state l) state)
                                                        (not (loop-skip-review l))))
                                       loops)))
      (let ((l nil))
        (cond ((setf l (authored :checking))
               (list :state :checking :label "Running check" :detail (part-name l)))
              ((setf l (authored :reviewing))
               (list :state :waiting :label "Waiting for review" :detail (part-name l)))
              ((setf l (reviewing :working))
               (list :state :waiting :label "Waiting to review" :detail (part-name l)))
              ((member (workflow-integration-state workflow) '(:committing :merging))
               (list :state :waiting :label "Merging the parts" :detail nil))
              (t (list :state :idle :label "Idle" :detail nil)))))))

(defun presentation-agent-outcome (workflow agent)
  "How AGENT's part of finished WORKFLOW went, from its loops' states."
  (let ((loops (remove-if-not (lambda (l) (if (presentation-review-only-p workflow)
                                              (eq (loop-reviewer l) agent)
                                              (eq (loop-author l) agent)))
                              (workflow-loops workflow))))
    (and loops
         (format nil "~{~a~^; ~}"
                 (mapcar (lambda (l)
                           (if (presentation-review-only-p workflow)
                               (if (eq (loop-state l) :approved) "approved" "changes requested")
                               (format nil "~a ~a" (presentation-capitalize (part-name l)) (loop-outcome l))))
                         loops)))))

(defun presentation-direct-message-p (agent)
  "Whether AGENT's running turn is the user's own message to it (sent from its
pane, so nobody waits on the reply), not a workflow step."
  (and (agent-busy agent) (car (agent-turn-token agent))
       (agent-turn agent) (null (second (agent-turn agent)))))

(defun presentation-out-of-usage (agent)
  "\"out of usage\", with when it resets if known, while AGENT is out; else nil."
  (let ((until (agent-out-until agent)))
    (cond ((agent-out-of-usage-p agent) (format nil "out of usage until ~a" (format-reset-time until)))
          ((eq until :unknown) "out of usage"))))

(defun workflow-agent-presentation (workflow agent)
  "What AGENT is doing in WORKFLOW (nil: there is none), for its panel: a
plist of :state, :label, :detail and :attention-p."
  (let ((workflow (and workflow (workflow-agent-member-p workflow agent) workflow))
        (out (and (not (agent-busy agent)) (presentation-out-of-usage agent))))
    (flet ((result (state label &optional detail attention)
             ;; Whatever else it says, an idle agent's usage running out shows.
             (list :state state :label label
                   :detail (if (and out (not (eq state :out-of-usage)))
                               (presentation-join detail (presentation-capitalize out))
                               detail)
                   :attention-p (and attention t))))
      (cond
        ((agent-waiting-for-user-p agent)
         (result :needs-input "Needs your answer"
                 (format nil "~d request~:p waiting" (length (agent-approvals agent))) t))
        ((or (null workflow) (eq (workflow-state workflow) :new))
         (cond ((agent-busy agent) (result :working "Working"))
               (out (result :out-of-usage "Out of usage" (presentation-capitalize out)))
               (t (result :idle "Idle"))))
        ;; Talking to it directly, outside its part of the task.
        ((and (presentation-direct-message-p agent) (presentation-terminal-p workflow)
              (not (presentation-failed-p workflow)))
         (result :working "Answering your message"))
        ((eq (workflow-state workflow) :stopped)
         (result :stopped "Stopped"
                 (and (presentation-paused-steps workflow agent) "Its step can be recovered")))
        ((presentation-failed-p workflow)
         (result :error "Task failed" (workflow-completion-error workflow) t))
        ((eq (workflow-state workflow) :finished)
         (if (agent-busy agent)
             (result :working "Working")
             (result :complete "Done" (presentation-agent-outcome workflow agent))))
        (t
         (let ((paused (presentation-paused-steps workflow agent))
               (running (presentation-running-steps workflow agent))
               (pending (workflow-pending-plan workflow)))
           (let ((user-pause (find-if #'presentation-user-pause-p paused)))
             (cond
               ((and user-pause (eq (step-reason user-pause) :out-of-usage))
                (result :out-of-usage "Out of usage" (step-description user-pause) t))
               (user-pause
                (result :paused "Paused"
                        (format nil "~a: ~a" (step-description user-pause) (presentation-pause-reason user-pause))
                        t))
               ((presentation-direct-message-p agent)
                (result :working "Answering your message"
                        (and running
                             (format nil "Then ~(~a~)" (getf (presentation-step workflow (first running)) :label)))))
               (running
                (let ((p (presentation-step workflow (first running))))
                  (result (getf p :state) (getf p :label)
                          (presentation-join (getf p :detail)
                                             (and (rest running)
                                                  (format nil "~d more queued" (length (rest running))))))))
               (paused
                (result :waiting "Waiting" (format nil "Until ~a is done" (presentation-waiting-for (first paused)))))
               ((and pending (eq agent (workflow-lead workflow)))
                (result :awaiting-approval "Plan awaits your approval" (getf pending :error) t))
               (pending
                (result :waiting "Waiting for plan approval"))
               (t (let ((p (presentation-idle-in-workflow workflow agent)))
                    (result (getf p :state) (getf p :label) (getf p :detail))))))))))))

;;; An agent panel's status pill

(defun workflow-presentation-tone (presentation)
  "PRESENTATION's (from WORKFLOW-AGENT-PRESENTATION) color group: an error or
anything the user is to do outranks its state."
  (let ((state (getf presentation :state)))
    (cond ((eq state :error) :error)
          ((or (getf presentation :attention-p)
               (member state '(:needs-input :awaiting-approval :paused :out-of-usage)))
           :attention)
          ((member state '(:reviewing :waiting)) :review)
          ((eq state :complete) :success)
          ((member state '(:working :planning :proposing :fixing :checking :resolving)) :working)
          (t :idle))))

(defun workflow-agent-member-p (workflow agent)
  (or (eq agent (workflow-lead workflow)) (eq agent (workflow-partner workflow))))

;;; The task as a whole

(defun presentation-loop-id (workflow loop)
  "LOOP's stage id. A part's label is set as it starts, from whose part it
was, and stays when the work is handed over: its author may change."
  (let ((lead (agent-name (workflow-lead workflow))))
    (cond ((loop-combined-result loop) :merged-result)
          ((null (loop-label loop)) :work)
          ((presentation-review-only-p workflow)
           (if (equal (loop-label loop) (format nil "~a's review" lead)) :lead-review :partner-review))
          ((equal (loop-label loop) (format nil "~a's part" lead)) :lead-part)
          (t :partner-part))))

(defun presentation-loop-stage (workflow loop)
  (let* ((steps (remove loop (workflow-steps workflow) :key #'step-loop :test-not #'eq))
         (paused (find-if (lambda (s) (member s (workflow-paused workflow))) steps))
         (running (find :running steps :key #'step-state))
         (round (presentation-round-text workflow loop))
         (terminal (presentation-terminal-p workflow))
         (state :pending) (detail nil) (attention nil))
    (case (loop-state loop)
      (:approved (setf state :complete detail (presentation-join "Approved" round)))
      (:unreviewed (setf state :complete
                         detail (if (loop-check-failed loop) "Not reviewed; its check still fails" "Not reviewed")))
      (:unresolved
       (setf state :error
             detail (cond ((presentation-review-only-p workflow) "Changes requested")
                          ((loop-check-failed loop) "Not approved: its check still fails")
                          (t (presentation-join "Not approved" round)))))
      (:failed (setf state :error detail "Failed"))
      (t
       (cond ((eq (workflow-state workflow) :stopped)
              (setf state :pending detail (presentation-join "Stopped" round)))
             (terminal (setf state :skipped detail "Not finished"))
             ((and paused (presentation-user-pause-p paused))
              (setf state :active attention t
                    detail (presentation-join (format nil "Paused: ~a" (presentation-pause-reason paused)) round)))
             (paused
              (setf state :pending detail (format nil "Waits for ~a" (presentation-waiting-for paused))))
             ((eq (loop-state loop) :assigned) (setf state :pending))
             (t
              (setf state :active
                    detail (presentation-join
                            (if running
                                (getf (presentation-step workflow running) :label)
                                (case (loop-state loop)
                                  (:checking "Running check")
                                  (:reviewing "Reviewing")
                                  (t "Working")))
                            (and (loop-check-failed loop) "check failing")
                            round))))))
    (list :id (presentation-loop-id workflow loop)
          :label (presentation-capitalize (part-name loop))
          :state state :detail detail :attention-p attention)))

(defun presentation-split-p (workflow)
  (member (workflow-mode workflow) '(:divide :compare)))

(defun presentation-plan-evidence (workflow)
  "How a split plan's work came to start, from what's left of it: nil, not
yet; :planned, from the lead's plan; :handed-off, the planning went to the
partner, who does the whole task, no plan made or approved; :takeover, the
partner does it from its proposal (the lead out of usage), approved when
plans wait for approval; :unknown, it can't be told.

A lone part taken over (unlabelled, unreviewed) comes from a handed-off
plan, a takeover, or a planned part's work handed over. Only finished and
superseded steps tell those apart, and a checkpoint keeps neither: restored,
it's :unknown, never approved."
  (let ((loops (workflow-loops workflow))
        (steps (workflow-steps workflow)))
    (flet ((planning (state)
             (find-if (lambda (s) (and (member (step-role s) '(:plan :replan)) (eq (step-state s) state))) steps)))
      (cond ((null loops) nil)
            ((find-if (lambda (s) (and (eq (step-role s) :plan) (eq (step-state s) :superseded))) steps)
             :handed-off)
            ;; Split parts, or a lone part not taken over: only a plan starts those.
            ((some (lambda (l) (or (loop-label l) (not (loop-skip-review l)))) loops) :planned)
            ((planning :completed) :planned)
            ((and (eq (workflow-mode workflow) :compare)
                  (find :propose steps :key #'step-role)
                  (notany (lambda (s) (member (step-role s) '(:plan :replan))) steps))
             :takeover)
            (t :unknown)))))

(defun presentation-plan-stages (workflow)
  "The stages before a split plan's parts start: proposals, the plan, its approval."
  (let* ((loops (workflow-loops workflow))
         (pending (workflow-pending-plan workflow))
         (terminal (presentation-terminal-p workflow))
         (stopped (eq (workflow-state workflow) :stopped))
         (evidence (presentation-plan-evidence workflow))
         (planned (or pending loops))
         (stages '()))
    (flet ((role-steps (roles)
             (remove-if-not (lambda (s) (member (step-role s) roles)) (workflow-steps workflow)))
           (stage (id label state &optional detail attention)
             (push (list :id id :label label :state state :detail detail :attention-p (and attention t)) stages)))
      (when (eq (workflow-mode workflow) :compare)
        (let* ((steps (role-steps '(:propose)))
               (paused (find-if (lambda (s) (member s (workflow-paused workflow))) steps))
               (missing (loop for (agent . text) in (workflow-proposals workflow)
                              unless text collect (agent-name agent))))
          (cond ((or (= 2 (length (workflow-proposals workflow))) planned)
                 (stage :proposals "Proposals" :complete
                        (and missing (format nil "~{~a~^ and ~} had none" missing))))
                ((and paused (not stopped))
                 (stage :proposals "Proposals" :active
                        (format nil "Paused: ~a" (presentation-pause-reason paused)) t))
                (stopped (stage :proposals "Proposals" :pending "Stopped"))
                (terminal (stage :proposals "Proposals" :error))
                ((member (workflow-state workflow) '(:running)) (stage :proposals "Proposals" :active))
                (t (stage :proposals "Proposals" :pending)))))
      (let* ((steps (role-steps '(:plan :replan)))
             (paused (find-if (lambda (s) (member s (workflow-paused workflow))) steps))
             (running (find :running steps :key #'step-state)))
        (cond ((or (getf pending :takeover) (eq evidence :takeover))
               (stage :plan "Plan" :skipped (format nil "~a is out of usage" (agent-name (workflow-lead workflow)))))
              ((eq evidence :handed-off)
               (stage :plan "Plan" :skipped
                      (format nil "Handed to ~a, who does the whole task" (agent-name (workflow-partner workflow)))))
              ((eq evidence :unknown) (stage :plan "Plan" :skipped))
              (planned (stage :plan "Plan" :complete))
              (stopped (stage :plan "Plan" :pending "Stopped"))
              (terminal (stage :plan "Plan" (if steps :error :skipped)))
              ((and paused (presentation-user-pause-p paused))
               (stage :plan "Plan" :active (format nil "Paused: ~a" (presentation-pause-reason paused)) t))
              (running (stage :plan "Plan" :active (if (eq (step-role running) :replan) "Revising" "Planning")))
              (t (stage :plan "Plan" :pending))))
      (when (workflow-approve-plan-p workflow)
        (cond (pending (stage :approval "Your approval" (if stopped :pending :active)
                              (or (getf pending :error) "Waiting for you") (not terminal)))
              ;; Held for approval, a plan's parts start only once approved;
              ;; a handed-off planning skips approval.
              ((member evidence '(:planned :takeover)) (stage :approval "Your approval" :complete "Approved"))
              (loops (stage :approval "Your approval" :skipped))
              (stopped (stage :approval "Your approval" :pending))
              (terminal (stage :approval "Your approval" :skipped))
              (t (stage :approval "Your approval" :pending)))))
    (nreverse stages)))

(defun presentation-merge-stages (workflow)
  "With worktrees and two parts: merging them, and the merged result's review
until it has a loop of its own."
  (let ((parts (remove-if (lambda (l) (or (loop-combined-result l) (null (loop-label l))))
                          (workflow-loops workflow)))
        (state (workflow-integration-state workflow))
        (terminal (presentation-terminal-p workflow))
        (stopped (eq (workflow-state workflow) :stopped)))
    (when (and (workflow-worktrees-p workflow) (presentation-split-p workflow)
               (or (rest parts) (and (null (workflow-loops workflow)) (not terminal))))
      (let ((merge (cond ((eq state :interrupted) (list :error "Stopped before all parts merged"))
                         ((or (eq state :merged) (workflow-merged-loop workflow)) (list :complete nil))
                         ((and (member state '(:committing :merging)) (not terminal))
                          (list :active (if (eq state :committing) "Committing each part" "Merging")))
                         ((and terminal (not stopped)) (list :skipped "Not merged"))
                         (t (list :pending nil)))))
        (append (list (list :id :merge :label "Merge the parts" :state (first merge)
                            :detail (second merge) :attention-p nil))
                (unless (workflow-merged-loop workflow)
                  (list (list :id :merged-result :label "The merged result"
                              :state (if (and terminal (not stopped)) :skipped :pending)
                              :detail nil :attention-p nil))))))))

(defun presentation-stages (workflow)
  (let* ((loops (workflow-loops workflow))
         (merged (workflow-merged-loop workflow))
         (parts (remove merged loops)))
    (append
     (when (presentation-split-p workflow) (presentation-plan-stages workflow))
     (if (and (null loops) (member (workflow-mode workflow) '(:review :review-only)))
         ;; Not started, or failed before its loops: one placeholder.
         (list (list :id (if (presentation-review-only-p workflow) :review :work)
                     :label (if (presentation-review-only-p workflow) "Review" "Work")
                     :state (cond ((presentation-failed-p workflow) :error)
                                  ((eq (workflow-state workflow) :running) :active)
                                  (t :pending))
                     :detail nil :attention-p nil))
         (mapcar (lambda (l) (presentation-loop-stage workflow l)) parts))
     (presentation-merge-stages workflow)
     (when merged (list (presentation-loop-stage workflow merged)))
     (list (list :id :finish :label "Done"
                 :state (cond ((presentation-failed-p workflow) :error)
                              ((eq (workflow-state workflow) :finished) :complete)
                              (t :pending))
                 :detail (and (presentation-failed-p workflow) (workflow-completion-error workflow))
                 :attention-p nil)))))

(defun presentation-progress-label (workflow stages)
  (let* ((loops (workflow-loops workflow))
         (agents (list (workflow-lead workflow) (workflow-partner workflow)))
         (active-loops (remove-if-not (lambda (l) (eq :active (getf (presentation-loop-stage workflow l) :state)))
                                      loops))
         (user-pause (find-if #'presentation-user-pause-p (workflow-paused workflow)))
         (running (presentation-running-steps workflow)))
    (case (workflow-state workflow)
      (:new "Not started")
      (:stopped "Stopped")
      (:finished
       (cond ((presentation-failed-p workflow) "Failed")
             ((presentation-review-only-p workflow)
              (if (every (lambda (l) (eq (loop-state l) :approved)) loops)
                  "Review finished: approved" "Review finished: changes requested"))
             ((find :unresolved loops :key #'loop-state) "Finished, not approved")
             ((find :unreviewed loops :key #'loop-state) "Finished, not all reviewed")
             (t "Finished: approved")))
      (t
       (cond ((some #'agent-waiting-for-user-p agents) "Waiting for your answer")
             ((workflow-pending-plan workflow) "Waiting for you to approve the plan")
             ((and user-pause (eq (step-reason user-pause) :deadlock)) "Parts wait on each other")
             ((and user-pause (eq (step-reason user-pause) :out-of-usage))
              (format nil "Paused: ~a is out of usage" (agent-name (step-agent user-pause))))
             (user-pause "Paused")
             ((and (member (workflow-integration-state workflow) '(:committing :merging))
                   (not (workflow-merged-loop workflow)))
              "Merging the parts")
             ((find-if (lambda (s) (member (step-role s) '(:plan :replan))) running) "Planning")
             ((find :propose running :key #'step-role) "Proposing approaches")
             ((rest active-loops) (format nil "~d parts in progress" (length active-loops)))
             (active-loops
              (let ((l (first active-loops)))
                (case (loop-state l)
                  (:checking "Running check")
                  (:reviewing (if (presentation-review-only-p workflow) "Reviewing" "In review"))
                  (t (if (and (plusp (loop-round l)) (not (presentation-review-only-p workflow)))
                         "Addressing the review" "Working")))))
             ((find :pending stages :key (lambda (s) (getf s :state))) "Waiting")
             (t "Working"))))))

(defun workflow-progress-presentation (workflow)
  "Where WORKFLOW (or nil) has got to, for the progress strip: a plist of
:label, :round, :max-rounds, :concurrent-p and :stages."
  (if (null workflow)
      (list :label "Not started" :round nil :max-rounds nil :concurrent-p nil :stages '())
      (let* ((stages (presentation-stages workflow))
             (review-only (presentation-review-only-p workflow))
             (active (count :active stages :key (lambda (s) (getf s :state))))
             (loops (workflow-loops workflow))
             (merged (workflow-merged-loop workflow))
             (current (cond (review-only nil)
                            ;; The merged result's review is what goes on once it exists.
                            (merged (list merged))
                            (t (or (remove-if #'loop-done-p loops) loops)))))
        (list :label (presentation-progress-label workflow stages)
              :round (and (= 1 (length current)) (presentation-loop-round workflow (first current)))
              :max-rounds (and (not review-only) (workflow-max-rounds workflow))
              :concurrent-p (> active 1)
              :stages stages))))
