;;;; tests/workflow-presentation.lisp — what the UI shows of a workflow and its agents

(in-package #:agent-duet/tests)

(defun presented-agents ()
  (values (make-instance 'fake-agent :id :claude :name "Claude" :replies '())
          (make-instance 'fake-agent :id :codex :name "ChatGPT" :replies '())))

(defun presented-workflow (lead partner &rest initargs &key (state :running) &allow-other-keys)
  (let ((workflow (apply #'make-instance 'ai:workflow :lead lead :partner partner :request "Do it"
                         (loop for (key value) on initargs by #'cddr
                               unless (eq key :state) append (list key value)))))
    (setf (ai:workflow-state workflow) state)
    workflow))

(defun presented-step (workflow agent role &rest initargs &key (state :running) paused &allow-other-keys)
  "A step of WORKFLOW's, as SUBMIT-STEP and PAUSE-STEP leave it, without running it."
  (let ((step (apply #'ai::make-step :agent agent :role role :state (if paused :paused state)
                     (loop for (key value) on initargs by #'cddr
                           unless (member key '(:state :paused)) append (list key value)))))
    (push step (ai::workflow-steps workflow))
    (when paused (push step (ai::workflow-paused workflow)))
    step))

(defun presented-stage (progress id)
  (find id (getf progress :stages) :key (lambda (s) (getf s :id))))

(defun presented-stage-state (progress id) (getf (presented-stage progress id) :state))

(define-test presentation-without-workflow :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (declare (ignore partner))
    (let ((p (ai::workflow-agent-presentation nil lead)))
      (is eq :idle (getf p :state))
      (is string= "Idle" (getf p :label))
      (false (getf p :detail))
      (false (getf p :attention-p)))
    (setf (ai:agent-busy lead) t)
    (is eq :working (getf (ai::workflow-agent-presentation nil lead) :state))
    (let ((progress (ai::workflow-progress-presentation nil)))
      (is string= "Not started" (getf progress :label))
      (false (getf progress :round))
      (false (getf progress :max-rounds))
      (is equal '() (getf progress :stages)))))

(define-test presentation-new-review :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner :state :new :max-rounds 3))
           (progress (ai::workflow-progress-presentation workflow)))
      (is string= "Not started" (getf progress :label))
      (is = 3 (getf progress :max-rounds))
      (false (getf progress :round))
      (is eq :pending (presented-stage-state progress :work))
      (is eq :pending (presented-stage-state progress :finish))
      (is eq :idle (getf (ai::workflow-agent-presentation workflow partner) :state)))))

(define-test presentation-agent-not-in-workflow :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (let ((other (make-instance 'fake-agent :id :claude :name "Other" :replies '()))
          (workflow (presented-workflow lead partner)))
      (presented-step workflow lead :work)
      (is eq :idle (getf (ai::workflow-agent-presentation workflow other) :state)))))

(define-test presentation-review-rounds :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner :max-rounds 3))
           (loop (ai::make-review-loop :author lead :reviewer partner :task "Do it" :state :working)))
      (setf (ai:workflow-loops workflow) (list loop))
      (let ((step (presented-step workflow lead :work :loop loop)))
        (let ((p (ai::workflow-agent-presentation workflow lead)))
          (is eq :working (getf p :state))
          (true (search "round 1 of 3" (getf p :detail)))
          (false (getf p :attention-p)))
        (let ((p (ai::workflow-agent-presentation workflow partner)))
          (is eq :waiting (getf p :state))
          (is string= "Waiting to review" (getf p :label)))
        (let ((progress (ai::workflow-progress-presentation workflow)))
          (is string= "Working" (getf progress :label))
          (is = 1 (getf progress :round))
          (is = 3 (getf progress :max-rounds))
          (false (getf progress :concurrent-p))
          (is eq :active (presented-stage-state progress :work))
          (is eq :pending (presented-stage-state progress :finish))
          (false (presented-stage progress :plan)))
        ;; The work is in: the partner reviews it.
        (setf (ai::step-state step) :completed
              (ai::loop-state loop) :reviewing)
        (presented-step workflow partner :review :loop loop)
        (is eq :reviewing (getf (ai::workflow-agent-presentation workflow partner) :state))
        (is string= "Waiting for review" (getf (ai::workflow-agent-presentation workflow lead) :label))
        (is string= "In review" (getf (ai::workflow-progress-presentation workflow) :label))
        ;; One review done, changes requested: the author fixes, in round 2.
        (setf (ai::step-state (first (ai::workflow-steps workflow))) :completed
              (ai::loop-round loop) 1
              (ai::loop-state loop) :working)
        (presented-step workflow lead :work :loop loop)
        (let ((p (ai::workflow-agent-presentation workflow lead)))
          (is eq :fixing (getf p :state))
          (true (search "round 2 of 3" (getf p :detail))))
        (is = 2 (getf (ai::workflow-progress-presentation workflow) :round))
        (is string= "Addressing the review" (getf (ai::workflow-progress-presentation workflow) :label))))))

(define-test presentation-check-not-claimed :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner :check-command "make test"))
           (loop (ai::make-review-loop :author lead :reviewer partner :task "Do it" :state :checking)))
      (setf (ai:workflow-loops workflow) (list loop))
      (let ((p (ai::workflow-agent-presentation workflow lead)))
        (is eq :checking (getf p :state))
        (is string= "Running check" (getf p :label)))
      (is string= "Running check" (getf (ai::workflow-progress-presentation workflow) :label))
      ;; A check fix attempt is fixing, not passing.
      (setf (ai::loop-state loop) :working (ai::loop-check-failed loop) t)
      (presented-step workflow lead :work :loop loop :check-fixes 1)
      (is string= "Fixing the check" (getf (ai::workflow-agent-presentation workflow lead) :label))
      (true (search "check failing" (getf (presented-stage (ai::workflow-progress-presentation workflow) :work) :detail)))
      ;; Finished, the reviewer approved, but the check still fails.
      (setf (ai::loop-state loop) :unresolved (ai::loop-round loop) 1
            (ai::workflow-state workflow) :finished)
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is eq :error (presented-stage-state progress :work))
        (true (search "check still fails" (getf (presented-stage progress :work) :detail)))
        (false (search "pass" (format nil "~s" progress)))
        (is string= "Finished, not approved" (getf progress :label))))))

(define-test presentation-plan-approval :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (let ((workflow (presented-workflow lead partner :mode :divide :approve-plan t)))
      (presented-step workflow lead :plan)
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is string= "Planning" (getf progress :label))
        (is eq :active (presented-stage-state progress :plan))
        (is eq :pending (presented-stage-state progress :approval))
        (false (getf progress :round)))
      (is eq :planning (getf (ai::workflow-agent-presentation workflow lead) :state))
      (is eq :idle (getf (ai::workflow-agent-presentation workflow partner) :state))
      ;; The plan waits for the user: not yet approved.
      (setf (ai::step-state (first (ai::workflow-steps workflow))) :completed
            (ai:workflow-pending-plan workflow) (list :lead "A" :partner "B" :order :parallel :error nil))
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is string= "Waiting for you to approve the plan" (getf progress :label))
        (is eq :complete (presented-stage-state progress :plan))
        (is eq :active (presented-stage-state progress :approval))
        (true (getf (presented-stage progress :approval) :attention-p))
        (false (search "Approved" (format nil "~s" progress))))
      (let ((p (ai::workflow-agent-presentation workflow lead)))
        (is eq :awaiting-approval (getf p :state))
        (true (getf p :attention-p)))
      (let ((p (ai::workflow-agent-presentation workflow partner)))
        (is eq :waiting (getf p :state))
        (false (getf p :attention-p)))
      ;; Stopped while waiting: the approval never happened.
      (setf (ai:workflow-state workflow) :stopped)
      (is eq :pending (presented-stage-state (ai::workflow-progress-presentation workflow) :approval)))))

(define-test presentation-plan-without-approval-step :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (let ((workflow (presented-workflow lead partner :mode :divide)))
      (setf (ai:workflow-loops workflow)
            (list (ai::make-review-loop :author lead :reviewer partner :label "Claude's part" :state :working)))
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is eq :complete (presented-stage-state progress :plan))
        ;; Not held for approval: there's no approval to claim.
        (false (presented-stage progress :approval))))))

(define-test presentation-plan-failed :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (let ((workflow (presented-workflow lead partner :mode :divide :approve-plan t :state :finished)))
      (presented-step workflow lead :plan :state :completed)
      (setf (ai::workflow-completion-error workflow) "Claude couldn't make a plan: no")
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is string= "Failed" (getf progress :label))
        (is eq :error (presented-stage-state progress :plan))
        (is eq :skipped (presented-stage-state progress :approval))
        (is eq :error (presented-stage-state progress :finish)))
      (let ((p (ai::workflow-agent-presentation workflow lead)))
        (is eq :error (getf p :state))
        (true (search "couldn't make a plan" (getf p :detail)))
        (true (getf p :attention-p))))))

(define-test presentation-compare-proposals :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (let ((workflow (presented-workflow lead partner :mode :compare)))
      (presented-step workflow lead :propose)
      (presented-step workflow partner :propose)
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is string= "Proposing approaches" (getf progress :label))
        (is eq :active (presented-stage-state progress :proposals))
        (is eq :pending (presented-stage-state progress :plan)))
      (is eq :proposing (getf (ai::workflow-agent-presentation workflow partner) :state))
      (dolist (s (ai::workflow-steps workflow)) (setf (ai::step-state s) :completed))
      (setf (ai::workflow-proposals workflow) (list (cons partner nil) (cons lead "Mine")))
      (presented-step workflow lead :plan)
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is eq :complete (presented-stage-state progress :proposals))
        (true (search "ChatGPT had none" (getf (presented-stage progress :proposals) :detail)))
        (is eq :active (presented-stage-state progress :plan))))))

(define-test presentation-concurrent-parts :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner :mode :divide :max-rounds 3))
           (mine (ai::make-review-loop :author lead :reviewer partner :label "Claude's part" :state :reviewing
                                       :round 1))
           (theirs (ai::make-review-loop :author partner :reviewer lead :label "ChatGPT's part" :state :working)))
      (setf (ai:workflow-loops workflow) (list mine theirs))
      (presented-step workflow partner :review :loop mine)
      (presented-step workflow partner :work :loop theirs)
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is string= "2 parts in progress" (getf progress :label))
        ;; Each part has its own round: there's no one round to show.
        (false (getf progress :round))
        (is = 3 (getf progress :max-rounds))
        (true (getf progress :concurrent-p))
        (is eq :active (presented-stage-state progress :lead-part))
        (is eq :active (presented-stage-state progress :partner-part))
        (true (search "round 2 of 3" (getf (presented-stage progress :lead-part) :detail)))
        (true (search "round 1 of 3" (getf (presented-stage progress :partner-part) :detail))))
      ;; The partner's oldest running step goes first; the other waits behind it.
      (let ((p (ai::workflow-agent-presentation workflow partner)))
        (is eq :reviewing (getf p :state))
        (true (search "1 more queued" (getf p :detail))))
      ;; One part done: the round shown is the one still going.
      (setf (ai::loop-state mine) :approved)
      (dolist (s (ai::workflow-steps workflow))
        (when (eq (ai::step-loop s) mine) (setf (ai::step-state s) :completed)))
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is = 1 (getf progress :round))
        (false (getf progress :concurrent-p))
        (is eq :complete (presented-stage-state progress :lead-part))
        (is eq :active (presented-stage-state progress :partner-part))))))

(define-test presentation-dependent-part-waits :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner :mode :divide))
           (mine (ai::make-review-loop :author lead :reviewer partner :label "Claude's part" :state :working))
           (theirs (ai::make-review-loop :author partner :reviewer lead :label "ChatGPT's part" :state :paused
                                         :dependencies nil)))
      (setf (ai::loop-dependencies theirs) (list mine)
            (ai:workflow-loops workflow) (list mine theirs))
      (presented-step workflow lead :work :loop mine)
      (presented-step workflow partner :work :loop theirs :paused t :reason :dependency)
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is eq :active (presented-stage-state progress :lead-part))
        (is eq :pending (presented-stage-state progress :partner-part))
        (true (search "Waits for Claude's part" (getf (presented-stage progress :partner-part) :detail)))
        (false (getf (presented-stage progress :partner-part) :attention-p))
        (false (getf progress :concurrent-p))
        (is string= "Working" (getf progress :label)))
      (let ((p (ai::workflow-agent-presentation workflow partner)))
        (is eq :waiting (getf p :state))
        (false (getf p :attention-p))
        (true (search "Claude's part" (getf p :detail))))
      ;; Neither can go on: the user chooses.
      (setf (ai::step-reason (first (ai::workflow-paused workflow))) :deadlock)
      (is string= "Parts wait on each other" (getf (ai::workflow-progress-presentation workflow) :label))
      (let ((p (ai::workflow-agent-presentation workflow partner)))
        (is eq :paused (getf p :state))
        (true (getf p :attention-p))))))

(define-test presentation-pauses :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner))
           (loop (ai::make-review-loop :author lead :reviewer partner :state :paused)))
      (setf (ai:workflow-loops workflow) (list loop))
      (let ((step (presented-step workflow lead :work :loop loop :paused t :reason :out-of-usage)))
        (let ((p (ai::workflow-agent-presentation workflow lead)))
          (is eq :out-of-usage (getf p :state))
          (true (getf p :attention-p)))
        (let ((progress (ai::workflow-progress-presentation workflow)))
          (is string= "Paused: Claude is out of usage" (getf progress :label))
          (is eq :active (presented-stage-state progress :work))
          (true (getf (presented-stage progress :work) :attention-p)))
        (setf (ai::step-reason step) :interrupted)
        (let ((p (ai::workflow-agent-presentation workflow lead)))
          (is eq :paused (getf p :state))
          (true (search "interrupted" (getf p :detail))))
        (is string= "Paused" (getf (ai::workflow-progress-presentation workflow) :label))))))

(define-test presentation-user-questions :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner))
           (loop (ai::make-review-loop :author lead :reviewer partner :state :working)))
      (setf (ai:workflow-loops workflow) (list loop))
      (presented-step workflow lead :work :loop loop)
      (ai::agent-ask-question lead "q1" '() (lambda (answer) (declare (ignore answer))))
      (let ((p (ai::workflow-agent-presentation workflow lead)))
        (is eq :needs-input (getf p :state))
        (true (getf p :attention-p)))
      (is string= "Waiting for your answer" (getf (ai::workflow-progress-presentation workflow) :label))
      (ai::agent-answer-approval lead "q1" :deny)
      (is eq :working (getf (ai::workflow-agent-presentation workflow lead) :state)))))

(define-test presentation-stopped :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner :state :stopped))
           (loop (ai::make-review-loop :author lead :reviewer partner :state :paused :round 1)))
      (setf (ai:workflow-loops workflow) (list loop))
      (presented-step workflow lead :work :loop loop :paused t :reason :interrupted)
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is string= "Stopped" (getf progress :label))
        (is eq :pending (presented-stage-state progress :work))
        (is = 2 (getf progress :round))
        (is eq :pending (presented-stage-state progress :finish)))
      (let ((p (ai::workflow-agent-presentation workflow lead)))
        (is eq :stopped (getf p :state))
        (true (getf p :detail)))
      (false (getf (ai::workflow-agent-presentation workflow partner) :detail)))))

(define-test presentation-worktree-merge :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner :mode :divide :worktrees t))
           (mine (ai::make-review-loop :author lead :reviewer partner :label "Claude's part" :state :approved
                                       :round 1))
           (theirs (ai::make-review-loop :author partner :reviewer lead :label "ChatGPT's part" :state :approved
                                         :round 1)))
      (setf (ai:workflow-loops workflow) (list mine theirs))
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is eq :pending (presented-stage-state progress :merge))
        (is eq :pending (presented-stage-state progress :merged-result)))
      (setf (ai::workflow-integration-state workflow) :merging)
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is string= "Merging the parts" (getf progress :label))
        (is eq :active (presented-stage-state progress :merge)))
      (is string= "Merging the parts" (getf (ai::workflow-agent-presentation workflow lead) :label))
      (presented-step workflow lead :resolve)
      (is eq :resolving (getf (ai::workflow-agent-presentation workflow lead) :state))
      (setf (ai::step-state (first (ai::workflow-steps workflow))) :completed)
      ;; Merged: the merged result goes round its own review loop.
      (let ((merged (ai::make-review-loop :author lead :reviewer partner :label "the merged result"
                                          :combined-result t :state :working)))
        (setf (ai::workflow-integration-state workflow) :merged
              (ai::workflow-merged-loop workflow) merged
              (ai:workflow-loops workflow) (list mine theirs merged))
        (presented-step workflow lead :work :loop merged)
        (let ((progress (ai::workflow-progress-presentation workflow)))
          (is eq :complete (presented-stage-state progress :merge))
          (is eq :active (presented-stage-state progress :merged-result))
          (is = 1 (count :merged-result (getf progress :stages) :key (lambda (s) (getf s :id))))
          (is = 1 (getf progress :round)))
        (is string= "Combining the parts" (getf (ai::workflow-agent-presentation workflow lead) :label)))
      (setf (ai::workflow-integration-state workflow) :interrupted
            (ai::workflow-completion-error workflow) "Merging the parts stopped"
            (ai:workflow-state workflow) :finished)
      (is eq :error (presented-stage-state (ai::workflow-progress-presentation workflow) :merge)))))

(define-test presentation-review-only :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner :mode :review-only))
           (by-lead (ai::make-review-loop :author partner :reviewer lead :label "Claude's review" :state :reviewing))
           (by-partner (ai::make-review-loop :author lead :reviewer partner :label "ChatGPT's review"
                                             :state :reviewing)))
      (setf (ai:workflow-loops workflow) (list by-lead by-partner))
      (presented-step workflow lead :review :loop by-lead)
      (presented-step workflow partner :review :loop by-partner)
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (false (getf progress :round))
        (false (getf progress :max-rounds))
        (true (getf progress :concurrent-p))
        (is eq :active (presented-stage-state progress :lead-review))
        (is eq :active (presented-stage-state progress :partner-review)))
      (let ((p (ai::workflow-agent-presentation workflow lead)))
        (is eq :reviewing (getf p :state))
        (is string= "Read-only review" (getf p :detail)))
      (dolist (s (ai::workflow-steps workflow)) (setf (ai::step-state s) :completed))
      (setf (ai::loop-state by-lead) :approved (ai::loop-state by-partner) :unresolved
            (ai:workflow-state workflow) :finished)
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is string= "Review finished: changes requested" (getf progress :label))
        (is eq :complete (presented-stage-state progress :lead-review))
        (is eq :error (presented-stage-state progress :partner-review)))
      (is string= "approved" (getf (ai::workflow-agent-presentation workflow lead) :detail))
      (is string= "changes requested" (getf (ai::workflow-agent-presentation workflow partner) :detail)))))

(define-test presentation-finished-run :parent agent-duet
  ;; A real run with fake agents, start to finish.
  (multiple-value-bind (lead partner) (presented-agents)
    (let ((directory (merge-pathnames (format nil "duet-presentation-~a/" (ai::make-uuid))
                                      (uiop:temporary-directory)))
          (workflow (presented-workflow lead partner :state :new)))
      (setf (fake-replies lead) (list (says "Done"))
            (fake-replies partner) (list (says *approve*))
            (ai:agent-directory lead) directory
            (ai:agent-directory partner) directory)
      (ensure-directories-exist directory)
      (unwind-protect
           (progn
             (ai:start-workflow workflow)
             (true (wait-until (lambda () (eq :finished (ai:workflow-state workflow)))))
             (let ((progress (ai::workflow-progress-presentation workflow)))
               (is string= "Finished: approved" (getf progress :label))
               (is = 1 (getf progress :round))
               (is eq :complete (presented-stage-state progress :work))
               (is eq :complete (presented-stage-state progress :finish)))
             (let ((p (ai::workflow-agent-presentation workflow lead)))
               (is eq :complete (getf p :state))
               (is string= "Claude's work approved" (getf p :detail))
               (false (getf p :attention-p))))
        (ai:stop-workflow workflow)
        (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))))

(define-test presentation-plan-handed-off :parent agent-duet
  ;; The lead ran out while planning; the partner was handed the whole task
  ;; (%HAND-OFF-STEP), which starts without a plan or an approval.
  (multiple-value-bind (lead partner) (presented-agents)
    (let ((workflow (presented-workflow lead partner :mode :divide :approve-plan t)))
      (presented-step workflow lead :plan :state :superseded)
      (setf (ai:workflow-loops workflow)
            (list (ai::make-review-loop :author partner :reviewer lead :task "Do it" :skip-review t
                                        :state :working)))
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is eq :skipped (presented-stage-state progress :plan))
        (is eq :skipped (presented-stage-state progress :approval))
        (false (search "Approved" (format nil "~s" progress)))
        (is eq :active (presented-stage-state progress :work)))
      ;; Its work handed back to the lead is still no approved plan.
      (setf (ai::loop-author (first (ai:workflow-loops workflow))) lead)
      (is eq :skipped (presented-stage-state (ai::workflow-progress-presentation workflow) :approval)))))

(define-test presentation-plan-approved-then-handed-over :parent agent-duet
  ;; An approved lone task whose work was handed over keeps its plan and approval.
  (multiple-value-bind (lead partner) (presented-agents)
    (let ((workflow (presented-workflow lead partner :mode :divide :approve-plan t)))
      (presented-step workflow lead :plan :state :completed)
      (setf (ai:workflow-loops workflow)
            (list (ai::make-review-loop :author partner :reviewer partner :task "A" :skip-review t
                                        :state :working)))
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is eq :complete (presented-stage-state progress :plan))
        (is eq :complete (presented-stage-state progress :approval))))))

(define-test presentation-compare-takeover :parent agent-duet
  ;; The lead had no proposal and is out: the partner goes on from its own,
  ;; through the approval it waited for.
  (multiple-value-bind (lead partner) (presented-agents)
    (let ((workflow (presented-workflow lead partner :mode :compare :approve-plan t)))
      (presented-step workflow lead :propose :state :completed)
      (presented-step workflow partner :propose :state :completed)
      (setf (ai::workflow-proposals workflow) (list (cons partner "Theirs") (cons lead nil))
            (ai:workflow-loops workflow)
            (list (ai::make-review-loop :author partner :reviewer lead :task "Do it" :skip-review t
                                        :state :working)))
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is eq :skipped (presented-stage-state progress :plan))
        (is eq :complete (presented-stage-state progress :approval))))))

(define-test presentation-part-handed-over :parent agent-duet
  ;; ChatGPT's part was handed to Claude: the stages keep their ids, and the
  ;; finished part isn't counted as going on.
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner :mode :divide :max-rounds 3))
           (mine (ai::make-review-loop :author lead :reviewer partner :label "Claude's part" :state :approved
                                       :round 1))
           (theirs (ai::make-review-loop :author lead :reviewer lead :label "ChatGPT's part" :state :working
                                         :skip-review t)))
      (setf (ai:workflow-loops workflow) (list mine theirs))
      (presented-step workflow lead :work :loop theirs)
      (let* ((progress (ai::workflow-progress-presentation workflow))
             (ids (mapcar (lambda (s) (getf s :id)) (getf progress :stages))))
        (is equal ids (remove-duplicates ids))
        (is eq :complete (presented-stage-state progress :lead-part))
        (is eq :active (presented-stage-state progress :partner-part))
        (is string= "Working" (getf progress :label))
        (is = 1 (getf progress :round))
        (false (getf progress :concurrent-p))))))

(define-test presentation-confirming-review-paused :parent agent-duet
  ;; Settling open findings is part of the round reviewed: pausing it, or
  ;; stopping, doesn't move on to the next round.
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner :max-rounds 3))
           (loop (ai::make-review-loop :author lead :reviewer partner :state :reviewing :round 1)))
      (setf (ai:workflow-loops workflow) (list loop))
      (let ((step (presented-step workflow partner :review :loop loop :confirming t)))
        (is = 1 (getf (ai::workflow-progress-presentation workflow) :round))
        (setf (ai::step-state step) :paused (ai::step-reason step) :interrupted
              (ai::loop-state loop) :paused)
        (push step (ai::workflow-paused workflow))
        (let ((progress (ai::workflow-progress-presentation workflow)))
          (is = 1 (getf progress :round))
          (true (search "round 1 of 3" (getf (presented-stage progress :work) :detail))))
        (setf (ai:workflow-state workflow) :stopped)
        (is = 1 (getf (ai::workflow-progress-presentation workflow) :round))
        ;; Resumed, it's running again, still the same round.
        (setf (ai:workflow-state workflow) :running
              (ai::workflow-paused workflow) '()
              (ai::step-state step) :running
              (ai::loop-state loop) :reviewing)
        (is = 1 (getf (ai::workflow-progress-presentation workflow) :round))
        (is string= "Settling open findings" (getf (ai::workflow-agent-presentation workflow partner) :label))))))

(defun presented-restored (workflow lead partner)
  "WORKFLOW saved as its checkpoint, through JSON, and restored, as the app does."
  (ai:restore-workflow (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow)))
                       :lead lead :partner partner))

(define-test presentation-restored-handoffs-unknown :parent agent-duet
  ;; A checkpoint keeps no finished or superseded steps: restored, a lone
  ;; part taken over can't be told from a planned one, so no approval is
  ;; claimed.
  (multiple-value-bind (lead partner) (presented-agents)
    (flet ((approval (workflow)
             (presented-stage-state (ai::workflow-progress-presentation workflow) :approval)))
      ;; Divide: planning handed to the partner, then its work handed back to the lead.
      (let ((workflow (presented-workflow lead partner :mode :divide :approve-plan t)))
        (presented-step workflow lead :plan :state :superseded)
        (setf (ai:workflow-loops workflow)
              (list (ai::make-review-loop :author lead :reviewer lead :task "Do it" :skip-review t
                                          :state :working)))
        (presented-step workflow lead :work :loop (first (ai:workflow-loops workflow)) :paused t
                                            :reason :interrupted)
        (is eq :skipped (approval workflow))
        (let ((restored (presented-restored workflow lead partner)))
          (true (ai:workflow-loops restored))
          (is eq :skipped (approval restored))
          (is eq :skipped (presented-stage-state (ai::workflow-progress-presentation restored) :plan))
          (false (search "Approved" (format nil "~s" (ai::workflow-progress-presentation restored))))))
      ;; Compare: planning handed to the partner.
      (let ((workflow (presented-workflow lead partner :mode :compare :approve-plan t)))
        (presented-step workflow lead :propose :state :completed)
        (presented-step workflow partner :propose :state :completed)
        (presented-step workflow lead :plan :state :superseded)
        (setf (ai::workflow-proposals workflow) (list (cons partner "Theirs") (cons lead "Mine"))
              (ai:workflow-loops workflow)
              (list (ai::make-review-loop :author partner :reviewer lead :task "Do it" :skip-review t
                                          :state :working)))
        (presented-step workflow partner :work :loop (first (ai:workflow-loops workflow)) :paused t
                                               :reason :interrupted)
        (is eq :skipped (approval workflow))
        (let ((restored (presented-restored workflow lead partner)))
          (is eq :skipped (approval restored))
          (false (search "Approved" (format nil "~s" (ai::workflow-progress-presentation restored)))))))))

(define-test presentation-restored-split-approved :parent agent-duet
  ;; Split parts start only from an approved plan, restored or not.
  (multiple-value-bind (lead partner) (presented-agents)
    (let ((workflow (presented-workflow lead partner :mode :divide :approve-plan t)))
      (setf (ai:workflow-loops workflow)
            (list (ai::make-review-loop :author lead :reviewer partner :label "Claude's part" :task "A"
                                        :state :working)
                  (ai::make-review-loop :author partner :reviewer lead :label "ChatGPT's part" :task "B"
                                        :state :working)))
      (presented-step workflow lead :work :loop (first (ai:workflow-loops workflow)) :paused t
                                          :reason :interrupted)
      (let ((progress (ai::workflow-progress-presentation (presented-restored workflow lead partner))))
        (is eq :complete (presented-stage-state progress :plan))
        (is eq :complete (presented-stage-state progress :approval))
        (is eq :active (presented-stage-state progress :lead-part))))))

(define-test presentation-pause-reasons :parent agent-duet
  ;; Each recoverable pause says why, on the panel and on its stage.
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner))
           (loop (ai::make-review-loop :author lead :reviewer partner :state :paused))
           (step (presented-step workflow lead :work :loop loop :paused t :reason :interrupted)))
      (setf (ai:workflow-loops workflow) (list loop))
      (loop for (reason text) in '((:disconnected "disconnected")
                                   (:failed "the turn failed")
                                   (:continuation-failed "couldn't carry on from the reply")
                                   (:integration-unresolved "the merge isn't resolved")
                                   (:something-new "something-new"))
            do (setf (ai::step-reason step) reason)
               (let ((p (ai::workflow-agent-presentation workflow lead))
                     (stage (presented-stage (ai::workflow-progress-presentation workflow) :work)))
                 (is eq :paused (getf p :state) "~s" reason)
                 (true (getf p :attention-p) "~s" reason)
                 (true (search text (getf p :detail)) "~s" reason)
                 (is eq :active (getf stage :state) "~s" reason)
                 (true (getf stage :attention-p) "~s" reason)
                 (true (search text (getf stage :detail)) "~s" reason)))
      ;; The partner, with nothing of its own paused, isn't asked to act.
      (false (getf (ai::workflow-agent-presentation workflow partner) :attention-p)))))

(define-test presentation-both-agents-unavailable :parent agent-duet
  ;; Both parts out of usage: each panel says so, and the task stays paused.
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner :mode :divide))
           (mine (ai::make-review-loop :author lead :reviewer partner :label "Claude's part" :state :paused))
           (theirs (ai::make-review-loop :author partner :reviewer lead :label "ChatGPT's part" :state :paused)))
      (setf (ai:workflow-loops workflow) (list mine theirs))
      (presented-step workflow lead :work :loop mine :paused t :reason :out-of-usage)
      (presented-step workflow partner :work :loop theirs :paused t :reason :out-of-usage)
      (dolist (agent (list lead partner))
        (let ((p (ai::workflow-agent-presentation workflow agent)))
          (is eq :out-of-usage (getf p :state))
          (true (getf p :attention-p))))
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (true (search "is out of usage" (getf progress :label)))
        (is eq :active (presented-stage-state progress :lead-part))
        (is eq :active (presented-stage-state progress :partner-part))
        (is eq :pending (presented-stage-state progress :finish))))))

(define-test presentation-failed-task :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner :state :finished))
           (loop (ai::make-review-loop :author lead :reviewer partner :state :failed :round 1)))
      (setf (ai:workflow-loops workflow) (list loop)
            (ai::workflow-completion-error workflow) "Claude's turn failed")
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is string= "Failed" (getf progress :label))
        (is eq :error (presented-stage-state progress :work))
        (is string= "Failed" (getf (presented-stage progress :work) :detail))
        (is eq :error (presented-stage-state progress :finish))
        (is string= "Claude's turn failed" (getf (presented-stage progress :finish) :detail)))
      (let ((p (ai::workflow-agent-presentation workflow lead)))
        (is eq :error (getf p :state))
        (true (getf p :attention-p))
        (is string= "Claude's turn failed" (getf p :detail))))))

(define-test presentation-unreviewed-work :parent agent-duet
  ;; Work completed without review is done, never shown as approved.
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner :state :finished))
           (loop (ai::make-review-loop :author lead :reviewer partner :state :unreviewed :round 1)))
      (setf (ai:workflow-loops workflow) (list loop))
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is string= "Finished, not all reviewed" (getf progress :label))
        (is eq :complete (presented-stage-state progress :work))
        (is string= "Not reviewed" (getf (presented-stage progress :work) :detail))
        (false (search "pprove" (format nil "~s" progress))))
      (is string= "Claude's work not reviewed" (getf (ai::workflow-agent-presentation workflow lead) :detail))
      (setf (ai::loop-check-failed loop) t)
      (is string= "Not reviewed; its check still fails"
          (getf (presented-stage (ai::workflow-progress-presentation workflow) :work) :detail)))))

(define-test presentation-stopped-recoverable :parent agent-duet
  ;; Stopped is resumable later: the agent whose step was paused is told so,
  ;; and unfinished stages stay pending rather than skipped.
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner :state :stopped :mode :divide))
           (mine (ai::make-review-loop :author lead :reviewer partner :label "Claude's part" :state :paused))
           (theirs (ai::make-review-loop :author partner :reviewer lead :label "ChatGPT's part" :state :approved
                                         :round 1)))
      (setf (ai:workflow-loops workflow) (list mine theirs))
      (presented-step workflow lead :work :loop mine :paused t :reason :interrupted)
      (is string= "Its step can be recovered" (getf (ai::workflow-agent-presentation workflow lead) :detail))
      (let ((progress (ai::workflow-progress-presentation workflow)))
        (is eq :pending (presented-stage-state progress :lead-part))
        (true (search "Stopped" (getf (presented-stage progress :lead-part) :detail)))
        (is eq :complete (presented-stage-state progress :partner-part))
        (false (find :skipped (getf progress :stages) :key (lambda (s) (getf s :state)))))
      ;; Finished with that part unfinished, it's skipped, not pending.
      (setf (ai:workflow-state workflow) :finished)
      (let ((stage (presented-stage (ai::workflow-progress-presentation workflow) :lead-part)))
        (is eq :skipped (getf stage :state))
        (is string= "Not finished" (getf stage :detail))))))

(define-test presentation-restored-pause :parent agent-duet
  ;; A paused step restored from its checkpoint still asks for the user.
  (multiple-value-bind (lead partner) (presented-agents)
    (let ((workflow (presented-workflow lead partner :max-rounds 3)))
      (setf (ai:workflow-loops workflow)
            (list (ai::make-review-loop :author lead :reviewer partner :task "Do it" :state :paused :round 1)))
      (presented-step workflow lead :work :loop (first (ai:workflow-loops workflow)) :paused t
                                          :reason :disconnected)
      (let* ((restored (presented-restored workflow lead partner))
             (progress (ai::workflow-progress-presentation restored))
             (p (ai::workflow-agent-presentation restored lead)))
        (is string= "Paused" (getf progress :label))
        (is eq :active (presented-stage-state progress :work))
        (true (getf (presented-stage progress :work) :attention-p))
        (is = 2 (getf progress :round))
        (is = 3 (getf progress :max-rounds))
        (true (getf p :attention-p))
        (true (search "disconnected" (getf p :detail)))))))

(define-test presentation-direct-message-during-task :parent agent-duet
  ;; A message sent from an agent's pane during a task is said to be
  ;; answered, not hidden behind what its part of the task waits for.
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner :max-rounds 3))
           (loop (ai::make-review-loop :author lead :reviewer partner :task "Do it" :state :working)))
      (setf (ai:workflow-loops workflow) (list loop))
      (presented-step workflow lead :work :loop loop)
      ;; The lead's step is a workflow turn: someone waits on its reply.
      (setf (fake-replies lead) (list (says :hang)))
      (ai:agent-submit lead "Do it" (lambda (text error) (declare (ignore text error))))
      (is string= "Working" (getf (ai::workflow-agent-presentation workflow lead) :label))
      (is string= "Waiting to review" (getf (ai::workflow-agent-presentation workflow partner) :label))
      (setf (fake-replies partner) (list (says :hang) (says :hang) (says :hang)))
      (ai:agent-send-now partner "What do you think so far?")
      (let ((p (ai::workflow-agent-presentation workflow partner)))
        (is eq :working (getf p :state))
        (is string= "Answering your message" (getf p :label))
        (false (getf p :detail))
        (false (getf p :attention-p)))
      ;; Its review, submitted meanwhile, waits behind the message.
      (presented-step workflow partner :review :loop loop)
      (is string= "Then reviewing" (getf (ai::workflow-agent-presentation workflow partner) :detail))
      (ai:agent-turn-finished partner "Looks fine." nil)
      (is string= "Reviewing" (getf (ai::workflow-agent-presentation workflow partner) :label))
      ;; Stopping the message: it's no longer being answered.
      (ai:agent-send-now partner "And now?")
      (setf (ai::workflow-steps workflow) (remove partner (ai::workflow-steps workflow) :key #'ai::step-agent))
      (true (ai::presentation-direct-message-p partner))
      (ai:agent-cancel partner)
      (false (ai::presentation-direct-message-p partner))
      (ai:agent-turn-finished partner ai::*stopped-text* :interrupted)
      ;; After the task, stopped or finished, a message is still answered;
      ;; a failed task keeps saying it failed.
      (ai:agent-send-now partner "One more thing")
      (dolist (state '(:stopped :finished))
        (setf (ai:workflow-state workflow) state)
        (is string= "Answering your message" (getf (ai::workflow-agent-presentation workflow partner) :label)))
      (setf (ai::workflow-completion-error workflow) "The check failed")
      (is eq :error (getf (ai::workflow-agent-presentation workflow partner) :state)))))

(define-test presentation-out-of-usage-without-paused-step :parent agent-duet
  ;; Usage running out shows on an idle agent even when no step is paused
  ;; for it; the conversation-only view shows only this, not the status line.
  (multiple-value-bind (lead partner) (presented-agents)
    (let ((reset (+ (get-universal-time) 3600)))
      (ai::agent-ran-out lead reset)
      (let ((p (ai::workflow-agent-presentation nil lead)))
        (is eq :out-of-usage (getf p :state))
        (is string= "Out of usage" (getf p :label))
        (is string= (format nil "Out of usage until ~a" (ai::format-reset-time reset)) (getf p :detail)))
      (ai::agent-ran-out lead nil)
      (is string= "Out of usage" (getf (ai::workflow-agent-presentation nil lead) :detail))
      ;; Usage back: idle again.
      (ai::agent-has-usage lead)
      (is eq :idle (getf (ai::workflow-agent-presentation nil lead) :state))
      (false (getf (ai::workflow-agent-presentation nil lead) :detail))
      ;; A reset time gone by is no longer out.
      (setf (ai::agent-out-until lead) (- (get-universal-time) 60))
      (is eq :idle (getf (ai::workflow-agent-presentation nil lead) :state))
      (setf (ai::agent-out-until lead) nil)
      ;; In a task: what it waits for stays, with the usage after it.
      (let* ((workflow (presented-workflow lead partner :max-rounds 3))
             (loop (ai::make-review-loop :author lead :reviewer partner :task "Do it" :state :working)))
        (setf (ai:workflow-loops workflow) (list loop))
        (presented-step workflow lead :work :loop loop)
        (ai::agent-ran-out partner reset)
        (let ((p (ai::workflow-agent-presentation workflow partner)))
          (is eq :waiting (getf p :state))
          (is string= "Waiting to review" (getf p :label))
          (true (search "Out of usage until" (getf p :detail))))
        ;; A step paused for it says so once.
        (setf (ai::workflow-steps workflow) '())
        (presented-step workflow partner :review :loop loop :paused t :reason :out-of-usage)
        (let ((p (ai::workflow-agent-presentation workflow partner)))
          (is eq :out-of-usage (getf p :state))
          (false (search "Out of usage until" (or (getf p :detail) ""))))
        ;; Busy, it isn't idle: a turn may have started as usage came back.
        (setf (ai:agent-busy partner) t)
        (false (search "Out of usage" (or (getf (ai::workflow-agent-presentation nil partner) :detail) "")))))))
