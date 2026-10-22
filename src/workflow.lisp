;;;; workflow.lisp — the two agents working on one request together
;;;;
;;;; Four modes:
;;;;
;;;;   :review-only  One or both agents inspect existing Git changes, read-only.
;;;;
;;;;   :review   The lead does the work; the partner reviews it; the lead
;;;;             addresses the review; and so on until the partner approves
;;;;             or MAX-ROUNDS reviews have been done.
;;;;
;;;;   :divide   The lead splits the request into its part and the partner's.
;;;;             Both work at once; then each reviews the other's part, and
;;;;             each part goes round its own review loop, as in :review.
;;;;
;;;;   :compare  Both propose an approach without seeing the other's; the
;;;;             lead reconciles the two into a plan, split as in :divide.
;;;;
;;;; With APPROVE-PLAN, a plan (:divide, :compare) waits for the user, who
;;;; may edit either part, before the work starts: ON-PLAN is told, and
;;;; APPROVE-PLAN starts it.
;;;;
;;;; A split plan has an ORDER: :parallel, or :lead-first / :partner-first,
;;;; when one part builds on the other. The dependent part waits (a STEP
;;;; paused with reason :dependency) until its prerequisite is done, and then
;;;; starts with the prerequisite's report and, in worktrees, its work merged
;;;; in. Each part's work has a REVISION; a prerequisite reworked after its
;;;; dependent started has the dependent revalidate (:prerequisite-changed).
;;;; When no unfinished part can go on, the waiting step pauses with reason
;;;; :deadlock, for the user to have the order revised or the parts run
;;;; independently (RECOVER-WORKFLOW).
;;;;
;;;; With WORKTREES, a plan split in two has each part done in its own git
;;;; worktree (worktree.lisp), each agent in a new conversation there, so
;;;; the parts can't get in each other's way. Once both are done, each is
;;;; committed on its branch and the branches are merged into the folder's
;;;; current branch; the lead resolves any conflicts and reconciles both written
;;;; replies; then the partner reviews the merged result,
;;;; as another review loop. Worktrees and recovery refs stay until approval.
;;;; A failed part stops it before anything is merged. It needs a git
;;;; repository with nothing uncommitted: the worktrees start from HEAD.
;;;;
;;;; Each review lists its findings in a FINDINGS section; the loop keeps
;;;; them as a checklist (FINDING), open, fixed or dismissed, and the next
;;;; review brings each one's status up to date, so none is lost between
;;;; rounds.
;;;;
;;;; A REVIEW-LOOP is one piece of work with its author and reviewer. The
;;;; workflow's ON-MESSAGE is told of each message passed between them, for
;;;; the conversation view: (on-message from to text kind), FROM and TO
;;;; being agents, :user or :system, and KIND one of :request :proposal
;;;; :plan :work :review :findings :note :check.
;;;;
;;;; When an agent's account runs out of usage, the step it was given (a
;;;; STEP) pauses rather than fails, and ON-PAUSE is told: the user decides
;;;; whether to RESUME-STEP (once the usage resets), HAND-OFF-STEP to the
;;;; other agent, or STOP-WORKFLOW. Work handed off goes unreviewed when the
;;;; one that ran out was to review it, and says so.
;;;;
;;;; A turn that goes quiet (agents.lisp) can be retried within the
;;;; workflow: the step's prompt is given again once the interrupted turn
;;;; has ended, and its reply goes on as the first one's would have
;;;; (RETRY-STALLED-TURN). Or the task can be stopped (STOP-STALLED-TURN).
;;;;
;;;; A workflow that finished with a loop still unapproved after MAX-ROUNDS
;;;; can be carried on for more rounds (CONTINUE-WORKFLOW): the author
;;;; addresses the last review, as it would have in another round. Only
;;;; while both agents are still in the folder and conversations it
;;;; finished in: elsewhere, the review would be out of context.

(in-package #:agent-duet)

(defparameter *default-max-rounds* 3)

(defclass workflow ()
  ((started-at :initform (get-universal-time) :accessor workflow-started-at)
   (ended-at :initform nil :accessor workflow-ended-at)
   (metrics :initform nil :accessor workflow-metrics)
   (review-target :initarg :review-target :initform :uncommitted :reader workflow-review-target)
   (review-base :initarg :review-base :initform "HEAD" :reader workflow-review-base)
   (review-branch :initarg :review-branch :initform nil :reader workflow-review-branch)
   (review-count :initarg :review-count :initform 1 :reader workflow-review-count)
   (review-agents :initarg :review-agents :initform :both :reader workflow-review-agents)
   (review-description :initform nil :accessor workflow-review-description)
   (review-access :initform nil :accessor workflow-review-access)
   (completion :initform nil :accessor workflow-completion-data)
   (completion-state :initform nil :accessor workflow-completion-state)
   (completion-capture :initform nil :accessor workflow-completion-capture)
   (repository-start :initform nil :accessor workflow-repository-start)
   (completion-error :initform nil :accessor workflow-completion-error)
   (commit-hash :initform nil :accessor workflow-commit-hash)
   (check-command :initarg :check-command :initform nil :reader workflow-check-command)
   (mode :initarg :mode :initform :review :reader workflow-mode)
   (lead :initarg :lead :reader workflow-lead)
   (partner :initarg :partner :reader workflow-partner)
   (request :initarg :request :reader workflow-request)
   (max-rounds :initarg :max-rounds :initform *default-max-rounds* :accessor workflow-max-rounds)
   (on-message :initarg :on-message :initform nil :accessor workflow-on-message)
   (on-finish :initarg :on-finish :initform nil :accessor workflow-on-finish)
   (on-pause :initarg :on-pause :initform nil :accessor workflow-on-pause)
   (on-plan :initarg :on-plan :initform nil :accessor workflow-on-plan
            :documentation "Told (workflow lead-task partner-task) when a plan waits for approval")
   (approve-plan :initarg :approve-plan :initform nil :reader workflow-approve-plan-p)
   (pending-plan :initform nil :accessor workflow-pending-plan
                 :documentation "The plan waiting for approval: (:lead task :partner task :order order
:error why-it-can't-start :takeover out-agent)")
   (plan-revisions :initform 0 :accessor workflow-plan-revisions
                   :documentation "How often the lead was asked to revise a plan that couldn't start")
   (prerequisite-merges :initform nil :accessor workflow-prerequisite-merges
                        :documentation "(dependent-tree prerequisite-tree head)s: prerequisite work merged into worktrees")
   (proposals :initform '() :accessor workflow-proposals :documentation "(agent . text or nil), as they come in")
   (worktrees :initarg :worktrees :initform nil :reader workflow-worktrees-p)
   (home :initform nil :accessor workflow-home :documentation "The folder, while the agents are in worktrees")
   (base :initform nil :accessor workflow-base :documentation "The commit the worktrees started from")
   (trees :initform '() :accessor workflow-trees :documentation "(agent . worktree directory)s")
   (tree-branches :initform nil :accessor workflow-tree-branches)
   (merged-loop :initform nil :accessor workflow-merged-loop)
   (worktrees-cleaned :initform nil :accessor workflow-worktrees-cleaned)
   (recovery-recorded :initform nil :accessor workflow-recovery-recorded)
   (retention-id :initform (make-uuid) :accessor workflow-retention-id)
   (integrated :initform nil :accessor workflow-integrated :documentation "True once merging has begun")
   (stopping-checks :initform nil :accessor workflow-stopping-checks)
   (start-retry :initform nil :accessor workflow-start-retry
                :documentation "True while a dependent part waits for stopped work to stop (RETRY-WHEN-STOPPED)")
   (saved-agent-context :initform nil :accessor workflow-saved-agent-context)
   (merged-refs :initform nil :accessor workflow-merged-refs)
   (integration-journal :initform nil :accessor workflow-integration-journal)
   (shutdown-processes :initform nil :accessor workflow-shutdown-processes)
   (shutdown-pids :initform nil :accessor workflow-shutdown-pids)
   (steps :initform nil :accessor workflow-steps)
   (recovery-baseline :initform nil :accessor workflow-recovery-baseline)
   (recovery-capture :initform nil :accessor workflow-recovery-capture)
   (recovery-stale :initform nil :accessor workflow-recovery-stale
                   :documentation "Restored from a checkpoint saved before its snapshot was ready: :baseline
while the workflow's baseline is that older one, and the paused steps whose own are.")
   (integration-state :initform nil :accessor workflow-integration-state)
   (on-checkpoint :initarg :on-checkpoint :initform nil :accessor workflow-on-checkpoint)
   (paused :initform '() :accessor workflow-paused :documentation "STEPs waiting for the user")
   (loops :initform '() :accessor workflow-loops)
   (context :initform nil :accessor workflow-context
            :documentation "Each agent's folder and conversation when it finished (WORKING-CONTEXT)")
   (state :initform :new :accessor workflow-state :documentation ":new :running :finished :stopped")))

(defstruct (review-loop (:conc-name loop-))
  author reviewer
  task                                  ; what the author was asked to do
  label                                 ; how to name it in notes: nil, or "Claude's part"
  (summary "")                          ; the author's latest report
  (review-history nil)                 ; (reviewer conversation) from its last reply
  (work-history nil)                    ; (author conversation) from its last report
  (last-review nil)                     ; the reviewer's latest review
  (findings '())                        ; the review checklist: FINDINGs, by id
  (combined-result nil)                ; replies must remain full combined answers
  (file-changes nil)                    ; whether this part changed files against its base
  (tree nil)                            ; the worktree it's done in, or nil for the folder
  (check-handle nil)                    ; active asynchronous check
  (check-report nil)                    ; most recent result for the reviewer
  (check-fixes 0)
  (check-failed nil)                    ; true while the check fails after the fix attempts
  (dependencies nil)                    ; the loops it builds on, which must be done first
  (revision 0)                          ; bumped each time its work (re)starts
  (prerequisite-revisions nil)          ; (prerequisite . revision)s its work last started against
  (round 0)                             ; reviews done
  (skip-review nil)                     ; true once taken over: its reviewer ran out
  (state :assigned))                    ; :assigned :working :checking :reviewing :paused :stopped or final outcome

(defstruct (workflow-step (:conc-name step-) (:constructor make-step))
  "A prompt given to an agent for the workflow, kept so it can be given
again, or to the other agent, after the first ran out of usage."
  (id (make-uuid))
  (generation 0) (state :new) reason progress
  agent prompt original-prompt then target confirming (check-fixes 0) repositories repository-capture
  role                                  ; :propose, :plan, :replan, :work, :review, :resolve or :check
  loop)                                 ; the review loop it belongs to (not for :plan)

(defun post (workflow from to text kind)
  (when (workflow-on-message workflow)
    (funcall (workflow-on-message workflow) from to text kind)))

(defun note (workflow fmt &rest args)
  (post workflow :system nil (apply #'format nil fmt args) :note))

(defun workflow-live-p (workflow)
  (eq (workflow-state workflow) :running))

(defun submit (workflow agent prompt then &key role loop target confirming (check-fixes 0))
  "Give AGENT PROMPT; call THEN with its reply, unless the workflow has
been stopped meanwhile. Usage exhaustion, interruption and agent failure
preserve the step for explicit recovery (PAUSE-STEP)."
  (submit-step workflow (make-step :agent agent :prompt prompt :then then :role role :loop loop
                                   :target target :confirming confirming :check-fixes check-fixes)))

(defun checkpoint-changed (workflow)
  (when (workflow-on-checkpoint workflow)
    (funcall (workflow-on-checkpoint workflow) workflow)))

(defparameter *turn-metric-keys*
  '(:prompt-characters :elapsed-seconds :duration-ms :cost-usd :input-tokens :output-tokens
    :cache-read-tokens :cache-write-tokens :reasoning-tokens :total-tokens))

(defun workflow-agent-submit (workflow step agent prompt callback)
  ;; Persist an unknown entry before dispatch: a crash must not make a
  ;; partially measured task look complete. Retries can publish more than once.
  (let ((pending (make-uuid)))
    (push (list :id pending :agent (agent-id agent) :role (step-role step) :metrics (list :prompt-characters (length prompt)))
          (workflow-metrics workflow))
    (checkpoint-changed workflow)
    (agent-submit agent prompt callback nil
                  (lambda (id metrics)
                    (unless (find id (workflow-metrics workflow) :key (lambda (m) (getf m :id)) :test #'equal)
                      (setf (workflow-metrics workflow)
                            (remove pending (workflow-metrics workflow)
                                    :key (lambda (m) (getf m :id)) :test #'equal))
                      (push (list :id id :agent (agent-id agent) :role (step-role step) :metrics (list* :prompt-characters (length prompt) metrics))
                            (workflow-metrics workflow))
                      (checkpoint-changed workflow))))))

(defun task-metric-totals (workflow &optional role)
  "Known sums and their coverage, so partial measurements never look complete."
  (loop for agent in (list (workflow-lead workflow) (workflow-partner workflow))
        for records = (remove (agent-id agent)
                              (if role (remove role (workflow-metrics workflow)
                                               :key (lambda (m) (getf m :role)) :test-not #'eq)
                                  (workflow-metrics workflow))
                              :key (lambda (m) (getf m :agent)) :test-not #'eq)
        collect (list :agent (agent-name agent) :agent-id (agent-id agent) :turns (length records)
                      :incomplete-p (some (lambda (m) (getf (getf m :metrics) :incomplete-p)) records)
                      :values (loop for key in *turn-metric-keys*
                                    for values = (loop for record in records
                                                       for v = (getf (getf record :metrics) key)
                                                       when (metric-number v) collect v)
                                    collect (list key (and values (reduce #'+ values)) (length values))))))

(defun summary-metric (totals key label &optional (format-control "~d"))
  (destructuring-bind (key value count) (assoc key (getf totals :values))
    (declare (ignore key))
    (if value
        (format nil "~a ~a~a" label (format nil format-control value)
                (if (and (= count (getf totals :turns)) (not (getf totals :incomplete-p))) "" " (partial)"))
        (format nil "~a unknown" label))))

(defun summary-files-text (files known)
  (cond ((not known) "Changed files unavailable; use Changes for recorded snapshots.")
        ((null files) "No files changed.")
        (t (let* ((count (length files)) (more (max 0 (- count 10))))
             (format nil "Files: ~{~a~^, ~}~a. See Changes for the full list."
                     (subseq files 0 (min count 10))
                     (if (plusp more) (format nil ", and ~d more" more) ""))))))

(defun task-summary-lines (completion)
  "Plain text facts shared by the GTK card and headless tests."
  (append
   (list (format nil "Elapsed: ~a (includes pauses)"
                 (let ((seconds (getf completion :elapsed-seconds)))
                   (if seconds (format nil "~dm ~ds" (floor seconds 60) (mod seconds 60)) "unknown")))
         (format nil "Open findings: ~d" (getf completion :open-findings 0)))
   (loop for round in (getf completion :rounds)
         collect (format nil "~a: ~d review round~:p" (first round) (second round)))
   (loop for stage in (getf completion :stage-metrics)
         append (cons (format nil "Stage: ~a" (car stage))
                      (metric-summary-lines (cdr stage))))
   (metric-summary-lines (getf completion :metrics))))

(defun metric-summary-lines (metrics)
  (loop for totals in metrics
         append
         (list (format nil "~a: ~d submitted turn~:p · ~a · ~a · ~a · ~a"
                       (getf totals :agent) (getf totals :turns)
                       (summary-metric totals :input-tokens
                                       (if (eq (getf totals :agent-id) :claude)
                                           "Uncached input tokens" "Input tokens (including cache)"))
                       (summary-metric totals :prompt-characters "Prompt characters")
                       (summary-metric totals :output-tokens "Output tokens")
                       (summary-metric totals :cost-usd "Cost" "$~,4f"))
               (format nil "~a · ~a · ~a"
                       (summary-metric totals :duration-ms "CLI duration (ms)")
                       (summary-metric totals :cache-read-tokens "Cache read tokens")
                       (summary-metric totals :cache-write-tokens "Cache write tokens")))))

(defun submit-step (workflow step)
  ;; A per-submission generation also rejects callbacks captured before cancellation.
  (unless (step-original-prompt step) (setf (step-original-prompt step) (step-prompt step)))
  (pushnew step (workflow-steps workflow))
  (let ((generation (incf (step-generation step))))
    (when (and (step-loop step) (not (loop-prerequisites-ready-p (step-loop step))))
      (setf (step-reason step) :dependency)
      (pause-step workflow step)
      (return-from submit-step nil))
    (setf (step-state step) :running)
    (when (and (step-loop step) (eq (step-role step) :work))
      (work-started workflow (step-loop step)))
    (checkpoint-changed workflow)
    (unless (and (workflow-live-p workflow) (eq (step-state step) :running)
                 (= generation (step-generation step)))
      (return-from submit-step nil))
    (workflow-agent-submit workflow step (step-agent step) (step-prompt step)
                  (lambda (text error)
                    (when (and (workflow-live-p workflow)
                               (= generation (step-generation step))
                               (eq (step-state step) :running))
                      (if error
                          (progn
                            (setf (step-reason step)
                                  (case error (:out-of-usage :out-of-usage)
                                    (:interrupted :interrupted) (:disconnected :disconnected) (t :failed))
                                  (step-progress step)
                                  (if (plusp (length (agent-partial-text (step-agent step))))
                                      (format nil "~a~%~%Partial reply:~%~a" text (agent-partial-text (step-agent step))) text))
                            ;; Its dependents wait (PAUSE-STEP says so); they are
                            ;; invalidated only once its work is redone (WORK-STARTED).
                            (pause-step workflow step))
                          (progn
                            (setf (step-state step) :completed (step-progress step) text)
                            ;; Refresh restart state before the continuation can
                            ;; start another turn or pause. Its newer capture wins.
                            (refresh-recovery-baseline workflow nil)
                            (handler-case (funcall (step-then step) text nil)
                              (error (e)
                                (setf (step-reason step) :continuation-failed
                                      (step-progress step) (format nil "~a~%~%Continuation stopped: ~a" text e))
                                (pause-step workflow step)))
                            (checkpoint-changed workflow))))))))

;;; Carrying on past the last round

(defun working-context (workflow)
  "Each agent's folder and conversation, to tell whether they've moved on."
  (loop for agent in (list (workflow-lead workflow) (workflow-partner workflow))
        collect (list (agent-directory agent) (agent-conversation-handle agent))))

(defun continuable-loop-p (loop)
  "Stopped unapproved after a review, and not in a worktree (gone by then)."
  (and (eq (loop-state loop) :unresolved) (loop-last-review loop)
       (or (null (loop-tree loop)) (probe-file (loop-tree loop)))))

(defun workflow-continuable-p (workflow)
  "Whether WORKFLOW finished with a loop stopped at its last round, unapproved,
and the agents are still where it finished."
  (and (eq (workflow-state workflow) :finished)
       (not (eq (workflow-mode workflow) :review-only))
       (equal (workflow-context workflow) (working-context workflow))
       (some #'continuable-loop-p (workflow-loops workflow))))

(defun continue-workflow (workflow rounds)
  "Carry WORKFLOW on for up to ROUNDS more review rounds: each loop stopped
unapproved has its author address the last review. True if it carried on."
  (when (and (workflow-continuable-p workflow)
             (every #'agent-quiescent-p (list (workflow-lead workflow) (workflow-partner workflow))))
    (let ((loops (remove-if-not #'continuable-loop-p (workflow-loops workflow))))
      (setf (workflow-ended-at workflow) nil
            (workflow-state workflow) :running
            (workflow-completion-data workflow) nil
            (workflow-completion-error workflow) nil)
      (incf (workflow-max-rounds workflow) rounds)
      (note workflow "Carrying on for up to ~d more round~:p." rounds)
      ;; All are set working before any starts, so the first to finish
      ;; doesn't find every loop done.
      (when (and (workflow-trees workflow) (not (workflow-integrated workflow)))
        (dolist (entry (workflow-trees workflow)) (move-agent (car entry) (cdr entry))))
      (dolist (l loops) (setf (loop-state l) :working))
      (dolist (l loops)
        (note workflow "Round ~d~@[ (~a)~]: ~a addresses the review."
              (1+ (loop-round l)) (loop-label l) (agent-name (loop-author l)))
        (loop-work workflow l (fix-prompt l (loop-last-review l))))
      t)))

;;; Running out of usage

(defun other-agent (workflow agent)
  (if (eq agent (workflow-lead workflow)) (workflow-partner workflow) (workflow-lead workflow)))

(defun step-description (step)
  "What STEP was, for the user: \"Claude's review of ChatGPT's part\"."
  (let ((name (agent-name (step-agent step)))
        (loop (step-loop step)))
    (ecase (step-role step)
      (:check (format nil "~a’s check" name))
      (:propose (format nil "~a's proposal" name))
      (:resolve (format nil "~a's resolving of the merge conflicts" name))
      (:plan (format nil "~a's plan for splitting the work" name))
      (:replan (format nil "~a's revised plan" name))
      (:work (format nil "~a's work~@[ on ~a~]" name (and loop (loop-label loop))))
      (:review (format nil "~a's review of ~a" name
                       (or (and loop (loop-label loop))
                           (format nil "~a's work" (agent-name (loop-author loop)))))))))

(defstruct recovery-capture thread repositories ready error steps)

(defun captured-recovery-repositories (capture fallback)
  "Read a ready worker result without blocking checkpoint serialization."
  (if (and capture (recovery-capture-ready capture) (not (recovery-capture-error capture)))
      (recovery-capture-repositories capture) fallback))

(defun apply-recovery-capture (workflow capture)
  "Publish only the current capture, on the agent's delivery thread."
  (when (and (recovery-capture-ready capture) (not (recovery-capture-error capture))
             (not (eq (workflow-state workflow) :abandoned)))
    (let ((changed nil))
      (when (eq capture (workflow-recovery-capture workflow))
        (setf (workflow-recovery-baseline workflow) (recovery-capture-repositories capture)
              (workflow-recovery-capture workflow) nil
              (workflow-recovery-stale workflow) (remove :baseline (workflow-recovery-stale workflow))
              changed t))
      (dolist (entry (recovery-capture-steps capture))
        (let ((step (car entry)))
          (when (and (eq capture (step-repository-capture step))
                     (= (cdr entry) (step-generation step)) (eq (step-state step) :paused))
            (setf (step-repositories step) (recovery-capture-repositories capture)
                  (step-repository-capture step) nil
                  (workflow-recovery-stale workflow) (remove step (workflow-recovery-stale workflow))
                  changed t))))
      (when changed (checkpoint-changed workflow)))))

(defun refresh-recovery-baseline (workflow steps)
  "Capture completed-step or pause/stop state off GTK; newer captures win."
  (let ((capture (make-recovery-capture
                  :steps (mapcar (lambda (s) (cons s (step-generation s))) steps))))
    (setf (workflow-recovery-capture workflow) capture)
    (dolist (step steps) (setf (step-repository-capture step) capture))
    (setf (recovery-capture-thread capture)
          (sb-thread:make-thread
           (lambda ()
             (handler-case (setf (recovery-capture-repositories capture) (workflow-repositories workflow))
               (error (e) (setf (recovery-capture-error capture) (princ-to-string e))))
             (setf (recovery-capture-ready capture) t)
             (funcall (agent-deliver (workflow-lead workflow))
                      (lambda () (apply-recovery-capture workflow capture))))
           :name "workflow repository snapshot"))
    capture))

(defun recovery-snapshot-pending-p (workflow)
  "Whether the repository snapshot that changes are judged against may predate
the agents' last edits: one is still being taken, or the task was restored
from a checkpoint saved before its snapshot was ready."
  (let ((stale (workflow-recovery-stale workflow)))
    (and (or (member :baseline stale)
             (some (lambda (step) (member step stale)) (workflow-paused workflow))
             (some (lambda (capture) (and capture (not (recovery-capture-ready capture))))
                   (cons (workflow-recovery-capture workflow)
                         (mapcar #'step-repository-capture (workflow-paused workflow)))))
         t)))

(defun await-recovery-captures (workflow)
  "Wait only from inspection workers or headless callers, never GTK."
  (dolist (capture (remove-duplicates
                    (remove nil (cons (workflow-recovery-capture workflow)
                                      (mapcar #'step-repository-capture (workflow-paused workflow))))))
    (when (and (not (recovery-capture-ready capture)) (recovery-capture-thread capture))
      (sb-thread:join-thread (recovery-capture-thread capture)))))

(defun pause-step (workflow step)
  (when (and (null (step-progress step))
             (plusp (length (agent-partial-text (step-agent step)))))
    (setf (step-progress step) (agent-partial-text (step-agent step))))
  (setf (step-state step) :paused)
  (pushnew step (workflow-paused workflow) :test #'eq)
  (unless (step-reason step) (setf (step-reason step) :out-of-usage))
  (setf (step-repositories step) (workflow-recovery-baseline workflow))
  (refresh-recovery-baseline workflow (list step))
  (when (step-loop step)
    (update-loop-state workflow (step-loop step) :paused))
  ;; Waiting for a prerequisite is part of the plan: it starts again by
  ;; itself (CHECK-FINISHED), so the user isn't asked, unless it deadlocks.
  (let ((waiting (and (step-loop step) (waiting-step-p step))))
    (if waiting
        (note workflow "~a waits for ~{~a~^ and ~}." (step-description step)
              (mapcar #'part-name (loop-dependencies (step-loop step))))
        (progn
          (note workflow "~a paused (~a). Available progress: ~a"
                (step-description step) (step-reason step) (or (step-progress step) "No final reply yet."))
          (let ((other (other-agent workflow (step-agent step)))
                (message (format nil "~a paused (~a). Its incomplete result is unavailable; continue only independent work."
                                 (step-description step) (step-reason step))))
            (post workflow :system other message :note))
          ;; Blocked, not invalidated: they go on once it's complete.
          (dolist (dependent (workflow-loops workflow))
            (when (and (step-loop step) (member (step-loop step) (loop-dependencies dependent)))
              (note workflow "~a is blocked until ~a is complete."
                    (string-capitalize (part-name dependent) :end 1) (part-name (step-loop step)))))))
    (note-retained-worktrees workflow)
    (checkpoint-changed workflow)
    (when (and (workflow-on-pause workflow) (not waiting))
      (funcall (workflow-on-pause workflow) workflow step))))

(defun waiting-step-p (step)
  "Whether STEP is paused only to wait for its loop's prerequisites."
  (member (step-reason step) '(:dependency :prerequisite-changed)))

(defun part-name (loop)
  "LOOP for a note: \"Claude's part\", or \"Claude's work\"."
  (or (loop-label loop) (format nil "~a's work" (agent-name (loop-author loop)))))

(defun unpause (workflow step)
  "Take STEP off the paused list; true if it was on it (so each choice counts once)."
  (when (member step (workflow-paused workflow))
    (setf (workflow-paused workflow) (remove step (workflow-paused workflow)))
    t))

(defun %resume-step (workflow step)
  "Give STEP's prompt to its agent again (its usage has reset, or the user wants to try)."
  (when (and (workflow-live-p workflow) (agent-quiescent-p (step-agent step))
             (not (agent-out-of-usage-p (step-agent step))) (unpause workflow step))
    (note workflow "Trying ~a again." (step-description step))
    (when (step-loop step)
      (update-loop-state workflow (step-loop step)
                         (if (eq (step-role step) :review) :reviewing :working)))
    (submit-step workflow step)))

(defun can-hand-off-p (workflow step)
  "Whether the other agent could take STEP over (it isn't out of usage too)."
  (and (agent-quiescent-p (step-agent step))
       (agent-quiescent-p (other-agent workflow (step-agent step)))
       (not (agent-out-of-usage-p (other-agent workflow (step-agent step))))))

(defun %hand-off-step (workflow step)
  "Have the other agent take STEP over. A review isn't handed off: the work
finishes unreviewed. Nor is a proposal: the other's goes on alone. Work or a plan goes to the other agent, and what it
does then goes unreviewed, since the one that would review it is out."
  (when (and (workflow-live-p workflow) (can-hand-off-p workflow step) (unpause workflow step))
    (incf (step-generation step))
    (setf (step-state step) :superseded)
    (let* ((out (step-agent step))
           (other (other-agent workflow out))
           (loop (step-loop step)))
      (ecase (step-role step)
        (:propose
         (proposal-in workflow out nil (format nil "~a is unavailable" (agent-name out))))
        ((:resolve :replan)
         (note workflow "~a ~a instead." (agent-name other)
               (if (eq (step-role step) :resolve) "resolves the conflicts" "revises the plan"))
         (submit-step workflow (make-step :agent other :prompt (step-prompt step) :then (step-then step)
                                          :role (step-role step) :target (step-target step))))
        (:review
         (update-loop-state workflow loop :unreviewed)
         (note workflow "~@[~a ~]finished without a review: ~a is unavailable."
               (and (loop-label loop) (string-capitalize (loop-label loop) :end 1)) (agent-name out))
         (check-finished workflow))
        (:work
         (note workflow "~a takes over ~a. It won't be reviewed: ~a is unavailable."
               (agent-name other) (step-description step) (agent-name out))
         (setf (loop-author loop) other
               (loop-skip-review loop) t)
         (loop-work workflow loop (takeover-prompt out (step-prompt step) (loop-tree loop) (step-reason step))))
        (:plan
         (note workflow "~a does the whole task. It won't be reviewed: ~a is unavailable."
               (agent-name other) (agent-name out))
         (run-loop workflow (make-review-loop :author other :reviewer out :task (workflow-request workflow)
                                              :skip-review t)
                   (takeover-plan-prompt out (workflow-request workflow))))))))

;;; The prompts

(defun work-prompt (reviewer task)
  (format nil "~a~%~%(When you finish, ~a will review your work.)" task (agent-name reviewer)))

(defun full-review-prompt (loop)
  (let ((author (agent-name (loop-author loop)))
        (findings (loop-findings loop)))
    (format nil "~a was asked to do this:~%~%~a~%~%~a reports:~%~%~a~%~%~
~@[~a~]~
Review the work: read the changed files (and `git diff`, if this is a git repository) rather than ~
relying on the report. Don't change any files yourself. Look for bugs, missed requirements, and ~
anything risky or needlessly complicated.~%~%~
~:[~*~;Your earlier findings:~%~%~a~%~%Repeat each of them in your FINDINGS, its status brought up to date: ~
[fixed] once it has been dealt with, [dismissed] if the reasons given for leaving it convince you, ~
[open] if it still stands. ~]~
Be brief; put the substance in the FINDINGS lines. List ~:[~;any new ~]issues in a FINDINGS section, ~
most important first, one line each, in this form (the location is optional):~%~%~
FINDINGS:~%~
- F~d [open] `path/to/file:line` What is wrong, and what to do about it~%~%~
Write FINDINGS: NONE if there are none.~%~%~
End your reply with one line on its own, exactly one of:~%~
VERDICT: APPROVED~%~
VERDICT: CHANGES REQUESTED"
            author (loop-task loop) author (loop-summary loop)
            (let ((tree (and (loop-tree loop) (worktree-note loop))))
              (if (loop-check-report loop)
                  (format nil "~a~a~%~%" (or tree "") (loop-check-report loop))
                  tree))
            findings (findings-lines findings) findings
            (next-finding-id findings))))

(defun review-prompt (loop &key full)
  "Use a compact follow-up only while the same reviewer retains its history."
  (let* ((reviewer (loop-reviewer loop))
         (handle (agent-conversation-handle reviewer))
         (complete (full-review-prompt loop)))
    (if (and (not full) (plusp (loop-round loop)) (loop-findings loop) handle
             (equal (loop-review-history loop) (list reviewer handle)))
        (let ((prompt
                (format nil "~a reports:~%~%~a~%~%~a~@[~a~%~%~]~
Review the work again; read the changed files and git diff. Don't change files.~%~
Earlier findings:~%~a~%~
Repeat each finding with its updated status: [fixed] if dealt with, [dismissed] if the reasons convince you, [open] if it still stands. Keep existing IDs.~%~
Be brief; put the substance in the FINDINGS lines.~%~
FINDINGS:~%- F~d [open] `path/to/file:line` Issue and remedy~%~
Or FINDINGS: NONE. End with exactly VERDICT: APPROVED or VERDICT: CHANGES REQUESTED on its own line."
                        (agent-name (loop-author loop)) (loop-summary loop)
                        (if (loop-tree loop) (worktree-note loop) "")
                        (loop-check-report loop) (findings-lines (loop-findings loop))
                        (next-finding-id (loop-findings loop)))))
          (remember-context-fallback reviewer prompt complete)
          prompt)
        complete)))

(defun confirm-prompt (loop)
  (format nil "You approved, but these findings of yours are still open:~%~%~a~%~%~
Settle each of them in a FINDINGS section: [fixed] if it has been dealt with, [dismissed] if it needn't be. ~
If any still stands, keep it [open] and request changes instead.~%~%~
End your reply with one line on its own, exactly one of:~%~
VERDICT: APPROVED~%~
VERDICT: CHANGES REQUESTED"
          (findings-lines (remove :open (loop-findings loop) :key #'finding-status :test-not #'eq))))

(defparameter *split-reply-limit* 60000 "Maximum inline part reply length in combined results.")

(defun worktree-note (loop)
  "Give the reviewer the worktree and a summary against its saved starting revision."
  (let* ((tree (loop-tree loop))
         (base nil)
         (stat (handler-case
                   (progn
                     (setf base (worktree-git-line tree "config"
                                                  (worktree-base-key (worktree-branch tree))))
                     (worktree-git tree "add" "-A")
                     (worktree-git tree "diff" "--cached" "--stat" base "--"))
                 (error (e) (format nil "(The diff summary couldn't be read: ~a)" e)))))
    (format nil "~a worked in its own git worktree, ~a, so its changes aren't in your folder. ~
Read the files there and run `git diff --cached ~a --` there yourself to review all changes against where this part started.~%~%~
Diff stat:~%~a~%~%"
            (agent-name (loop-author loop)) (namestring tree) (or base "<saved base>") stat)))

(defparameter *commit-suggestion-instruction*
  "If files changed during this task, end your report with one line:
Suggested commit: <a short imperative subject for the whole task's completed changes, including earlier work; at most 72 characters>
This is only a suggestion, not an instruction to commit.")

(defparameter *commit-suggestion-reminder*
  "If files changed, end with the Suggested commit line as before, for the whole task."
  "What replaces the instruction for an author whose conversation already has it.")

(defun replace-once (text old new)
  (let ((at (search old text)))
    (if at (concatenate 'string (subseq text 0 at) new (subseq text (+ at (length old)))) text)))

(defun expand-commit-suggestion (prompt)
  "PROMPT with a reminder of the instruction replaced by the instruction."
  (if (stringp prompt) (replace-once prompt *commit-suggestion-reminder* *commit-suggestion-instruction*) prompt))

(defun with-commit-suggestion-instruction (prompt)
  "Add the reply instruction once, including when a takeover reuses a prompt."
  (let ((prompt (expand-commit-suggestion prompt)))
    (if (search *commit-suggestion-instruction* prompt)
        prompt
        (format nil "~a~%~%~a" prompt *commit-suggestion-instruction*))))

(defun author-work-prompt (loop prompt)
  "PROMPT for LOOP's author, with the instruction; only a reminder of it while
the author's conversation, which had it, goes on. The full prompt is kept for
a conversation that can't be resumed."
  (let* ((full (with-commit-suggestion-instruction prompt))
         (author (loop-author loop))
         (handle (agent-conversation-handle author)))
    (if (and handle (equal (loop-work-history loop) (list author handle)))
        (let ((compact (replace-once full *commit-suggestion-instruction* *commit-suggestion-reminder*)))
          (remember-context-fallback author compact full)
          compact)
        full)))

(defun work-done (loop text)
  "Take the author's report TEXT; its conversation now has the instruction."
  (setf (loop-summary loop) text
        (loop-work-history loop)
        (list (loop-author loop) (agent-conversation-handle (loop-author loop)))))

(defun work-reply-instruction (loop)
  (if (loop-combined-result loop)
      (with-commit-suggestion-instruction
       "Then provide the full revised combined answer to the overall request, preserving useful results from both parts; a summary of edits alone is not sufficient.")
      *commit-suggestion-instruction*))

(defun fix-prompt (loop review)
  (let* ((quoted (parse-findings review))
         (open (remove-if
                (lambda (finding)
                  (or (not (eq :open (finding-status finding)))
                      (some (lambda (entry)
                              (and (eql (finding-id finding) (getf entry :id))
                                   (eq :open (getf entry :status))
                                   (equal (finding-location finding) (getf entry :location))
                                   (equal (finding-text finding) (getf entry :text))))
                            quoted)))
                (loop-findings loop))))
    (format nil "~a reviewed your work~:[ and requested changes~;~]:~%~%~a~%~%~
~@[These findings are still open:~%~%~a~%~%~]~
Address the points you agree with, and say briefly why for any you don't. ~a"
            (agent-name (loop-reviewer loop)) (eq (review-verdict review) :approved) review
            (and open (findings-lines open))
            (work-reply-instruction loop))))

(defun plan-form (partner)
  "How the lead is to set out a plan split with PARTNER."
  (let ((p (agent-name partner)))
    (format nil "Split the work into two parts that don't edit the same files: ~
one for you and one for ~a. Reply in exactly this form:~%~%~
LEAD TASK:~%<your part>~%~%~
PARTNER TASK:~%<~a's part, complete in itself: ~a won't see this conversation>~%~%~
ORDER: PARALLEL~%~%~
Split it so both parts can be done at the same time if you can. If one part can't start without the ~
other's result, write ORDER: LEAD FIRST (yours is done first) or ORDER: PARTNER FIRST (~a's is) ~
instead; neither part can wait for the other both ways. ~
If the task is too small to split, write NONE under PARTNER TASK."
            p p p p)))

(defun revise-plan-prompt (partner problem)
  "The lead's prompt to revise a plan that couldn't start, for PROBLEM."
  (format nil "Your plan can't be started: ~a~%~%Revise it, without changing anything yet. ~a"
          problem (plan-form partner)))

(defun plan-prompt (partner request)
  (format nil "~a~%~%You are leading this task with ~a, another coding agent working in the same ~
directory at the same time. Don't change anything yet. ~a"
          request (agent-name partner) (plan-form partner)))

(defun proposal-prompt (other request)
  (format nil "~a~%~%Before anyone changes anything, you and ~a, another coding agent, are each ~
proposing how to do this, without seeing the other's proposal; the two will then be reconciled. ~
Don't change any files. Read what you need to, then propose your approach: what you would change ~
and how, in which files, and any risks or open questions. Keep it concise."
          request (agent-name other)))

(defun reconcile-prompt (partner lead-proposal partner-proposal)
  "The lead's prompt to settle on one approach, from the proposals it has (either may be nil)."
  (let ((p (agent-name partner)))
    (format nil "~:[You couldn't make a proposal, but ~a~;~a also~] proposed an approach, independently:~%~%~a~%~%~
~:[Look it over~;Compare it with yours~]. Settle on one approach: say briefly where the proposals differ, and ~
what you chose and why, taking the better ideas from each. Don't change anything yet. Then, with ~a, ~
another coding agent working in the same directory at the same time: ~a"
            lead-proposal p partner-proposal lead-proposal p (plan-form partner))))

(defun lone-plan-prompt (partner)
  "The lead's prompt to plan from its proposal alone: PARTNER had none."
  (format nil "~a couldn't make a proposal, so go with yours. Don't change anything yet. Then, with ~a, ~
another coding agent working in the same directory at the same time: ~a"
          (agent-name partner) (agent-name partner) (plan-form partner)))

(defun lead-go-prompt (partner lead-task partner-task edited)
  (format nil "~:[~*~;The user edited the plan. Your part is now:~%~%~a~%~%~]~
Go ahead with your part~:[ (LEAD TASK)~;~]. ~a is doing this part at the same time, so leave ~
its files alone:~%~%~a~%~%When you finish, ~a will review your part."
          edited lead-task edited (agent-name partner) partner-task (agent-name partner)))

(defun lead-alone-prompt (partner lead-task edited)
  (format nil "~:[Then do the whole task yourself.~*~;The user edited the plan: do this yourself.~%~%~a~]~%~%~
(When you finish, ~a will review your work.)"
          edited lead-task (agent-name partner)))

(defun partner-task-prompt (lead request lead-task partner-task)
  (let ((l (agent-name lead)))
    (format nil "~a is sharing a task with you. The overall request:~%~%~a~%~%Your part:~%~%~a~%~%~
~:[~2*No one else is working on it.~;~a is doing this part at the same time, so leave its files alone:~%~%~a~]~%~%~
When you finish, ~a will review your part."
            l request partner-task (and lead-task t) l lead-task l)))

(defun stopped-text (reason)
  "How an agent's turn ended, for REASON (STEP-REASON), as a verb phrase."
  (case reason
    (:out-of-usage "ran out of usage")
    (:failed "failed")
    (:disconnected "lost its connection")
    (:interrupted "was stopped")
    (:restart "was interrupted when the app closed")
    (t "stopped")))

(defun takeover-prompt (out prompt &optional tree (reason :out-of-usage))
  (format nil "~a ~a partway through a task, so you're taking it over. Some of its work ~
may already be in the files: look before you continue, and don't redo what's done.~@[ Its work is in ~
its own git worktree, ~a: make your changes there, not in your own folder.~] Here is what ~a was ~
asked (\"you\" there meant ~a):~%~%~a~%~%~
No one will review your work this time, since ~a is ~:[unavailable~;out of usage~]. Finish with a summary of what you changed."
          (agent-name out) (stopped-text reason) (and tree (namestring tree)) (agent-name out) (agent-name out) prompt
          (agent-name out) (eq reason :out-of-usage)))

(defun proposal-takeover-prompt (out request)
  (format nil "~a ran out of usage before your proposals could be reconciled, so carry out yours yourself, ~
all of the task:~%~%~a~%~%~
No one will review your work this time, since ~a is out of usage. Finish with a summary of what you changed."
          (agent-name out) request (agent-name out)))

(defun worktree-part-prompt (other request task other-task tree)
  "An agent's part, TASK, to do in its worktree TREE, for a new conversation there."
  (format nil "You're sharing a task with ~a, another coding agent. The overall request:~%~%~a~%~%~
Your part:~%~%~a~%~%~a is doing this part at the same time:~%~%~a~%~%~
You each have your own git worktree. Yours is ~a, on branch ~a: make your changes there, and only your part. ~
Don't commit: once both parts are done, each is committed and merged.~%~%~
When you finish, ~a will review your part."
          (agent-name other) request task (agent-name other) other-task
          (namestring tree) (worktree-branch tree) (agent-name other)))

(defun resolve-prompt (request branch files)
  (format nil "Two agents did the parts of this task each in its own git worktree, and their branches are ~
being merged into this folder. The overall request:~%~%~a~%~%~
Merging ~a conflicted in:~%~%~{- ~a~%~}~%~
Resolve the conflicts, keeping what each part meant to do. Then stage the files and conclude the merge with ~
`git commit --no-edit`. Finish with a summary of how you resolved them."
          request branch files))

(defun takeover-plan-prompt (out request)
  (format nil "~a ran out of usage before it could plan this task with you, so do all of it yourself:~%~%~a~%~%~
No one will review your work this time, since ~a is out of usage. Finish with a summary of what you changed."
          (agent-name out) request (agent-name out)))

;;; Reading replies

(defun review-verdict (text)
  "From a review's TEXT: :approved when its last verdict line says exactly
APPROVED (give or take Markdown emphasis and punctuation), otherwise
:changes. \"NOT APPROVED\", or no verdict at all, is :changes."
  (let ((at (search "VERDICT:" text :from-end t :test #'char-equal)))
    (if (and at (string-equal "APPROVED"
                              (string-trim '(#\Space #\Tab #\Return #\* #\_ #\` #\. #\! #\")
                                           (subseq text (+ at (length "VERDICT:"))
                                                   (or (position #\Newline text :start at) (length text))))))
        :approved
        :changes)))

(defun clean-section (text)
  "TEXT without surrounding space or stray Markdown emphasis."
  (string-trim '(#\Space #\Tab #\Newline #\Return #\* #\# #\_)
               text))

(defun plan-order (text)
  "An ORDER line's TEXT as an order: :parallel (also for anything unknown),
:lead-first, :partner-first, or :circular for each part first."
  (let* ((words (substitute #\Space #\- (substitute #\Space #\_ (string-upcase text))))
         (lead (search "LEAD" words))
         (partner (search "PARTNER" words)))
    (cond ((or (search "CIRCULAR" words) (search "MUTUAL" words) (search "EACH OTHER" words)
               (and lead partner (search "FIRST" words) (not (search "PARALLEL" words))
                    (search "FIRST" words :start2 (1+ (search "FIRST" words)))))
           :circular)
          ((search "PARALLEL" words) :parallel)
          ((and lead (or (null partner) (< lead partner))) :lead-first)
          (partner :partner-first)
          (t :parallel))))

(defun split-order-line (text)
  "TEXT without its last ORDER: line, and that line's order (nil without one)."
  (let ((found nil))
    (loop with start = 0
          for end = (or (position #\Newline text :start start) (length text))
          for line = (string-left-trim '(#\Space #\Tab #\* #\_ #\#) (subseq text start end))
          when (and (>= (length line) 6) (string-equal "ORDER:" line :end2 6))
            do (setf found (list start end (subseq line 6)))
          while (< end (length text))
          do (setf start (1+ end)))
    (if found
        (destructuring-bind (start end value) found
          (values (concatenate 'string (subseq text 0 start) (subseq text end))
                  (plan-order (string-trim '(#\Space #\Tab #\Return #\* #\_ #\` #\.) value))))
        (values text nil))))

(defun parse-plan (text)
  "The lead's and partner's tasks from a plan's TEXT, and its order, as three
values. The partner's is nil when there is none (NONE, or no PARTNER TASK
section). The order (PLAN-ORDER) is :parallel without an ORDER line."
  (multiple-value-bind (text order) (split-order-line text)
    (multiple-value-bind (lead partner) (parse-plan-parts text)
      (values lead partner (or order :parallel)))))

(defun parse-plan-parts (text)
  (let ((lead-at (search "LEAD TASK:" text :test #'char-equal))
        (partner-at (search "PARTNER TASK:" text :test #'char-equal)))
    (if (null partner-at)
        (values (clean-section text) nil)
        (let* ((lead (if (and lead-at (< lead-at partner-at))
                         (clean-section (subseq text (+ lead-at (length "LEAD TASK:")) partner-at))
                         (clean-section (subseq text 0 partner-at))))
               (partner (clean-section (subseq text (+ partner-at (length "PARTNER TASK:"))))))
          (values lead (unless (no-task-p partner) partner))))))

(defun no-task-p (task)
  "Whether TASK, a part from a plan, is no task: empty, or NONE."
  (let ((task (string-trim '(#\Space #\Tab #\Newline #\Return) (or task ""))))
    (or (zerop (length task))
        (and (>= (length task) 4) (string-equal "NONE" task :end2 4)))))

;;; The review checklist

(defstruct (finding (:conc-name finding-))
  id                                    ; its number: F3 is 3
  (status :open)                        ; :open :fixed :dismissed
  location                              ; "src/a.lisp:42", or nil
  (text "")
  round)                                ; the review round that raised it

(defparameter *finding-statuses*
  '(("open" . :open) ("still open" . :open) ("fixed" . :fixed) ("addressed" . :fixed) ("resolved" . :fixed)
    ("dismissed" . :dismissed) ("wontfix" . :dismissed) ("won't fix" . :dismissed)))

(defun parse-finding-line (line)
  "A finding from one LINE of a FINDINGS section, \"- F2 [open] `a.lisp:4` What's wrong\",
as a plist (:id :status :location :text); nil for a line that isn't one.
It needs an F number or a [status]; the location is optional."
  (let* ((line (string-trim '(#\Space #\Tab #\Return) line))
         (start (cond ((and (plusp (length line)) (find (char line 0) "-*+")) 1)
                      ((let ((dot (position #\. line)))
                         (and dot (plusp dot) (every #'digit-char-p (subseq line 0 dot)) (1+ dot))))
                      (t nil))))
    (when start
      (let ((rest (string-left-trim " *_" (subseq line start)))
            id status location)
        (when (and (> (length rest) 1) (char-equal (char rest 0) #\F) (digit-char-p (char rest 1)))
          (multiple-value-bind (n end) (parse-integer rest :start 1 :junk-allowed t)
            (setf id n rest (string-left-trim " *_:." (subseq rest end)))))
        (when (and (plusp (length rest)) (char= (char rest 0) #\[))
          (let ((close (position #\] rest)))
            (when close
              (setf status (cdr (assoc (string-trim " " (subseq rest 1 close)) *finding-statuses* :test #'string-equal))
                    rest (string-left-trim " *_:" (subseq rest (1+ close)))))))
        (when (and (plusp (length rest)) (char= (char rest 0) #\`))
          (let ((close (position #\` rest :start 1)))
            (when close
              (setf location (subseq rest 1 close)
                    rest (subseq rest (1+ close))))))
        (when (or id status)
          (list :id id :status (or status :open) :location location
                :text (string-trim '(#\Space #\Tab #\— #\– #\- #\: #\*) rest)))))))

(defun parse-findings (text)
  "The findings in TEXT's last FINDINGS section, as plists (PARSE-FINDING-LINE);
the second value is true if there was a section (FINDINGS: NONE is an empty one)."
  (let ((at (search "FINDINGS:" text :from-end t :test #'char-equal)))
    (if (null at)
        (values '() nil)
        (values (loop for line in (rest (uiop:split-string (subseq text at) :separator '(#\Newline)))
                      until (search "VERDICT:" line :test #'char-equal)
                      for finding = (parse-finding-line line)
                      when finding collect finding)
                t))))

(defun next-finding-id (findings)
  (1+ (reduce #'max findings :key #'finding-id :initial-value 0)))

(defun merge-findings (findings parsed round)
  "FINDINGS brought up to date by a review's PARSED findings: those it names
take its status (and location, if it gives one); the rest are new, raised in
ROUND. Those it leaves out stay as they were. A fresh list, by id."
  (let ((findings (mapcar #'copy-finding findings)))
    (dolist (p parsed)
      (let ((old (and (getf p :id) (find (getf p :id) findings :key #'finding-id))))
        (if old
            (progn (setf (finding-status old) (getf p :status))
              (when (getf p :location) (setf (finding-location old) (getf p :location)))
              (when (zerop (length (finding-text old))) (setf (finding-text old) (getf p :text))))
            (push (make-finding :id (or (getf p :id) (next-finding-id findings))
                                :status (getf p :status) :location (getf p :location)
                                :text (getf p :text) :round round)
                  findings))))
    (sort findings #'< :key #'finding-id)))

(defun findings-lines (findings)
  "FINDINGS in the form a review lists them."
  (format nil "~{~a~^~%~}"
          (loop for f in findings
                collect (format nil "- F~d [~(~a~)] ~@[`~a` ~]~a"
                                (finding-id f) (finding-status f) (finding-location f) (finding-text f)))))

(defun count-findings (findings status)
  (count status findings :key #'finding-status))

(defun checklist-findings-text (findings)
  "Group findings by status without changing their stable IDs or stored order."
  (if (null findings)
      "No review findings."
      (with-output-to-string (out)
        (loop for (status title) in '((:open "Open")
                                      (:fixed "Fixed") (:dismissed "Dismissed"))
              for group = (remove status findings :key #'finding-status :test-not #'eq)
              when group do
                (format out "**~a (~d)**~%~%" title (length group))
                (dolist (f group)
                  (format out "- ~a **F~d** ~(~a~)~@[ · `~a`~] — ~a~%"
                          (ecase status (:open "☐") (:fixed "☑") (:dismissed "☒"))
                          (finding-id f) status (finding-location f) (finding-text f)))
                (terpri out)))))

(defun checklist-text (loop)
  "LOOP's checklist, in Markdown, for the conversation."
  (let ((findings (loop-findings loop)))
    (format nil "**Task checklist~@[, ~a~]**~%~%~a Assigned to: **~a** · Progress: **~a** · Review rounds: ~d~%~%~d open, ~d fixed, ~d dismissed~%~%~a~%~%**Task**~%~%~a"
            (loop-label loop)
            (case (loop-state loop)
              ((:approved :unreviewed) "☑")
              ((:failed :unresolved :stopped) "☒")
              (otherwise "☐"))
            (agent-name (loop-author loop))
            (ecase (loop-state loop)
              (:assigned "Assigned") (:working (if (plusp (loop-round loop)) "Addressing review" "Working"))
              (:checking "Checking") (:reviewing "In review") (:approved "Approved")
              (:unresolved "Not approved") (:unreviewed "Done without review")
              (:failed "Failed") (:paused "Paused") (:stopped "Stopped"))
            (loop-round loop)
            (count-findings findings :open) (count-findings findings :fixed) (count-findings findings :dismissed)
            (checklist-findings-text findings)
            ;; Keep the full merged prompt for work and review, but not in the panel.
            ;; Quote every task line so its Markdown cannot consume progress/findings.
            (with-output-to-string (out)
              (with-input-from-string (in (if (loop-combined-result loop)
                                             "Combine both parts and review the merged result."
                                             (loop-task loop)))
                (loop for line = (read-line in nil) while line
                      do (format out "> ~a~%" line)))))))

(defun checklist-sections (text)
  "Separate a saved checklist snapshot into assignment and findings Markdown.
Accept older snapshots too, so replay uses the same grouped presentation."
  (let* ((progress-end (search (format nil "~%~%") text
                               :start2 (or (search "Assigned to:" text) 0)))
         (task-start (search (format nil "**Task**~%~%") text)))
    (if (and progress-end task-start (< progress-end task-start))
        (values (concatenate 'string (subseq text 0 progress-end)
                             (format nil "~%~%")
                             (subseq text (+ task-start (length (format nil "**Task**~%~%")))))
                (let ((body (subseq text (+ progress-end 2) task-start)))
                  (if (string= (string-trim '(#\Space #\Newline #\Return) body)
                               (format nil "0 open, 0 fixed, 0 dismissed~%~%No review findings."))
                      ""
                      (let* ((heading (subseq text 0 (or (position #\Newline text) 0)))
                             (comma (position #\, heading)))
                        (format nil "**~a**~%~%~a"
                                (if comma
                                    (subseq heading (+ comma 2) (- (length heading) 2))
                                    "Review findings")
                                body)))))
        (values text ""))))

(defun publish-checklist (workflow loop)
  "Publish a complete snapshot, retaining the same heading throughout LOOP."
  (post workflow :system (loop-author loop) (checklist-text loop) :findings))

(defun update-loop-state (workflow loop state)
  (setf (loop-state loop) state)
  (publish-checklist workflow loop)
  (checkpoint-changed workflow))

(defun take-findings (workflow loop review)
  "Bring LOOP's findings up to date from REVIEW; progress snapshots include them."
  (multiple-value-bind (parsed found) (parse-findings review)
    (when found
      (setf (loop-findings loop) (merge-findings (loop-findings loop) parsed (loop-round loop))))))

;;; Running

(defun start-workflow (workflow &key repository-start)
  (setf (workflow-ended-at workflow) nil
        (workflow-state workflow) :running
        (workflow-completion-data workflow) nil
        (workflow-completion-error workflow) nil
        (workflow-commit-hash workflow) nil
        (workflow-repository-start workflow)
        (or repository-start
            (ignore-errors (repository-start-state (agent-directory (workflow-lead workflow))))))
  (setf (workflow-recovery-baseline workflow) (workflow-repositories workflow))
  (let ((lead (workflow-lead workflow))
        (partner (workflow-partner workflow))
        (request (workflow-request workflow)))
    (post workflow :user lead request :request)
    (let ((problem (and (workflow-worktrees-p workflow) (not (eq (workflow-mode workflow) :review))
                        (worktrees-problem (agent-directory lead)))))
      (when problem
        (fail workflow "Can't give each part its own worktree: ~a." problem)
        (return-from start-workflow workflow)))
    (ecase (workflow-mode workflow)
      (:review-only
       (start-review-only workflow))
      (:review
       (note workflow "~a writes, ~a reviews (up to ~d rounds)." (agent-name lead) (agent-name partner)
             (workflow-max-rounds workflow))
       (run-loop workflow (make-review-loop :author lead :reviewer partner :task request)
                 (work-prompt partner request)))
      (:divide
       (note workflow "~a splits the work with ~a." (agent-name lead) (agent-name partner))
       (submit-plan workflow (plan-prompt partner request)))
      (:compare
       (note workflow "~a and ~a each propose an approach; then ~a reconciles them."
             (agent-name lead) (agent-name partner) (agent-name lead))
       (dolist (agent (list lead partner))
         (let ((agent agent))
           (submit workflow agent (proposal-prompt (other-agent workflow agent) request)
                   (lambda (text error)
                     (if error
                         (proposal-in workflow agent nil (format nil "its turn failed: ~a" text))
                         (proposal-in workflow agent text)))
                   :role :propose)))))
    workflow))

(defun submit-plan (workflow prompt)
  "Have the lead make a plan from PROMPT, and then go on with it (PLAN-READY)."
  (let ((lead (workflow-lead workflow)))
    (submit workflow lead prompt
            (lambda (text error)
              (if error
                  (fail workflow "~a couldn't make a plan: ~a" (agent-name lead) text)
                  (plan-ready workflow text)))
            :role :plan)))

(defun proposal-in (workflow agent text &optional why)
  "AGENT's proposal is in: TEXT, or nil for none (WHY not). Once both are,
the lead reconciles them into a plan."
  (let ((lead (workflow-lead workflow))
        (partner (workflow-partner workflow)))
    (if text
        ;; Only the lead sees the partner's; the lead's is for the user.
        (post workflow agent (if (eq agent lead) :user lead) text :proposal)
        (note workflow "~a has no proposal: ~a." (agent-name agent) why))
    (push (cons agent text) (workflow-proposals workflow))
    (when (= 2 (length (workflow-proposals workflow)))
      (let ((mine (cdr (assoc lead (workflow-proposals workflow))))
            (theirs (cdr (assoc partner (workflow-proposals workflow)))))
        (cond ((not (or mine theirs))
               (fail workflow "Neither could propose an approach."))
              ((and (null mine) (agent-out-until lead))
               ;; The lead ran out: planning with it would only pause again.
               (note workflow "~a is to do the whole task from its proposal. It won't be reviewed: ~a is out of usage."
                     (agent-name partner) (agent-name lead))
               (if (workflow-approve-plan-p workflow)
                   (await-plan workflow nil (workflow-request workflow) :takeover lead)
                   (start-takeover workflow lead (workflow-request workflow))))
              (theirs
               (note workflow "~a reconciles the proposals." (agent-name lead))
               (submit-plan workflow (reconcile-prompt partner mine theirs)))
              (t
               (note workflow "~a plans from its own proposal." (agent-name lead))
               (submit-plan workflow (lone-plan-prompt partner))))))))

(defun plan-ready (workflow plan)
  "The lead's PLAN is in: wait for the user to approve it, if asked to, or start on it."
  (post workflow (workflow-lead workflow) (workflow-partner workflow) plan :plan)
  (multiple-value-bind (lead-task partner-task order) (parse-plan plan)
    (let ((problem (and lead-task partner-task (plan-order-problem order))))
      (cond ((workflow-approve-plan-p workflow)
             (await-plan workflow lead-task partner-task :order order :error problem))
            (problem (plan-not-started workflow lead-task partner-task order problem))
            (t (let ((problem (start-parts workflow lead-task partner-task :order order)))
                 (when problem (plan-not-started workflow lead-task partner-task order problem))))))))

(defparameter *circular-order-text*
  "each part would wait for the other, so neither could start. Choose which part goes first, or have them done independently.")

(defun plan-order-problem (order)
  "Why a plan in ORDER can't start, or nil."
  (case order
    ((nil :parallel :lead-first :partner-first) nil)
    (:circular *circular-order-text*)
    (t (format nil "~a isn't an order." order))))

(defun plan-not-started (workflow lead-task partner-task order problem)
  "A plan, not to wait for approval, couldn't start for PROBLEM: the lead is
asked once to revise it; after that, it waits for the user after all."
  (cond ((< (workflow-plan-revisions workflow) 1)
         (incf (workflow-plan-revisions workflow))
         (note workflow "The plan can't start: ~a ~a revises it." problem (agent-name (workflow-lead workflow)))
         (submit-plan workflow (revise-plan-prompt (workflow-partner workflow) problem)))
        (t (note workflow "The revised plan can't start either: ~a" problem)
           (await-plan workflow lead-task partner-task :order order :error problem))))

(defun pending-plan-order (workflow)
  "The order of WORKFLOW's pending plan: :parallel, :lead-first or :partner-first."
  (let ((order (getf (workflow-pending-plan workflow) :order)))
    (if (member order '(:lead-first :partner-first)) order :parallel)))

(defun pending-plan-error (workflow)
  "Why WORKFLOW's pending plan can't start as it is, or nil."
  (getf (workflow-pending-plan workflow) :error))

(export '(pending-plan-order pending-plan-error))

(defun await-plan (workflow lead-task partner-task &key takeover order error)
  "Hold the plan for the user's approval (APPROVE-PLAN). TAKEOVER: the
partner is to do it all, as that agent, the lead, is out of usage. ORDER:
its order; ERROR, why it can't start as it is (a circular order shows as
:parallel, with the ERROR)."
  (setf (workflow-pending-plan workflow)
        (list :lead lead-task :partner partner-task :takeover takeover
              :order (if (member order '(:lead-first :partner-first)) order :parallel) :error error))
  (checkpoint-changed workflow)
  (note workflow "Waiting for you to approve the plan.~@[ It can't start as it is: ~a~]" error)
  (when (workflow-on-plan workflow)
    (funcall (workflow-on-plan workflow) workflow lead-task partner-task)))

(defun start-takeover (workflow out task)
  "The partner does TASK alone from its proposal, unreviewed: OUT, the lead, is out of usage."
  (run-loop workflow (make-review-loop :author (workflow-partner workflow) :reviewer out :task task :skip-review t)
            (proposal-takeover-prompt out task)))

(defun blank-to-nil (text)
  (let ((text (and text (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
    (and text (plusp (length text)) text)))

(defun approve-plan (workflow lead-task partner-task &key order)
  "Start on WORKFLOW's pending plan with the tasks as the user left them; a
blank one is no part for that agent. ORDER: :parallel, :lead-first or
:partner-first, by default the plan's. True if it started: false when nothing
is pending, or both are blank, or it can't start in that order (then the
plan stays pending, saying why: PENDING-PLAN-ERROR)."
  (let ((lead-task (blank-to-nil lead-task))
        (partner-task (blank-to-nil partner-task)))
    (when (and (workflow-live-p workflow) (workflow-pending-plan workflow) (or lead-task partner-task))
      (let ((pending (workflow-pending-plan workflow)))
        (destructuring-bind (&key lead partner takeover &allow-other-keys) pending
          (let ((edited (not (and (equal lead-task lead) (equal partner-task partner))))
                (order (or order (getf pending :order) :parallel)))
            (flet ((refuse (problem)
                     (setf (workflow-pending-plan workflow)
                           (list* :error problem
                                  :order (if (member order '(:lead-first :partner-first)) order :parallel)
                                  (loop for (key value) on pending by #'cddr
                                        unless (member key '(:error :order)) append (list key value))))
                     (note workflow "The plan can't start: ~a" problem)
                     (checkpoint-changed workflow)
                     (return-from approve-plan nil)))
              (let ((problem (and lead-task partner-task (plan-order-problem order))))
                (when problem (refuse problem)))
              (setf (workflow-pending-plan workflow) nil)
              ;; Given a part after all, the lead takes its chances with its usage.
              (if (and takeover (null lead-task))
                  (progn (note workflow (if edited "You edited the plan." "You approved the plan."))
                         (start-takeover workflow takeover partner-task))
                  (let ((problem (start-parts workflow lead-task partner-task :edited edited :order order
                                                                               :approved t)))
                    (when problem (refuse problem))))
              t)))))))

(defun order-assignments (order mine theirs)
  "The (loop . prerequisites) edges for ORDER between the lead's part MINE and the partner's THEIRS."
  (ecase order
    ((nil :parallel) (list (list mine) (list theirs)))
    (:lead-first (list (list mine) (list theirs mine)))
    (:partner-first (list (list mine theirs) (list theirs)))
    (:circular (list (list mine theirs) (list theirs mine)))))

(defun order-note (workflow loop)
  "What LOOP's author is to know of the order: what it builds on, or what builds on it."
  (let ((before (loop-dependencies loop))
        (after (remove-if-not (lambda (l) (member loop (loop-dependencies l))) (workflow-loops workflow))))
    (cond (before
           (format nil "Your part builds on ~{~a~^ and ~}: it starts once that is done, and you're given its result~
~:[~;, merged into your worktree~]."
                   (mapcar #'part-name before) (loop-tree loop)))
          (after
           (format nil "~{~a~^ and ~} build~:[s~;~] on yours: ~:*~:[it starts~;they start~] once yours is done."
                   (mapcar #'part-name after) (rest after))))))

(defun with-order-note (workflow loop prompt)
  (format nil "~a~@[~%~%~a~]" prompt (order-note workflow loop)))

(defun start-parts (workflow lead-task partner-task &key edited order approved)
  "Start the work on a plan: a loop for each agent with a part (LEAD-TASK,
PARTNER-TASK, nil for none), in ORDER (:parallel, :lead-first or
:partner-first). EDITED: the user changed the plan, so the lead, who made
it, is told its part again. APPROVED: the user approved it. Returns nil, or
why it couldn't start in that order: then nothing has started."
  (let ((lead (workflow-lead workflow))
        (partner (workflow-partner workflow))
        (request (workflow-request workflow)))
    (cond ((null partner-task)
           (when approved (note workflow (if edited "You edited the plan." "You approved the plan.")))
           (note workflow "~a is doing it alone; ~a will review." (agent-name lead) (agent-name partner))
           (run-loop workflow (make-review-loop :author lead :reviewer partner :task (if edited lead-task request))
                     (lead-alone-prompt partner lead-task edited)))
          ((null lead-task)
           (when approved (note workflow (if edited "You edited the plan." "You approved the plan.")))
           (note workflow "~a is doing it alone; ~a will review." (agent-name partner) (agent-name lead))
           (run-loop workflow (make-review-loop :author partner :reviewer lead :task partner-task)
                     (partner-task-prompt lead request nil partner-task)))
          (t
           (let ((mine (make-review-loop :author lead :reviewer partner :task lead-task
                                         :label (format nil "~a's part" (agent-name lead))))
                 (theirs (make-review-loop :author partner :reviewer lead :task partner-task
                                           :label (format nil "~a's part" (agent-name partner))))
                 (old (workflow-loops workflow)))
             ;; Both are registered before either starts, so the first to
             ;; finish doesn't find every loop done; and their order is set
             ;; before either starts, or nothing starts.
             (setf (workflow-loops workflow) (list mine theirs))
             (handler-case (set-workflow-dependencies workflow (order-assignments order mine theirs))
               (error (e)
                 (setf (workflow-loops workflow) old)
                 (return-from start-parts (princ-to-string e))))
             (when approved (note workflow (if edited "You edited the plan." "You approved the plan.")))
             (when (loop-dependencies theirs) (note workflow "~a's part goes first." (agent-name lead)))
             (when (loop-dependencies mine) (note workflow "~a's part goes first." (agent-name partner)))
             (publish-checklist workflow mine)
             (publish-checklist workflow theirs)
             (when (and (workflow-worktrees-p workflow) (not (open-worktrees workflow)))
               (return-from start-parts nil))
             (when (workflow-worktrees-p workflow)
               (setf (loop-tree mine) (cdr (assoc lead (workflow-trees workflow)))
                     (loop-tree theirs) (cdr (assoc partner (workflow-trees workflow)))))
             (let ((starts
                     (if (workflow-worktrees-p workflow)
                         (list (cons mine (worktree-part-prompt partner request lead-task partner-task (loop-tree mine)))
                               (cons theirs (worktree-part-prompt lead request partner-task lead-task (loop-tree theirs))))
                         (list (cons mine (lead-go-prompt partner lead-task partner-task edited))
                               (cons theirs (partner-task-prompt lead request lead-task partner-task))))))
               ;; A dependent part first: it waits, while its prerequisite
               ;; (which may finish at once) goes ahead.
               (dolist (entry (stable-sort starts #'> :key (lambda (e) (length (loop-dependencies (car e))))))
                 (loop-work workflow (car entry) (with-order-note workflow (car entry) (cdr entry))))))))
    nil))

(defun run-loop (workflow loop prompt)
  "Start LOOP: its author works on PROMPT, and then the reviewing begins."
  (setf (workflow-loops workflow) (append (workflow-loops workflow) (list loop)))
  (publish-checklist workflow loop)
  (loop-work workflow loop prompt))

(defun loop-work (workflow loop prompt &key prepare)
  "LOOP's author works on PROMPT. PREPARE: it starts as a dependent part
does (START-DEPENDENT-STEP), given its ready prerequisites' work; one not
ready waits for it in any case."
  (setf (loop-check-report loop) nil
        (loop-check-failed loop) nil)
  (update-loop-state workflow loop :working)
  (let* ((prompt (author-work-prompt loop prompt))
         (step (make-step :agent (loop-author loop) :prompt prompt :role :work :loop loop
                         :then (lambda (text error)
                                 (cond (error
                                        (loop-failed workflow loop (format nil "~a's turn failed: ~a"
                                                                           (agent-name (loop-author loop)) text)))
                                       (t
                                        (work-done loop text)
                                        (post workflow (loop-author loop) (loop-reviewer loop) text :work)
                                        (loop-check workflow loop)))))))
    (if (and prepare (loop-dependencies loop) (loop-prerequisites-ready-p loop))
        (progn (setf (step-reason step) :dependency)
               (push step (workflow-steps workflow))
               (start-dependent-step workflow step))
        (submit-step workflow step))))

(defun loop-checked (workflow loop)
  "The checks are done with: review the work, or, taken over, finish it unreviewed."
  (if (loop-skip-review loop)
      (progn (update-loop-state workflow loop :unreviewed)
        (note workflow "~@[~a ~]done, not reviewed~:[~;; its check still fails~]."
              (and (loop-label loop) (string-capitalize (loop-label loop) :end 1))
              (loop-check-failed loop))
        (check-finished workflow))
      (loop-review workflow loop)))

(defun check-in (workflow loop token command fixes code output timed-out)
  "Accept only the current check's result, delivered on the workflow thread."
  (when (and (workflow-live-p workflow) (eq token (loop-check-handle loop)))
    (let ((log-path (second token)))
      (setf (loop-check-handle loop) nil)
      (let* ((passed (and (not timed-out) (eql code 0)))
             (status (cond (timed-out
                            (format nil "Check `~a` failed (exit code ~a; timed out)." command code))
                           ((null code) (format nil "Check `~a` couldn't run (no exit code)." command))
                           (t (format nil "Check `~a` ~a (exit code ~a)."
                                      command (if passed "passed" "failed") code))))
             (report (format nil "~a~%~%~a~@[~%Failure log (up to last 4 MiB; subject to read permissions): ~a~]"
                             status (check-output-block (check-output-tail output)) log-path)))
        ;; Users see all results, but successful output adds no information
        ;; to the review. Failures stay explicit even after the fix budget,
        ;; and the work can't be approved until the check passes.
        (setf (loop-check-report loop)
              (if passed status
                  (format nil "The check still fails after ~d fix attempt~:p, so the work can't be approved ~
until it passes: request changes, saying what needs fixing.~%~%~a"
                          fixes report))
              (loop-check-failed loop) (not passed))
        (post workflow :system (loop-author loop) report :check)
        (when (workflow-live-p workflow)
          (if (or passed (>= fixes *check-fix-attempts*))
              (loop-checked workflow loop)
              (progn
                (update-loop-state workflow loop :working)
                (submit workflow (loop-author loop)
                        (author-work-prompt
                         loop (format nil "~a~%~%Fix the failing check. ~a" report (work-reply-instruction loop)))
                        (lambda (text error)
                          (if error
                              (loop-failed workflow loop (format nil "Check fix failed: ~a" text))
                              (progn
                                (work-done loop text)
                                (post workflow (loop-author loop) (loop-reviewer loop) text :work)
                                (loop-check workflow loop (1+ fixes)))))
                        :role :work :loop loop :check-fixes (1+ fixes)))))))))

(defun loop-check (workflow loop &optional (fixes 0))
  "Check the current work before review; FIXES does not count review rounds."
  (setf (loop-check-fixes loop) fixes)
  ;; Work on a prerequisite merge that isn't concluded can't be checked or reviewed.
  (let ((problem (prerequisite-merge-problem workflow loop)))
    (when problem
      (return-from loop-check (prerequisite-unmerged workflow loop problem fixes))))
  (let ((command (blank-to-nil (workflow-check-command workflow))))
    (if (null command)
        (loop-checked workflow loop)
        (let ((token (list nil nil)))
          (setf (loop-check-handle loop) token)
          (update-loop-state workflow loop :checking)
          (post workflow :system (loop-author loop)
                (format nil "Running check~@[ (~a)~]: `~a`" (loop-label loop) command) :check)
          ;; Install a token before launching: immediate delivery must not
          ;; overwrite a newer check's handle when RUN-CHECK returns.
          (when (and (workflow-live-p workflow) (eq token (loop-check-handle loop)))
            (setf (car token)
                  (run-check command (or (loop-tree loop) (agent-directory (loop-author loop)))
                             (agent-deliver (loop-author loop))
                             (lambda (code output timed-out)
                               (check-in workflow loop token command fixes code output timed-out))
                             (lambda (path) (setf (second token) path))))
            ;; Also cover cancellation between launching and storing the handle.
            (unless (and (workflow-live-p workflow) (eq token (loop-check-handle loop)))
              (when (car token) (pushnew (car token) (workflow-stopping-checks workflow)))
              (kill-check (car token))))))))

(defun cancel-workflow-checks (workflow)
  (dolist (loop (workflow-loops workflow))
    (let ((token (loop-check-handle loop)))
      (setf (loop-check-handle loop) nil)
      (when token
        (when (car token) (pushnew (car token) (workflow-stopping-checks workflow)))
        (kill-check (car token))))))

(defun loop-review (workflow loop &optional confirming)
  "Have LOOP's reviewer review the work; CONFIRMING: it approved with findings
still open, and is asked once to settle them (that's no new round)."
  (update-loop-state workflow loop :reviewing)
  (submit workflow (loop-reviewer loop) (if confirming (confirm-prompt loop) (review-prompt loop))
          (lambda (text error)
            (if error
                (loop-failed workflow loop (format nil "~a's review failed: ~a" (agent-name (loop-reviewer loop)) text))
                (review-in workflow loop text confirming)))
          :role :review :loop loop :confirming confirming))

(defun review-in (workflow loop text confirming)
  (unless confirming (incf (loop-round loop)))
  (setf (loop-review-history loop)
        (list (loop-reviewer loop) (agent-conversation-handle (loop-reviewer loop)))
        (loop-last-review loop) text)
  (post workflow (loop-reviewer loop) (loop-author loop) text :review)
  (take-findings workflow loop text)
  (let ((open (count-findings (loop-findings loop) :open)))
    (cond ((and (eq (review-verdict text) :approved) (loop-check-failed loop))
           ;; Approval means the check passes too; the user can continue it.
           (update-loop-state workflow loop :unresolved)
           (note workflow "~a approved ~a, but its check still fails, so it isn't approved."
                 (agent-name (loop-reviewer loop)) (or (loop-label loop) "the work"))
           (check-finished workflow))
          ((and (eq (review-verdict text) :approved) (plusp open) (not confirming))
           (note workflow "~a approved with ~d finding~:p still open; asking it to settle them."
                 (agent-name (loop-reviewer loop)) open)
           (loop-review workflow loop t))
          ((and (eq (review-verdict text) :approved) (plusp open))
           ;; Asked once already: not approved, but no more asking either.
           ;; The user can continue it (CONTINUE-WORKFLOW).
           (update-loop-state workflow loop :unresolved)
           (note workflow "~a approved ~a but left ~d finding~:p open, so it isn't approved."
                 (agent-name (loop-reviewer loop)) (or (loop-label loop) "the work") open)
           (check-finished workflow))
          ((eq (review-verdict text) :approved)
           (update-loop-state workflow loop :approved)
           (note workflow "~a approved ~a." (agent-name (loop-reviewer loop)) (or (loop-label loop) "the work"))
           (check-finished workflow))
          ((>= (loop-round loop) (workflow-max-rounds workflow))
           (update-loop-state workflow loop :unresolved)
           (note workflow "Stopping ~a after ~d review round~:p without approval."
                 (or (loop-label loop) "the review") (loop-round loop))
           (check-finished workflow))
          (t
           (note workflow "Round ~d~@[ (~a)~]: ~a addresses the review."
                 (1+ (loop-round loop)) (loop-label loop) (agent-name (loop-author loop)))
           (loop-work workflow loop (fix-prompt loop text))))))

(defun loop-failed (workflow loop why)
  (setf (workflow-completion-error workflow) why)
  (update-loop-state workflow loop :failed)
  (note workflow "~a" why)
  (check-finished workflow))

(defun loop-outcome (loop)
  "How LOOP ended, in words: \"approved\", \"not approved\"..."
  (case (loop-state loop)
    (:approved "approved")
    (:unresolved "not approved")
    (:unreviewed "not reviewed")
    (t "failed")))

(defun loop-done-p (loop)
  (member (loop-state loop) '(:approved :unresolved :unreviewed :failed)))

(defun check-finished (workflow)
  ;; Steps waiting for prerequisites now done start by themselves.
  (dolist (step (copy-list (workflow-paused workflow)))
    (when (and (workflow-live-p workflow) (member step (workflow-paused workflow))
               (waiting-step-p step) (step-loop step)
               (loop-prerequisites-ready-p (step-loop step)))
      (start-dependent-step workflow step)))
  (detect-deadlock workflow)
  (when (and (workflow-live-p workflow) (null (workflow-paused workflow))
             (every #'loop-done-p (workflow-loops workflow)))
    (let ((failed (find :failed (workflow-loops workflow) :key #'loop-state)))
      (cond ((or (null (workflow-trees workflow)) (workflow-integrated workflow))
             (finish-workflow workflow))
            ;; Its work may be half done: keep it out of the folder.
            (failed
             (fail workflow "~a failed, so the parts aren't merged." (string-capitalize (loop-label failed) :end 1)))
            ((or (some #'loop-check-failed (workflow-loops workflow))
                 (some (lambda (l) (eq (loop-state l) :unresolved)) (workflow-loops workflow)))
             (note workflow "The parts are not approved; their worktrees are kept and nothing is merged. Continue for more review rounds.")
             (finish-workflow workflow))
            (t (integrate workflow))))))

(defun finish-workflow (workflow)
  (cancel-workflow-checks workflow)
  (restore-review-access workflow)
  (let ((merged (workflow-merged-loop workflow)))
    (when (and merged (eq (loop-state merged) :approved))
      (cleanup-workflow-worktrees workflow)))
  (when (and (workflow-trees workflow) (not (workflow-integrated workflow)))
    (leave-worktrees workflow))
  (setf (workflow-state workflow) :finished
        (workflow-context workflow) (working-context workflow))
  (checkpoint-changed workflow)
  (note workflow "Done: ~{~a~^; ~}."
        (loop for l in (workflow-loops workflow)
              collect (format nil "~@[~a ~]~a~[~:;, ~:*~d finding~:p open~]" (loop-label l)
                              (loop-outcome l) (count-findings (loop-findings l) :open))))
  (note-retained-worktrees workflow)
  (capture-workflow-completion workflow)
  (when (workflow-on-finish workflow)
    (funcall (workflow-on-finish workflow) workflow)))

(defun fail (workflow fmt &rest args)
  (cancel-workflow-checks workflow)
  (restore-review-access workflow)
  (setf (workflow-completion-error workflow) (apply #'format nil fmt args))
  (apply #'note workflow fmt args)
  (leave-worktrees workflow)
  (setf (workflow-state workflow) :finished)
  (checkpoint-changed workflow)
  (note-retained-worktrees workflow)
  (capture-workflow-completion workflow)
  (when (workflow-on-finish workflow)
    (funcall (workflow-on-finish workflow) workflow)))

(defun pause-workflow-agent (workflow agent)
  "Interrupt one agent, preserving its steps; independent work can continue."
  (unless (and (workflow-live-p workflow)
               (member agent (list (workflow-lead workflow) (workflow-partner workflow))))
    (return-from pause-workflow-agent nil))
  (dolist (step (workflow-steps workflow))
    (when (and (eq agent (step-agent step)) (eq (step-state step) :running))
      (incf (step-generation step))
      (setf (step-reason step) :interrupted)
      (pause-step workflow step)))
  (dolist (l (workflow-loops workflow))
    (when (and (eq agent (loop-author l)) (eq (loop-state l) :checking))
      (let ((token (loop-check-handle l))
            (s (make-step :agent agent :role :check :loop l :reason :interrupted
                          :prompt (workflow-check-command workflow))))
        (setf (loop-check-handle l) nil)
        (when token
          (when (car token) (pushnew (car token) (workflow-stopping-checks workflow)))
          (kill-check (car token)))
        (push s (workflow-steps workflow))
        (pause-step workflow s))))
  (agent-cancel agent)
  (checkpoint-changed workflow)
  t)

(defun stop-workflow (workflow)
  "Stop both agents but keep all progress and pending steps recoverable."
  (when (member (workflow-state workflow) '(:running :paused))
    (pause-workflow-agent workflow (workflow-lead workflow))
    (pause-workflow-agent workflow (workflow-partner workflow))
    (cancel-workflow-checks workflow)
    ;; PAUSE-WORKFLOW-AGENT has captured newly interrupted steps. Preserve
    ;; older pause baselines so user edits made before Stop remain visible.
    (let ((missing
            (remove-if-not
             (lambda (step)
               (let ((capture (step-repository-capture step)))
                 (or (and capture (recovery-capture-ready capture) (recovery-capture-error capture))
                     (not (or capture (step-repositories step))))))
             (workflow-paused workflow))))
      (when missing (refresh-recovery-baseline workflow missing)))
    (setf (workflow-state workflow) :stopped)
    (dolist (loop (workflow-loops workflow))
      (when (eq (loop-state loop) :checking) (setf (loop-state loop) :paused)))
    (restore-review-access workflow)
    (leave-worktrees workflow)
    (capture-workflow-completion workflow)
    (note-retained-worktrees workflow)
    (note workflow "Stopped; partial work and recovery state are preserved.")
    (checkpoint-changed workflow)
    (when (workflow-on-finish workflow)
      (funcall (workflow-on-finish workflow) workflow))))

;;; A turn gone quiet

(defun workflow-agent-p (workflow agent)
  "Whether WORKFLOW (or nil) is running with AGENT in it."
  (and workflow (workflow-live-p workflow)
       (member agent (list (workflow-lead workflow) (workflow-partner workflow)))
       t))

(defun retry-stalled-turn (workflow agent)
  "Interrupt AGENT's quiet turn and give it the same prompt once that turn
has ended (AGENT-RETRY-TURN); within WORKFLOW (or nil), its reply carries
the step on. True if there was a turn to retry."
  (when (agent-retry-turn agent)
    (when (workflow-agent-p workflow agent)
      (note workflow "~a's turn was quiet too long: interrupting it, to give the same prompt again once it has ended."
            (agent-name agent)))
    t))

(defun stop-stalled-turn (workflow agent)
  "Stop the task AGENT's quiet turn is part of, if any (WORKFLOW, or nil),
and end the turn, even if it doesn't answer an interrupt (AGENT-END-TURN)."
  (when (workflow-agent-p workflow agent)
    (stop-workflow workflow))
  (agent-end-turn agent))

;;; What a task changed: the folder's state as it starts and once it has
;;; ended, as snapshots (snapshot.lisp)

(defvar *task-snapshot* 'snapshot-folder
  "Takes (directory label); returns a snapshot, or nil outside a git repository with a commit.")
(defvar *task-snapshot-delete* 'delete-snapshot "Takes (directory snapshot).")
(defvar *task-snapshot-prune* 'prune-snapshots "Takes (directory).")

(defstruct (task-changes (:constructor %make-task-changes))
  workflow directory label
  start                                 ; the snapshot as it started, or nil
  end                                   ; the snapshot as it last ended, or nil
  (final nil))                          ; nil while it runs, :pending until its agents
                                        ; have stopped, then :taken or :failed

(defun task-snapshot (task suffix)
  "A snapshot of TASK's folder, labelled with SUFFIX; nil and why on failure."
  (handler-case
      (values (funcall *task-snapshot* (task-changes-directory task)
                       (format nil "~a-~a" (task-changes-label task) suffix)))
    (error (e) (values nil (princ-to-string e)))))

(defun begin-task-changes (workflow directory)
  "Record DIRECTORY as WORKFLOW starts. Returns the TASK-CHANGES, and why its
start couldn't be recorded, if it failed (outside a git repository, it
just has no start)."
  (let ((task (%make-task-changes :workflow workflow :directory directory
                                  :label (format nil "task-~d-~a" (get-universal-time)
                                                 (subseq (make-uuid) 0 8)))))
    (multiple-value-bind (start why) (task-snapshot task "start")
      (setf (task-changes-start task) start)
      (values task why))))

(defun end-task-changes (task)
  "TASK's workflow has ended (finished, failed or stopped): its end is to be
recorded once its agents have stopped too (FINISH-TASK-CHANGES)."
  (when (task-changes-start task)
    (setf (task-changes-final task) :pending)))

(defun task-agents-idle-p (task)
  (let ((workflow (task-changes-workflow task)))
    (notany #'agent-busy (list (workflow-lead workflow) (workflow-partner workflow)))))

(defun finish-task-changes (task)
  "Record TASK's end if it's waiting to be and nothing of the task is still
running: a stopped agent may write until its turn has ended. Returns
:taken; :failed and why; or nil when it isn't time. A new end replaces
the one from before a continuation only once it's taken."
  (when (and (eq (task-changes-final task) :pending)
             (not (workflow-live-p (task-changes-workflow task)))
             (task-agents-idle-p task))
    (multiple-value-bind (end why) (task-snapshot task (format nil "end-~a" (subseq (make-uuid) 0 8)))
      (let ((directory (task-changes-directory task))
            (old (task-changes-end task)))
        (cond (end
               (setf (task-changes-end task) end
                     (task-changes-final task) :taken)
               (when old (ignore-errors (funcall *task-snapshot-delete* directory old)))
               (ignore-errors (funcall *task-snapshot-prune* directory))
               :taken)
              (t
               (setf (task-changes-final task) :failed)
               (values :failed (or why "the folder is no longer a git repository with a commit"))))))))

(defun task-changes-range (task)
  "What TASK's changes are, as (values kind start end), KIND being
:unrecorded (no start), :live (still running or ending: from START to
now), :final (from START to END), or :failed (its end couldn't be recorded)."
  (cond ((null (task-changes-start task)) :unrecorded)
        ((or (workflow-live-p (task-changes-workflow task))
             (member (task-changes-final task) '(nil :pending)))
         (values :live (task-changes-start task) nil))
        ((eq (task-changes-final task) :failed) :failed)
        (t (values :final (task-changes-start task) (task-changes-end task)))))

;;; Separate worktrees

(defun worktrees-problem (directory)
  "Why the parts can't each have a worktree of DIRECTORY, or nil."
  (cond ((not (and directory (git-repository-p directory)))
         "the folder isn't a git repository")
        ((not (ignore-errors (worktree-git-line directory "rev-parse" "HEAD")))
         "the repository has no commits yet")
        ((plusp (length (ignore-errors (worktree-git directory "status" "--porcelain"))))
         "the folder has uncommitted changes, which the worktrees wouldn't have: commit or stash them first")))

(defun move-agent (agent directory)
  "AGENT, in a new conversation, working in DIRECTORY."
  (agent-new-conversation agent)
  (setf (agent-directory agent) directory))

(defun open-worktrees (workflow)
  "A worktree for each agent, each in a new conversation there. True if
made; otherwise the workflow fails."
  (let ((home (agent-directory (workflow-lead workflow))))
    (setf (workflow-home workflow) home)
    (handler-case
        (progn
          (setf (workflow-base workflow) (worktree-git-line home "rev-parse" "HEAD"))
          (dolist (agent (list (workflow-lead workflow) (workflow-partner workflow)))
            (let ((tree (create-worktree home (format nil "~(~a~)-~a" (agent-id agent) (subseq (make-uuid) 0 8)))))
              (push (cons agent tree) (workflow-trees workflow))
              (push (cons tree (worktree-branch tree)) (workflow-tree-branches workflow))
              (move-agent agent tree)
              (note workflow "~a works in its own worktree, ~a (branch ~a), in a new conversation."
                    (agent-name agent) (namestring tree) (worktree-branch tree))))
          (setf (workflow-recovery-baseline workflow) (workflow-repositories workflow))
          t)
      (error (e)
        (fail workflow "Couldn't make the worktrees: ~a" e)
        nil))))

(defun leave-worktrees (workflow)
  "Ending before the parts are merged: the agents go back to the folder,
and the worktrees are kept, with the work done in them."
  (when (and (workflow-trees workflow) (workflow-home workflow)
             (some (lambda (entry) (not (equal (agent-directory (car entry)) (workflow-home workflow))))
                   (workflow-trees workflow)))
    (dolist (entry (workflow-trees workflow))
      (unless (equal (agent-directory (car entry)) (workflow-home workflow))
        (move-agent (car entry) (workflow-home workflow))))
    (note workflow "The worktrees are kept, with their work: ~{~a~^, ~}."
          (mapcar (lambda (e) (namestring (cdr e))) (reverse (workflow-trees workflow))))))

(defun integrate (workflow)
  "Both parts are done: commit each on its branch, bring the agents back
to the folder, and merge the branches into it."
  (unless (and (null (workflow-paused workflow))
               (every (lambda (l) (and (member (loop-state l) '(:approved :unreviewed))
                                       (not (loop-check-failed l))))
                      (workflow-loops workflow))
               (every #'agent-quiescent-p (list (workflow-lead workflow) (workflow-partner workflow))))
    (return-from integrate nil))
  ;; A part done against an earlier version of its prerequisite is
  ;; revalidated first, never merged as it is.
  (let ((stale (remove-if-not #'loop-stale-p (workflow-loops workflow))))
    (when stale
      (dolist (l stale)
        (note workflow "~a was done against an earlier version of what it builds on, so it isn't merged yet."
              (string-capitalize (part-name l) :end 1))
        (revalidate-loop workflow l (car (find-if (lambda (entry) (/= (cdr entry) (loop-revision (car entry))))
                                                   (loop-prerequisite-revisions l)))))
      (check-finished workflow)
      (return-from integrate nil)))
  (setf (workflow-integrated workflow) t
        (workflow-integration-state workflow) :committing)
  (checkpoint-changed workflow)
  (let ((home (workflow-home workflow))
        (trees (mapcar #'cdr (reverse (workflow-trees workflow)))))
    (dolist (entry (workflow-trees workflow))
      (move-agent (car entry) home))
    (handler-case
        (progn
          (dolist (l (workflow-loops workflow))
            (when (loop-tree l)
              (setf (loop-file-changes l) (plusp (length (worktree-diff (loop-tree l)))))
              (commit-worktree (loop-tree l) (format nil "~a~%~%~a" (string-capitalize (loop-label l) :end 1)
                                                     (loop-task l)))
              (retain-worktree home (loop-tree l) (workflow-retention-id workflow))
              (setf (workflow-recovery-recorded workflow) t)
              (let ((entry (cons (namestring (loop-tree l))
                                 (worktree-git-line (loop-tree l) "rev-parse" "HEAD"))))
                (setf (workflow-integration-journal workflow)
                      (acons (car entry) (cdr entry)
                             (remove (car entry) (workflow-integration-journal workflow) :key #'car :test #'equal))))
              (checkpoint-changed workflow)))
          (note workflow "Both parts are committed on their branches; recovery refs are kept for task ~a. Merging them into the folder."
                (workflow-retention-id workflow))
          (setf (workflow-integration-state workflow) :merging)
          (checkpoint-changed workflow)
          (merge-parts workflow trees))
      (error (e) (integration-failed workflow (princ-to-string e) trees)))))

(defun merge-in-progress-p (directory)
  (or (ignore-errors (worktree-git directory "rev-parse" "-q" "--verify" "MERGE_HEAD") t)
      (plusp (length (ignore-errors (worktree-git directory "diff" "--name-only" "--diff-filter=U"))))))

(defun branch-merged-p (directory branch)
  "Whether BRANCH's commits are all in DIRECTORY's HEAD."
  (ignore-errors (worktree-git directory "merge-base" "--is-ancestor" branch "HEAD") t))

(defun merge-parts (workflow trees)
  "Merge TREES' branches into the folder one by one, the lead resolving any
conflicts; then the merged result is reviewed."
  (let ((home (workflow-home workflow)))
    (checkpoint-changed workflow)
    (if (null trees)
        (progn (setf (workflow-integration-state workflow) :merged)
               (checkpoint-changed workflow)
               (merged workflow))
        (let ((branch (or (cdr (assoc (namestring (first trees)) (workflow-integration-journal workflow) :test #'equal))
                          (cdr (assoc (first trees) (workflow-tree-branches workflow) :test #'equal))
                          (worktree-branch (first trees)))))
          (multiple-value-bind (status files)
              (handler-case (if (branch-merged-p home branch) :merged
                                  (merge-recovery-ref home branch))
                (error (e) (values :error (princ-to-string e))))
            (case status
              (:merged
               (pushnew branch (workflow-merged-refs workflow) :test #'equal)
               (checkpoint-changed workflow)
               (note workflow "Merged ~a." (or (cdr (assoc (first trees) (workflow-tree-branches workflow) :test #'equal)) branch))
               (merge-parts workflow (rest trees)))
              (:conflict
               (note workflow "Merging ~a conflicts in ~{~a~^, ~}; ~a resolves them."
                     branch files (agent-name (workflow-lead workflow)))
               (submit workflow (workflow-lead workflow) (resolve-prompt (workflow-request workflow) branch files)
                       (lambda (text error)
                         (cond (error (integration-failed workflow (format nil "resolving the conflicts failed: ~a" text) trees))
                               ((merge-in-progress-p home)
                                (integration-failed workflow "the merge is still unresolved" trees))
                               ;; Aborted, say, rather than concluded.
                               ((not (branch-merged-p home branch))
                                (integration-failed workflow (format nil "~a wasn't merged" branch) trees))
                               (t
                                (pushnew branch (workflow-merged-refs workflow) :test #'equal)
                                (checkpoint-changed workflow)
                                (post workflow (workflow-lead workflow) :user text :work)
                                (note workflow "Merged ~a." (or (cdr (assoc (first trees) (workflow-tree-branches workflow) :test #'equal)) branch))
                                (merge-parts workflow (rest trees)))))
                       :role :resolve))
              (t (integration-failed workflow files trees))))))))

(defun long-part-reply (workflow reply)
  "Use a retained reply only when both receiving agents have unrestricted reads."
  (let ((path (and (every (lambda (agent) (eq :full (agent-permission agent)))
                          (list (workflow-lead workflow) (workflow-partner workflow)))
                   (retain-task-artifact "reply" :text reply))))
    (if path
        (format nil "Reply excerpt (read the full reply before combining results):~%~a~%Full reply: ~a"
                (subseq reply 0 (min 1200 (length reply))) path)
        (format nil "~a~%[Final reply cut short; full reply retained in the part's conversation.]"
                (subseq reply 0 *split-reply-limit*)))))

(defun split-results-text (workflow)
  "Keep both parts' written results, including parts that changed no files."
  (format nil "~{~a~^~%~%~}"
          (loop for part in (workflow-loops workflow)
                when (loop-tree part)
                  collect (format nil "~a (~a):~%Task: ~a~%Final reply:~%~a"
                                  (loop-label part)
                                  (if (loop-file-changes part) "file changes" "no file changes")
                                  (loop-task part)
                                  (let ((reply (loop-summary part)))
                                    (if (> (length reply) *split-reply-limit*)
                                        (long-part-reply workflow reply)
                                        reply))))))

(defun note-retained-worktrees (workflow)
  "Make recovery locations visible when integration ends without cleanup."
  (when (and (workflow-recovery-recorded workflow) (workflow-trees workflow)
             (not (workflow-worktrees-cleaned workflow)))
    (note workflow "Split-task worktrees and branches are kept: ~{~a~^, ~}. Recovery task ID: ~a; recorded recovery refs remain under refs/agent-duet/tasks/~a/."
          (loop for entry in (reverse (workflow-trees workflow))
                for tree = (cdr entry)
                collect (format nil "~a (branch ~a)" (namestring tree)
                                (or (cdr (assoc tree (workflow-tree-branches workflow) :test #'equal))
                                    "unknown")))
          (workflow-retention-id workflow) (workflow-retention-id workflow))))

(defun cleanup-workflow-worktrees (workflow)
  "Release split work only after the merged result has been approved.
Worktrees intentionally stay through review for inspection; recovery refs also
protect committed results if a worktree or branch is removed outside the workflow."
  (let ((cleaned t)
        (home (workflow-home workflow)))
    (dolist (entry (workflow-trees workflow))
      (let* ((tree (cdr entry))
             (branch (cdr (assoc tree (workflow-tree-branches workflow) :test #'equal))))
        (handler-case
            (if (probe-file tree)
                (remove-worktree home tree :delete-branch t)
                (progn
                  ;; Git can remove a registered worktree whose folder is missing.
                  ;; It may already have been unregistered. Branch deletion below
                  ;; still fails safely if Git retains a locked registration.
                  (ignore-errors (worktree-git home "worktree" "remove" (string-right-trim "/" (namestring tree))))
                  (when (and branch (plusp (length (worktree-git home "branch" "--list" branch))))
                    (worktree-git home "branch" "-D" branch))))
          (error (e)
            (setf cleaned nil)
            (note workflow "Couldn't remove the worktree ~a: ~a" (namestring tree) e)))))
    (when cleaned
      (handler-case
          (progn
            (delete-worktree-recovery-refs home (workflow-retention-id workflow))
            (setf (workflow-worktrees-cleaned workflow) t))
        (error (e) (note workflow "Couldn't remove recovery refs for task ~a: ~a"
                         (workflow-retention-id workflow) e))))))

(defun merged (workflow)
  "Reconcile the written results as well as the merged files, then review both."
  (let* ((base (workflow-base workflow))
         (lead (workflow-lead workflow))
         (results (split-results-text workflow))
         (task (format nil "Combine the two parts of this task, each done in its own git worktree. ~
The overall request:~%~%~a~%~%Both parts' final replies follow. Review their written results as well as any files, ~
including parts with no file changes. A text-only task is complete with a combined answer; ~
a nonempty Git diff is not required.~%~%~a~%~%~
Both branches are merged. `git diff ~a` shows any file changes."
                       (workflow-request workflow) results base))
         (l (make-review-loop :author lead :reviewer (workflow-partner workflow)
                              :label "the merged result" :combined-result t :task task :summary results)))
    (setf (workflow-loops workflow) (append (workflow-loops workflow) (list l))
          (workflow-merged-loop workflow) l)
    (publish-checklist workflow l)
    (note workflow "~a combines both parts' written results before the merged-result review."
          (agent-name lead))
    (loop-work workflow l
               (format nil "~a~%~%Produce one combined answer to the overall request, preserving the useful ~
results from both parts and resolving any disagreement or overlap. For text-only work, write the ~
answer in your final reply; do not create files just to produce a diff. Summarize any merged file ~
changes where relevant. The partner will review this combined answer and the merged files."
                       task))))

(defun integration-failed (workflow why trees)
  "Merging stopped at TREES (the first not merged): say so, and end. The
worktrees are kept, with their branches."
  (setf (workflow-integration-state workflow) :interrupted)
  (checkpoint-changed workflow)
  (fail workflow "Merging the parts stopped: ~a. The folder is as the merge left it~@[, with ~a not merged yet~]; ~
the worktrees are kept: ~{~a~^, ~}."
        why (and trees (ignore-errors (worktree-branch (first trees))))
        (mapcar (lambda (e) (namestring (cdr e))) (reverse (workflow-trees workflow)))))

;;; Completion cards and explicit user commits. The callback remains (WORKFLOW).
(defun capture-workflow-completion (workflow)
  (unless (workflow-ended-at workflow)
    (setf (workflow-ended-at workflow) (get-universal-time)))
  (let* ((loops (workflow-loops workflow))
         (final (or (and (workflow-merged-loop workflow)
                        (list (workflow-merged-loop workflow))) loops))
         (outcome (cond ((eq (workflow-state workflow) :stopped) :stopped)
                        ((or (workflow-completion-error workflow)
                             (some (lambda (l) (eq (loop-state l) :failed)) loops)
                             (null final)) :failed)
                        ((every (lambda (l) (eq (loop-state l) :approved)) final) :approved)
                        ((some (lambda (l) (eq (loop-state l) :unresolved)) final) :unresolved)
                        (t :unreviewed))))
    ;; Capture immediately, but never run diffs or hash file contents on the
    ;; caller's GTK thread. Readers join this worker off the UI thread.
    (let ((directory (or (workflow-home workflow)
                         (agent-directory (workflow-lead workflow)))))
      (setf (workflow-completion-state workflow) nil
            (workflow-completion-capture workflow)
            (sb-thread:make-thread
             (lambda () (ignore-errors (repository-state directory)))
             :name "completion repository capture")))
    (setf (workflow-completion-data workflow)
          (list :outcome outcome :review-status outcome
                :elapsed-seconds (and (workflow-started-at workflow)
                                      (max 0 (- (workflow-ended-at workflow) (workflow-started-at workflow))))
                :metrics (task-metric-totals workflow)
                :stage-metrics (loop for role in (remove-duplicates
                                                  (mapcar (lambda (m) (getf m :role))
                                                          (workflow-metrics workflow)))
                                     when role collect (cons role (task-metric-totals workflow role)))
                :rounds (mapcar (lambda (l) (list (or (loop-label l) "Work") (loop-round l))) loops)
                :open-findings (reduce #'+ loops :key (lambda (l) (count-findings (loop-findings l) :open))
                                       :initial-value 0)
                :summary (or (workflow-completion-error workflow)
                             (let ((summaries (remove-if
                                               (lambda (text) (or (null text) (zerop (length text))))
                                               (mapcar #'loop-summary final))))
                               (if summaries (format nil "~{~a~^~%~%~}" summaries)
                                   (if (eq outcome :stopped) "Work was stopped."
                                       "No work summary was returned."))))
                :check-status (cond ((some #'loop-check-failed loops) :failed)
                                    ((some #'loop-check-report loops) :passed)
                                    (t :not-run))))))

(defun await-completion-capture (workflow)
  "Resolve the completion snapshot. Call from a worker, never the GTK thread."
  (let ((capture (workflow-completion-capture workflow)))
    (when capture
      (let ((state (sb-thread:join-thread capture)))
        ;; A continued task may already have scheduled a newer capture.
        (when (eq capture (workflow-completion-capture workflow))
          (setf (workflow-completion-state workflow) state
                (workflow-completion-capture workflow) nil))))))

(defun workflow-commit-reason (workflow)
  "NIL when eligible; otherwise a user-facing reason. Always rechecks Git."
  (await-completion-capture workflow)
  (let* ((start (workflow-repository-start workflow))
         (end (workflow-completion-state workflow))
         (root (getf end :root)))
    (cond
      ((not (eq (workflow-state workflow) :finished)) "The work is not finished.")
      ((workflow-commit-hash workflow) "These changes have already been committed.")
      ((not (eq (getf (workflow-completion-data workflow) :outcome) :approved))
       "The work has not been approved.")
      ((eq (getf (workflow-completion-data workflow) :check-status) :failed)
       "The configured checks failed.")
      ((or (agent-busy (workflow-lead workflow)) (agent-busy (workflow-partner workflow)))
       "An agent is still working.")
      ((or (null start) (null end)) "No supported Git repository was captured.")
      ((not (equal (getf start :root) root)) "The repository changed during the work.")
      ((repository-operation-p root) "Finish the repository's merge, rebase or other Git operation first.")
      ((not (equal end (repository-state root)))
       "The repository changed after completion. Review the current changes before committing.")
      ((zerop (length (getf end :status))) "There are no uncommitted changes.")
      ((and (getf start :tree)
            (null (getf (prepare-repository-commit root :start start) :changed)))
       "This task changed no files. Earlier uncommitted changes remain in the folder.")
      (t nil))))

(defun workflow-completion (workflow)
  "Completion plist for the UI, or NIL before completion. Eligibility is live.
Keys: :OUTCOME, :SUMMARY, :REVIEW-STATUS, :CHECK-STATUS, :REPOSITORY,
:CAN-COMMIT, :COMMIT-REASON, :CLEAN and :COMMIT-HASH. Does not stage or commit."
  (when (workflow-completion-data workflow)
    (let ((reason (handler-case (workflow-commit-reason workflow)
                    (error (e) (format nil "Cannot inspect the repository: ~a" e)))))
      (append (let ((data (copy-list (workflow-completion-data workflow))))
                (setf (getf data :summary) (report-without-commit-suggestion (getf data :summary)))
                data)
              (list :repository (getf (workflow-completion-state workflow) :root)
                    :can-commit (null reason) :commit-reason reason
                    :clean (let ((root (getf (workflow-completion-state workflow) :root)))
                             (and root (ignore-errors
                                         (zerop (length (worktree-git root "status" "--porcelain=v1"
                                                                      "--untracked-files=all"))))))
                    :commit-hash (workflow-commit-hash workflow))))))

(defun commit-subject (text)
  "One plain, bounded subject from an agent's completed-work report."
  (let* ((line (first (uiop:split-string (or text "") :separator '(#\Newline #\Return))))
         (subject (string-trim '(#\Space #\Tab #\* #\_ #\` #\# #\> #\- #\") (or line ""))))
    (when (plusp (length subject))
      (if (> (length subject) 72)
          (concatenate 'string (string-right-trim " " (subseq subject 0 71)) "…")
          subject))))

(defun suggested-commit-line (line)
  "Return the metadata value, or NIL for an ordinary report line."
  (let ((plain (string-left-trim '(#\Space #\Tab #\* #\# #\>) line))
        (prefix "Suggested commit:"))
    (when (and (<= (length prefix) (length plain))
               (string-equal prefix plain :end2 (length prefix)))
      (subseq plain (length prefix)))))

(defun report-without-commit-suggestion (report)
  "Hide commit metadata in the completion card, retaining the original report."
  (when report
    (string-trim '(#\Space #\Tab #\Newline #\Return)
                 (format nil "~{~a~^~%~}"
                         (remove-if #'suggested-commit-line
                                    (uiop:split-string report :separator '(#\Newline)))))))

(defun report-commit-suggestion (report)
  "Prefer the author's explicit suggestion; tolerate older reports without one."
  (let ((lines (uiop:split-string (or report "") :separator '(#\Newline #\Return))))
    (or (loop for line in (reverse lines)
              for suggestion = (suggested-commit-line line)
              thereis (and suggestion (commit-subject suggestion)))
        (loop for line in lines
              for subject = (commit-subject line)
              when (and subject (>= (length subject) 12)
                        (not (find #\: subject))
                        (not (member subject '("Implemented the request" "No work summary was returned."
                                               "Work was stopped.") :test #'string-equal)))
                return subject))))

(defun default-commit-message (workflow)
  "Suggest a subject from completed work, never from the user's request."
  (let* ((final (or (and (workflow-merged-loop workflow) (list (workflow-merged-loop workflow)))
                    (workflow-loops workflow)))
         (suggestions (remove nil (mapcar (lambda (loop) (report-commit-suggestion (loop-summary loop))) final))))
    (or (and suggestions (commit-subject (format nil "~{~a~^; ~}" suggestions)))
        (report-commit-suggestion (getf (workflow-completion-data workflow) :summary))
        "Update completed workflow changes")))

(defun commit-index-git (root index &rest arguments)
  "Use an isolated index, preserving the user's identity and hook configuration."
  (multiple-value-bind (out err code)
      (run-git-program (append (list "env" (format nil "GIT_INDEX_FILE=~a" index)
                                      "git" "--literal-pathspecs" "-C" (namestring root)) arguments)
                       :output :string :error-output :string :ignore-error-status t)
    (unless (zerop code) (error "Git failed: ~a" err))
    (string-right-trim '(#\Newline #\Return) out)))

(defun call-with-commit-index (root function)
  (let ((index (merge-pathnames (format nil "duet-commit-~a" (make-uuid))
                                (uiop:pathname-directory-pathname
                                 (pathname (worktree-git-line root "rev-parse" "--path-format=absolute" "--git-path" "index"))))))
    (unwind-protect (funcall function index)
      (when (probe-file index) (delete-file index))
      (let ((lock (pathname (concatenate 'string (namestring index) ".lock"))))
        (when (probe-file lock) (delete-file lock))))))

(defun prepare-repository-commit (root &key start (paths nil paths-p))
  "Build an exact preview without changing staging. PATHS selects whole files."
  (when (repository-operation-p root) (error "Finish the current Git operation first."))
  (let* ((state (repository-state root))
         (root (getf state :root))
         (snapshot (or (snapshot-folder root (concatenate 'string "commit-preview-" (make-uuid)))
                       (snapshot-unborn-folder root)))
         (all (repository-changed-paths root snapshot))
         (dirty (getf start :dirty-paths))
         (changed (if (getf start :tree)
                      (git-path-list root "diff" "--name-only" "--no-renames" "-z"
                                     (getf start :tree) (snapshot-working-tree snapshot) "--")
                      all))
         (selected (if paths-p paths
                       (if start (set-difference (intersection all changed :test #'equal) dirty :test #'equal) all))))
    (when (snapshot-label snapshot) (delete-snapshot root snapshot))
    (unless (equal state (repository-state root))
      (error "The repository changed while preparing the preview. Reopen it."))
    (unless (every (lambda (p) (member p all :test #'equal)) selected)
      (error "The file selection is no longer current."))
    (call-with-commit-index
     root (lambda (index)
            (commit-index-git root index "read-tree" (getf state :head))
            (when selected
              (apply #'commit-index-git root index
                     (append (list "restore" "--source" (snapshot-working-tree snapshot) "--staged" "--") selected)))
            (list :root root :state state :paths selected :all all :dirty dirty :changed changed
                  :tree (commit-index-git root index "write-tree")
                  :diff (commit-index-git root index "diff" "--cached" "--no-ext-diff" "--no-textconv"
                                          "--no-color" "--binary" (getf state :head) "--"))))))

(defun commit-repository-preview (preview message)
  "Commit the preview, rejecting stale contents and preserving unrelated staging."
  (let* ((root (getf preview :root)) (paths (getf preview :paths))
         (index-path (pathname (worktree-git-line root "rev-parse" "--path-format=absolute" "--git-path" "index")))
         (lock (pathname (concatenate 'string (namestring index-path) ".lock")))
         (locked nil))
    (unless (and paths (plusp (length (string-trim '(#\Space #\Tab #\Newline) message))))
      (error "Select files and give the commit a message."))
    (unwind-protect
         (progn
           ;; Hold Git's index lock throughout the commit and reconciliation.
           ;; Other Git writers cannot stage edits that our update would lose.
           (with-open-file (out lock :direction :output :if-exists :error :if-does-not-exist :create)
             (declare (ignore out)))
           (setf locked t)
           (unless (equal (getf preview :state) (repository-state root))
             (error "The repository changed since preview. Reopen the preview."))
           (when (repository-operation-p root) (error "Finish the current Git operation first."))
           (if (probe-file index-path) (uiop:copy-file index-path lock)
               (call-with-commit-index root
                 (lambda (empty-index)
                   (commit-index-git root empty-index "read-tree" "--empty")
                   (uiop:copy-file empty-index lock))))
           (call-with-commit-index
            root (lambda (index)
                   (commit-index-git root index "read-tree" (getf preview :tree))
                   (commit-index-git root index "commit" "-m" message)))
           ;; Reconcile only selected paths in a copy of the original index.
           (apply #'commit-index-git root lock (append '("reset" "-q" "HEAD" "--") paths))
           (sb-posix:rename (namestring lock) (namestring index-path))
           (worktree-git-line root "rev-parse" "HEAD"))
      (when (and locked (probe-file lock)) (delete-file lock)))))

(defun commit-workflow-changes (workflow &key message preview)
  "Explicit user action; commit the reviewed selection if the task is still eligible."
  (handler-case
      (let ((reason (workflow-commit-reason workflow)))
        (when reason
          (return-from commit-workflow-changes
            (list :status (if (and (not (workflow-commit-hash workflow))
                                         (getf (workflow-completion workflow) :clean)) :clean :error) :message reason)))
        (let* ((root (getf (workflow-completion-state workflow) :root))
               (preview (or preview (prepare-repository-commit root :start (workflow-repository-start workflow))))
               (hash (progn
                       (unless (and (equal root (getf preview :root))
                                    (equal (workflow-completion-state workflow) (getf preview :state)))
                         (error "The preview does not match this task's completed repository."))
                       (commit-repository-preview preview (or message (default-commit-message workflow))))))
          (setf (workflow-commit-hash workflow) hash)
          (list :status :committed :commit hash
                :message (format nil "Committed changes (~a)." (subseq hash 0 12)))))
    (error (e) (list :status :error :message (format nil "Could not commit changes: ~a" e)))))

;;; Review existing work without an author turn or a fix loop.

(defun restore-review-access (workflow)
  (dolist (entry (workflow-review-access workflow))
    (setf (agent-read-only-p (car entry)) (cdr entry)))
  (setf (workflow-review-access workflow) nil))

(defun review-commit (directory ref)
  ;; Resolve before constructing a range; user text never becomes a Git option.
  (unless (blank-to-nil ref) (error "Choose a branch or commit."))
  (worktree-git-line directory "rev-parse" "--verify" "--end-of-options"
                     (concatenate 'string ref "^{commit}")))

(defun review-target-description (workflow)
  (let* ((directory (agent-directory (workflow-lead workflow)))
         (head (review-commit directory "HEAD")))
    (ecase (workflow-review-target workflow)
      (:uncommitted
       (format nil "Review all uncommitted changes against ~a (staged, unstaged, and untracked files). Use git diff ~a -- and git ls-files --others --exclude-standard; read untracked files too. Do not stage anything." head head))
      ((:branch :branch-tips)
       (let* ((base (review-commit directory (workflow-review-base workflow)))
              (branch (review-commit directory (workflow-review-branch workflow)))
              (start (if (eq (workflow-review-target workflow) :branch)
                         (worktree-git-line directory "merge-base" base branch) base)))
         (format nil "Review the changes from ~a to ~a (~a comparison). Use git diff ~a ~a -- and git show to read the target files; the working tree may be on a different branch."
                 start branch (if (eq (workflow-review-target workflow) :branch) "merge-base" "branch tips") start branch)))
      (:commits
       (let ((count (workflow-review-count workflow)))
         (unless (and (integerp count) (plusp count)) (error "Commit count must be a positive integer."))
         (let* ((history (uiop:split-string
                          (worktree-git-line directory "rev-list" "--first-parent"
                                             (format nil "--max-count=~d" (1+ count)) head)
                          :separator '(#\Newline)))
                (base (if (> (length history) count) (nth count history)
                          (progn
                            (unless (= (length history) count)
                              (error "The branch has fewer than ~d commits." count))
                            (worktree-git-line directory "hash-object" "-t" "tree" "/dev/null")))))
           (format nil "Review the last ~d first-parent commits, the changes from ~a to ~a. Use git diff ~a ~a -- and git log --first-parent -n ~d ~a."
                   count base head base head count head)))))))

(defun start-review-only (workflow)
  (handler-case
      (let* ((target (review-target-description workflow))
             (agents (ecase (workflow-review-agents workflow)
                       (:lead (list (workflow-lead workflow)))
                       (:partner (list (workflow-partner workflow)))
                       (:both (list (workflow-lead workflow) (workflow-partner workflow)))))
             (loops (mapcar (lambda (agent)
                              (make-review-loop :author (other-agent workflow agent) :reviewer agent
                                                :task (workflow-request workflow) :state :reviewing
                                                :label (format nil "~a's review" (agent-name agent)))) agents)))
        (setf (workflow-loops workflow) loops
              (workflow-review-description workflow) target)
        ;; Register all loops and lock all reviewers before submitting: fake agents
        ;; and cached failures may call back synchronously.
        (dolist (agent agents)
          (push (cons agent (agent-read-only-p agent)) (workflow-review-access workflow))
          (setf (agent-read-only-p agent) t))
        (dolist (loop loops)
          (let ((loop loop))
            (publish-checklist workflow loop)
            (submit workflow (loop-reviewer loop)
                    (format nil "Review only. Do not modify files or run commands that write. ~a~%~%Review focus: ~a~%~%Report bugs and missed requirements with locations. List findings in this form:~%FINDINGS:~%- F1 [open] `path:line` Problem and suggested fix~%Write FINDINGS: NONE if there are none. End with VERDICT: APPROVED or VERDICT: CHANGES REQUESTED."
                            target (workflow-request workflow))
                    (lambda (text error)
                      (if error
                          (loop-failed workflow loop (format nil "Review failed: ~a" text))
                          (progn
                            (incf (loop-round loop))
                            (setf (loop-last-review loop) text
                                  (loop-summary loop) text)
                            (post workflow (loop-reviewer loop) :user text :review)
                            (take-findings workflow loop text)
                            (update-loop-state workflow loop
                                               (if (and (eq (review-verdict text) :approved)
                                                        (zerop (count-findings (loop-findings loop) :open)))
                                                   :approved :unresolved))
                            (check-finished workflow))))
                    :role :review :loop loop))))
    (error (e) (fail workflow "Can't start review: ~a" e))))

(defun review-fix-request (workflow)
  "A standalone writing task, retaining reviewer identity and local finding IDs."
  (when (eq (workflow-mode workflow) :review-only)
    (let ((sections
            (loop for loop in (workflow-loops workflow)
                  for open = (remove-if-not (lambda (f) (eq (finding-status f) :open)) (loop-findings loop))
                  when open collect (format nil "~a:~%~a" (loop-label loop) (findings-lines open)))))
      (when sections
        (format nil "Fix these review findings. Inspect the current repository and address the findings you agree with; explain any you dismiss. The reviewed target was:~%~a~%~%Original focus: ~a~%~%~{~a~^~%~%~}"
                (workflow-review-description workflow) (workflow-request workflow) sections)))))

;;; Durable recovery. Checkpoints are data, never serialized executable closures.
(export '(workflow-checkpoint restore-workflow inspect-workflow-recovery recover-workflow
          pause-workflow-agent workflow-on-checkpoint inspect-recovery-repository
          workflow-paused step-id step-role step-reason step-progress step-prompt step-agent step-loop
          agent-quiescent-p))

(defun recovery-null (value) (unless (member value '(nil :null :false)) value))
(defun recovery-keyword (value &optional default)
  (if (stringp value) (intern (string-upcase value) :keyword) default))
(defun recovery-path (value)
  (let ((value (recovery-null value)))
    (when value (uiop:ensure-directory-pathname (uiop:parse-native-namestring value)))))
(defun workflow-repositories (workflow)
  (coerce (mapcar #'inspect-recovery-repository
                 (remove-duplicates
                  (remove nil (append (list (or (workflow-home workflow)
                                               (agent-directory (workflow-lead workflow))))
                                      (mapcar #'cdr (workflow-trees workflow))))
                  :test #'equal)) 'vector))

(defun checkpoint-step (workflow step)
  (jobj "id" (step-id step) "agent" (agent-id (step-agent step))
        "prompt" (step-prompt step) "original-prompt" (step-original-prompt step)
        "repositories" (captured-recovery-repositories (step-repository-capture step) (step-repositories step))
        "target" (step-target step) "confirming" (step-confirming step) "check-fixes" (step-check-fixes step)
        "role" (step-role step)
        "loop" (position (step-loop step) (workflow-loops workflow))
        "state" (step-state step) "reason" (step-reason step)
        "progress" (or (step-progress step)
                       (and (eq (step-state step) :running) (agent-partial-text (step-agent step))))))

(defun workflow-checkpoint (workflow)
  "JSON-compatible restart state. Taking a checkpoint does not run or stage work."
  (flet ((agent-data (agent)
           (jobj "id" (agent-id agent) "directory" (and (agent-directory agent)
                                                        (namestring (agent-directory agent)))
                 "conversation" (agent-conversation-handle agent)
                 "unavailable" (agent-out-until agent))))
    (jobj "version" 1 "mode" (workflow-mode workflow) "state" (workflow-state workflow)
          "ended-at" (when (member (workflow-state workflow) '(:finished :stopped))
                       (or (workflow-ended-at workflow)
                           (setf (workflow-ended-at workflow) (get-universal-time))))
          "started-at" (workflow-started-at workflow)
          "metrics" (coerce (mapcar (lambda (record)
                                     (jobj "id" (getf record :id) "agent" (getf record :agent)
                                           "role" (getf record :role)
                                           "incomplete" (getf (getf record :metrics) :incomplete-p)
                                           "values" (apply #'jobj
                                                           (loop for key in *turn-metric-keys*
                                                                 append (list (string-downcase key)
                                                                              (getf (getf record :metrics) key))))))
                                   (workflow-metrics workflow)) 'vector)
          "request" (workflow-request workflow) "max-rounds" (workflow-max-rounds workflow)
          "check-command" (workflow-check-command workflow)
          "review-target" (workflow-review-target workflow) "review-base" (workflow-review-base workflow)
          "review-branch" (workflow-review-branch workflow) "review-count" (workflow-review-count workflow)
          "review-agents" (workflow-review-agents workflow)
          "approve-plan" (workflow-approve-plan-p workflow)
          "pending-plan" (let ((plan (workflow-pending-plan workflow)))
                           (when plan (jobj "lead" (getf plan :lead) "partner" (getf plan :partner)
                                            "order" (pending-plan-order workflow) "error" (getf plan :error)
                                            "takeover" (and (getf plan :takeover)
                                                            (agent-id (getf plan :takeover))))))
          "plan-revisions" (workflow-plan-revisions workflow)
          "prerequisite-merges" (coerce (mapcar (lambda (entry) (jobj "path" (first entry) "from" (second entry)
                                                                      "head" (third entry)))
                                                (workflow-prerequisite-merges workflow)) 'vector)
          "proposals" (coerce (mapcar (lambda (entry) (jobj "agent" (agent-id (car entry)) "text" (cdr entry)))
                                       (workflow-proposals workflow)) 'vector)
          "agents" (vector (agent-data (workflow-lead workflow)) (agent-data (workflow-partner workflow)))
          "home" (and (workflow-home workflow) (namestring (workflow-home workflow)))
          "base" (workflow-base workflow) "worktrees" (workflow-worktrees-p workflow)
          "retention-id" (workflow-retention-id workflow)
          "merged-refs" (coerce (workflow-merged-refs workflow) 'vector)
          "integration-journal" (coerce (mapcar (lambda (entry) (jobj "path" (car entry) "head" (cdr entry)))
                                                (workflow-integration-journal workflow)) 'vector)
          "integrated" (workflow-integrated workflow) "integration" (workflow-integration-state workflow)
          "worktrees-cleaned" (workflow-worktrees-cleaned workflow)
          "recovery-recorded" (workflow-recovery-recorded workflow)
          "trees" (coerce (mapcar (lambda (entry)
                                    (jobj "agent" (agent-id (car entry)) "path" (namestring (cdr entry))
                                          "branch" (cdr (assoc (cdr entry) (workflow-tree-branches workflow) :test #'equal))))
                                  (workflow-trees workflow)) 'vector)
          "shutdown-pids" #()
          "shutdown-processes" (coerce (workflow-process-records workflow) 'vector)
          "repositories" (or (captured-recovery-repositories (workflow-recovery-capture workflow)
                                                             (workflow-recovery-baseline workflow)) #())
          "repository-snapshot-pending" (if (recovery-snapshot-pending-p workflow) t :false)
          "error" (workflow-completion-error workflow)
          "merged-loop" (position (workflow-merged-loop workflow) (workflow-loops workflow))
          "loops" (coerce
                   (mapcar (lambda (l)
                             (jobj "author" (agent-id (loop-author l)) "reviewer" (agent-id (loop-reviewer l))
                                   "dependencies" (coerce (mapcar (lambda (dep) (position dep (workflow-loops workflow)))
                                                                            (loop-dependencies l)) 'vector)
                                   "revision" (loop-revision l)
                                   "prerequisite-revisions"
                                   (coerce (mapcar (lambda (entry) (jobj "loop" (position (car entry) (workflow-loops workflow))
                                                                         "revision" (cdr entry)))
                                                   (loop-prerequisite-revisions l)) 'vector)
                                   "task" (loop-task l) "label" (loop-label l) "summary" (loop-summary l)
                                   "last-review" (loop-last-review l) "state" (loop-state l) "round" (loop-round l)
                                   "tree" (and (loop-tree l) (namestring (loop-tree l)))
                                   "combined-result" (loop-combined-result l) "file-changes" (loop-file-changes l)
                                   "check-fixes" (loop-check-fixes l)
                                   "skip-review" (loop-skip-review l) "check-report" (loop-check-report l)
                                   "check-failed" (loop-check-failed l)
                                   "findings" (coerce
                                               (mapcar (lambda (f) (jobj "id" (finding-id f) "status" (finding-status f)
                                                                        "location" (finding-location f) "text" (finding-text f)
                                                                        "round" (finding-round f))) (loop-findings l)) 'vector)))
                           (workflow-loops workflow)) 'vector)
          "steps" (coerce (mapcar (lambda (s) (checkpoint-step workflow s))
                                  (remove-if-not (lambda (s) (member (step-state s) '(:new :running :paused)))
                                                 (workflow-steps workflow))) 'vector))))

(defun restored-step-continuation (workflow step)
  "Rebuild only known continuations; the saved data supplies no executable code."
  (lambda (text error)
    (declare (ignore error))
    (let ((l (step-loop step)))
      (case (step-role step)
        (:work (setf (loop-summary l) text)
               (post workflow (step-agent step) (loop-reviewer l) text :work)
               (loop-check workflow l (step-check-fixes step)))
        (:review (if (eq (workflow-mode workflow) :review-only)
                     (progn (incf (loop-round l))
                            (setf (loop-summary l) text (loop-last-review l) text)
                            (take-findings workflow l text)
                            (update-loop-state workflow l
                                               (if (and (eq (review-verdict text) :approved)
                                                        (zerop (count-findings (loop-findings l) :open)))
                                                   :approved :unresolved))
                            (check-finished workflow))
                     (review-in workflow l text (step-confirming step))))
        (:plan (plan-ready workflow text))
        (:replan (order-revised workflow text))
        (:propose (proposal-in workflow (step-agent step) text))
        (:resolve
         ;; Recovery never repeats a merge from an arbitrary old prompt.
         (if (or (repository-operation-p (workflow-home workflow))
                 (and (step-target step) (not (branch-merged-p (workflow-home workflow) (step-target step)))))
             (progn (setf (step-reason step) :integration-unresolved) (pause-step workflow step))
             (recover-integration workflow)))
        (t (error "Unknown recovery step role ~a" (step-role step)))))))

(defun restore-workflow (checkpoint &key lead partner on-message on-finish on-pause on-plan on-checkpoint)
  "Restore data with caller-supplied agents and callbacks. Never execute a saved step."
  (unless (and (= (or (jget checkpoint "version") 0) 1) lead partner (not (eq lead partner)))
    (error "Recovery requires a version 1 checkpoint and two explicitly supplied agents."))
  (let ((workflow (make-instance 'workflow :mode (recovery-keyword (jget checkpoint "mode") :review)
                                 :lead lead :partner partner :request (jget checkpoint "request")
                                 :max-rounds (or (jget checkpoint "max-rounds") 3)
                                 :check-command (recovery-null (jget checkpoint "check-command"))
                                 :worktrees (jtrue-p (jget checkpoint "worktrees"))
                                 :approve-plan (jtrue-p (jget checkpoint "approve-plan"))
                                 :review-target (recovery-keyword (jget checkpoint "review-target") :uncommitted)
                                 :review-base (or (recovery-null (jget checkpoint "review-base")) "HEAD")
                                 :review-branch (recovery-null (jget checkpoint "review-branch"))
                                 :review-count (or (jget checkpoint "review-count") 1)
                                 :review-agents (recovery-keyword (jget checkpoint "review-agents") :both)
                                 :on-message on-message :on-finish on-finish :on-pause on-pause :on-plan on-plan
                                 :on-checkpoint on-checkpoint)))
    (labels ((agent-for (id)
               (or (find (recovery-keyword id) (list lead partner) :key #'agent-id)
                   (error "Unknown checkpoint agent ~a" id)))
             (loop-for (index) (when (integerp index) (nth index (workflow-loops workflow)))))
      (setf (workflow-ended-at workflow) (metric-number (jget checkpoint "ended-at"))
            (workflow-started-at workflow) (metric-number (jget checkpoint "started-at"))
            (workflow-metrics workflow)
            (loop for record in (jlist (jget checkpoint "metrics"))
                  when (stringp (jget record "id"))
                  collect (list :id (jget record "id") :agent (recovery-keyword (jget record "agent"))
                                :role (recovery-keyword (jget record "role"))
                                :metrics (append (list :incomplete-p (jtrue-p (jget record "incomplete")))
                                                 (loop for key in *turn-metric-keys*
                                                       for v = (metric-number (jget record "values" (string-downcase key)))
                                                       when v append (list key v)))))
            (workflow-state workflow) :paused
            (workflow-home workflow) (or (recovery-path (jget checkpoint "home"))
                                          (recovery-path (jget checkpoint "agents" 0 "directory")))
            ;; Raw PIDs from older checkpoints have no identity and cannot
            ;; establish whether a process still belongs to this task.
            (workflow-shutdown-processes workflow) (jlist (jget checkpoint "shutdown-processes"))
            (workflow-saved-agent-context workflow) (jlist (jget checkpoint "agents"))
            (workflow-base workflow) (recovery-null (jget checkpoint "base"))
            (workflow-retention-id workflow) (jget checkpoint "retention-id")
            (workflow-integrated workflow) (jtrue-p (jget checkpoint "integrated"))
            (workflow-integration-state workflow) (recovery-keyword (jget checkpoint "integration"))
            (workflow-merged-refs workflow) (jlist (jget checkpoint "merged-refs"))
            (workflow-integration-journal workflow)
            (mapcar (lambda (entry) (cons (jget entry "path") (jget entry "head")))
                    (jlist (jget checkpoint "integration-journal")))
            (workflow-worktrees-cleaned workflow) (jtrue-p (jget checkpoint "worktrees-cleaned"))
            (workflow-recovery-recorded workflow) (jtrue-p (jget checkpoint "recovery-recorded"))
            (workflow-recovery-baseline workflow) (jget checkpoint "repositories")
            (workflow-completion-error workflow) (recovery-null (jget checkpoint "error")))
      ;; Agent directory/conversation are deliberately not changed here: supplied agents
      ;; may be active elsewhere. An explicit recovery action acquires them first.
      (dolist (tree (jlist (jget checkpoint "trees")))
        (let ((path (recovery-path (jget tree "path"))))
          (push (cons (agent-for (jget tree "agent")) path) (workflow-trees workflow))
          (push (cons path (recovery-null (jget tree "branch"))) (workflow-tree-branches workflow))))
      (setf (workflow-trees workflow) (nreverse (workflow-trees workflow)))
      (setf (workflow-proposals workflow)
            (mapcar (lambda (p) (cons (agent-for (jget p "agent")) (recovery-null (jget p "text"))))
                    (jlist (jget checkpoint "proposals"))))
      (let ((plan (jget checkpoint "pending-plan")))
        (when (jobject-p plan)
          (setf (workflow-pending-plan workflow)
                (list :lead (recovery-null (jget plan "lead")) :partner (recovery-null (jget plan "partner"))
                      :order (recovery-keyword (jget plan "order") :parallel)
                      :error (recovery-null (jget plan "error"))
                      :takeover (and (recovery-null (jget plan "takeover")) (agent-for (jget plan "takeover")))))))
      (setf (workflow-plan-revisions workflow) (or (jget checkpoint "plan-revisions") 0)
            (workflow-prerequisite-merges workflow)
            (mapcar (lambda (entry) (list (jget entry "path") (jget entry "from") (jget entry "head")))
                    (jlist (jget checkpoint "prerequisite-merges"))))
      (setf (workflow-loops workflow)
            (mapcar (lambda (data)
                      (make-review-loop
                       :author (agent-for (jget data "author")) :reviewer (agent-for (jget data "reviewer"))
                       :task (jget data "task") :label (recovery-null (jget data "label"))
                       :summary (or (recovery-null (jget data "summary")) "")
                       :last-review (recovery-null (jget data "last-review"))
                       :state (recovery-keyword (jget data "state") :paused) :round (or (jget data "round") 0)
                       :tree (recovery-path (jget data "tree"))
                       :combined-result (jtrue-p (jget data "combined-result"))
                       :file-changes (jtrue-p (jget data "file-changes"))
                       :skip-review (jtrue-p (jget data "skip-review"))
                       :check-fixes (or (jget data "check-fixes") 0)
                       :check-failed (jtrue-p (jget data "check-failed"))
                       :check-report (recovery-null (jget data "check-report"))
                       :findings (mapcar (lambda (f) (make-finding :id (jget f "id") :text (jget f "text")
                                                                  :status (recovery-keyword (jget f "status") :open)
                                                                  :location (recovery-null (jget f "location"))
                                                                  :round (jget f "round"))) (jlist (jget data "findings")))))
                    (jlist (jget checkpoint "loops"))))
      (loop for l in (workflow-loops workflow)
            for data in (jlist (jget checkpoint "loops"))
            do (setf (loop-dependencies l) (mapcar #'loop-for (jlist (jget data "dependencies")))
                     (loop-revision l) (or (jget data "revision") 0)
                     (loop-prerequisite-revisions l)
                     (mapcar (lambda (entry)
                               (let ((dep (loop-for (jget entry "loop"))))
                                 (unless (and dep (member dep (loop-dependencies l)) (integerp (jget entry "revision")))
                                   (error "The checkpoint's prerequisite revisions don't match its dependencies."))
                                 (cons dep (jget entry "revision"))))
                             (jlist (jget data "prerequisite-revisions")))))
      ;; Unknown or circular dependencies would leave parts waiting for ever.
      (check-dependency-graph (workflow-loops workflow))
      (setf (workflow-merged-loop workflow) (loop-for (jget checkpoint "merged-loop")))
      (dolist (data (jlist (jget checkpoint "steps")))
        (let* ((state (recovery-keyword (jget data "state")))
               (step (make-step :id (jget data "id") :agent (agent-for (jget data "agent"))
                                :repositories (recovery-null (jget data "repositories"))
                                :original-prompt (recovery-null (jget data "original-prompt"))
                                :target (recovery-null (jget data "target"))
                                :confirming (jtrue-p (jget data "confirming"))
                                :check-fixes (or (jget data "check-fixes") 0)
                                :prompt (jget data "prompt") :role (recovery-keyword (jget data "role"))
                                :loop (loop-for (jget data "loop"))
                                :state (if (member state '(:running :new :paused)) :paused state)
                                :reason (or (recovery-keyword (jget data "reason")) :restart)
                                :progress (recovery-null (jget data "progress")))))
          ;; Checkpoints may contain a compact prompt, but restored sessions can
          ;; fail to resume. Rebuild reviews with the complete assignment.
          (when (and (eq (step-state step) :paused)
                     (eq (step-role step) :review) (step-loop step))
            (setf (step-prompt step) (review-prompt (step-loop step) :full t)))
          ;; Likewise an author's reminder of the commit line.
          (setf (step-prompt step) (expand-commit-suggestion (step-prompt step)))
          (setf (step-original-prompt step) (expand-commit-suggestion (step-original-prompt step)))
          (setf (step-then step) (restored-step-continuation workflow step))
          (push step (workflow-steps workflow))
          (when (eq (step-state step) :paused) (push step (workflow-paused workflow)))))
      ;; Saved before the snapshot after the agents' last edits was ready: the
      ;; baselines are older, so their edits can read as changes since.
      (when (jtrue-p (jget checkpoint "repository-snapshot-pending"))
        (setf (workflow-recovery-stale workflow) (cons :baseline (copy-list (workflow-paused workflow)))))
      ;; A restart during a check must not replay the shell command automatically.
      (dolist (l (workflow-loops workflow))
        (when (and (eq (loop-state l) :checking)
                   (not (find l (workflow-paused workflow) :key #'step-loop)))
          (let ((s (make-step :agent (loop-author l) :role :check :loop l :state :paused
                              :reason :restart :prompt (workflow-check-command workflow))))
            (push s (workflow-steps workflow)) (push s (workflow-paused workflow)))))
      (dolist (l (workflow-loops workflow))
        (when (and (member (loop-state l) '(:working :reviewing :assigned :paused))
                   (not (find l (workflow-paused workflow) :key #'step-loop)))
          (let* ((reviewing (eq (loop-state l) :reviewing))
                 (s (make-step :agent (if reviewing (loop-reviewer l) (loop-author l))
                               :role (if reviewing :review :work) :loop l :state :paused :reason :restart
                               :prompt (cond (reviewing (review-prompt l :full t))
                                             ((loop-last-review l) (fix-prompt l (loop-last-review l)))
                                             (t (loop-task l))))))
            (setf (step-then s) (restored-step-continuation workflow s))
            (push s (workflow-steps workflow)) (push s (workflow-paused workflow)))))
      (when (and (workflow-home workflow)
                 (member (workflow-integration-state workflow) '(:merging :interrupted))
                 (ignore-errors (merge-in-progress-p (workflow-home workflow)))
                 (not (find :resolve (workflow-paused workflow) :key #'step-role)))
        (let* ((home (workflow-home workflow))
               (target (ignore-errors (worktree-git-line home "rev-parse" "MERGE_HEAD")))
               (files (uiop:split-string (worktree-git home "diff" "--name-only" "--diff-filter=U")
                                         :separator '(#\Newline)))
               (s (make-step :agent lead :role :resolve :target target :state :paused :reason :restart
                             :prompt (resolve-prompt (workflow-request workflow) (or target "the interrupted merge") files))))
          (setf (step-then s) (restored-step-continuation workflow s))
          (push s (workflow-steps workflow)) (push s (workflow-paused workflow))))
      workflow)))

(defun inspect-workflow-recovery (workflow)
  "Inspection is read-only and safe even when both agents are unavailable."
  (await-recovery-captures workflow)
  (let ((now (workflow-repositories workflow))
        (baseline (captured-recovery-repositories (workflow-recovery-capture workflow)
                                                   (workflow-recovery-baseline workflow))))
    (jobj "pending-plan" (and (workflow-pending-plan workflow)
                                   (jobj "lead" (getf (workflow-pending-plan workflow) :lead)
                                         "partner" (getf (workflow-pending-plan workflow) :partner)
                                         "order" (pending-plan-order workflow)
                                         "error" (pending-plan-error workflow)))
          "state" (workflow-state workflow) "integration" (workflow-integration-state workflow)
          "merged-refs" (coerce (workflow-merged-refs workflow) 'vector)
          "repositories" now
          "repository-changed" (if (and baseline (not (equalp now baseline))) t :false)
          "repository-snapshot-pending" (if (recovery-snapshot-pending-p workflow) t :false)
          "recovery-refs" (coerce (mapcar (lambda (entry)
                                            (jobj "part" (getf entry :part) "head" (getf entry :head)
                                                  "base" (getf entry :base)))
                                          (ignore-errors
                                            (worktree-recovery-refs
                                             (or (workflow-home workflow) (agent-directory (workflow-lead workflow)))
                                             (workflow-retention-id workflow)))) 'vector)
          "steps" (coerce (mapcar (lambda (s) (checkpoint-step workflow s)) (workflow-paused workflow)) 'vector)
          "agents" (coerce
                    (mapcar (lambda (a) (jobj "id" (agent-id a) "busy" (agent-busy a)
                                              "available" (if (and (agent-quiescent-p a) (not (agent-out-of-usage-p a))) t :false)
                                              "shutdown-confirmed" (if (and (agent-quiescent-p a)
                                                                            (workflow-old-processes-stopped-p workflow)) t :false)
                                              "unavailable" (agent-out-until a)))
                            (list (workflow-lead workflow) (workflow-partner workflow))) 'vector))))

(defun recover-integration (workflow)
  "Explicitly continue integration, skipping branches already in HEAD."
  (let ((home (workflow-home workflow)))
    (when (or (not home) (not (probe-file home)) (repository-operation-p home))
      (return-from recover-integration :blocked))
    (unless (every (lambda (l) (or (eq l (workflow-merged-loop workflow))
                                   (and (member (loop-state l) '(:approved :unreviewed))
                                        (not (loop-check-failed l)))))
                   (workflow-loops workflow))
      (return-from recover-integration :blocked))
    ;; A part done against an earlier version of its prerequisite isn't merged.
    (when (some #'loop-stale-p (workflow-loops workflow))
      (return-from recover-integration :blocked))
    (setf (workflow-ended-at workflow) nil
          (workflow-state workflow) :running
          (workflow-completion-error workflow) nil)
    ;; Older checkpoints mark even commit failures :INTERRUPTED. Every
    ;; part needs a retained commit in the journal before we may merge.
    (if (or (eq (workflow-integration-state workflow) :committing)
            (some (lambda (l)
                    (and (loop-tree l)
                         (not (assoc (namestring (loop-tree l))
                                     (workflow-integration-journal workflow) :test #'equal))))
                  (workflow-loops workflow)))
        (integrate workflow)
        (if (and (eq (workflow-integration-state workflow) :merged) (null (workflow-merged-loop workflow)))
            (merged workflow)
            (merge-parts workflow (mapcar #'cdr (reverse (workflow-trees workflow))))))
    :started))

(defun recovery-progress-prompt (workflow step &optional inspection)
  (format nil "Explicit recovery of an interrupted ~a step. Inspect existing files and partial work before editing; preserve useful work.~%~%Original assignment:~%~a~%~%Reason: ~a~%Available turn progress:~%~a~%~%Written result:~%~a~%Last review:~%~a~%~%Current repository inspection:~%~a"
          (step-role step)
          ;; Recovery may be in a new conversation: the whole commit-line instruction.
          (expand-commit-suggestion (or (step-original-prompt step) (step-prompt step)))
          (step-reason step) (or (step-progress step) "No final reply.")
          (if (step-loop step) (loop-summary (step-loop step)) "")
          (if (step-loop step) (or (loop-last-review (step-loop step)) "") "")
          (json-string (or inspection (inspect-workflow-recovery workflow)))))

(defun recover-workflow (workflow action &key step acknowledge-changes inspection)
  "Only explicit actions may start recovered work. A pending handoff stays paused."
  (when (eq action :restore-worktrees)
    (return-from recover-workflow (restore-workflow-worktrees workflow)))
  (when (eq action :review) (return-from recover-workflow (inspect-workflow-recovery workflow)))
  (when (eq action :stop) (stop-workflow workflow) (return-from recover-workflow :stopped))
  (when (eq action :abandon)
    (stop-workflow workflow)
    (setf (workflow-state workflow) :abandoned)
    (checkpoint-changed workflow)
    (return-from recover-workflow :abandoned))
  (when (eq (workflow-state workflow) :abandoned) (return-from recover-workflow :blocked))
  (when (and (stringp step) (not (find step (workflow-paused workflow) :key #'step-id :test #'equal)))
    (return-from recover-workflow :blocked))
  (unless (member action '(:resume :retry :hand-off :revise-order :run-independently))
    (error "Unknown recovery action ~a" action))
  (let* ((order (member action '(:revise-order :run-independently)))
         ;; The order actions act on every waiting step, not one.
         (step (cond (order nil)
                     ((stringp step) (find step (workflow-paused workflow) :key #'step-id :test #'equal))
                     (t (or step (first (workflow-paused workflow))))))
         (inspection (or inspection (inspect-workflow-recovery workflow))))
    ;; A supplied inspection must have waited for the pause snapshot. Never
    ;; block GTK here, or treat an unfinished snapshot as permission to resume.
    (dolist (capture (remove-duplicates
                      (remove nil (list (workflow-recovery-capture workflow)
                                        (and step (step-repository-capture step))))))
      (unless (recovery-capture-ready capture) (return-from recover-workflow :pending-shutdown))
      (when (recovery-capture-error capture) (return-from recover-workflow :blocked))
      (apply-recovery-capture workflow capture))
    (when (and (recovery-step-repository-changed-p workflow step inspection) (not acknowledge-changes))
      (return-from recover-workflow :repository-changed))
    (when (some (lambda (r) (or (and (not (jtrue-p (jget r "available")))
                                           (not (and (null step)
                                                     (assoc (jget r "path") (workflow-integration-journal workflow) :test #'equal))))
                                      (and (jtrue-p (jget r "operation"))
                                           (not (and step (eq (step-role step) :resolve)
                                                     (workflow-home workflow)
                                                     (equal (jget r "path") (namestring (workflow-home workflow)))))
                                           ;; A prerequisite's merge left for the dependent's agent to resolve.
                                           (not (and step (eq (step-reason step) :integration-unresolved)
                                                     (step-loop step) (loop-tree (step-loop step))
                                                     (equal (jget r "path") (namestring (loop-tree (step-loop step)))))))))
                (jlist (jget inspection "repositories")))
      (return-from recover-workflow :blocked))
    (when (not (workflow-old-processes-stopped-p workflow))
      (return-from recover-workflow :pending-shutdown))
    (unless (workflow-checks-quiescent-p workflow) (return-from recover-workflow :pending-shutdown))
    (when order (return-from recover-workflow (recover-order workflow action inspection)))
    (unless step
      (unless (every #'agent-quiescent-p (list (workflow-lead workflow) (workflow-partner workflow)))
        (return-from recover-workflow :pending-shutdown))
      (return-from recover-workflow
        (cond ((workflow-pending-plan workflow)
               (setf (workflow-ended-at workflow) nil
                     (workflow-state workflow) :running)
               (when (workflow-on-plan workflow)
                 (funcall (workflow-on-plan workflow) workflow
                          (getf (workflow-pending-plan workflow) :lead)
                          (getf (workflow-pending-plan workflow) :partner)))
               (checkpoint-changed workflow) :awaiting-plan)
              ((member (workflow-integration-state workflow) '(:committing :merging :interrupted :merged))
               (recover-integration workflow))
              (t :blocked))))
    (when (and (step-loop step) (not (loop-prerequisites-ready-p (step-loop step))))
      (return-from recover-workflow :blocked))
    (unless (member step (workflow-paused workflow)) (return-from recover-workflow :blocked))
    (unless (agent-quiescent-p (step-agent step))
      (return-from recover-workflow :pending-shutdown))
    (when (and (waiting-step-p step) (step-loop step) (not (eq action :hand-off))
               (not (dependent-can-start-p workflow step)))
      (return-from recover-workflow :pending-shutdown))
    (let ((agent (if (eq action :hand-off) (other-agent workflow (step-agent step)) (step-agent step))))
      (when (agent-out-of-usage-p agent) (return-from recover-workflow :unavailable))
      (unless (agent-quiescent-p agent) (return-from recover-workflow :pending-shutdown))
      (recovery-acquire-agent workflow agent
                              (or (and (step-loop step) (loop-tree (step-loop step))) (workflow-home workflow)
                                  (agent-directory agent))
                              :fresh (eq action :hand-off))
      (when (eq (workflow-mode workflow) :review-only)
        (unless (assoc agent (workflow-review-access workflow))
          (push (cons agent (agent-read-only-p agent)) (workflow-review-access workflow)))
        (setf (agent-read-only-p agent) t))
      (setf (workflow-ended-at workflow) nil
            (workflow-state workflow) :running
            (workflow-recovery-baseline workflow) (jget inspection "repositories")
            (workflow-recovery-capture workflow) nil
            (workflow-recovery-stale workflow) (remove :baseline (workflow-recovery-stale workflow)))
      (cond ((eq (step-role step) :check)
             (unpause workflow step) (setf (step-state step) :completed)
             (loop-check workflow (step-loop step) (loop-check-fixes (step-loop step))))
            ((eq action :hand-off)
             (setf (step-prompt step) (recovery-progress-prompt workflow step inspection))
             (%hand-off-step workflow step))
            ;; Its prerequisites are ready now: it starts as it would have by itself.
            ((and (waiting-step-p step) (step-loop step))
             (start-dependent-step workflow step))
            (t
             (setf (step-prompt step)
                   (if (eq action :retry) (or (step-original-prompt step) (step-prompt step))
                       (recovery-progress-prompt workflow step inspection)))
             (%resume-step workflow step)))
      (checkpoint-changed workflow)
      :started)))

(defun resume-step (workflow step)
  (recover-workflow workflow :resume :step step))
(defun hand-off-step (workflow step)
  (recover-workflow workflow :hand-off :step step))

(defun loop-ready-p (l)
  "Whether L is done well enough for what builds on it: approved, or done
unreviewed, with its check passing."
  (and (member (loop-state l) '(:approved :unreviewed)) (not (loop-check-failed l))))

(defun loop-prerequisites-ready-p (l)
  (every #'loop-ready-p (loop-dependencies l)))

(defun check-dependency-graph (loops)
  "Signal an error unless each of LOOPS depends only on others of LOOPS, and
none waits for itself, directly or through others."
  (dolist (l loops)
    (unless (every (lambda (dep) (member dep loops)) (loop-dependencies l))
      (error "Dependencies must name loops in this workflow.")))
  (labels ((visit (l path)
             (when (member l path) (error "Circular dependencies: revise the plan or dependency order."))
             (dolist (dep (loop-dependencies l)) (visit dep (cons l path)))))
    (dolist (l loops) (visit l nil))))

(defun set-workflow-dependencies (workflow assignments)
  "Set (LOOP . PREREQUISITES) edges atomically, rejecting mutual waiting/cycles."
  (let ((old (mapcar (lambda (l) (cons l (loop-dependencies l))) (workflow-loops workflow))))
    (handler-case
        (progn
          (dolist (entry assignments)
            (unless (member (car entry) (workflow-loops workflow))
              (error "Dependencies must name loops in this workflow."))
            (setf (loop-dependencies (car entry)) (cdr entry)))
          (check-dependency-graph (workflow-loops workflow))
          (checkpoint-changed workflow) t)
      (error (e)
        (dolist (entry old) (setf (loop-dependencies (car entry)) (cdr entry)))
        (error e)))))
(export '(set-workflow-dependencies loop-dependencies))

;;; Prerequisites that change

(defun loop-stale-p (l)
  "Whether L's work last started against a revision of a prerequisite that
has been redone since."
  (some (lambda (entry) (/= (cdr entry) (loop-revision (car entry)))) (loop-prerequisite-revisions l)))

(defun work-started (workflow loop)
  "LOOP's work (re)starts: a retry, a hand-off, a fix or another round. It's a
new revision, so parts that started against an earlier one revalidate; and
LOOP now works against its own prerequisites as they are."
  (incf (loop-revision loop))
  (setf (loop-prerequisite-revisions loop)
        (mapcar (lambda (dep) (cons dep (loop-revision dep))) (loop-dependencies loop)))
  (invalidate-dependent-loops workflow loop :started-only t))

(defun invalidate-dependent-loops (workflow prerequisite &key started-only (seen (make-hash-table :test #'eq)))
  "PREREQUISITE changed: the loops that build on it, and those that build on
them, revalidate (REVALIDATE-LOOP). STARTED-ONLY: only those whose work has
started; the others will start against it as it is then."
  (unless (gethash prerequisite seen)
    (setf (gethash prerequisite seen) t)
    (dolist (dependent (workflow-loops workflow))
      (when (and (member prerequisite (loop-dependencies dependent)) (not (gethash dependent seen))
                 (or (not started-only) (loop-prerequisite-revisions dependent)))
        (revalidate-loop workflow dependent prerequisite)
        (invalidate-dependent-loops workflow dependent :started-only started-only :seen seen)))))

(defun revalidation-prompt (dependent prerequisite progress)
  (format nil "~a, which your part builds on, changed after you started, so your part must be revalidated ~
against it. Your part:~%~%~a~%~%What you had reported so far:~%~%~a~%~%~
~@[What you had written when you were stopped:~%~%~a~%~%~]~
Look at your work as it is now: keep what still holds against the changed ~a, and change what doesn't. ~a"
          (string-capitalize (part-name prerequisite) :end 1) (loop-task dependent)
          (if (plusp (length (loop-summary dependent))) (loop-summary dependent) "Nothing yet.")
          progress (part-name prerequisite) (work-reply-instruction dependent)))

(defun revalidate-loop (workflow dependent prerequisite)
  "PREREQUISITE changed after DEPENDENT's work started: stop that work and
its checks, tell its author, and give the author a revalidation step, which
starts once DEPENDENT's prerequisites are ready again (CHECK-FINISHED).
DEPENDENT's written result and findings are kept."
  (when (find-if (lambda (s) (and (eq dependent (step-loop s)) (eq (step-reason s) :prerequisite-changed)))
                 (workflow-paused workflow))
    ;; Already waiting to revalidate: it will see the latest change.
    (return-from revalidate-loop nil))
  (let* ((author (loop-author dependent))
         (old (remove-if-not (lambda (s) (and (eq dependent (step-loop s))
                                              (member (step-state s) '(:new :running :paused))))
                             (workflow-steps workflow)))
         (running (remove-duplicates (loop for s in old when (eq (step-state s) :running) collect (step-agent s))))
         ;; A running turn's reply so far, taken before it's cancelled.
         (progress (format nil "~{~a~^~%~%~}"
                           (remove-if (lambda (text) (zerop (length text)))
                                      (mapcar (lambda (s)
                                                (or (step-progress s)
                                                    (and (eq (step-state s) :running)
                                                         (agent-partial-text (step-agent s)))))
                                              old)))))
    (note workflow "~a changed after ~a started on it: ~a revalidates it once ~a is ready again."
          (string-capitalize (part-name prerequisite) :end 1) (part-name dependent) (agent-name author)
          (part-name prerequisite))
    (post workflow :system author
          (format nil "~a, which your part builds on, has changed. Stop working on your part: once ~
it's ready again, you'll be asked to revalidate your part against it."
                  (string-capitalize (part-name prerequisite) :end 1))
          :note)
    (let ((token (loop-check-handle dependent)))
      (setf (loop-check-handle dependent) nil)
      (when token
        (when (car token) (pushnew (car token) (workflow-stopping-checks workflow)))
        (kill-check (car token))))
    ;; Its steps are replaced: their late results can't carry it on.
    (dolist (s old)
      (incf (step-generation s))
      (setf (step-state s) :superseded)
      (unpause workflow s))
    (dolist (agent running) (agent-cancel agent))
    (let ((step (make-step :agent author :role :work :loop dependent :reason :prerequisite-changed
                           :prompt (revalidation-prompt dependent prerequisite (blank-to-nil progress)))))
      ;; Its progress is what's in its prompt, not what the cancelled turn adds later.
      (setf (step-progress step) (blank-to-nil progress)
            (step-original-prompt step) (step-prompt step)
            (step-then step) (restored-step-continuation workflow step))
      (push step (workflow-steps workflow))
      (pause-step workflow step))))

;;; Starting a dependent part

(defun commit-prerequisite (workflow dep)
  "Commit DEP's work in its worktree; its head, and the files it changed."
  (let ((tree (loop-tree dep)))
    (commit-worktree tree (format nil "~a~%~%~a" (string-capitalize (part-name dep) :end 1) (loop-task dep)))
    (let ((head (worktree-git-line tree "rev-parse" "HEAD")))
      (values head
              (ignore-errors
                (remove "" (uiop:split-string
                            (worktree-git tree "diff" "--name-only"
                                          (or (workflow-base workflow)
                                              (worktree-git-line tree "config" (worktree-base-key (worktree-branch tree))))
                                          head)
                            :separator '(#\Newline))
                        :test #'equal))))))

(defun merge-prerequisite (workflow l dep head)
  "Merge HEAD, DEP's committed work, into L's worktree, once: recorded in
WORKFLOW-PREREQUISITE-MERGES. L's own work so far is committed first, so
the merge can't touch it. MERGE-PREREQUISITE-INTO-WORKTREE's status and files."
  (let* ((key (list (namestring (loop-tree l)) (namestring (loop-tree dep))))
         (done (find key (workflow-prerequisite-merges workflow) :key (lambda (e) (subseq e 0 2)) :test #'equal)))
    (if (equal head (third done))
        :already
        (progn
          (commit-worktree (loop-tree l) (format nil "~a, in progress" (string-capitalize (part-name l) :end 1)))
          (multiple-value-bind (status files)
              (merge-prerequisite-into-worktree (loop-tree l) head
                                                :message (format nil "Merge ~a into ~a" (part-name dep) (part-name l)))
            (when (member status '(:merged :already))
              (setf (workflow-prerequisite-merges workflow)
                    (cons (append key (list head)) (remove done (workflow-prerequisite-merges workflow))))
              (checkpoint-changed workflow))
            (values status files))))))

(defun conflict-marker-files (tree head)
  "The files in TREE with conflict markers added since HEAD."
  (multiple-value-bind (output errors status)
      (run-git-program (list "git" "--no-optional-locks" "-C" (namestring (uiop:ensure-directory-pathname tree)) "diff" "--check" head)
                        :output :string :error-output :string :ignore-error-status t)
    (declare (ignore errors status))
    (remove-duplicates
     (loop for line in (uiop:split-string output :separator '(#\Newline))
           when (search "conflict marker" line)
             collect (subseq line 0 (position #\: line)))
     :test #'equal)))

(defun prerequisite-merge-problem (workflow l)
  "Why L's worktree doesn't hold its prerequisites' work, merged, or nil: a
merge isn't concluded, a prerequisite's commit isn't in its history, or
conflict markers are left. Merges found done are recorded (WORKFLOW-PREREQUISITE-MERGES)."
  (let ((tree (loop-tree l))
        (done '()))
    (when (and tree (loop-dependencies l))
      (handler-case
          (progn
            (when (repository-operation-p tree)
              (return-from prerequisite-merge-problem
                "The merge of the work your part builds on isn't concluded in your worktree: resolve any ~
conflicts, stage the files and conclude it with `git commit --no-edit`."))
            (dolist (dep (loop-dependencies l))
              (when (loop-tree dep)
                (let ((head (worktree-git-line (loop-tree dep) "rev-parse" "HEAD")))
                  (unless (zerop (nth-value 2 (run-git-program
                                               (list "git" "--no-optional-locks" "-C" (namestring (uiop:ensure-directory-pathname tree))
                                                     "merge-base" "--is-ancestor" head "HEAD")
                                               :ignore-error-status t)))
                    (return-from prerequisite-merge-problem
                      (format nil "~a (commit ~a), which your part builds on, isn't merged into your worktree. ~
Merge it (`git merge ~a`), resolving any conflicts, and conclude the merge."
                              (string-capitalize (part-name dep) :end 1) head head)))
                  (let ((markers (conflict-marker-files tree head)))
                    (when markers
                      (return-from prerequisite-merge-problem
                        (format nil "Conflict markers are left from merging ~a in:~%~%~{- ~a~%~}~%Resolve them, ~
keeping what both parts meant to do."
                                (part-name dep) markers))))
                  (push (list (namestring tree) (namestring (loop-tree dep)) head) done))))
            (dolist (entry done)
              (unless (member entry (workflow-prerequisite-merges workflow) :test #'equal)
                (setf (workflow-prerequisite-merges workflow)
                      (cons entry (remove (subseq entry 0 2) (workflow-prerequisite-merges workflow)
                                          :key (lambda (e) (subseq e 0 2)) :test #'equal)))
                (checkpoint-changed workflow)))
            nil)
        (error (e) (format nil "The work your part builds on couldn't be verified in your worktree: ~a" e))))))

(defun prerequisite-unmerged (workflow l problem fixes)
  "L's work came back on a prerequisite merge that isn't done, PROBLEM: it
pauses (:integration-unresolved) for its author to finish the merge once resumed."
  (note workflow "~a can't be checked yet: ~a" (string-capitalize (part-name l) :end 1) problem)
  (let ((step (make-step :agent (loop-author l) :role :work :loop l :reason :integration-unresolved
                         :check-fixes fixes
                         :prompt (format nil "~a~%~%Your part:~%~%~a~%~%~a" problem (loop-task l)
                                         (work-reply-instruction l)))))
    (setf (step-original-prompt step) (step-prompt step)
          (step-then step) (restored-step-continuation workflow step))
    (push step (workflow-steps workflow))
    (pause-step workflow step)))

(defun prerequisite-context (workflow l)
  "What L's prerequisites did, for its author. In worktrees, each one's work
is first committed and merged into L's. Returns the text; or nil and, when a
merge conflicted or failed, what L's author is to do about it first."
  (let ((parts '()))
    (dolist (dep (loop-dependencies l))
      (let ((files nil))
        (when (and (loop-tree dep) (loop-tree l))
          (handler-case
              (multiple-value-bind (head changed) (commit-prerequisite workflow dep)
                (setf files changed)
                (multiple-value-bind (status conflicted) (merge-prerequisite workflow l dep head)
                  (case status
                    ((:merged :already) nil)
                    (:conflict
                     (return-from prerequisite-context
                       (values nil (format nil "Merging ~a (commit ~a), which your part builds on, into your ~
worktree conflicted in:~%~%~{- ~a~%~}~%Resolve the conflicts first, keeping what both parts meant to do; ~
then stage the files and conclude the merge with `git commit --no-edit`."
                                           (part-name dep) head conflicted))))
                    (t
                     (return-from prerequisite-context
                       (values nil (format nil "~a (commit ~a), which your part builds on, couldn't be merged ~
into your worktree (~(~a~)). Merge it (`git merge ~a`) before you go on."
                                           (string-capitalize (part-name dep) :end 1) head status head)))))))
            (error (e)
              (return-from prerequisite-context
                (values nil (format nil "~a, which your part builds on, couldn't be brought into your worktree: ~a"
                                    (string-capitalize (part-name dep) :end 1) e))))))
        (push (format nil "~a is done (~a). ~a reported:~%~%~a~%~%~:[It's in the folder you share.~;~
It's merged into your worktree. Files it changed: ~:*~{~a~^, ~}.~]"
                      (string-capitalize (part-name dep) :end 1) (loop-outcome dep) (agent-name (loop-author dep))
                      (if (plusp (length (loop-summary dep))) (loop-summary dep) "Nothing.")
                      (and (loop-tree dep) (loop-tree l) (or files '("none"))))
              parts)))
    (values (format nil "Your part builds on work that is now done.~%~%~{~a~^~%~%~}" (nreverse parts)) nil)))

(defun dependent-can-start-p (workflow step)
  "Whether what was stopped before STEP starts has stopped: checks, old
processes, its agent's turns, and, in worktrees, its prerequisites' authors'."
  (and (workflow-checks-quiescent-p workflow)
       (workflow-old-processes-stopped-p workflow)
       (agent-quiescent-p (step-agent step))
       ;; A prerequisite's worktree is committed: nothing of its author's may still edit it.
       (every (lambda (dep) (or (null (loop-tree dep)) (agent-quiescent-p (loop-author dep))))
              (loop-dependencies (step-loop step)))))

(defun retry-when-stopped (workflow)
  "Have CHECK-FINISHED start the ready dependent parts again shortly, on the
workflow's thread, until what they wait for has stopped."
  (unless (workflow-start-retry workflow)
    (setf (workflow-start-retry workflow) t)
    (note workflow "Waiting for stopped work to stop before a dependent part starts.")
    (sb-thread:make-thread
     (lambda ()
       (sleep 0.25)
       (funcall (agent-deliver (workflow-lead workflow))
                (lambda ()
                  (setf (workflow-start-retry workflow) nil)
                  (when (workflow-live-p workflow) (check-finished workflow)))))
     :name "dependent part start")))

(defun start-dependent-step (workflow step)
  "STEP's prerequisites are ready: bring their work into its worktree, tell
its agent what they did, and start it. A merge that conflicts pauses it
instead (:integration-unresolved), for its agent to resolve once resumed."
  (let ((l (step-loop step)))
    ;; Cancelled checks and turns must be gone before worktrees are committed and merged.
    (unless (dependent-can-start-p workflow step)
      (unless (member step (workflow-paused workflow))
        (unless (waiting-step-p step) (setf (step-reason step) :dependency))
        (pause-step workflow step))
      (retry-when-stopped workflow)
      (return-from start-dependent-step nil))
    (when (eq (step-role step) :work)
      (multiple-value-bind (context problem) (prerequisite-context workflow l)
        (let ((prompt (format nil "~a~%~%~a" (or (step-original-prompt step) (step-prompt step))
                              (or context ""))))
          (when problem
            (setf (step-reason step) :integration-unresolved
                  (step-prompt step) (format nil "~a~%~%~a" problem prompt)
                  (step-original-prompt step) (step-prompt step))
            (pause-step workflow step)
            (return-from start-dependent-step nil))
          ;; Its assignment now includes their result, also for a retry.
          (setf (step-prompt step) prompt
                (step-original-prompt step) prompt))))
    (unpause workflow step)
    (note workflow "~{~a~^ and ~} ~:[is~;are~] done: ~a starts."
          (mapcar #'part-name (loop-dependencies l)) (rest (loop-dependencies l)) (step-description step))
    (update-loop-state workflow l (if (eq (step-role step) :review) :reviewing :working))
    (submit-step workflow step)))

;;; Mutual waiting

(defun detect-deadlock (workflow)
  "When no unfinished part can go on, each waiting for a prerequisite that
can't become ready by itself (not approved, failed, or waiting in turn),
the waiting steps pause with reason :deadlock, for the user to choose."
  (let ((waiting (remove-if-not (lambda (s) (and (waiting-step-p s) (step-loop s))) (workflow-paused workflow))))
    (when (and (workflow-live-p workflow) waiting
               (every (lambda (s) (member (step-reason s) '(:dependency :prerequisite-changed :deadlock)))
                      (workflow-paused workflow))
               (notany (lambda (s) (eq (step-state s) :running)) (workflow-steps workflow))
               (notany #'loop-check-handle (workflow-loops workflow))
               (every (lambda (l) (or (loop-done-p l) (find l waiting :key #'step-loop))) (workflow-loops workflow))
               (notany (lambda (s) (loop-prerequisites-ready-p (step-loop s))) waiting))
      (note workflow "No part can go on: ~{~a~^; ~}. Revise the order, or have the parts go on independently."
            (loop for s in waiting
                  collect (format nil "~a waits for ~{~a (~a)~^ and ~}" (part-name (step-loop s))
                                  (loop for dep in (loop-dependencies (step-loop s))
                                        unless (loop-ready-p dep)
                                          append (list (part-name dep)
                                                       (if (find dep waiting :key #'step-loop) "waiting"
                                                           (loop-outcome dep)))))))
      (dolist (s waiting)
        (setf (step-reason s) :deadlock))
      (checkpoint-changed workflow)
      (when (workflow-on-pause workflow)
        (dolist (s waiting) (funcall (workflow-on-pause workflow) workflow s))))))

(defun recovery-acquire-agent (workflow agent directory &key fresh)
  "Put AGENT where recovered work goes on: in DIRECTORY, and, unless FRESH,
in the conversation it was in when the task was saved."
  (when (and directory (not (equal directory (agent-directory agent)))) (move-agent agent directory))
  (let ((saved (find (agent-id agent) (workflow-saved-agent-context workflow)
                     :key (lambda (entry) (recovery-keyword (jget entry "id"))))))
    (when (and saved (not fresh))
      (agent-resume-conversation agent (recovery-null (jget saved "conversation")))
      (setf (workflow-saved-agent-context workflow)
            (remove saved (workflow-saved-agent-context workflow))))))

(defun part-loops (workflow)
  "The parts of a split plan, the lead's first: not the merged result."
  (remove (workflow-merged-loop workflow) (workflow-loops workflow)))

(defun recover-order (workflow action inspection)
  ":revise-order or :run-independently, for parts waiting on each other.
RECOVER-WORKFLOW has checked the repositories, INSPECTION."
  (let ((waiting (remove-if-not (lambda (s) (and (step-loop s)
                                                 (member (step-reason s) '(:deadlock :dependency :prerequisite-changed))))
                                (workflow-paused workflow)))
        (agents (list (workflow-lead workflow) (workflow-partner workflow))))
    (cond ((or (null waiting) (eq (workflow-mode workflow) :review-only)) :blocked)
          ((not (and (workflow-old-processes-stopped-p workflow) (workflow-checks-quiescent-p workflow)
                     (every #'agent-quiescent-p agents)))
           :pending-shutdown)
          ((and (eq action :revise-order) (agent-out-of-usage-p (workflow-lead workflow))) :unavailable)
          (t
           (setf (workflow-ended-at workflow) nil
                 (workflow-state workflow) :running
                 (workflow-recovery-baseline workflow) (jget inspection "repositories")
                 (workflow-recovery-capture workflow) nil
                 (workflow-recovery-stale workflow) (remove :baseline (workflow-recovery-stale workflow)))
           (if (eq action :run-independently)
               (run-independently workflow waiting)
               (revise-order workflow waiting))
           (checkpoint-changed workflow)
           :started))))

(defun run-independently (workflow waiting)
  "Drop every part's prerequisites and start the WAITING steps."
  (set-workflow-dependencies workflow (mapcar #'list (workflow-loops workflow)))
  (dolist (l (workflow-loops workflow)) (setf (loop-prerequisite-revisions l) nil))
  (note workflow "The parts now go on independently: none waits for another.")
  (dolist (step waiting)
    (when (member step (workflow-paused workflow))
      (recovery-acquire-agent workflow (step-agent step) (or (loop-tree (step-loop step)) (workflow-home workflow)))
      (unpause workflow step)
      (setf (step-prompt step)
            (format nil "~a~%~%The parts now go on independently: don't wait for the other part, or count on its result."
                    (or (step-original-prompt step) (step-prompt step))))
      (update-loop-state workflow (step-loop step) (if (eq (step-role step) :review) :reviewing :working))
      (submit-step workflow step))))

(defun revise-order-prompt (workflow)
  (format nil "The overall request:~%~%~a~%~%You split it into parts, but no part can go on: each is waiting ~
for another, or for one that can't finish as it is. Each part as it stands:~%~%~{~a~^~%~%~}~%~%~
Revise the plan so the parts can go on, using what's already done: change the order, or the tasks. ~
Don't change anything yet. Reply in exactly this form, giving both parts' tasks in full, even one that ~
doesn't change (neither can be NONE now):~%~%LEAD TASK:~%<your part>~%~%PARTNER TASK:~%<~a's part, complete ~
in itself: ~:*~a won't see this conversation>~%~%ORDER: PARALLEL~%~%~
Write ORDER: LEAD FIRST (yours is done first) or ORDER: PARTNER FIRST (~:*~a's is) if one part needs the other's result."
          (workflow-request workflow)
          (loop for l in (part-loops workflow)
                collect (format nil "~a, by ~a: ~a~@[, builds on ~{~a~^ and ~}~].~%Task:~%~a~%Done so far:~%~a~@[~%Last review:~%~a~]"
                                (string-capitalize (part-name l) :end 1) (agent-name (loop-author l))
                                (if (loop-done-p l) (loop-outcome l) "waiting")
                                (mapcar #'part-name (loop-dependencies l)) (loop-task l)
                                (if (plusp (length (loop-summary l))) (loop-summary l) "Nothing yet.")
                                (loop-last-review l)))
          (agent-name (workflow-partner workflow))))

(defun revise-order (workflow waiting)
  "Have the lead revise the plan from both parts' progress; the WAITING steps
are replaced by what the revised plan has the parts do (ORDER-REVISED)."
  (let* ((lead (workflow-lead workflow))
         (prompts (mapcar (lambda (s) (cons (step-loop s) (or (step-original-prompt s) (step-prompt s)))) waiting))
         (own (find lead (part-loops workflow) :key #'loop-author)))
    (dolist (s waiting)
      (incf (step-generation s))
      (setf (step-state s) :superseded)
      (unpause workflow s))
    (recovery-acquire-agent workflow lead (or (and own (loop-tree own)) (workflow-home workflow)))
    (note workflow "~a revises the plan, with both parts' progress." (agent-name lead))
    (submit-step workflow (make-step :agent lead :role :replan :prompt (revise-order-prompt workflow)
                                     :then (lambda (text error)
                                             (declare (ignore error))
                                             (order-revised workflow text prompts))))))

(defun revised-part-prompt (workflow l changed)
  (format nil "The plan was revised~:[~;, and your part changed~]. Your part is now:~%~%~a~%~%~
What you've done on it so far:~%~%~a~%~%~@[The last review of it:~%~%~a~%~%~]~
Carry on from there: look at the files as they are, keep what still holds, and don't redo what's done. ~a~@[~%~%~a~]"
          changed (loop-task l) (if (plusp (length (loop-summary l))) (loop-summary l) "Nothing yet.")
          (loop-last-review l) (work-reply-instruction l) (order-note workflow l)))

(defun deadlock-parts (workflow prompts)
  "The parts still can't go on: each unfinished one pauses again, :deadlock,
with its assignment from PROMPTS ((loop . prompt)s), for the user to choose."
  (dolist (l (or (remove-if #'loop-done-p (part-loops workflow)) (part-loops workflow)))
    (let ((step (make-step :agent (loop-author l) :role :work :loop l :reason :deadlock
                           :prompt (or (cdr (assoc l prompts)) (revised-part-prompt workflow l nil)))))
      (setf (step-original-prompt step) (step-prompt step)
            (step-then step) (restored-step-continuation workflow step))
      (push step (workflow-steps workflow))
      (pause-step workflow step))))

(defun order-revised (workflow text &optional prompts)
  "The lead's revised plan, TEXT, is in: its tasks and order go to the parts,
and the parts it changes, or that aren't done, carry on from their progress."
  (post workflow (workflow-lead workflow) :user text :plan)
  (multiple-value-bind (lead-task partner-task order) (parse-plan text)
    (destructuring-bind (&optional mine theirs &rest others) (part-loops workflow)
      (declare (ignore others))
      (let* ((before (mapcar (lambda (l) (cons l (copy-list (loop-dependencies l)))) (workflow-loops workflow)))
             (problem (cond ((null theirs) "there aren't two parts to order")
                            ;; A part can't be dropped here: its work and worktree would be left behind.
                            ((or (no-task-p lead-task) (no-task-p partner-task))
                             "it must give both parts a task. To carry on without the order, run the parts independently.")
                            ((plan-order-problem order))
                            (t (handler-case
                                   (progn (set-workflow-dependencies workflow (order-assignments order mine theirs))
                                          nil)
                                 (error (e) (princ-to-string e)))))))
        (when problem
          (note workflow "The revised plan can't go on: ~a" problem)
          (deadlock-parts workflow prompts)
          (return-from order-revised nil))
        (note workflow "The revised plan: ~(~a~)." (substitute #\Space #\- (string order)))
        (let ((restart '()))
          (loop for l in (list mine theirs)
                for task in (list lead-task partner-task)
                do (let* ((task (blank-to-nil task))
                          (changed (and task (not (equal task (loop-task l))))))
                     (when changed (setf (loop-task l) task))
                     (unless (and (not changed) (loop-ready-p l) (loop-done-p l)
                                  (equal (loop-dependencies l) (cdr (assoc l before))))
                       (push (cons l changed) restart))))
          ;; A dependent part first: it waits while its prerequisite goes ahead.
          (dolist (entry (stable-sort (nreverse restart) #'> :key (lambda (e) (length (loop-dependencies (car e))))))
            (let ((l (car entry)))
              (setf (workflow-max-rounds workflow) (max (workflow-max-rounds workflow) (1+ (loop-round l))))
              (recovery-acquire-agent workflow (loop-author l) (or (loop-tree l) (workflow-home workflow)))
              (loop-work workflow l (revised-part-prompt workflow l (cdr entry)) :prepare t)))
          (unless restart (check-finished workflow)))))))

(defun restore-workflow-worktrees (workflow)
  "An explicit action recovers missing worktrees from committed recovery refs."
  (unless (and (workflow-old-processes-stopped-p workflow)
               (every #'agent-quiescent-p (list (workflow-lead workflow) (workflow-partner workflow))))
    (return-from restore-workflow-worktrees :pending-shutdown))
  (dolist (entry (workflow-trees workflow))
    (let* ((old (cdr entry))
           (branch (cdr (assoc old (workflow-tree-branches workflow) :test #'equal))))
      (unless (probe-file old)
        (unless branch (return-from restore-workflow-worktrees :blocked))
        (let* ((part (if (uiop:string-prefix-p "agent-duet/" branch) (subseq branch 11) branch))
               (path (handler-case
                         (restore-recovery-worktree (workflow-home workflow) (workflow-retention-id workflow) part)
                       (error (e)
                         (note workflow "Couldn't restore committed progress: ~a" e)
                         (return-from restore-workflow-worktrees :blocked)))))
          (dolist (l (workflow-loops workflow))
            (when (equal old (loop-tree l)) (setf (loop-tree l) path)))
          (setf (cdr entry) path)
          (push (cons path (worktree-branch path)) (workflow-tree-branches workflow))
          (note workflow "Restored committed progress in ~a. Uncommitted edits from the missing worktree cannot be reconstructed." path)
          (checkpoint-changed workflow)))))
  (setf (workflow-recovery-baseline workflow) (workflow-repositories workflow)
        (workflow-recovery-stale workflow) (remove :baseline (workflow-recovery-stale workflow)))
  :restored)

(defun workflow-checks-quiescent-p (workflow)
  (setf (workflow-stopping-checks workflow)
        (remove-if (lambda (handle)
                     (and (check-handle-thread handle)
                          (not (sb-thread:thread-alive-p (check-handle-thread handle)))
                          (or (null (check-handle-process handle))
                              (not (sb-ext:process-alive-p (check-handle-process handle))))))
                   (workflow-stopping-checks workflow)))
  (null (workflow-stopping-checks workflow)))

(defun workflow-old-processes-stopped-p (workflow)
  "Require identity-matched old processes to exit; never trust a recycled PID."
  (every (lambda (record)
           (or (not (recovery-process-alive-p record))
               (some (lambda (a)
                       (let ((process (agent-cli-process a)))
                         (and process (= (jget record "pid") (sb-ext:process-pid process))
                              (agent-quiescent-p a))))
                     (list (workflow-lead workflow) (workflow-partner workflow)))))
         (workflow-shutdown-processes workflow)))

(defun workflow-process-records (workflow)
  "Use cached process identities. Serializing a checkpoint never runs ps."
  (remove-duplicates
   (append (workflow-shutdown-processes workflow)
           (loop for a in (list (workflow-lead workflow) (workflow-partner workflow))
                 append (append (agent-editing-processes a)
                                (agent-retiring-records a)
                                (and (agent-busy a) (agent-process-record a)
                                     (list (agent-process-record a)))))) :test #'equalp))

(defun recovery-step-repository-changed-p (workflow step inspection)
  "Compare each paused step against its own snapshot, including shared folders."
  (if (null step)
      (jtrue-p (jget inspection "repository-changed"))
      (let* ((tree (and (step-loop step) (loop-tree (step-loop step))))
             (home (or (workflow-home workflow) (agent-directory (step-agent step))))
             (baseline (captured-recovery-repositories
                        (step-repository-capture step)
                        (or (step-repositories step) (workflow-recovery-baseline workflow))))
             (paths (remove-duplicates
                     (mapcar #'namestring (remove nil (list tree home))) :test #'equal)))
        (if (and baseline paths)
            (some (lambda (path)
                    (let ((before (find path (jlist baseline) :key (lambda (r) (jget r "path")) :test #'equal))
                          (now (find path (jlist (jget inspection "repositories"))
                                     :key (lambda (r) (jget r "path")) :test #'equal)))
                      (not (recovery-repositories-equal-p before now))))
                  paths)
            (jtrue-p (jget inspection "repository-changed"))))))

(defun recovery-repositories-equal-p (before now)
  "Registered peer worktrees may advance without changing this repository."
  (every (lambda (key) (equalp (jget before key) (jget now key)))
         '("path" "available" "head" "branch" "status" "index" "working" "untracked" "operation" "error" "git")))
