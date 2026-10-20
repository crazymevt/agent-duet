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
  ((review-target :initarg :review-target :initform :uncommitted :reader workflow-review-target)
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
                 :documentation "The plan waiting for approval: (:lead task :partner task :takeover out-agent)")
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
   (saved-agent-context :initform nil :accessor workflow-saved-agent-context)
   (merged-refs :initform nil :accessor workflow-merged-refs)
   (integration-journal :initform nil :accessor workflow-integration-journal)
   (shutdown-processes :initform nil :accessor workflow-shutdown-processes)
   (shutdown-pids :initform nil :accessor workflow-shutdown-pids)
   (steps :initform nil :accessor workflow-steps)
   (recovery-baseline :initform nil :accessor workflow-recovery-baseline)
   (recovery-capture :initform nil :accessor workflow-recovery-capture)
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
  (last-review nil)                     ; the reviewer's latest review
  (findings '())                        ; the review checklist: FINDINGs, by id
  (combined-result nil)                ; replies must remain full combined answers
  (file-changes nil)                    ; whether this part changed files against its base
  (tree nil)                            ; the worktree it's done in, or nil for the folder
  (check-handle nil)                    ; active asynchronous check
  (check-report nil)                    ; most recent result for the reviewer
  (check-fixes 0)
  (check-failed nil)                    ; true while the check fails after the fix attempts
  (dependencies nil)
  (revision 0)
  (round 0)                             ; reviews done
  (skip-review nil)                     ; true once taken over: its reviewer ran out
  (state :assigned))                    ; :assigned :working :checking :reviewing :paused :stopped or final outcome

(defstruct (workflow-step (:conc-name step-) (:constructor make-step))
  "A prompt given to an agent for the workflow, kept so it can be given
again, or to the other agent, after the first ran out of usage."
  (id (make-uuid))
  (generation 0) (state :new) reason progress
  agent prompt original-prompt then target confirming (check-fixes 0) repositories repository-capture
  role                                  ; :propose, :plan, :work, :review or :resolve
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
    (checkpoint-changed workflow)
    (unless (and (workflow-live-p workflow) (eq (step-state step) :running)
                 (= generation (step-generation step)))
      (return-from submit-step nil))
    (agent-submit (step-agent step) (step-prompt step)
                  (lambda (text error)
                    (when (and (workflow-live-p workflow)
                               (= generation (step-generation step))
                               (eq (step-state step) :running))
                      (if error
                          (progn
                            (setf (step-reason step)
                                  (case error (:out-of-usage :out-of-usage)
                                    (:interrupted :interrupted) (t :failed))
                                  (step-progress step)
                                  (if (plusp (length (agent-partial-text (step-agent step))))
                                      (format nil "~a~%~%Partial reply:~%~a" text (agent-partial-text (step-agent step))) text))
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
      (setf (workflow-state workflow) :running
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
              changed t))
      (dolist (entry (recovery-capture-steps capture))
        (let ((step (car entry)))
          (when (and (eq capture (step-repository-capture step))
                     (= (cdr entry) (step-generation step)) (eq (step-state step) :paused))
            (setf (step-repositories step) (recovery-capture-repositories capture)
                  (step-repository-capture step) nil
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
  (note workflow "~a paused (~a). Available progress: ~a"
        (step-description step) (step-reason step) (or (step-progress step) "No final reply yet."))
  (let ((other (other-agent workflow (step-agent step)))
        (message (format nil "~a paused (~a). Its incomplete result is unavailable; continue only independent work."
                         (step-description step) (step-reason step))))
    (post workflow :system other message :note))
  (dolist (dependent (workflow-loops workflow))
    (when (member (step-loop step) (loop-dependencies dependent))
      (note workflow "~a is blocked until ~a is complete."
            (or (loop-label dependent) "Dependent work") (or (loop-label (step-loop step)) "its prerequisite"))))
  (note-retained-worktrees workflow)
  (checkpoint-changed workflow)
  (when (workflow-on-pause workflow)
    (funcall (workflow-on-pause workflow) workflow step)))

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
        (:resolve
         (note workflow "~a resolves the conflicts instead." (agent-name other))
         (submit-step workflow (make-step :agent other :prompt (step-prompt step) :then (step-then step)
                                          :role :resolve :target (step-target step))))
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
         (loop-work workflow loop (takeover-prompt out (step-prompt step) (loop-tree loop))))
        (:plan
         (note workflow "~a does the whole task. It won't be reviewed: ~a is unavailable."
               (agent-name other) (agent-name out))
         (run-loop workflow (make-review-loop :author other :reviewer out :task (workflow-request workflow)
                                              :skip-review t)
                   (takeover-plan-prompt out (workflow-request workflow))))))))

;;; The prompts

(defun work-prompt (reviewer task)
  (format nil "~a~%~%(When you finish, ~a will review your work.)" task (agent-name reviewer)))

(defun review-prompt (loop)
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
Explain what you found as fully as you need, then list ~:[~;any new ~]issues in a FINDINGS section, ~
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

(defun confirm-prompt (loop)
  (format nil "You approved, but these findings of yours are still open:~%~%~a~%~%~
Settle each of them in a FINDINGS section: [fixed] if it has been dealt with, [dismissed] if it needn't be. ~
If any still stands, keep it [open] and request changes instead.~%~%~
End your reply with one line on its own, exactly one of:~%~
VERDICT: APPROVED~%~
VERDICT: CHANGES REQUESTED"
          (findings-lines (remove :open (loop-findings loop) :key #'finding-status :test-not #'eq))))

(defparameter *review-diff-limit* 60000 "The most of a worktree's diff put in a review prompt, in characters.")

(defun worktree-note (loop)
  "For the reviewer of LOOP's part, done in a worktree: where it is, and its diff."
  (let* ((tree (loop-tree loop))
         (diff (handler-case (worktree-diff tree)
                 (error (e) (format nil "(The diff couldn't be read: ~a)" e)))))
    (format nil "~a worked in its own git worktree, ~a, so its changes aren't in your folder. ~
Read the files there; here is everything it changed, against where it started~:[~;, cut short~]:~%~%~
```diff~%~a~%```~%~%"
            (agent-name (loop-author loop)) (namestring tree)
            (> (length diff) *review-diff-limit*) (subseq diff 0 (min (length diff) *review-diff-limit*)))))

(defun work-reply-instruction (loop)
  (if (loop-combined-result loop)
      "Then provide the full revised combined answer to the overall request, preserving useful results from both parts; a summary of edits alone is not sufficient."
      "Then summarize what you changed."))

(defun fix-prompt (loop review)
  (let ((open (remove :open (loop-findings loop) :key #'finding-status :test-not #'eq)))
    (format nil "~a reviewed your work~:[ and requested changes~;~]:~%~%~a~%~%~
~@[These findings are still open:~%~%~a~%~%~]~
Address the points you agree with, and say briefly why for any you don't. ~a"
            (agent-name (loop-reviewer loop)) (eq (review-verdict review) :approved) review
            (and open (findings-lines open))
            (work-reply-instruction loop))))

(defun plan-form (partner)
  "How the lead is to set out a plan split with PARTNER."
  (let ((p (agent-name partner)))
    (format nil "Split the work into two parts that can be done in parallel without editing the same files: ~
one for you and one for ~a. Reply in exactly this form:~%~%~
LEAD TASK:~%<your part>~%~%~
PARTNER TASK:~%<~a's part, complete in itself: ~a won't see this conversation>~%~%~
If the task is too small to split, write NONE under PARTNER TASK."
            p p p)))

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

(defun takeover-prompt (out prompt &optional tree)
  (format nil "~a ran out of usage partway through a task, so you're taking it over. Some of its work ~
may already be in the files: look before you continue, and don't redo what's done.~@[ Its work is in ~
its own git worktree, ~a: make your changes there, not in your own folder.~] Here is what ~a was ~
asked (\"you\" there meant ~a):~%~%~a~%~%~
No one will review your work this time, since ~a is out of usage. Finish with a summary of what you changed."
          (agent-name out) (and tree (namestring tree)) (agent-name out) (agent-name out) prompt (agent-name out)))

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

(defun parse-plan (text)
  "The lead's and partner's tasks from a plan's TEXT, as two values. The
partner's is nil when there is none (NONE, or no PARTNER TASK section)."
  (let ((lead-at (search "LEAD TASK:" text :test #'char-equal))
        (partner-at (search "PARTNER TASK:" text :test #'char-equal)))
    (if (null partner-at)
        (values (clean-section text) nil)
        (let* ((lead (if (and lead-at (< lead-at partner-at))
                         (clean-section (subseq text (+ lead-at (length "LEAD TASK:")) partner-at))
                         (clean-section (subseq text 0 partner-at))))
               (partner (clean-section (subseq text (+ partner-at (length "PARTNER TASK:"))))))
          (values lead
                  (unless (or (zerop (length partner))
                              (and (>= (length partner) 4) (string-equal "NONE" partner :end2 4)))
                    partner))))))

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

(defun checklist-text (loop)
  "LOOP's checklist, in Markdown, for the conversation."
  (let ((findings (loop-findings loop)))
    (format nil "**Task checklist~@[, ~a~]**~%~%~a~%~%Assigned to: **~a** · Progress: **~a** · Review rounds: ~d~%~%~d open, ~d fixed, ~d dismissed~%~%~{~a~^~%~}"
            (loop-label loop)
            ;; Keep the full merged prompt for work and review, but not in the panel.
            ;; Quote every task line so its Markdown cannot consume progress/findings.
            (with-output-to-string (out)
              (with-input-from-string (in (if (loop-combined-result loop)
                                             "Combine both parts and review the merged result."
                                             (loop-task loop)))
                (loop for line = (read-line in nil) while line
                      do (format out "> ~a~%" line))))
            (agent-name (loop-author loop))
            (ecase (loop-state loop)
              (:assigned "Assigned") (:working (if (plusp (loop-round loop)) "Addressing review" "Working"))
              (:checking "Checking") (:reviewing "In review") (:approved "Approved")
              (:unresolved "Not approved") (:unreviewed "Done without review")
              (:failed "Failed") (:paused "Paused") (:stopped "Stopped"))
            (loop-round loop)
            (count-findings findings :open) (count-findings findings :fixed) (count-findings findings :dismissed)
            (loop for f in findings
                  collect (format nil "- ~a **F~d** ~(~a~)~@[ · `~a`~] — ~a"
                                  (ecase (finding-status f) (:open "○") (:fixed "✓") (:dismissed "✕"))
                                  (finding-id f) (finding-status f) (finding-location f) (finding-text f))))))

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

(defun start-workflow (workflow)
  (setf (workflow-state workflow) :running
        (workflow-completion-data workflow) nil
        (workflow-completion-error workflow) nil
        (workflow-commit-hash workflow) nil
        (workflow-repository-start workflow)
        (ignore-errors (repository-start-state (agent-directory (workflow-lead workflow)))))
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
  (multiple-value-bind (lead-task partner-task) (parse-plan plan)
    (if (workflow-approve-plan-p workflow)
        (await-plan workflow lead-task partner-task)
        (start-parts workflow lead-task partner-task))))

(defun await-plan (workflow lead-task partner-task &key takeover)
  "Hold the plan for the user's approval (APPROVE-PLAN). TAKEOVER: the
partner is to do it all, as that agent, the lead, is out of usage."
  (setf (workflow-pending-plan workflow) (list :lead lead-task :partner partner-task :takeover takeover))
  (note workflow "Waiting for you to approve the plan.")
  (when (workflow-on-plan workflow)
    (funcall (workflow-on-plan workflow) workflow lead-task partner-task)))

(defun start-takeover (workflow out task)
  "The partner does TASK alone from its proposal, unreviewed: OUT, the lead, is out of usage."
  (run-loop workflow (make-review-loop :author (workflow-partner workflow) :reviewer out :task task :skip-review t)
            (proposal-takeover-prompt out task)))

(defun blank-to-nil (text)
  (let ((text (and text (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
    (and text (plusp (length text)) text)))

(defun approve-plan (workflow lead-task partner-task)
  "Start on WORKFLOW's pending plan with the tasks as the user left them; a
blank one is no part for that agent. True if it started: false when nothing
is pending, or both are blank."
  (let ((lead-task (blank-to-nil lead-task))
        (partner-task (blank-to-nil partner-task)))
    (when (and (workflow-live-p workflow) (workflow-pending-plan workflow) (or lead-task partner-task))
      (destructuring-bind (&key lead partner takeover) (workflow-pending-plan workflow)
        (let ((edited (not (and (equal lead-task lead) (equal partner-task partner)))))
          (setf (workflow-pending-plan workflow) nil)
          (note workflow (if edited "You edited the plan." "You approved the plan."))
          ;; Given a part after all, the lead takes its chances with its usage.
          (if (and takeover (null lead-task))
              (start-takeover workflow takeover partner-task)
              (start-parts workflow lead-task partner-task :edited edited))
          t)))))

(defun start-parts (workflow lead-task partner-task &key edited)
  "Start the work on a plan: a loop for each agent with a part (LEAD-TASK,
PARTNER-TASK, nil for none). EDITED: the user changed the plan, so the
lead, who made it, is told its part again."
  (let ((lead (workflow-lead workflow))
        (partner (workflow-partner workflow))
        (request (workflow-request workflow)))
    (cond ((null partner-task)
           (note workflow "~a is doing it alone; ~a will review." (agent-name lead) (agent-name partner))
           (run-loop workflow (make-review-loop :author lead :reviewer partner :task (if edited lead-task request))
                     (lead-alone-prompt partner lead-task edited)))
          ((null lead-task)
           (note workflow "~a is doing it alone; ~a will review." (agent-name partner) (agent-name lead))
           (run-loop workflow (make-review-loop :author partner :reviewer lead :task partner-task)
                     (partner-task-prompt lead request nil partner-task)))
          (t
           (let ((mine (make-review-loop :author lead :reviewer partner :task lead-task
                                         :label (format nil "~a's part" (agent-name lead))))
                 (theirs (make-review-loop :author partner :reviewer lead :task partner-task
                                           :label (format nil "~a's part" (agent-name partner)))))
             ;; Both are registered before either starts, so the first to
             ;; finish doesn't find every loop done.
             (setf (workflow-loops workflow) (list mine theirs))
             (publish-checklist workflow mine)
             (publish-checklist workflow theirs)
             (cond ((not (workflow-worktrees-p workflow))
                    (loop-work workflow mine (lead-go-prompt partner lead-task partner-task edited))
                    (loop-work workflow theirs (partner-task-prompt lead request lead-task partner-task)))
                   ((open-worktrees workflow)
                    (setf (loop-tree mine) (cdr (assoc lead (workflow-trees workflow)))
                          (loop-tree theirs) (cdr (assoc partner (workflow-trees workflow))))
                    (loop-work workflow mine (worktree-part-prompt partner request lead-task partner-task
                                                                   (loop-tree mine)))
                    (loop-work workflow theirs (worktree-part-prompt lead request partner-task lead-task
                                                                     (loop-tree theirs))))))))))

(defun run-loop (workflow loop prompt)
  "Start LOOP: its author works on PROMPT, and then the reviewing begins."
  (setf (workflow-loops workflow) (append (workflow-loops workflow) (list loop)))
  (publish-checklist workflow loop)
  (loop-work workflow loop prompt))

(defun loop-work (workflow loop prompt)
  (when (plusp (loop-revision loop)) (invalidate-dependent-loops workflow loop))
  (setf (loop-check-report loop) nil
        (loop-check-failed loop) nil)
  (update-loop-state workflow loop :working)
  (submit workflow (loop-author loop) prompt
          (lambda (text error)
            (cond (error
                   (loop-failed workflow loop (format nil "~a's turn failed: ~a" (agent-name (loop-author loop)) text)))
                  (t
                   (incf (loop-revision loop))
                   (setf (loop-summary loop) text)
                   (post workflow (loop-author loop) (loop-reviewer loop) text :work)
                   (loop-check workflow loop))))
          :role :work :loop loop))

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
    (setf (loop-check-handle loop) nil)
    (let* ((passed (and (not timed-out) (eql code 0)))
           (status (cond (timed-out
                          (format nil "Check `~a` failed (exit code ~a; timed out)." command code))
                         ((null code) (format nil "Check `~a` couldn't run (no exit code)." command))
                         (t (format nil "Check `~a` ~a (exit code ~a)."
                                    command (if passed "passed" "failed") code))))
           (report (format nil "~a~%~%~a" status (check-output-block (check-output-tail output)))))
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
                      (format nil "~a~%~%Fix the failing check. ~a" report (work-reply-instruction loop))
                      (lambda (text error)
                        (if error
                            (loop-failed workflow loop (format nil "Check fix failed: ~a" text))
                            (progn
                              (setf (loop-summary loop) text)
                              (post workflow (loop-author loop) (loop-reviewer loop) text :work)
                              (loop-check workflow loop (1+ fixes)))))
                      :role :work :loop loop :check-fixes (1+ fixes))))))))

(defun loop-check (workflow loop &optional (fixes 0))
  "Check the current work before review; FIXES does not count review rounds."
  (setf (loop-check-fixes loop) fixes)
  (let ((command (blank-to-nil (workflow-check-command workflow))))
    (if (null command)
        (loop-checked workflow loop)
        (let ((token (list nil)))
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
                               (check-in workflow loop token command fixes code output timed-out))))
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
  (setf (loop-last-review loop) text)
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
  (dolist (step (copy-list (workflow-paused workflow)))
    (when (and (eq (step-reason step) :dependency) (step-loop step)
               (loop-prerequisites-ready-p (step-loop step)))
      (%resume-step workflow step)))
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
                                    (if (> (length reply) *review-diff-limit*)
                                        (format nil "~a~%[Final reply cut short; full reply retained in the part's conversation.]"
                                                (subseq reply 0 *review-diff-limit*))
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
      ((plusp (length (getf start :status))) "The repository had tracked edits or untracked files before this work. Start with a clean repository to use this commit action.")
      ((repository-operation-p root) "Finish the repository's merge, rebase or other Git operation first.")
      ((not (equal end (repository-state root)))
       "The repository changed after completion. Review the current changes before committing.")
      ((zerop (length (getf end :status))) "There are no uncommitted changes.")
      (t nil))))

(defun workflow-completion (workflow)
  "Completion plist for the UI, or NIL before completion. Eligibility is live.
Keys: :OUTCOME, :SUMMARY, :REVIEW-STATUS, :CHECK-STATUS, :REPOSITORY,
:CAN-COMMIT, :COMMIT-REASON, :CLEAN and :COMMIT-HASH. Does not stage or commit."
  (when (workflow-completion-data workflow)
    (let ((reason (handler-case (workflow-commit-reason workflow)
                    (error (e) (format nil "Cannot inspect the repository: ~a" e)))))
      (append (copy-list (workflow-completion-data workflow))
              (list :repository (getf (workflow-completion-state workflow) :root)
                    :can-commit (null reason) :commit-reason reason
                    :clean (let ((root (getf (workflow-completion-state workflow) :root)))
                             (and root (ignore-errors
                                         (zerop (length (worktree-git root "status" "--porcelain=v1"
                                                                      "--untracked-files=all"))))))
                    :commit-hash (workflow-commit-hash workflow))))))

(defun commit-workflow-changes (workflow &key message)
  "User action only. Commit the captured approved edits if still current.
Return a plist with :STATUS (:COMMITTED, :CLEAN or :ERROR), :MESSAGE and :COMMIT.
Git hook/identity errors are returned; a failed commit restores the prior index."
  (handler-case
      (let* ((reason (workflow-commit-reason workflow))
             (end (workflow-completion-state workflow))
             (root (getf end :root)))
        (when reason
          (return-from commit-workflow-changes
            (list :status (if (and root (not (workflow-commit-hash workflow))
                                  (eq (workflow-state workflow) :finished)
                                  (equal end (repository-state root))
                                  (zerop (length (getf end :status)))) :clean :error)
                  :message reason :commit nil)))
        (let ((message (or message (format nil "Agent Duet: ~a" (workflow-request workflow)))))
          (when (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) message)))
            (error "The commit message must not be empty."))
          ;; Explicit action, after comparing HEAD, branch, index and file contents.
          (let ((index-tree (worktree-git-line root "write-tree"))
                (head (getf end :head))
                (committed nil))
            (unwind-protect
                 (progn (worktree-git root "add" "-A")
                        (worktree-git root "commit" "-m" message)
                        (setf committed t))
              ;; Restore staging after hook/identity/signing failures. Keep any
              ;; hook edits in the working tree. Never reset an advanced HEAD.
              (when (and (not committed)
                         (string= head (worktree-git-line root "rev-parse" "HEAD")))
                (worktree-git root "read-tree" index-tree))))
          (let ((hash (worktree-git-line root "rev-parse" "HEAD")))
            (setf (workflow-commit-hash workflow) hash)
            (list :status :committed :message (format nil "Committed changes (~a)." (subseq hash 0 12))
                  :commit hash))))
    (error (e) (list :status :error :message (format nil "Could not commit changes: ~a" e) :commit nil))))

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
          "request" (workflow-request workflow) "max-rounds" (workflow-max-rounds workflow)
          "check-command" (workflow-check-command workflow)
          "review-target" (workflow-review-target workflow) "review-base" (workflow-review-base workflow)
          "review-branch" (workflow-review-branch workflow) "review-count" (workflow-review-count workflow)
          "review-agents" (workflow-review-agents workflow)
          "approve-plan" (workflow-approve-plan-p workflow)
          "pending-plan" (let ((plan (workflow-pending-plan workflow)))
                           (when plan (jobj "lead" (getf plan :lead) "partner" (getf plan :partner)
                                            "takeover" (and (getf plan :takeover)
                                                            (agent-id (getf plan :takeover))))))
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
          "repository-snapshot-pending" (if (and (workflow-recovery-capture workflow)
                                                   (not (recovery-capture-ready (workflow-recovery-capture workflow)))) t :false)
          "error" (workflow-completion-error workflow)
          "merged-loop" (position (workflow-merged-loop workflow) (workflow-loops workflow))
          "loops" (coerce
                   (mapcar (lambda (l)
                             (jobj "author" (agent-id (loop-author l)) "reviewer" (agent-id (loop-reviewer l))
                                   "dependencies" (coerce (mapcar (lambda (dep) (position dep (workflow-loops workflow)))
                                                                            (loop-dependencies l)) 'vector)
                                   "revision" (loop-revision l)
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
        (:work (incf (loop-revision l)) (setf (loop-summary l) text)
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
      (setf (workflow-state workflow) :paused
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
                      :takeover (and (recovery-null (jget plan "takeover")) (agent-for (jget plan "takeover")))))))
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
                     (loop-revision l) (or (jget data "revision") 0)))
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
          (setf (step-then step) (restored-step-continuation workflow step))
          (push step (workflow-steps workflow))
          (when (eq (step-state step) :paused) (push step (workflow-paused workflow)))))
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
                               :prompt (cond (reviewing (review-prompt l))
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
                                         "partner" (getf (workflow-pending-plan workflow) :partner)))
          "state" (workflow-state workflow) "integration" (workflow-integration-state workflow)
          "merged-refs" (coerce (workflow-merged-refs workflow) 'vector)
          "repositories" now
          "repository-changed" (if (and baseline (not (equalp now baseline))) t :false)
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
    (setf (workflow-state workflow) :running)
    (if (eq (workflow-integration-state workflow) :committing)
        (integrate workflow)
        (if (and (eq (workflow-integration-state workflow) :merged) (null (workflow-merged-loop workflow)))
            (merged workflow)
            (merge-parts workflow (mapcar #'cdr (reverse (workflow-trees workflow))))))
    :started))

(defun recovery-progress-prompt (workflow step &optional inspection)
  (format nil "Explicit recovery of an interrupted ~a step. Inspect existing files and partial work before editing; preserve useful work.~%~%Original assignment:~%~a~%~%Reason: ~a~%Available turn progress:~%~a~%~%Written result:~%~a~%Last review:~%~a~%~%Current repository inspection:~%~a"
          (step-role step) (or (step-original-prompt step) (step-prompt step)) (step-reason step) (or (step-progress step) "No final reply.")
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
  (unless (member action '(:resume :retry :hand-off)) (error "Unknown recovery action ~a" action))
  (let* ((step (if (stringp step) (find step (workflow-paused workflow) :key #'step-id :test #'equal)
                   (or step (first (workflow-paused workflow)))))
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
                                                     (equal (jget r "path") (namestring (workflow-home workflow))))))))
                (jlist (jget inspection "repositories")))
      (return-from recover-workflow :blocked))
    (when (not (workflow-old-processes-stopped-p workflow))
      (return-from recover-workflow :pending-shutdown))
    (unless (workflow-checks-quiescent-p workflow) (return-from recover-workflow :pending-shutdown))
    (unless step
      (unless (every #'agent-quiescent-p (list (workflow-lead workflow) (workflow-partner workflow)))
        (return-from recover-workflow :pending-shutdown))
      (return-from recover-workflow
        (cond ((workflow-pending-plan workflow)
               (setf (workflow-state workflow) :running)
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
    (let ((agent (if (eq action :hand-off) (other-agent workflow (step-agent step)) (step-agent step))))
      (when (agent-out-of-usage-p agent) (return-from recover-workflow :unavailable))
      (unless (agent-quiescent-p agent) (return-from recover-workflow :pending-shutdown))
      (let ((directory (or (and (step-loop step) (loop-tree (step-loop step))) (workflow-home workflow)
                           (agent-directory agent))))
        (when (and directory (not (equal directory (agent-directory agent)))) (move-agent agent directory)))
      (let ((saved (find (agent-id agent) (workflow-saved-agent-context workflow)
                         :key (lambda (entry) (recovery-keyword (jget entry "id"))))))
        (when (and saved (not (eq action :hand-off)))
          (agent-resume-conversation agent (recovery-null (jget saved "conversation")))
          (setf (workflow-saved-agent-context workflow)
                (remove saved (workflow-saved-agent-context workflow)))))
      (when (eq (workflow-mode workflow) :review-only)
        (unless (assoc agent (workflow-review-access workflow))
          (push (cons agent (agent-read-only-p agent)) (workflow-review-access workflow)))
        (setf (agent-read-only-p agent) t))
      (setf (workflow-state workflow) :running
            (workflow-recovery-baseline workflow) (jget inspection "repositories")
            (workflow-recovery-capture workflow) nil)
      (cond ((eq (step-role step) :check)
             (unpause workflow step) (setf (step-state step) :completed)
             (loop-check workflow (step-loop step) (loop-check-fixes (step-loop step))))
            ((eq action :hand-off)
             (setf (step-prompt step) (recovery-progress-prompt workflow step inspection))
             (%hand-off-step workflow step))
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

(defun loop-prerequisites-ready-p (l)
  (every (lambda (dep) (and (member (loop-state dep) '(:approved :unreviewed))
                             (not (loop-check-failed dep)))) (loop-dependencies l)))

(defun set-workflow-dependencies (workflow assignments)
  "Set (LOOP . PREREQUISITES) edges atomically, rejecting mutual waiting/cycles."
  (let ((old (mapcar (lambda (l) (cons l (loop-dependencies l))) (workflow-loops workflow))))
    (handler-case
        (progn
          (dolist (entry assignments)
            (unless (and (member (car entry) (workflow-loops workflow))
                         (every (lambda (l) (member l (workflow-loops workflow))) (cdr entry)))
              (error "Dependencies must name loops in this workflow."))
            (setf (loop-dependencies (car entry)) (cdr entry)))
          (labels ((visit (l path)
                     (when (member l path) (error "Circular dependencies: revise the plan or dependency order."))
                     (dolist (dep (loop-dependencies l)) (visit dep (cons l path)))))
            (dolist (l (workflow-loops workflow)) (visit l nil)))
          (checkpoint-changed workflow) t)
      (error (e)
        (dolist (entry old) (setf (loop-dependencies (car entry)) (cdr entry)))
        (error e)))))
(export '(set-workflow-dependencies loop-dependencies))

(defun invalidate-dependent-loops (workflow prerequisite &optional (seen (make-hash-table :test #'eq)))
  "A changed prerequisite invalidates dependent checks, reviews and completion."
  (when (gethash prerequisite seen) (return-from invalidate-dependent-loops nil))
  (setf (gethash prerequisite seen) t)
  (dolist (dependent (workflow-loops workflow))
    (when (and (member prerequisite (loop-dependencies dependent)) (not (gethash dependent seen)))
      (note workflow "~a changed; ~a must be revalidated before completion."
            (or (loop-label prerequisite) "Prerequisite") (or (loop-label dependent) "dependent work"))
      (let ((token (loop-check-handle dependent)))
        (setf (loop-check-handle dependent) nil)
        (when token (kill-check (car token))))
      (let ((steps (remove-if-not (lambda (s) (and (eq dependent (step-loop s))
                                                  (member (step-state s) '(:running :paused)))) (workflow-steps workflow))))
        (if steps
            (dolist (step steps)
              (let ((running (eq (step-state step) :running)))
                (incf (step-generation step))
                (setf (step-reason step) :prerequisite-changed)
                (pause-step workflow step)
                (when running (agent-cancel (step-agent step)))))
            (let ((step (make-step :agent (loop-author dependent) :role :work :loop dependent
                                   :prompt (format nil "Revalidate your work against the changed prerequisite.~%~a" (loop-task dependent))
                                   :reason :prerequisite-changed)))
              (setf (step-then step) (restored-step-continuation workflow step))
              (push step (workflow-steps workflow))
              (pause-step workflow step))))
      (invalidate-dependent-loops workflow dependent seen))))

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
  (setf (workflow-recovery-baseline workflow) (workflow-repositories workflow))
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
