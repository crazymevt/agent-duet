;;;; workflow.lisp — the two agents working on one request together
;;;;
;;;; Three modes:
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
  ((completion :initform nil :accessor workflow-completion-data)
   (completion-state :initform nil :accessor workflow-completion-state)
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
   (retention-id :initform (make-uuid) :reader workflow-retention-id)
   (integrated :initform nil :accessor workflow-integrated :documentation "True once merging has begun")
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
  (check-failed nil)                    ; true while the check fails after the fix attempts
  (round 0)                             ; reviews done
  (skip-review nil)                     ; true once taken over: its reviewer ran out
  (state :assigned))                    ; :assigned :working :checking :reviewing :paused :stopped or final outcome

(defstruct (workflow-step (:conc-name step-) (:constructor make-step))
  "A prompt given to an agent for the workflow, kept so it can be given
again, or to the other agent, after the first ran out of usage."
  agent prompt then
  role                                  ; :propose, :plan, :work, :review or :resolve
  loop)                                 ; the review loop it belongs to (not for :plan)

(defun post (workflow from to text kind)
  (when (workflow-on-message workflow)
    (funcall (workflow-on-message workflow) from to text kind)))

(defun note (workflow fmt &rest args)
  (post workflow :system nil (apply #'format nil fmt args) :note))

(defun workflow-live-p (workflow)
  (eq (workflow-state workflow) :running))

(defun submit (workflow agent prompt then &key role loop)
  "Give AGENT PROMPT; call THEN with its reply, unless the workflow has
been stopped meanwhile. A turn that failed for want of usage pauses
instead (PAUSE-STEP); another failed turn ends the workflow's part in it."
  (submit-step workflow (make-step :agent agent :prompt prompt :then then :role role :loop loop)))

(defun submit-step (workflow step)
  (agent-submit (step-agent step) (step-prompt step)
                (lambda (text error)
                  (when (workflow-live-p workflow)
                    (if (eq error :out-of-usage)
                        (pause-step workflow step)
                        (funcall (step-then step) text error))))))

;;; Carrying on past the last round

(defun working-context (workflow)
  "Each agent's folder and conversation, to tell whether they've moved on."
  (loop for agent in (list (workflow-lead workflow) (workflow-partner workflow))
        collect (list (agent-directory agent) (agent-conversation-handle agent))))

(defun continuable-loop-p (loop)
  "Stopped unapproved after a review, and not in a worktree (gone by then)."
  (and (eq (loop-state loop) :unresolved) (loop-last-review loop) (null (loop-tree loop))))

(defun workflow-continuable-p (workflow)
  "Whether WORKFLOW finished with a loop stopped at its last round, unapproved,
and the agents are still where it finished."
  (and (eq (workflow-state workflow) :finished)
       (equal (workflow-context workflow) (working-context workflow))
       (some #'continuable-loop-p (workflow-loops workflow))))

(defun continue-workflow (workflow rounds)
  "Carry WORKFLOW on for up to ROUNDS more review rounds: each loop stopped
unapproved has its author address the last review. True if it carried on."
  (when (workflow-continuable-p workflow)
    (let ((loops (remove-if-not #'continuable-loop-p (workflow-loops workflow))))
      (setf (workflow-state workflow) :running
            (workflow-completion-data workflow) nil
            (workflow-completion-error workflow) nil)
      (incf (workflow-max-rounds workflow) rounds)
      (note workflow "Carrying on for up to ~d more round~:p." rounds)
      ;; All are set working before any starts, so the first to finish
      ;; doesn't find every loop done.
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
      (:propose (format nil "~a's proposal" name))
      (:resolve (format nil "~a's resolving of the merge conflicts" name))
      (:plan (format nil "~a's plan for splitting the work" name))
      (:work (format nil "~a's work~@[ on ~a~]" name (and loop (loop-label loop))))
      (:review (format nil "~a's review of ~a" name
                       (or (and loop (loop-label loop))
                           (format nil "~a's work" (agent-name (loop-author loop)))))))))

(defun pause-step (workflow step)
  (setf (workflow-paused workflow) (append (workflow-paused workflow) (list step)))
  (when (step-loop step)
    (update-loop-state workflow (step-loop step) :paused))
  (note workflow "~a ~a paused." (agent-out-message (step-agent step)) (step-description step))
  (when (workflow-on-pause workflow)
    (funcall (workflow-on-pause workflow) workflow step)))

(defun unpause (workflow step)
  "Take STEP off the paused list; true if it was on it (so each choice counts once)."
  (when (member step (workflow-paused workflow))
    (setf (workflow-paused workflow) (remove step (workflow-paused workflow)))
    t))

(defun resume-step (workflow step)
  "Give STEP's prompt to its agent again (its usage has reset, or the user wants to try)."
  (when (and (workflow-live-p workflow) (unpause workflow step))
    (note workflow "Trying ~a again." (step-description step))
    (when (step-loop step)
      (update-loop-state workflow (step-loop step)
                         (if (eq (step-role step) :review) :reviewing :working)))
    (submit-step workflow step)))

(defun can-hand-off-p (workflow step)
  "Whether the other agent could take STEP over (it isn't out of usage too)."
  (not (agent-out-of-usage-p (other-agent workflow (step-agent step)))))

(defun hand-off-step (workflow step)
  "Have the other agent take STEP over. A review isn't handed off: the work
finishes unreviewed. Nor is a proposal: the other's goes on alone. Work or a plan goes to the other agent, and what it
does then goes unreviewed, since the one that would review it is out."
  (when (and (workflow-live-p workflow) (unpause workflow step))
    (let* ((out (step-agent step))
           (other (other-agent workflow out))
           (loop (step-loop step)))
      (ecase (step-role step)
        (:propose
         (proposal-in workflow out nil (format nil "~a ran out of usage" (agent-name out))))
        (:resolve
         (note workflow "~a resolves the conflicts instead." (agent-name other))
         (submit-step workflow (make-step :agent other :prompt (step-prompt step) :then (step-then step)
                                          :role :resolve)))
        (:review
         (update-loop-state workflow loop :unreviewed)
         (note workflow "~@[~a ~]finished without a review: ~a ran out of usage."
               (and (loop-label loop) (string-capitalize (loop-label loop) :end 1)) (agent-name out))
         (check-finished workflow))
        (:work
         (note workflow "~a takes over ~a. It won't be reviewed: ~a is out of usage."
               (agent-name other) (step-description step) (agent-name out))
         (setf (loop-author loop) other
               (loop-skip-review loop) t)
         (loop-work workflow loop (takeover-prompt out (step-prompt step) (loop-tree loop))))
        (:plan
         (note workflow "~a does the whole task. It won't be reviewed: ~a is out of usage."
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
  (publish-checklist workflow loop))

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
        (ignore-errors (repository-state (agent-directory (workflow-lead workflow)))))
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
  (setf (loop-check-report loop) nil
        (loop-check-failed loop) nil)
  (update-loop-state workflow loop :working)
  (submit workflow (loop-author loop) prompt
          (lambda (text error)
            (cond (error
                   (loop-failed workflow loop (format nil "~a's turn failed: ~a" (agent-name (loop-author loop)) text)))
                  (t
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
                      :role :work :loop loop)))))))

(defun loop-check (workflow loop &optional (fixes 0))
  "Check the current work before review; FIXES does not count review rounds."
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
              (kill-check (car token))))))))

(defun cancel-workflow-checks (workflow)
  (dolist (loop (workflow-loops workflow))
    (let ((token (loop-check-handle loop)))
      (setf (loop-check-handle loop) nil)
      (when token (kill-check (car token))))))

(defun loop-review (workflow loop &optional confirming)
  "Have LOOP's reviewer review the work; CONFIRMING: it approved with findings
still open, and is asked once to settle them (that's no new round)."
  (update-loop-state workflow loop :reviewing)
  (submit workflow (loop-reviewer loop) (if confirming (confirm-prompt loop) (review-prompt loop))
          (lambda (text error)
            (if error
                (loop-failed workflow loop (format nil "~a's review failed: ~a" (agent-name (loop-reviewer loop)) text))
                (review-in workflow loop text confirming)))
          :role :review :loop loop))

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
  (when (and (workflow-live-p workflow) (null (workflow-paused workflow))
             (every #'loop-done-p (workflow-loops workflow)))
    (let ((failed (find :failed (workflow-loops workflow) :key #'loop-state)))
      (cond ((or (null (workflow-trees workflow)) (workflow-integrated workflow))
             (finish-workflow workflow))
            ;; Its work may be half done: keep it out of the folder.
            (failed
             (fail workflow "~a failed, so the parts aren't merged." (string-capitalize (loop-label failed) :end 1)))
            (t (integrate workflow))))))

(defun finish-workflow (workflow)
  (cancel-workflow-checks workflow)
  (let ((merged (workflow-merged-loop workflow)))
    (when (and merged (eq (loop-state merged) :approved))
      (cleanup-workflow-worktrees workflow)))
  (setf (workflow-state workflow) :finished
        (workflow-context workflow) (working-context workflow))
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
  (setf (workflow-completion-error workflow) (apply #'format nil fmt args))
  (apply #'note workflow fmt args)
  (leave-worktrees workflow)
  (setf (workflow-state workflow) :finished)
  (note-retained-worktrees workflow)
  (capture-workflow-completion workflow)
  (when (workflow-on-finish workflow)
    (funcall (workflow-on-finish workflow) workflow)))

(defun stop-workflow (workflow)
  "Stop WORKFLOW and both agents' work on it."
  (when (workflow-live-p workflow)
    (setf (workflow-state workflow) :stopped
          (workflow-paused workflow) '()
          (workflow-pending-plan workflow) nil)
    (cancel-workflow-checks workflow)
    (agent-cancel (workflow-lead workflow))
    (agent-cancel (workflow-partner workflow))
    (dolist (loop (workflow-loops workflow))
      (unless (loop-done-p loop)
        (update-loop-state workflow loop :stopped)))
    (note workflow "Stopped.")
    (leave-worktrees workflow)
    (note-retained-worktrees workflow)
    (capture-workflow-completion workflow)
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
          t)
      (error (e)
        (fail workflow "Couldn't make the worktrees: ~a" e)
        nil))))

(defun leave-worktrees (workflow)
  "Ending before the parts are merged: the agents go back to the folder,
and the worktrees are kept, with the work done in them."
  (when (and (workflow-trees workflow) (not (workflow-integrated workflow)))
    (setf (workflow-integrated workflow) t)
    (dolist (entry (workflow-trees workflow))
      (move-agent (car entry) (workflow-home workflow)))
    (note workflow "The worktrees are kept, with their work: ~{~a~^, ~}."
          (mapcar (lambda (e) (namestring (cdr e))) (reverse (workflow-trees workflow))))))

(defun integrate (workflow)
  "Both parts are done: commit each on its branch, bring the agents back
to the folder, and merge the branches into it."
  (setf (workflow-integrated workflow) t)
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
              (setf (workflow-recovery-recorded workflow) t)))
          (note workflow "Both parts are committed on their branches; recovery refs are kept for task ~a. Merging them into the folder."
                (workflow-retention-id workflow))
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
    (if (null trees)
        (merged workflow)
        (let ((branch (worktree-branch (first trees))))
          (multiple-value-bind (status files)
              (handler-case (merge-worktree home (first trees))
                (error (e) (values :error (princ-to-string e))))
            (case status
              (:merged
               (note workflow "Merged ~a." branch)
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
                                (post workflow (workflow-lead workflow) :user text :work)
                                (note workflow "Merged ~a." branch)
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
    (setf (workflow-completion-state workflow)
          (ignore-errors (repository-state
                          (or (workflow-home workflow)
                              (agent-directory (workflow-lead workflow)))))
          (workflow-completion-data workflow)
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

(defun workflow-commit-reason (workflow)
  "NIL when eligible; otherwise a user-facing reason. Always rechecks Git."
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
