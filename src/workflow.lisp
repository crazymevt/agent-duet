;;;; workflow.lisp — the two agents working on one request together
;;;;
;;;; Two modes:
;;;;
;;;;   :review  The lead does the work; the partner reviews it; the lead
;;;;            addresses the review; and so on until the partner approves
;;;;            or MAX-ROUNDS reviews have been done.
;;;;
;;;;   :divide  The lead splits the request into its part and the partner's.
;;;;            Both work at once; then each reviews the other's part, and
;;;;            each part goes round its own review loop, as in :review.
;;;;
;;;; A REVIEW-LOOP is one piece of work with its author and reviewer. The
;;;; workflow's ON-MESSAGE is told of each message passed between them, for
;;;; the conversation view: (on-message from to text kind), FROM and TO
;;;; being agents, :user or :system, and KIND one of :request :plan :work
;;;; :review :note.
;;;;
;;;; When an agent's account runs out of usage, the step it was given (a
;;;; STEP) pauses rather than fails, and ON-PAUSE is told: the user decides
;;;; whether to RESUME-STEP (once the usage resets), HAND-OFF-STEP to the
;;;; other agent, or STOP-WORKFLOW. Work handed off goes unreviewed when the
;;;; one that ran out was to review it, and says so.
;;;;
;;;; A workflow that finished with a loop still unapproved after MAX-ROUNDS
;;;; can be carried on for more rounds (CONTINUE-WORKFLOW): the author
;;;; addresses the last review, as it would have in another round. Only
;;;; while both agents are still in the folder and conversations it
;;;; finished in: elsewhere, the review would be out of context.

(in-package #:agent-duet)

(defparameter *default-max-rounds* 3)

(defclass workflow ()
  ((mode :initarg :mode :initform :review :reader workflow-mode)
   (lead :initarg :lead :reader workflow-lead)
   (partner :initarg :partner :reader workflow-partner)
   (request :initarg :request :reader workflow-request)
   (max-rounds :initarg :max-rounds :initform *default-max-rounds* :accessor workflow-max-rounds)
   (on-message :initarg :on-message :initform nil :accessor workflow-on-message)
   (on-finish :initarg :on-finish :initform nil :accessor workflow-on-finish)
   (on-pause :initarg :on-pause :initform nil :accessor workflow-on-pause)
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
  (round 0)                             ; reviews done
  (skip-review nil)                     ; true once taken over: its reviewer ran out
  (state :working))                     ; :working :reviewing :approved :unresolved :unreviewed :failed

(defstruct (workflow-step (:conc-name step-) (:constructor make-step))
  "A prompt given to an agent for the workflow, kept so it can be given
again, or to the other agent, after the first ran out of usage."
  agent prompt then
  role                                  ; :plan, :work or :review
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

(defun workflow-continuable-p (workflow)
  "Whether WORKFLOW finished with a loop stopped at its last round, unapproved,
and the agents are still where it finished."
  (and (eq (workflow-state workflow) :finished)
       (equal (workflow-context workflow) (working-context workflow))
       (some (lambda (l) (and (eq (loop-state l) :unresolved) (loop-last-review l)))
             (workflow-loops workflow))))

(defun continue-workflow (workflow rounds)
  "Carry WORKFLOW on for up to ROUNDS more review rounds: each loop stopped
unapproved has its author address the last review. True if it carried on."
  (when (workflow-continuable-p workflow)
    (let ((loops (remove-if-not (lambda (l) (and (eq (loop-state l) :unresolved) (loop-last-review l)))
                                (workflow-loops workflow))))
      (setf (workflow-state workflow) :running)
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
      (:plan (format nil "~a's plan for splitting the work" name))
      (:work (format nil "~a's work~@[ on ~a~]" name (and loop (loop-label loop))))
      (:review (format nil "~a's review of ~a" name
                       (or (and loop (loop-label loop))
                           (format nil "~a's work" (agent-name (loop-author loop)))))))))

(defun pause-step (workflow step)
  (setf (workflow-paused workflow) (append (workflow-paused workflow) (list step)))
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
    (submit-step workflow step)))

(defun can-hand-off-p (workflow step)
  "Whether the other agent could take STEP over (it isn't out of usage too)."
  (not (agent-out-of-usage-p (other-agent workflow (step-agent step)))))

(defun hand-off-step (workflow step)
  "Have the other agent take STEP over. A review isn't handed off: the work
finishes unreviewed. Work or a plan goes to the other agent, and what it
does then goes unreviewed, since the one that would review it is out."
  (when (and (workflow-live-p workflow) (unpause workflow step))
    (let* ((out (step-agent step))
           (other (other-agent workflow out))
           (loop (step-loop step)))
      (ecase (step-role step)
        (:review
         (setf (loop-state loop) :unreviewed)
         (note workflow "~@[~a ~]finished without a review: ~a ran out of usage."
               (and (loop-label loop) (string-capitalize (loop-label loop) :end 1)) (agent-name out))
         (check-finished workflow))
        (:work
         (note workflow "~a takes over ~a. It won't be reviewed: ~a is out of usage."
               (agent-name other) (step-description step) (agent-name out))
         (setf (loop-author loop) other
               (loop-skip-review loop) t)
         (loop-work workflow loop (takeover-prompt out (step-prompt step))))
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
  (let ((author (agent-name (loop-author loop))))
    (format nil "~a was asked to do this:~%~%~a~%~%~a reports:~%~%~a~%~%~
Review the work: read the changed files (and `git diff`, if this is a git repository) rather than ~
relying on the report. Don't change any files yourself. Look for bugs, missed requirements, and ~
anything risky or needlessly complicated, and list concrete issues, most important first.~%~%~
End your reply with one line on its own, exactly one of:~%~
VERDICT: APPROVED~%~
VERDICT: CHANGES REQUESTED"
            author (loop-task loop) author (loop-summary loop))))

(defun fix-prompt (loop review)
  (format nil "~a reviewed your work and requested changes:~%~%~a~%~%~
Address the points you agree with, and say briefly why for any you don't. Then summarize what you changed."
          (agent-name (loop-reviewer loop)) review))

(defun plan-prompt (partner request)
  (let ((p (agent-name partner)))
    (format nil "~a~%~%You are leading this task with ~a, another coding agent working in the same ~
directory at the same time. Don't change anything yet. Split the work into two parts that can be done ~
in parallel without editing the same files: one for you and one for ~a. Reply in exactly this form:~%~%~
LEAD TASK:~%<your part>~%~%~
PARTNER TASK:~%<~a's part, complete in itself: ~a won't see this conversation>~%~%~
If the task is too small to split, write NONE under PARTNER TASK."
            request p p p p)))

(defun lead-go-prompt (partner partner-task)
  (format nil "Go ahead with your part (LEAD TASK). ~a is doing this part at the same time, so leave ~
its files alone:~%~%~a~%~%When you finish, ~a will review your part."
          (agent-name partner) partner-task (agent-name partner)))

(defun partner-task-prompt (lead request lead-task partner-task)
  (let ((l (agent-name lead)))
    (format nil "~a is sharing a task with you. The overall request:~%~%~a~%~%Your part:~%~%~a~%~%~
~a is doing this part at the same time, so leave its files alone:~%~%~a~%~%~
When you finish, ~a will review your part."
            l request partner-task l lead-task l)))

(defun takeover-prompt (out prompt)
  (format nil "~a ran out of usage partway through a task, so you're taking it over. Some of its work ~
may already be in the files: look before you continue, and don't redo what's done. Here is what ~a was ~
asked (\"you\" there meant ~a):~%~%~a~%~%~
No one will review your work this time, since ~a is out of usage. Finish with a summary of what you changed."
          (agent-name out) (agent-name out) (agent-name out) prompt (agent-name out)))

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

;;; Running

(defun start-workflow (workflow)
  (setf (workflow-state workflow) :running)
  (let ((lead (workflow-lead workflow))
        (partner (workflow-partner workflow))
        (request (workflow-request workflow)))
    (post workflow :user lead request :request)
    (ecase (workflow-mode workflow)
      (:review
       (note workflow "~a writes, ~a reviews (up to ~d rounds)." (agent-name lead) (agent-name partner)
             (workflow-max-rounds workflow))
       (run-loop workflow (make-review-loop :author lead :reviewer partner :task request)
                 (work-prompt partner request)))
      (:divide
       (note workflow "~a splits the work with ~a." (agent-name lead) (agent-name partner))
       (submit workflow lead (plan-prompt partner request)
               (lambda (text error)
                 (if error
                     (fail workflow "~a couldn't make a plan: ~a" (agent-name lead) text)
                     (start-parts workflow text)))
               :role :plan)))
    workflow))

(defun start-parts (workflow plan)
  (let ((lead (workflow-lead workflow))
        (partner (workflow-partner workflow))
        (request (workflow-request workflow)))
    (multiple-value-bind (lead-task partner-task) (parse-plan plan)
      (post workflow lead partner plan :plan)
      (if (null partner-task)
          (progn
            (note workflow "~a is doing it alone; ~a will review." (agent-name lead) (agent-name partner))
            (run-loop workflow (make-review-loop :author lead :reviewer partner :task request)
                      (format nil "Then do the whole task yourself.~%~%(When you finish, ~a will review your work.)"
                              (agent-name partner))))
          (let ((mine (make-review-loop :author lead :reviewer partner :task lead-task
                                        :label (format nil "~a's part" (agent-name lead))))
                (theirs (make-review-loop :author partner :reviewer lead :task partner-task
                                          :label (format nil "~a's part" (agent-name partner)))))
            ;; Both are registered before either starts, so the first to
            ;; finish doesn't find every loop done.
            (setf (workflow-loops workflow) (list mine theirs))
            (loop-work workflow mine (lead-go-prompt partner partner-task))
            (loop-work workflow theirs (partner-task-prompt lead request lead-task partner-task)))))))

(defun run-loop (workflow loop prompt)
  "Start LOOP: its author works on PROMPT, and then the reviewing begins."
  (setf (workflow-loops workflow) (append (workflow-loops workflow) (list loop)))
  (loop-work workflow loop prompt))

(defun loop-work (workflow loop prompt)
  (setf (loop-state loop) :working)
  (submit workflow (loop-author loop) prompt
          (lambda (text error)
            (cond (error
                   (loop-failed workflow loop (format nil "~a's turn failed: ~a" (agent-name (loop-author loop)) text)))
                  (t
                   (setf (loop-summary loop) text)
                   (post workflow (loop-author loop) (loop-reviewer loop) text :work)
                   (if (loop-skip-review loop)
                       (progn (setf (loop-state loop) :unreviewed)
                         (note workflow "~@[~a ~]done, not reviewed."
                               (and (loop-label loop) (string-capitalize (loop-label loop) :end 1)))
                         (check-finished workflow))
                       (loop-review workflow loop)))))
          :role :work :loop loop))

(defun loop-review (workflow loop)
  (setf (loop-state loop) :reviewing)
  (submit workflow (loop-reviewer loop) (review-prompt loop)
          (lambda (text error)
            (if error
                (loop-failed workflow loop (format nil "~a's review failed: ~a" (agent-name (loop-reviewer loop)) text))
                (progn
                  (incf (loop-round loop))
                  (setf (loop-last-review loop) text)
                  (post workflow (loop-reviewer loop) (loop-author loop) text :review)
                  (cond ((eq (review-verdict text) :approved)
                         (setf (loop-state loop) :approved)
                         (note workflow "~a approved ~a." (agent-name (loop-reviewer loop))
                               (or (loop-label loop) "the work"))
                         (check-finished workflow))
                        ((>= (loop-round loop) (workflow-max-rounds workflow))
                         (setf (loop-state loop) :unresolved)
                         (note workflow "Stopping ~a after ~d review round~:p without approval."
                               (or (loop-label loop) "the review") (loop-round loop))
                         (check-finished workflow))
                        (t
                         (note workflow "Round ~d~@[ (~a)~]: ~a addresses the review."
                               (1+ (loop-round loop)) (loop-label loop) (agent-name (loop-author loop)))
                         (loop-work workflow loop (fix-prompt loop text)))))))
          :role :review :loop loop))

(defun loop-failed (workflow loop why)
  (setf (loop-state loop) :failed)
  (note workflow "~a" why)
  (check-finished workflow))

(defun loop-done-p (loop)
  (member (loop-state loop) '(:approved :unresolved :unreviewed :failed)))

(defun check-finished (workflow)
  (when (and (workflow-live-p workflow) (null (workflow-paused workflow))
             (every #'loop-done-p (workflow-loops workflow)))
    (setf (workflow-state workflow) :finished
          (workflow-context workflow) (working-context workflow))
    (note workflow "Done: ~{~a~^; ~}."
          (loop for l in (workflow-loops workflow)
                collect (format nil "~@[~a ~]~(~a~)" (loop-label l)
                                (case (loop-state l)
                                  (:approved "approved")
                                  (:unresolved "not approved")
                                  (:unreviewed "not reviewed")
                                  (t "failed")))))
    (when (workflow-on-finish workflow)
      (funcall (workflow-on-finish workflow) workflow))))

(defun fail (workflow fmt &rest args)
  (apply #'note workflow fmt args)
  (setf (workflow-state workflow) :finished)
  (when (workflow-on-finish workflow)
    (funcall (workflow-on-finish workflow) workflow)))

(defun stop-workflow (workflow)
  "Stop WORKFLOW and both agents' work on it."
  (when (workflow-live-p workflow)
    (setf (workflow-state workflow) :stopped
          (workflow-paused workflow) '())
    (agent-cancel (workflow-lead workflow))
    (agent-cancel (workflow-partner workflow))
    (note workflow "Stopped.")
    (when (workflow-on-finish workflow)
      (funcall (workflow-on-finish workflow) workflow))))
