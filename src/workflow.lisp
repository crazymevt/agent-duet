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

(in-package #:agent-integration)

(defparameter *default-max-rounds* 3)

(defclass workflow ()
  ((mode :initarg :mode :initform :review :reader workflow-mode)
   (lead :initarg :lead :reader workflow-lead)
   (partner :initarg :partner :reader workflow-partner)
   (request :initarg :request :reader workflow-request)
   (max-rounds :initarg :max-rounds :initform *default-max-rounds* :reader workflow-max-rounds)
   (on-message :initarg :on-message :initform nil :accessor workflow-on-message)
   (on-finish :initarg :on-finish :initform nil :accessor workflow-on-finish)
   (loops :initform '() :accessor workflow-loops)
   (state :initform :new :accessor workflow-state :documentation ":new :running :finished :stopped")))

(defstruct (review-loop (:conc-name loop-))
  author reviewer
  task                                  ; what the author was asked to do
  label                                 ; how to name it in notes: nil, or "Claude's part"
  (summary "")                          ; the author's latest report
  (round 0)                             ; reviews done
  (state :working))                     ; :working :reviewing :approved :unresolved :failed

(defun post (workflow from to text kind)
  (when (workflow-on-message workflow)
    (funcall (workflow-on-message workflow) from to text kind)))

(defun note (workflow fmt &rest args)
  (post workflow :system nil (apply #'format nil fmt args) :note))

(defun workflow-live-p (workflow)
  (eq (workflow-state workflow) :running))

(defun submit (workflow agent prompt then)
  "Give AGENT PROMPT; call THEN with its reply, unless the workflow has
been stopped meanwhile. A failed turn ends the workflow's part in it."
  (agent-submit agent prompt
                (lambda (text error)
                  (when (workflow-live-p workflow)
                    (funcall then text error)))))

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
                     (start-parts workflow text))))))
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
            (if error
                (loop-failed workflow loop (format nil "~a's turn failed: ~a" (agent-name (loop-author loop)) text))
                (progn
                  (setf (loop-summary loop) text)
                  (post workflow (loop-author loop) (loop-reviewer loop) text :work)
                  (loop-review workflow loop))))))

(defun loop-review (workflow loop)
  (setf (loop-state loop) :reviewing)
  (submit workflow (loop-reviewer loop) (review-prompt loop)
          (lambda (text error)
            (if error
                (loop-failed workflow loop (format nil "~a's review failed: ~a" (agent-name (loop-reviewer loop)) text))
                (progn
                  (incf (loop-round loop))
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
                         (loop-work workflow loop (fix-prompt loop text)))))))))

(defun loop-failed (workflow loop why)
  (setf (loop-state loop) :failed)
  (note workflow "~a" why)
  (check-finished workflow))

(defun loop-done-p (loop)
  (member (loop-state loop) '(:approved :unresolved :failed)))

(defun check-finished (workflow)
  (when (and (workflow-live-p workflow) (every #'loop-done-p (workflow-loops workflow)))
    (setf (workflow-state workflow) :finished)
    (note workflow "Done: ~{~a~^; ~}."
          (loop for l in (workflow-loops workflow)
                collect (format nil "~@[~a ~]~(~a~)" (loop-label l)
                                (case (loop-state l)
                                  (:approved "approved")
                                  (:unresolved "not approved")
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
    (setf (workflow-state workflow) :stopped)
    (agent-cancel (workflow-lead workflow))
    (agent-cancel (workflow-partner workflow))
    (note workflow "Stopped.")
    (when (workflow-on-finish workflow)
      (funcall (workflow-on-finish workflow) workflow))))
