(in-package #:agent-duet/tests)

(defmacro with-check-workflow ((workflow author reviewer directory command replies) &body body)
  `(let* ((,directory (merge-pathnames (format nil "duet-check-~a/" (ai::make-uuid))
                                      (uiop:temporary-directory)))
          (,author (make-instance 'fake-agent :id :claude :name "Claude" :directory ,directory
                                  :replies ,replies))
          (,reviewer (make-instance 'fake-agent :id :codex :name "ChatGPT" :directory ,directory
                                    :replies (list (says *approve*))))
          (,workflow (make-instance 'ai:workflow :lead ,author :partner ,reviewer :request "Do it"
                                   :check-command ,command)))
     (ensure-directories-exist ,directory)
     (unwind-protect (progn ,@body)
       (ai:stop-workflow ,workflow)
       (uiop:delete-directory-tree ,directory :validate t :if-does-not-exist :ignore))))

(define-test check-passes :parent agent-duet
  (with-check-workflow (workflow author reviewer directory "printf checked; printf error >&2" (list (says "Done")))
    (let ((messages nil))
      (setf (ai::workflow-on-message workflow)
            (lambda (from to text kind)
              (declare (ignore to))
              (when (eq kind :check) (push (list from text) messages))))
      (ai:start-workflow workflow)
      (true (wait-until (lambda () (eq :finished (ai:workflow-state workflow)))))
      (true (search "passed" (first (fake-prompts reviewer))))
      (false (search "checkederror" (first (fake-prompts reviewer))))
      (true (search (format nil "```~%checkederror~%```") (second (first messages))))
      (is = 2 (length messages))
      (true (every (lambda (m) (eq :system (first m))) messages)))))

(define-test check-fixed :parent agent-duet
  (with-check-workflow (workflow author reviewer directory "test -f done-marker"
                                (list (says "Done")
                                      (lambda (prompt)
                                        (true (search "exit code 1" prompt))
                                        (true (search (format nil "```~%") prompt))
                                        (write-file-text directory "done-marker" "ok")
                                        "Fixed")))
    (ai:start-workflow workflow)
    (true (wait-until (lambda () (eq :finished (ai:workflow-state workflow)))))
    (is = 2 (length (fake-prompts author)))
    (is = 1 (ai::loop-round (first (ai:workflow-loops workflow))))
    (true (search "passed" (first (fake-prompts reviewer))))))

(define-test check-exhausted :parent agent-duet
  (with-check-workflow (workflow author reviewer directory "echo broken >&2; exit 7"
                                (loop repeat (1+ ai::*check-fix-attempts*) collect (says "Done")))
    (ai:start-workflow workflow)
    (true (wait-until (lambda () (eq :finished (ai:workflow-state workflow)))))
    (is = (1+ ai::*check-fix-attempts*) (length (fake-prompts author)))
    (true (search "failed (exit code 7)" (first (fake-prompts reviewer))))
    (true (search (format nil "```~%broken~%~%```") (first (fake-prompts reviewer))))
    (true (search (format nil "still fails after ~d fix attempts" ai::*check-fix-attempts*)
                  (first (fake-prompts reviewer))))
    ;; The reviewer approved, but approval means the check passes too.
    (let ((loop (first (ai:workflow-loops workflow))))
      (true (ai::loop-check-failed loop))
      (is eq :unresolved (ai::loop-state loop)))))

(define-test check-on-takeover :parent agent-duet
  ;; Work taken over goes unreviewed, but its check still runs and is fixed.
  (with-check-workflow (workflow author reviewer directory "test -f done-marker"
                                (list (says "Done")
                                      (lambda (prompt)
                                        (true (search "exit code 1" prompt))
                                        (write-file-text directory "done-marker" "ok")
                                        "Fixed")))
    (setf (ai:workflow-state workflow) :running)
    (ai::run-loop workflow (ai::make-review-loop :author author :reviewer reviewer :task "Do it"
                                                 :skip-review t)
                  "Do it")
    (true (wait-until (lambda () (eq :finished (ai:workflow-state workflow)))))
    (let ((loop (first (ai:workflow-loops workflow))))
      (is = 2 (length (fake-prompts author)))
      (false (fake-prompts reviewer))
      (false (ai::loop-check-failed loop))
      (is eq :unreviewed (ai::loop-state loop)))))

(define-test check-disabled :parent agent-duet
  (dolist (command '(nil "   "))
    (with-check-workflow (workflow author reviewer directory command (list (says "Done")))
      (ai:start-workflow workflow)
      (is eq :finished (ai:workflow-state workflow))
      (let ((loop (first (ai:workflow-loops workflow))))
        (is string= (ai::review-prompt loop) (first (fake-prompts reviewer)))
        (false (ai::loop-check-report loop))))))

(define-test check-stopped :parent agent-duet
  (with-check-workflow (workflow author reviewer directory "sleep 30" (list (says "Done")))
    (ai:start-workflow workflow)
    (let* ((loop (first (ai:workflow-loops workflow)))
           (handle (car (ai::loop-check-handle loop))))
      (true (wait-until (lambda () (ai::check-handle-process handle))))
      (ai:stop-workflow workflow)
      (true (wait-until (lambda () (not (sb-thread:thread-alive-p (ai::check-handle-thread handle))))))
      (false (sb-ext:process-alive-p (ai::check-handle-process handle)))
      (is eq :stopped (ai:workflow-state workflow))
      (false (fake-prompts reviewer)))))

(define-test check-delivery-and-timeout :parent agent-duet
  (let ((ai::*check-timeout* 0.05) (deliveries nil) (result nil)
        (lock (sb-thread:make-mutex)))
    (let ((handle (ai::run-check "echo before; sleep 30" (uiop:temporary-directory)
                                (lambda (fn) (sb-thread:with-mutex (lock) (push fn deliveries)))
                                (lambda (code output timeout) (setf result (list code output timeout))))))
      (unwind-protect
           (progn
             (true (wait-until (lambda () (sb-thread:with-mutex (lock) deliveries))))
             (false result)
             (funcall (sb-thread:with-mutex (lock) (pop deliveries)))
             (true (third result))
             (true (search "before" (second result))))
        (ai::kill-check handle)))))

(define-test check-worktree-directories :parent agent-duet
  (with-git-folder (repo)
    (multiple-value-bind (author reviewer)
        (worktree-agents repo
                         (lambda (agent prompt)
                           (cond ((search "Split the work" prompt) *split-plan*)
                                 ((search "Review the work" prompt) *approve*)
                                 (t (write-file-text (ai:agent-directory agent) "a.txt" "A") "Done")))
                         (lambda (agent prompt)
                           (if (search "Review the work" prompt) *approve*
                               (progn (write-file-text (ai:agent-directory agent) "b.txt" "B") "Done"))))
      (let* ((deliveries nil)
             (lock (sb-thread:make-mutex))
             (directories nil)
             (reports nil)
             (workflow (make-instance 'ai:workflow :mode :divide :lead author :partner reviewer
                                      :worktrees t :request "Do it" :check-command "pwd")))
        (setf (ai::workflow-on-message workflow)
              (lambda (from to text kind)
                (declare (ignore from to))
                (when (eq kind :check) (push text reports))))
        ;; Like GTK, deliver results on one thread, after the launch returns.
        (dolist (agent (list author reviewer))
          (setf (ai::agent-deliver agent)
                (lambda (fn) (sb-thread:with-mutex (lock) (push fn deliveries)))))
        (unwind-protect
             (progn
               (ai:start-workflow workflow)
               (setf directories
                     (append (mapcar (lambda (loop) (namestring (truename (ai::loop-tree loop))))
                                     (ai:workflow-loops workflow))
                             (list (namestring (truename repo)))))
               (true (wait-until
                      (lambda ()
                        (let ((pending (sb-thread:with-mutex (lock) (prog1 (reverse deliveries)
                                                                    (setf deliveries nil)))))
                          (dolist (fn pending) (funcall fn)))
                        (eq :finished (ai:workflow-state workflow))) 20))
               (is = 3 (length (ai:workflow-loops workflow)))
               (loop for loop in (ai:workflow-loops workflow)
                     for expected in directories
                     do (true (find (string-right-trim "/" expected) reports :test #'search))
                        (true (search "passed" (ai::loop-check-report loop)))))
          (ai:stop-workflow workflow))))))

(define-test check-tail-limit :parent agent-duet
  (let ((ai::*check-output-limit* 4))
    (is string= "6789" (ai::check-output-tail "0123456789"))
    (is string= "abc" (ai::check-output-tail "abc"))))

(define-test check-fix-pauses :parent agent-duet
  (with-check-workflow (workflow author reviewer directory "test -f done-marker"
                                (list (says "Done") (says :out-of-usage)
                                      (lambda (prompt)
                                        (declare (ignore prompt))
                                        (write-file-text directory "done-marker" "ok")
                                        "Fixed")))
    (ai:start-workflow workflow)
    (true (wait-until (lambda () (ai::workflow-paused workflow))))
    (let ((step (first (ai::workflow-paused workflow))))
      (is eq :work (ai::step-role step))
      (is eq (first (ai:workflow-loops workflow)) (ai::step-loop step))
      (ai::resume-step workflow step))
    (true (wait-until (lambda () (eq :finished (ai:workflow-state workflow)))))
    (true (search "passed" (first (fake-prompts reviewer))))))

(define-test check-late-delivery-ignored :parent agent-duet
  (with-check-workflow (workflow author reviewer directory "true" (list (says "Done")))
    (let ((deliveries nil) (lock (sb-thread:make-mutex)))
      (setf (ai::agent-deliver author)
            (lambda (fn) (sb-thread:with-mutex (lock) (push fn deliveries))))
      ;; The fake author's own result is synchronous, like the other workflow
      ;; tests; RUN-CHECK uses the explicit delivery function above.
      (ai:start-workflow workflow)
      (true (wait-until (lambda () (sb-thread:with-mutex (lock) deliveries))))
      (ai:stop-workflow workflow)
      (funcall (sb-thread:with-mutex (lock) (pop deliveries)))
      (is eq :stopped (ai:workflow-state workflow))
      (false (fake-prompts reviewer)))))

(define-test check-fences :parent agent-duet
  ;; The same report is delivered to the UI, the author and the reviewer.
  (with-check-workflow (workflow author reviewer directory "printf '# title\\n```\\n*** failure\\n'; exit 1"
                                (loop repeat (1+ ai::*check-fix-attempts*) collect (says "Done")))
    (let ((reports nil))
      (setf (ai::workflow-on-message workflow)
            (lambda (from to text kind)
              (declare (ignore from to))
              (when (eq kind :check) (push text reports))))
      (ai:start-workflow workflow)
      (true (wait-until (lambda () (eq :finished (ai:workflow-state workflow)))))
      (let ((block (ai::check-output-block (format nil "# title~%```~%*** failure~%"))))
        (true (uiop:string-prefix-p (format nil "````~%") block))
        (true (search block (first reports)))
        (true (search block (first (fake-prompts author))))
        (true (search block (first (fake-prompts reviewer))))))))

(define-test check-launch-error :parent agent-duet
  (with-check-workflow (workflow author reviewer directory "true"
                                (loop repeat (1+ ai::*check-fix-attempts*) collect (says "Done")))
    (setf (ai:agent-directory author) (merge-pathnames "missing/" directory))
    (ai:start-workflow workflow)
    (true (wait-until (lambda () (eq :finished (ai:workflow-state workflow)))))
    (let ((prompt (first (fake-prompts reviewer))))
      (true (search "couldn't run (no exit code)" prompt))
      (false (search "exit code NIL" prompt))
      (true (search "Couldn't run check:" prompt)))))

(define-test check-file-tail :parent agent-duet
  (uiop:with-temporary-file (:pathname path :stream out :external-format :utf-8)
    ;; The seek starts inside multibyte UTF-8 text, after a large prefix.
    (write-string (make-string 1000000 :initial-element #\x) out)
    (write-string (make-string 100 :initial-element #\漢) out)
    (write-string "界abc" out)
    (finish-output out)
    (is string= "界abc" (ai::read-check-output-tail path 4))
    (is string= "漢界abc" (ai::read-check-output-tail path 5))
    (is string= "abc" (ai::read-check-output-tail path 3))
    (is string= "" (ai::read-check-output-tail path 0)))
  (uiop:with-temporary-file (:pathname path :stream out :element-type '(unsigned-byte 8))
    (write-sequence #(255 128 254 97 98 99) out)
    (finish-output out)
    (is string= "?abc" (ai::read-check-output-tail path 4))))
