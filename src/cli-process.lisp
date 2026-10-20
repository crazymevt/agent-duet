;;;; Shared CLI process lifetime handling.
(in-package #:agent-duet)

(defun watch-cli-output (process read-output disconnected &key cleanup)
  "Read events in order, but detect process death even if a child holds stdout.
DISCONNECTED runs once, on EOF/read failure or process exit. A bounded drain
lets a normal final reply arrive before the exit notification. CLEANUP runs
on the exit watcher after reaping and after the reader releases its streams."
  (let ((lock (sb-thread:make-mutex :name "CLI disconnect")) (reported nil))
    (labels ((report-disconnect ()
               (let ((report (sb-thread:with-mutex (lock)
                               (unless reported (setf reported t)))))
                 (when report (funcall disconnected (sb-ext:process-exit-code process))))))
      (let ((reader (sb-thread:make-thread
                     (lambda () (unwind-protect (ignore-errors (funcall read-output)) (report-disconnect)))
                     :name "CLI output")))
        (sb-thread:make-thread
         (lambda ()
           (sb-ext:process-wait process)
           (ignore-errors (sb-thread:join-thread reader :timeout 0.25 :default nil))
           (unwind-protect (report-disconnect)
             (when cleanup
               ;; Reporting is bounded; resource disposal waits for the reader
               ;; independently, including when descendants still own stdout.
               (ignore-errors (sb-thread:join-thread reader :default nil))
               (funcall cleanup))))
         :name "CLI exit")
        reader))))

