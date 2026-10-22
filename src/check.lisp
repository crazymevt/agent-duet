;;;; check.lisp — asynchronous shell checks, independent of GTK
(in-package #:agent-duet)

(defparameter *check-timeout* 600 "Maximum seconds for a check command.")
(defparameter *check-output-limit* 4000 "Maximum characters of check output shown to agents and users.")
(defparameter *check-fix-attempts* 2 "Author fixes allowed per work round before reviewing a failed check.")

(defstruct check-handle
  process thread cancelled log-path
  (lock (sb-thread:make-mutex :name "check")))

(defun kill-check (handle &optional (cancelled t))
  "Cancel HANDLE, including a process that has not yet finished launching."
  (when handle
    (sb-thread:with-mutex ((check-handle-lock handle))
      (when cancelled (setf (check-handle-cancelled handle) t))
      (when (and (check-handle-process handle)
                 (sb-ext:process-alive-p (check-handle-process handle)))
        ;; SBCL starts children in their own process group. Kill the shell's
        ;; children too (test runners commonly start more processes).
        (ignore-errors (sb-ext:process-kill (check-handle-process handle) 9 :process-group))
        (ignore-errors (sb-ext:process-kill (check-handle-process handle) 9))))))

(defun check-output-tail (output &optional (limit *check-output-limit*))
  (subseq output (max 0 (- (length output) limit))))

(defun read-check-output-tail (path &optional (limit *check-output-limit*))
  "Read only enough final UTF-8 bytes for LIMIT characters, replacing invalid bytes."
  (if (zerop limit)
      ""
      (with-open-file (in path :element-type '(unsigned-byte 8))
        ;; A UTF-8 character uses at most four bytes. Three extra bytes
        ;; cover a character straddling the start of the slice.
        (let* ((size (file-length in))
               (start (max 0 (- size (+ (* 4 limit) 3))))
               (bytes (make-array (- size start) :element-type '(unsigned-byte 8))))
          (file-position in start)
          (let ((end (read-sequence bytes in))
                (first 0))
            ;; Don't decode a partial character at the slice boundary.
            (when (plusp start)
              (loop while (and (< first end) (= #x80 (logand #xc0 (aref bytes first))))
                    do (incf first)))
            (check-output-tail
             (sb-ext:octets-to-string bytes :start first :end end
                                     :external-format '(:utf-8 :replacement #\?))
             limit))))))

(defun check-diagnostic-line-p (line)
  "Recognize diagnostic words, excluding filenames and zero-failure summaries."
  (loop with previous = nil
        for word in (uiop:split-string (string-downcase line)
                                       :separator '(#\Space #\Tab #\Return #\: #\[ #\] #\( #\)))
        unless (zerop (length word))
          do (when (and (member word '("error" "errors" "fatal" "fail" "failed"
                                       "failure" "failures" "exception") :test #'string=)
                        (not (member previous '("0" "no" "without") :test #'equal)))
               (return t))
             (setf previous word)))

(defun read-check-output-excerpt (path &optional (limit *check-output-limit*))
  "First failure with two preceding lines and nearby output, plus the tail.
Bound both the result and buffered lines, even for unusually long lines."
  (let ((tail (read-check-output-tail path limit))
        (budget (floor limit 2)) (diagnostic nil) (remaining 0) (previous nil))
    (when (plusp budget)
      (with-open-file (in path :external-format '(:utf-8 :replacement #\?))
        (loop with line = (make-string-output-stream)
              with size = 0
              for char = (read-char in nil nil)
              do (cond
                   ((or (null char) (char= char #\Newline))
                    (let ((text (get-output-stream-string line)))
                      (when (and (null diagnostic) (check-diagnostic-line-p text))
                        ;; A diagnostic already in the tail needs no extra copy.
                        (when (search text tail) (return))
                        (setf diagnostic (format nil "~{~a~%~}" (reverse previous))
                              remaining 6))
                      (when diagnostic
                        (let ((piece (concatenate 'string text (string #\Newline))))
                          (setf diagnostic
                                (concatenate 'string diagnostic
                                             (subseq piece 0 (min (length piece)
                                                                  (- budget (length diagnostic)))))))
                        (when (or (zerop (decf remaining)) (>= (length diagnostic) budget))
                          (return)))
                      ;; At most one quarter of the diagnostic budget is context.
                      (push (subseq text 0 (min (length text) (max 0 (1- (floor budget 8))))) previous)
                      (setf previous (subseq previous 0 (min 2 (length previous)))
                            size 0)))
                   ((< size budget) (write-char char line) (incf size)))
              while char)))
    (let ((marker (format nil "~%[… final output …]~%")))
      (if (and diagnostic (> limit (+ (length marker) (length diagnostic))))
          (concatenate 'string diagnostic marker
                       (read-check-output-tail path (- limit (length diagnostic) (length marker))))
          tail))))

(defun check-output-block (output)
  "Fence OUTPUT without allowing embedded backticks to close the block."
  (let ((longest 0) (run 0))
    (loop for char across output
          do (if (char= char #\`)
                 (setf longest (max longest (incf run)))
                 (setf run 0)))
    (let ((fence (make-string (max 3 (1+ longest)) :initial-element #\`)))
      (format nil "~a~%~a~%~a" fence output fence))))

(defun run-check (command directory deliver callback &optional log-reporter)
  "Run /bin/sh -c COMMAND in DIRECTORY off the calling thread.
Return a cancellable handle. Through DELIVER, call CALLBACK with
(exit-code output-tail timed-out); launch errors have a nil exit code."
  (let ((handle (make-check-handle))
        ;; Capture dynamic parameters before entering a new thread.
        (timeout *check-timeout*)
        (output-limit *check-output-limit*)
        (artifact-directory (history-directory))
        (artifact-byte-limit *artifact-byte-limit*)
        (artifact-count-limit *artifact-count-limit*)
        (artifact-max-age *artifact-max-age*))
    (setf (check-handle-thread handle)
          (sb-thread:make-thread
           (lambda ()
             (let ((code nil) (output "") (timed-out nil))
               (handler-case
                   (uiop:with-temporary-file (:pathname path :stream stream)
                     (close stream)
                     (unwind-protect
                          (let ((process nil)
                                (deadline (+ (get-internal-real-time)
                                             (* timeout internal-time-units-per-second))))
                            (sb-thread:with-mutex ((check-handle-lock handle))
                              (unless (check-handle-cancelled handle)
                                (setf process (sb-ext:run-program "/bin/sh" (list "-c" command)
                                                                 :directory directory :wait nil
                                                                 :output path :if-output-exists :supersede
                                                                 :error :output)
                                      (check-handle-process handle) process)))
                            (when process
                              (loop while (sb-ext:process-alive-p process)
                                    do (when (>= (get-internal-real-time) deadline)
                                         (setf timed-out t)
                                         (kill-check handle nil)
                                         (return))
                                       (sleep 0.02))
                              (sb-ext:process-wait process)
                              (setf code (sb-ext:process-exit-code process)
                                    output (if (and (eql code 0) (not timed-out))
                                               (read-check-output-tail path output-limit)
                                               (read-check-output-excerpt path output-limit)))
                              (when (and (not (check-handle-cancelled handle))
                                         (or timed-out (not (eql code 0))))
                                (setf (check-handle-log-path handle)
                                      (let ((*history-directory* artifact-directory)
                                            (*artifact-byte-limit* artifact-byte-limit)
                                            (*artifact-count-limit* artifact-count-limit)
                                            (*artifact-max-age* artifact-max-age))
                                        (retain-task-artifact "check" :source path))))))
                       (when (check-handle-process handle)
                         (sb-ext:process-close (check-handle-process handle)))))
                 (error (e) (setf code nil output (check-output-tail (format nil "Couldn't run check: ~a" e) output-limit))))
               (when log-reporter (funcall log-reporter (check-handle-log-path handle)))
               (funcall deliver (lambda () (funcall callback code output timed-out)))))
           :name "workflow check"))
    handle))
