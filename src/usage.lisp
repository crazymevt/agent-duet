;;;; usage.lisp — how much each agent may still do, and running out
;;;;
;;;; Both CLIs report their account's usage windows (five hours, seven
;;;; days…): Claude in a rate_limit_event on each turn, Codex through
;;;; account/rateLimits. Here both become WINDOWs, and a turn that failed
;;;; because the account is out of usage is recognized as such, with the time
;;;; it resets when that is known.

(in-package #:agent-integration)

(defstruct (window (:constructor make-window (label remaining resets-at)))
  label         ; "5h", "7d"
  remaining     ; percent left, 0–100, exact: rounded only to show it
  resets-at)    ; a universal time, or nil

(defparameter *low-usage-percent* 10
  "Below this much left in any window, the agent's usage shows as low.")

(defun unix-to-universal (seconds)
  (and (realp seconds) (+ (round seconds) (encode-universal-time 0 0 0 1 1 1970 0))))

(defun clamp-percent (x) (max 0 (min 100 x)))

(defun claude-usage-windows (info)
  "Windows from a rate_limit_event's rate_limit_info."
  (let ((windows (jget info "unifiedWindows")))
    (when (jobject-p windows)
      (loop for (name . window) in (cdr windows)
            for used = (jget window "utilization")
            when (realp used)
              collect (make-window (cond ((equal name "five_hour") "5h")
                                         ((equal name "seven_day") "7d")
                                         (t (substitute #\Space #\_ name)))
                                   (clamp-percent (* 100 (- 1 used)))
                                   (unix-to-universal (jget window "resetsAt")))))))

(defun codex-usage-windows (buckets)
  "Windows from Codex's rate-limit BUCKETS ((id . snapshot)s)."
  (loop for (bucket-id . bucket) in buckets
        for several = (> (length buckets) 1)
        append (loop for name in '("primary" "secondary")
                     for window = (jget bucket name)
                     for used = (jget window "usedPercent")
                     when (realp used)
                       collect (make-window (format nil "~@[~a ~]~@[~a~]" (and several bucket-id)
                                                    (rate-window-label (jget window "windowDurationMins")))
                                            (clamp-percent (- 100 used))
                                            (unix-to-universal (jget window "resetsAt"))))))

(defun usage-summary (windows)
  "What is left in WINDOWS, as \"72% 5h / 40% 7d\", or nil."
  (and windows
       (format nil "~{~a~^ / ~}"
               (mapcar (lambda (w) (format nil "~a%~@[ ~a~]"
                                           (let ((left (window-remaining w)))
                                             ;; A sliver left isn't empty: not \"0%\".
                                             (if (and (plusp left) (< left 0.5)) "<1" (round left)))
                                           (let ((l (string-trim " " (window-label w)))) (and (plusp (length l)) l))))
                       windows))))

(defun usage-low-p (windows)
  (some (lambda (w) (< (window-remaining w) *low-usage-percent*)) windows))

(defun exhausted-reset (windows)
  "When the windows that are used up all reset (the latest of their reset
times), if any is used up and that is known. Used up means nothing at all
left: a window at 99.6% isn't, however it shows."
  (let ((empty (remove-if-not (lambda (w) (<= (window-remaining w) 0)) windows)))
    (and empty (every #'window-resets-at empty)
         (reduce #'max empty :key #'window-resets-at))))

(defun format-reset-time (universal &optional (now (get-universal-time)))
  "UNIVERSAL as a local time: \"3:05 PM\" today, else \"Tue 3:05 PM\"."
  (multiple-value-bind (s minute hour day month year weekday) (decode-universal-time universal)
    (declare (ignore s month year))
    (let ((today (nth-value 3 (decode-universal-time now))))
      (format nil "~:[~a ~;~*~]~d:~2,'0d ~a"
              (= day today)
              (nth weekday '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
              (let ((h (mod hour 12))) (if (zerop h) 12 h))
              minute
              (if (< hour 12) "AM" "PM")))))

;;; Recognizing a turn that failed for want of usage

(defparameter *codex-limit-errors* '("usageLimitExceeded" "rateLimitExceeded"))

(defun codex-limit-error-p (turn)
  "Whether TURN (from turn/completed) failed because the account is out of usage."
  (let ((info (jget turn "error" "codexErrorInfo")))
    (and (stringp info) (member info *codex-limit-errors* :test #'string=) t)))

(defparameter *claude-limit-phrases*
  '("usage limit" "hit your limit" "limit reached" "rate limit" "out of extra usage" "limit will reset")
  "What Claude Code's error text says when the account is out of usage.")

(defun claude-limit-text-p (text)
  (and (stringp text)
       (some (lambda (phrase) (search phrase text :test #'char-equal)) *claude-limit-phrases*)
       t))

(defun claude-limit-text-reset (text)
  "The reset time in an old-style \"Claude AI usage limit reached|<unix time>\" message, or nil."
  (let ((bar (and (stringp text) (position #\| text :from-end t))))
    (and bar (unix-to-universal (ignore-errors (parse-integer text :start (1+ bar)))))))
