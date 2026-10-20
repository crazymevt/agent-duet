;;;; app.lisp — starting as a macOS app (make app)
;;;;
;;;; Started from the Finder or the Dock, a program gets launchd's bare
;;;; environment: PATH is /usr/bin:/bin:…, standard output goes nowhere, and
;;;; the current directory is /. So APP-MAIN, the saved image's entry point,
;;;; first sends its output to a log, takes PATH and the rest from a login
;;;; shell (as a terminal would have them: that's where claude, codex, git
;;;; and Homebrew's tools are found), then runs MAIN. As in Cadre.

(in-package #:agent-duet)

(defun state-directory ()
  "$XDG_STATE_HOME/agent-duet/, or ~/.local/state/agent-duet/."
  (let ((base (uiop:getenv-absolute-directory "XDG_STATE_HOME")))
    (merge-pathnames "agent-duet/" (or base (merge-pathnames ".local/state/" (user-homedir-pathname))))))

(defun log-file () (merge-pathnames "agent-duet.log" (state-directory)))

(defun redirect-output-to-log ()
  "Send standard output and error to ~/.local/state/agent-duet/agent-duet.log,
so a crash leaves its message. The previous run's log is kept as agent-duet.log.1."
  (handler-case
      (let* ((log (log-file))
             (previous (make-pathname :name "agent-duet.log" :type "1" :defaults log)))
        (ensure-directories-exist log)
        (when (probe-file log) (uiop:rename-file-overwriting-target log previous))
        (let ((fd (sb-posix:open (uiop:native-namestring log)
                                 (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-append) #o644)))
          (finish-output *standard-output*)
          (finish-output *error-output*)
          (sb-posix:dup2 fd 1)
          (sb-posix:dup2 fd 2)
          (sb-posix:close fd)
          (multiple-value-bind (s m h d mo y) (get-decoded-time)
            (format *error-output* "~&;; Agent Duet started ~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d, pid ~d~%"
                    y mo d h m s (sb-posix:getpid)))
          (finish-output *error-output*)))
    (error () nil)))

;;; The login shell's environment (from Cadre's resources.lisp)

(defparameter *environment-start-marker* "__AGENT_DUET_ENVIRONMENT_START__")

(defparameter *environment-skipped-names*
  '("PWD" "OLDPWD" "SHLVL" "_" "TERM" "TERM_PROGRAM" "TERM_PROGRAM_VERSION" "TERM_SESSION_ID"
    "COLUMNS" "LINES" "PS1" "PS2")
  "Variables a shell sets for itself, not to be copied into the app's environment.")

(defun parse-environment-block (output)
  "The variables in OUTPUT, the text a shell printed: anything (from its
startup files), then the start marker and a newline, then NAME=value
entries each ended by a NUL (env -0). An alist of (name . value)."
  (let ((start (search *environment-start-marker* output)))
    (when start
      (let ((position (1+ (or (position #\Newline output :start start) (1- (length output))))))
        (loop for end = (position (code-char 0) output :start position)
              while end
              for entry = (subseq output position end)
              for equals = (position #\= entry)
              when (and equals (plusp equals))
              collect (cons (subseq entry 0 equals) (subseq entry (1+ equals)))
              do (setf position (1+ end)))))))

(defun login-shell-environment (&key (shell (or (uiop:getenv "SHELL") "/bin/zsh")) (timeout 10))
  "The environment SHELL sets up as an interactive login shell, as an
alist; nil if it fails or takes longer than TIMEOUT seconds."
  (ignore-errors
   (let* ((process (uiop:launch-program
                    (list shell "-l" "-i" "-c"
                          (format nil "printf '%s\\n' ~a; /usr/bin/env -0" *environment-start-marker*))
                    :output :stream :error-output nil :input nil))
          (output (make-string-output-stream))
          (reader (sb-thread:make-thread
                   (lambda ()
                     (let ((stream (uiop:process-info-output process)))
                       (loop for c = (read-char stream nil) while c do (write-char c output))))
                   :name "login environment")))
     (loop with deadline = (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
           while (and (uiop:process-alive-p process) (< (get-internal-real-time) deadline))
           do (sleep 0.02))
     (when (uiop:process-alive-p process)
       (uiop:terminate-process process :urgent t))
     (sb-thread:join-thread reader :default nil :timeout 2)
     (parse-environment-block (get-output-stream-string output)))))

(defun adopt-login-shell-environment (&rest arguments)
  "Copy the login shell's variables (PATH above all) into this process's
environment. Returns the number of variables set."
  (let ((count 0))
    (loop for (name . value) in (apply #'login-shell-environment arguments)
          unless (member name *environment-skipped-names* :test #'string=)
          do (sb-posix:setenv name value 1)
             (incf count))
    count))

;;; The entry point

(defun app-folder-argument (arguments)
  "The folder named among ARGUMENTS (ignoring macOS's -psn_… and other options), or nil."
  (loop for argument in arguments
        unless (and (plusp (length argument)) (char= (char argument 0) #\-))
        do (let ((path (uiop:directory-exists-p argument)))
             (when path (return path)))))

(defun app-main ()
  "Run as an application: from the Finder, with the log and the login
shell's environment; with a folder from the command line, if given."
  (when (member "--review-hook" (rest sb-ext:*posix-argv*) :test #'string=)
    (run-review-hook))
  (setf *random-state* (make-random-state t))
  (unless (uiop:getenv "TERM")
    (redirect-output-to-log)
    (adopt-login-shell-environment)
    (format *error-output* "~&;; PATH: ~a~%" (uiop:getenv "PATH"))
    (finish-output *error-output*))
  (main (app-folder-argument (rest sb-ext:*posix-argv*)))
  (format *error-output* "~&;; Agent Duet exited normally~%")
  (finish-output *error-output*)
  0)
