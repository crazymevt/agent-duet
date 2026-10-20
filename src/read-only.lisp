;;;; read-only.lisp — review tool policy, shared with the CLI hook

(in-package #:agent-duet)

(eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-posix))

(defparameter *claude-edit-tools* '("Edit" "MultiEdit" "Write" "NotebookEdit")
  "The tools Claude changes files with, taken away when read-only.")

(defparameter *claude-read-only-tools* '("Read" "Glob" "Grep" "LS")
  "Claude's tools that only read, allowed anywhere when read-only.")

(defparameter *read-only-commands* '("ls" "pwd" "cat" "head" "tail" "wc" "grep")
  "Commands that only read, given safe arguments (see READ-ONLY-COMMAND-P).")

(defparameter *read-only-git-commands*
  '("diff" "status" "log" "show" "merge-base" "rev-parse" "rev-list" "blame" "ls-files"
    "cat-file" "shortlog" "describe" "name-rev")
  "Git subcommands that only read (but see READ-ONLY-COMMAND-P for --output).")

(defparameter *read-only-blocked-options*
  '("--output" "--ext-diff" "--textconv" "--filters" "--show-signature" "--submodule"))

(defun read-only-command-words (command)
  "A small, deliberately restricted shell grammar. Quotes group literal text;
expansion, escaping, globbing, redirection and command composition are refused."
  (when (and (stringp command)
             (not (find-if (lambda (c)
                             (or (and (< (char-code c) 32) (not (char= c #\Tab)))
                                 (find c ";&|<>`$(){}\\*?[]!"))) command)))
    (let ((words nil) (word nil) (started nil) (quote nil))
      (labels ((flush-word ()
                 (when started
                   (push (coerce (nreverse word) 'string) words)
                   (setf word nil started nil))))
        (loop for c across command do
          (cond (quote (if (char= c quote) (setf quote nil) (push c word)))
                ((find c "'\"") (setf quote c started t))
                ((find c '(#\Space #\Tab)) (flush-word))
                (t (setf started t) (push c word))))
        (unless quote
          (flush-word)
          (nreverse words))))))

(defun read-only-option-p (word)
  ;; Git accepts abbreviated long options. Check normalized arguments, not
  ;; raw shell text, including prefixes such as --out and --textc.
  (let ((option (subseq word 0 (position #\= word))))
    (not (and (> (length option) 2) (uiop:string-prefix-p "--" option)
              (some (lambda (blocked) (uiop:string-prefix-p option blocked))
                    *read-only-blocked-options*)))))

(defun read-only-command-p (command)
  "Whether COMMAND is admissible to the read-only hook. Git still requires
its helper-disabling rewrite; never use this predicate to approve raw Bash."
  (let ((words (read-only-command-words command)))
    (and words
         (or (member (first words) *read-only-commands* :test #'string=)
             (and (equal (first words) "git")
                  (member (second words) *read-only-git-commands* :test #'string=)
                  (every #'read-only-option-p (cddr words))))
         t)))

(defvar *review-hook-saved-image* nil)
(defun mark-review-hook-saved-image () (setf *review-hook-saved-image* t))
(pushnew 'mark-review-hook-saved-image sb-ext:*save-hooks*)

(defun review-hook-quote (string)
  (format nil "'~{~a~^'\\''~}'" (uiop:split-string string :separator "'")))

(defun review-hook-command ()
  (let ((arguments
          (if *review-hook-saved-image*
              (append (list (namestring sb-ext:*runtime-pathname*))
                      ;; Executable images have the core appended to the runtime.
                      (unless (equal sb-ext:*runtime-pathname* sb-ext:*core-pathname*)
                        (list "--core" (namestring sb-ext:*core-pathname*)))
                      (list "--noinform" "--no-sysinit" "--no-userinit" "--disable-debugger" "--" "--review-hook"))
              (append (list (namestring sb-ext:*runtime-pathname*)
                            "--noinform" "--no-sysinit" "--no-userinit" "--disable-debugger"
                            "--eval" "(require :asdf)")
                      (loop for file in '("src/package.lisp" "src/json.lisp" "src/read-only.lisp")
                            append (list "--load" (namestring (asdf:system-relative-pathname "agent-duet/core" file))))
                      (list "--eval" "(agent-duet::run-review-hook)")))))
    (format nil "~{~a~^ ~} || exit 2" (mapcar #'review-hook-quote arguments))))

(defun read-only-program (name)
  ;; Resolve an absolute executable, then quote every argument. No shell
  ;; aliases, expansions, startup files, or functions enter the invocation.
  (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "") :separator ":")
        when (and (plusp (length directory)) (uiop:absolute-pathname-p directory))
          do (let ((path (merge-pathnames name (uiop:ensure-directory-pathname directory))))
               (when (and (probe-file path) (not (uiop:directory-pathname-p path))
                          (zerop (sb-posix:access (namestring path) sb-posix:x-ok)))
                 (return (namestring path))))))

(defun read-only-hook-result (event)
  (let* ((name (jget event "tool_name"))
         (input (jget event "tool_input"))
         (result (jobj "hookEventName" "PreToolUse" "permissionDecision" "deny"
                       "permissionDecisionReason" "Review only: this tool or command may write files.")))
    (cond
      ((equal name "AskUserQuestion") (return-from read-only-hook-result (jobj)))
      ((member name *claude-read-only-tools* :test #'equal)
       (setf (cdr (assoc "permissionDecision" (cdr result) :test #'equal)) "allow"))
      ((and (equal name "Bash") (read-only-command-p (jget input "command")))
       (let* ((words (read-only-command-words (jget input "command")))
              (git (equal (first words) "git"))
              (subcommand (second words))
              (program (read-only-program (first words))))
         (when program
           (setf (first words) program)
           (when git
             (setf words (append (list program "--no-pager" "-c" "core.fsmonitor=false"
                                       "-c" "core.hooksPath=/dev/null" "-c" "log.showSignature=false"
                                       "-c" "diff.submodule=short") (rest words)))
             (when (member subcommand '("diff" "log" "show") :test #'equal)
               (let ((end (or (position-if (lambda (w) (member w '("--" "--end-of-options") :test #'equal)) words)
                              (length words))))
                 (setf words (append (subseq words 0 end) '("--no-ext-diff" "--no-textconv") (subseq words end))))))
           (setf (cdr (assoc "permissionDecision" (cdr result) :test #'equal)) "allow")
           (setf result
                 (append result
                         (list (cons "updatedInput"
                                     (cons :object
                                           (append (remove "command" (cdr input) :key #'car :test #'equal)
                                                   (list (cons "command"
                                                               (format nil "~a~{~a~^ ~}"
                                                                       (if git "GIT_OPTIONAL_LOCKS=0 GIT_NO_LAZY_FETCH=1 " "")
                                                                       (mapcar #'review-hook-quote words))))))))))))))
    (jobj "hookSpecificOutput" result)))

(defun run-review-hook ()
  (handler-case
      (let ((event (parse-json (uiop:slurp-stream-string *standard-input*))))
        (write-line (json-string (read-only-hook-result event)))
        (finish-output)
        (uiop:quit 0))
    (error ()
      (write-line "Review-only policy could not validate this tool call." *error-output*)
      (uiop:quit 2))))
