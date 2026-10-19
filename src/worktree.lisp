;;;; worktree.lisp — isolated git branches for split tasks

(in-package #:agent-duet)

(defun worktree-git (directory &rest arguments)
  (multiple-value-bind (output errors status)
      (uiop:run-program (append (list "git" "-C" (namestring (uiop:ensure-directory-pathname directory)))
                                arguments)
                        :output :string :error-output :string :ignore-error-status t)
    (unless (zerop status)
      (error "Git ~{~a~^ ~} failed in ~a: ~a" arguments directory errors))
    output))

(defun worktree-git-line (directory &rest arguments)
  (string-right-trim '(#\Newline #\Return)
                     (apply #'worktree-git directory arguments)))

(defun git-repository-p (directory)
  "Whether DIRECTORY belongs to a non-bare git working tree."
  (handler-case (string= "true" (worktree-git-line directory "rev-parse" "--is-inside-work-tree"))
    (error () nil)))

(defun worktree-branch (directory)
  (worktree-git-line directory "symbolic-ref" "--short" "HEAD"))

(defun worktree-base-key (branch)
  (format nil "branch.~a.agentDuetBase" branch))

(defun create-worktree (repo-directory name)
  "Create agent-duet/NAME at HEAD and return its directory pathname."
  (let* ((branch (concatenate 'string "agent-duet/" name))
         (base (worktree-git-line repo-directory "rev-parse" "HEAD"))
         (directory (merge-pathnames (format nil "agent-duet-~a/" (make-uuid))
                                    (uiop:temporary-directory))))
    (worktree-git repo-directory "check-ref-format" "--branch" branch)
    (worktree-git repo-directory "worktree" "add" "-b" branch (namestring directory) base)
    (handler-case
        (worktree-git repo-directory "config" (worktree-base-key branch) base)
      (error (condition)
        (ignore-errors (worktree-git repo-directory "worktree" "remove" (namestring directory)))
        (ignore-errors (worktree-git repo-directory "branch" "-D" branch))
        (error condition)))
    directory))

(defun worktree-diff (worktree-directory)
  "Diff all changes, including commits and untracked files, against the saved base. Stages files."
  (let ((base (worktree-git-line worktree-directory "config"
                                 (worktree-base-key (worktree-branch worktree-directory)))))
    (worktree-git worktree-directory "add" "-A")
    (worktree-git worktree-directory "diff" "--cached" base "--")))

(defun commit-worktree (worktree-directory message)
  "Commit all changes and return the commit hash, or NIL if unchanged."
  (worktree-git worktree-directory "add" "-A")
  (unless (zerop (length (worktree-git worktree-directory "diff" "--cached" "--name-only")))
    (worktree-git worktree-directory "commit" "-m" message)
    (worktree-git-line worktree-directory "rev-parse" "HEAD")))

(defun merge-worktree (repo-directory worktree-directory)
  "Return :MERGED, or :CONFLICT and conflicted filenames, leaving the merge open."
  (let ((branch (worktree-branch worktree-directory)))
    (handler-case
        (progn (worktree-git repo-directory "merge" "--no-edit" branch) :merged)
      (error (condition)
        (let* ((output (worktree-git repo-directory "diff" "--name-only" "--diff-filter=U" "-z"))
               (files (remove "" (uiop:split-string output :separator '(#\Null)) :test #'equal)))
          (if files (values :conflict files) (error condition)))))))

(defun remove-worktree (repo-directory worktree-directory &key delete-branch)
  "Remove a clean worktree and optionally its branch; return T."
  (let ((branch (worktree-branch worktree-directory)))
    (worktree-git repo-directory "worktree" "remove" (namestring (uiop:ensure-directory-pathname worktree-directory)))
    (when delete-branch (worktree-git repo-directory "branch" "-D" branch)))
  t)
