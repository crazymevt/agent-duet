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

(defun remove-worktree (repo-directory worktree-directory &key delete-branch recovery-task recovery-part)
  "Remove a clean worktree and optionally its branch; return T. With RECOVERY-TASK,
first keep its committed state with RETAIN-WORKTREE (RECOVERY-PART defaulting as there),
so nothing is removed if that fails."
  (let ((branch (worktree-branch worktree-directory)))
    (when recovery-task
      (retain-worktree repo-directory worktree-directory recovery-task recovery-part))
    (worktree-git repo-directory "worktree" "remove" (namestring (uiop:ensure-directory-pathname worktree-directory)))
    (when delete-branch (worktree-git repo-directory "branch" "-D" branch)))
  t)

;;; Recovery refs: a part's committed state, kept under
;;; refs/agent-duet/tasks/TASK/PART/{head,base} after its worktree and branch
;;; are gone, until the task's result is approved.

(defun worktree-recovery-name (name what)
  (when (or (not (stringp name)) (zerop (length name)) (find #\/ name))
    (error "A recovery ~a must be a nonempty string without /: ~s" what name))
  name)

(defun worktree-recovery-prefix (task &optional part)
  (format nil "refs/agent-duet/tasks/~a/~@[~a/~]" (worktree-recovery-name task "task")
          (and part (worktree-recovery-name part "part"))))

(defun worktree-refs (repo-directory prefix)
  "(refname . object)s of the refs under PREFIX."
  (mapcar (lambda (line)
            (let ((space (position #\Space line)))
              (cons (subseq line 0 space) (subseq line (1+ space)))))
          (remove "" (uiop:split-string
                      (worktree-git repo-directory "for-each-ref" "--format=%(refname) %(objectname)" prefix)
                      :separator '(#\Newline))
                  :test #'string=)))

(defun worktree-part-name (worktree-directory)
  "The part a worktree is for: its branch name without agent-duet/."
  (let ((branch (worktree-branch worktree-directory)))
    (if (uiop:string-prefix-p "agent-duet/" branch) (subseq branch (length "agent-duet/")) branch)))

(defun retain-worktree (repo-directory worktree-directory task &optional part)
  "Point TASK's recovery refs for PART (default: the worktree's branch name without
agent-duet/) at the worktree's HEAD and its saved base, replacing earlier ones.
Only committed work is kept: commit first. Return the head commit, and whether it
differs from the base (NIL for an unchanged part)."
  (let* ((prefix (worktree-recovery-prefix task (or part (worktree-part-name worktree-directory))))
         (head (worktree-git-line worktree-directory "rev-parse" "--verify" "HEAD^{commit}"))
         (base (worktree-git-line worktree-directory "config"
                                  (worktree-base-key (worktree-branch worktree-directory)))))
    (worktree-git repo-directory "check-ref-format" (concatenate 'string prefix "head"))
    (worktree-git repo-directory "update-ref" "-m" "agent-duet: retain part" (concatenate 'string prefix "base") base)
    (worktree-git repo-directory "update-ref" "-m" "agent-duet: retain part" (concatenate 'string prefix "head") head)
    (values head (not (string= head base)))))

(defun worktree-recovery-refs (repo-directory task)
  "TASK's retained parts, sorted by part: plists (:part :head :base :changed),
:changed NIL where the part made no commits. Parts missing either ref are left out."
  (let ((prefix (worktree-recovery-prefix task))
        (parts (make-hash-table :test #'equal)))
    (loop for (ref . object) in (worktree-refs repo-directory prefix)
          for name = (subseq ref (length prefix))
          for slash = (position #\/ name)
          for which = (and slash (subseq name (1+ slash)))
          when (member which '("head" "base") :test #'equal)
            do (setf (getf (gethash (subseq name 0 slash) parts) (if (string= which "head") :head :base))
                     object))
    (sort (loop for part being the hash-keys of parts using (hash-value refs)
                for head = (getf refs :head)
                for base = (getf refs :base)
                when (and head base)
                  collect (list :part part :head head :base base :changed (not (string= head base))))
          #'string< :key (lambda (entry) (getf entry :part)))))

(defun delete-worktree-recovery-refs (repo-directory task &optional part)
  "Delete TASK's recovery refs (only PART's, if given), once its result is approved.
Each ref is deleted only if unchanged since read. Return the number deleted."
  (let ((refs (worktree-refs repo-directory (worktree-recovery-prefix task part))))
    (loop for (ref . object) in refs
          do (worktree-git repo-directory "update-ref" "-d" ref object))
    (length refs)))

;;; Read-only state used to bind a user-triggered commit to completed work.
(defun repository-file-state (root path)
  (let* ((file (merge-pathnames (uiop:parse-native-namestring path) root))
         (mode (sb-posix:stat-mode (sb-posix:lstat file)))
         (type (logand mode sb-posix:s-ifmt)))
    (list path mode
          (cond ((= type sb-posix:s-iflnk) (sb-posix:readlink file))
                ((= type sb-posix:s-ifreg) nil) ; filled by the batch hasher
                (t (error "Cannot safely capture untracked file ~a." path))))))

(defun git-input-path (path)
  "C-quote PATH for Git's --stdin-paths, including embedded newlines."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for char across path
          do (cond ((find char '(#\\ #\")) (write-char #\\ out) (write-char char out))
                   ((< (char-code char) 32) (format out "\\~3,'0o" (char-code char)))
                   (t (write-char char out))))
    (write-char #\" out)))

(defun repository-untracked-state (root)
  "One hash-object process for all regular untracked files; lstat keeps modes and links."
  (let* ((files (loop for path in (uiop:split-string
                                  (worktree-git root "ls-files" "--others" "--exclude-standard" "-z")
                                  :separator '(#\Null))
                      unless (string= path "") collect (repository-file-state root path)))
         (regular (remove-if #'third files)))
    (when regular
      (multiple-value-bind (output errors status)
          (uiop:run-program
           (list "git" "-C" (namestring root) "hash-object" "--stdin-paths")
           :input (make-string-input-stream
                   (format nil "~{~a~%~}" (mapcar (lambda (file) (git-input-path (first file))) regular)))
           :output :string :error-output :string :ignore-error-status t)
        (unless (zerop status) (error "Cannot hash untracked files: ~a" errors))
        (let ((hashes (remove "" (uiop:split-string output :separator '(#\Newline)) :test #'equal)))
          (unless (= (length hashes) (length regular)) (error "Incomplete untracked file hashes."))
          (loop for file in regular for hash in hashes do (setf (third file) hash)))))
    files))

(defun repository-state (directory)
  "Capture HEAD, branch, index, tracked edits and untracked contents without staging."
  (let ((root (uiop:ensure-directory-pathname
               (uiop:parse-native-namestring (worktree-git-line directory "rev-parse" "--show-toplevel")))))
    (list :root root
          :head (worktree-git-line root "rev-parse" "HEAD")
          :branch (worktree-git-line root "rev-parse" "--symbolic-full-name" "HEAD")
          :status (worktree-git root "status" "--porcelain=v1" "-z" "--untracked-files=all")
          :index (worktree-git root "diff" "--cached" "--binary" "HEAD" "--")
          :working (worktree-git root "diff" "--binary" "--")
          :untracked (repository-untracked-state root))))

(defun repository-operation-p (directory)
  (or (plusp (length (worktree-git directory "ls-files" "--unmerged")))
      (some (lambda (name)
              (probe-file (merge-pathnames
                           (worktree-git-line directory "rev-parse" "--git-path" name)
                           (uiop:ensure-directory-pathname directory))))
            '("MERGE_HEAD" "CHERRY_PICK_HEAD" "REVERT_HEAD" "rebase-merge" "rebase-apply" "BISECT_LOG"))))
