;;;; Git snapshots, isolated from the user's index and working files.
(in-package #:agent-duet)

(defstruct snapshot label head index working-tree time)
(defstruct snapshot-change path status old-path diff)

(defun snapshot-ref (label)
  (concatenate 'string "refs/agent-duet/snapshots/" label))

(defun snapshot-output (directory arguments &optional index)
  "Run Git, replacing undecodable bytes for display and preserving stderr on errors."
  (multiple-value-bind (output errors status)
      (uiop:run-program
       (append (when index
                 (list "env" (format nil "GIT_INDEX_FILE=~a" index)
                       "GIT_AUTHOR_NAME=Agent Duet" "GIT_AUTHOR_EMAIL=snapshot@agent-duet.invalid"
                       "GIT_COMMITTER_NAME=Agent Duet" "GIT_COMMITTER_EMAIL=snapshot@agent-duet.invalid"))
               (list "git" "-C" (namestring directory)) arguments)
       :output :string :error-output :string :ignore-error-status t
       :external-format '(:utf-8 :replacement #\?))
    (unless (zerop status)
      (error "Git ~{~a~^ ~} failed in ~a (status ~d): ~a" arguments directory status errors))
    output))

(defun snapshot-git (directory index &rest args)
  (string-right-trim '(#\Newline #\Return) (snapshot-output directory args index)))

(defun snapshot-folder (directory label)
  "Capture HEAD, index and working tree under LABEL. NIL without a committed Git repository.
The working-tree commit has the index commit and HEAD as parents. Errors (including
an unresolved index) are reported; the user's index and files are never written."
  (unless (git-repository-p directory) (return-from snapshot-folder nil))
  (let ((head (ignore-errors (worktree-git-line directory "rev-parse" "--verify" "HEAD^{commit}"))))
    (unless head (return-from snapshot-folder nil))
    (let* ((directory (uiop:ensure-directory-pathname
                       (worktree-git-line directory "rev-parse" "--show-toplevel")))
           (index-path (pathname (worktree-git-line directory "rev-parse" "--path-format=absolute" "--git-path" "index")))
           ;; Beside the real index so split-index shared files resolve.
           (temporary (merge-pathnames (format nil "duet-index-~a" (make-uuid))
                                       (uiop:pathname-directory-pathname index-path)))
           (time (get-universal-time)))
      (worktree-git directory "check-ref-format" (snapshot-ref label))
      (unwind-protect
           (progn
             (if (probe-file index-path)
                 (uiop:copy-file index-path temporary)
                 (snapshot-git directory temporary "read-tree" "--empty"))
             (let* ((index-tree (snapshot-git directory temporary "write-tree"))
                    (index (snapshot-git directory temporary "commit-tree" index-tree "-p" head
                                         "-m" (format nil "Agent Duet index ~a" label))))
               ;; Reload the tree to discard index-only flags while preserving
               ;; staged additions, including files now matched by .gitignore.
               (delete-file temporary)
               (snapshot-git directory temporary "read-tree" index-tree)
               (snapshot-git directory temporary "-c" "core.sparseCheckout=false" "add" "-A" "--" ".")
               ;; A HEAD-tracked file can have been removed from the index and
               ;; subsequently ignored. It still belongs to the working state.
               (let ((paths (remove-if-not
                             (lambda (path)
                               (let ((file (merge-pathnames (uiop:parse-native-namestring path) directory)))
                                 (and (probe-file file) (not (uiop:directory-exists-p file)))))
                             (remove "" (uiop:split-string
                                         (snapshot-git directory temporary "diff-index" "--cached" "--name-only"
                                                       "--diff-filter=D" "-z" head "--")
                                         :separator '(#\Null)) :test #'string=))))
                 (loop while paths
                       for batch = (subseq paths 0 (min 100 (length paths)))
                       do (apply #'snapshot-git directory temporary
                                 (append '("--literal-pathspecs" "add" "-f" "--") batch))
                          (setf paths (nthcdr (length batch) paths))))
               (let* ((tree (snapshot-git directory temporary "write-tree"))
                      (working (snapshot-git directory temporary "commit-tree" tree "-p" head "-p" index
                                             "-m" (format nil "Agent Duet snapshot ~a (~d)" label time))))
                 (worktree-git directory "update-ref" (snapshot-ref label) working)
                 (make-snapshot :label label :head head :index index :working-tree working :time time))))
        (when (probe-file temporary) (delete-file temporary))
        (let ((lock (pathname (concatenate 'string (namestring temporary) ".lock"))))
          (when (probe-file lock) (delete-file lock)))))))

(defun snapshot-changes (directory start end)
  "Return SNAPSHOT-CHANGE records comparing the snapshots' working trees, including renames."
  (when (and start end (git-repository-p directory))
    (let* ((a (snapshot-working-tree start)) (b (snapshot-working-tree end))
           (fields (remove "" (uiop:split-string
                                (snapshot-output directory (list "diff" "--no-ext-diff" "--name-status" "-z" "-M" a b "--"))
                                :separator '(#\Null)) :test #'string=)))
      (loop while fields
            for code = (pop fields)
            for first = (pop fields)
            for renamed = (char= (char code 0) #\R)
            for path = (if renamed (pop fields) first)
            collect
            (make-snapshot-change
             :path path :old-path (and renamed first)
             :status (case (char code 0) (#\A :added) (#\D :deleted) (#\R :renamed) (t :modified))
             :diff (let ((diff (snapshot-output directory
                          (append (list "--literal-pathspecs" "diff" "--no-ext-diff" "--no-textconv"
                                        "--no-color" "-M" a b "--")
                                  (if renamed (list first path) (list path))))))
                     (if (plusp (length diff)) diff
                         "Diff unavailable: the path may contain non-UTF-8 bytes.")))))))

(defun delete-snapshot (directory snapshot)
  "Delete SNAPSHOT's ref only if it still points to that snapshot. Return T.
Return NIL outside Git; signal a Git error if the ref moved or deletion fails."
  (unless (git-repository-p directory) (return-from delete-snapshot nil))
  (worktree-git directory "update-ref" "-d" (snapshot-ref (snapshot-label snapshot))
                (snapshot-working-tree snapshot))
  t)

(defun prune-snapshots (directory &key (keep 50))
  "Remove the oldest snapshot refs, retaining KEEP. Return the number removed."
  (check-type keep (integer 0 *))
  (unless (git-repository-p directory) (return-from prune-snapshots nil))
  (let* ((lines (remove "" (uiop:split-string
                            (worktree-git directory "for-each-ref" "--sort=refname" "--sort=committerdate"
                                          "--format=%(refname) %(objectname)" "refs/agent-duet/snapshots/")
                            :separator '(#\Newline)) :test #'string=))
         (remove-count (max 0 (- (length lines) keep))))
    (dolist (line (subseq lines 0 remove-count))
      (let ((space (position #\Space line)))
        (worktree-git directory "update-ref" "-d" (subseq line 0 space) (subseq line (1+ space)))))
    remove-count))
