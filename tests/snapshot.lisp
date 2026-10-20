(in-package #:agent-duet/tests)

(define-test snapshot-state :parent agent-duet
  (with-worktree-repository (repo trees)
    (write-worktree-file repo "shared.txt" "staged")
    (ai::worktree-git repo "add" "shared.txt")
    (write-worktree-file repo "shared.txt" "unstaged")
    (write-worktree-file repo "new file.txt" "untracked")
    (write-worktree-file repo ".gitignore" "ignored")
    (write-worktree-file repo "ignored" "not captured")
    (let* ((status (ai::worktree-git repo "status" "--porcelain"))
           (cached (ai::worktree-git repo "diff" "--cached"))
           (index (uiop:read-file-string (merge-pathnames ".git/index" repo) :external-format :latin-1))
           (snapshot (ai:snapshot-folder repo "state")))
      (true snapshot)
      (is string= status (ai::worktree-git repo "status" "--porcelain"))
      (is string= cached (ai::worktree-git repo "diff" "--cached"))
      (is string= index (uiop:read-file-string (merge-pathnames ".git/index" repo) :external-format :latin-1))
      (is string= (format nil "staged~%") (ai::worktree-git repo "show" (format nil "~a:shared.txt" (ai:snapshot-index snapshot))))
      (is string= (format nil "unstaged~%") (ai::worktree-git repo "show" (format nil "~a:shared.txt" (ai:snapshot-working-tree snapshot))))
      (true (search "new file.txt" (ai::worktree-git repo "ls-tree" "--name-only" (ai:snapshot-working-tree snapshot))))
      (false (search "ignored" (ai::worktree-git repo "ls-tree" "--name-only" (ai:snapshot-working-tree snapshot))))
      (is string= (ai::worktree-git-line repo "rev-parse" "HEAD") (ai:snapshot-head snapshot))
      (true (ai:delete-snapshot repo snapshot)))))

(define-test snapshot-diffs :parent agent-duet
  (with-worktree-repository (repo trees)
    (write-worktree-file repo "delete.txt" "delete")
    (write-worktree-file repo "old name.txt" "rename unchanged contents")
    (let ((start (ai:snapshot-folder repo "a")))
      (delete-file (merge-pathnames "delete.txt" repo))
      (rename-file (merge-pathnames "old name.txt" repo) (merge-pathnames "new name.txt" repo))
      (write-worktree-file repo "shared.txt" "modified")
      (write-worktree-file repo "added.txt" "added")
      (let* ((end (ai:snapshot-folder repo "b"))
             (changes (ai:snapshot-changes repo start end)))
        (is = 4 (length changes))
        (dolist (pair '(("delete.txt" . :deleted) ("new name.txt" . :renamed)
                        ("shared.txt" . :modified) ("added.txt" . :added)))
          (let ((change (find (car pair) changes :key #'ai:snapshot-change-path :test #'string=)))
            (true change)
            (is eq (cdr pair) (ai:snapshot-change-status change))
            (true (plusp (length (ai:snapshot-change-diff change))))))
        (is string= "old name.txt" (ai:snapshot-change-old-path
                                    (find :renamed changes :key #'ai:snapshot-change-status)))
        (false (ai:snapshot-changes repo end end))))))

(define-test snapshot-unavailable-and-prune :parent agent-duet
  (let ((directory (merge-pathnames (format nil "duet-no-git-~a/" (ai::make-uuid)) (uiop:temporary-directory))))
    (ensure-directories-exist directory)
    (unwind-protect
         (progn (false (ai:snapshot-folder directory "none"))
                (ai::worktree-git directory "init")
                (false (ai:snapshot-folder directory "unborn")))
      (uiop:delete-directory-tree directory :validate t)))
  (with-worktree-repository (repo trees)
    (ai:snapshot-folder repo "a")
    (ai:snapshot-folder repo "b")
    (ai:snapshot-folder repo "c")
    (is = 1 (ai:prune-snapshots repo :keep 2))
    (false (search "refs/agent-duet/snapshots/a" (ai::worktree-git repo "for-each-ref" "--format=%(refname)" "refs/agent-duet/snapshots/")))
    (is = 2 (ai:prune-snapshots repo :keep 0))
    (is = 0 (ai:prune-snapshots repo))))

(define-test snapshot-binary-and-odd-paths :parent agent-duet
  (with-worktree-repository (repo trees)
    (let ((name (format nil "odd~%[file].txt")))
      (write-worktree-file repo (uiop:parse-native-namestring name) "before")
      (with-open-file (out (merge-pathnames "binary" repo) :direction :output
                          :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence #(0 1 2) out))
      (let ((start (ai:snapshot-folder repo "binary-start")))
        (write-worktree-file repo (uiop:parse-native-namestring name) "after")
        (with-open-file (out (merge-pathnames "binary" repo) :direction :output
                            :element-type '(unsigned-byte 8) :if-exists :supersede)
          (write-sequence #(0 3 4) out))
        (let* ((end (ai:snapshot-folder repo "binary-end"))
               (changes (ai:snapshot-changes repo start end)))
          (is = 2 (length changes))
          (true (search "Binary files" (ai:snapshot-change-diff
                                       (find "binary" changes :key #'ai:snapshot-change-path :test #'string=))))
          (true (search "+after" (ai:snapshot-change-diff
                                  (find name changes :key #'ai:snapshot-change-path :test #'string=)))))))))

(define-test snapshot-index-flags-and-ignored-tracked :parent agent-duet
  (with-worktree-repository (repo trees)
    (ai::worktree-git repo "update-index" "--split-index")
    (ai::worktree-git repo "update-index" "--assume-unchanged" "shared.txt")
    (write-worktree-file repo "shared.txt" "hidden modification")
    (let ((snapshot (ai:snapshot-folder repo "flags")))
      (true (search "hidden modification" (ai::worktree-git repo "show"
                                            (format nil "~a:shared.txt" (ai:snapshot-working-tree snapshot))))))
    (ai::worktree-git repo "update-index" "--no-assume-unchanged" "shared.txt")
    (ai::worktree-git repo "rm" "--cached" "-f" "shared.txt")
    (write-worktree-file repo ".gitignore" "shared.txt")
    (let ((snapshot (ai:snapshot-folder repo "ignored-tracked")))
      (true (search "hidden modification" (ai::worktree-git repo "show"
                                            (format nil "~a:shared.txt" (ai:snapshot-working-tree snapshot))))))))

(define-test snapshot-non-utf8-diff :parent agent-duet
  (with-worktree-repository (repo trees)
    (flet ((latin-file (last-byte)
             (with-open-file (out (merge-pathnames "latin.txt" repo) :direction :output
                                 :if-exists :supersede :if-does-not-exist :create
                                 :element-type '(unsigned-byte 8))
               (write-sequence (vector 99 97 102 233 last-byte 10) out))))
      (latin-file 49)
      (let ((start (ai:snapshot-folder repo "latin-start")))
        (latin-file 50)
        (let* ((end (ai:snapshot-folder repo "latin-end"))
               (changes (ai:snapshot-changes repo start end)))
          (is = 1 (length changes))
          (is eq :modified (ai:snapshot-change-status (first changes)))
          (true (search "-caf?1" (ai:snapshot-change-diff (first changes))))
          (true (search "+caf?2" (ai:snapshot-change-diff (first changes)))))))))

(define-test snapshot-file-becomes-directory :parent agent-duet
  (with-worktree-repository (repo trees)
    (delete-file (merge-pathnames "shared.txt" repo))
    (ensure-directories-exist (merge-pathnames "shared.txt/" repo))
    (write-worktree-file repo "shared.txt/ignored.txt" "private")
    (write-worktree-file repo ".gitignore" "shared.txt/ignored.txt")
    (let* ((before (ai::worktree-git repo "status" "--porcelain"))
           (snapshot (ai:snapshot-folder repo "directory")))
      (false (search "ignored.txt" (ai::worktree-git repo "ls-tree" "-r" "--name-only"
                                                   (ai:snapshot-working-tree snapshot))))
      (is string= before (ai::worktree-git repo "status" "--porcelain")))
    (write-worktree-file repo "shared.txt/keep.txt" "keep")
    (let* ((snapshot (ai:snapshot-folder repo "directory-with-file"))
           (paths (ai::worktree-git repo "ls-tree" "-r" "--name-only" (ai:snapshot-working-tree snapshot))))
      (true (search "shared.txt/keep.txt" paths))
      (false (search "ignored.txt" paths)))))

(define-test snapshot-errors-and-moved-ref :parent agent-duet
  (with-worktree-repository (repo trees)
    (let ((old (ai:snapshot-folder repo "replace")))
      (write-worktree-file repo "shared.txt" "new state")
      (let ((new (ai:snapshot-folder repo "replace")))
        (true (handler-case (progn (ai:delete-snapshot repo old) nil) (error () t)))
        (is string= (ai:snapshot-working-tree new)
            (ai::worktree-git-line repo "rev-parse" "refs/agent-duet/snapshots/replace"))))
    (let ((blob (ai::worktree-git-line repo "rev-parse" "HEAD:shared.txt")))
      ;; An unresolved index cannot be represented by WRITE-TREE. Keep its
      ;; reason in the error rather than just reporting Git's exit status.
      (uiop:run-program (list "git" "-C" (namestring repo) "update-index" "--index-info")
                        :input (make-string-input-stream
                                (format nil "0 ~a~cshared.txt~%100644 ~a 1~cshared.txt~%100644 ~a 2~cshared.txt~%"
                                        (make-string 40 :initial-element #\0) #\Tab blob #\Tab blob #\Tab)))
      (let* ((before (ai::worktree-git repo "ls-files" "--stage"))
             (message (handler-case (progn (ai:snapshot-folder repo "conflict") nil)
                        (error (e) (princ-to-string e)))))
        (true message)
        (true (search "unmerged" message))
        (true (search "shared.txt" message))
        (is string= before (ai::worktree-git repo "ls-files" "--stage"))))))

(define-test snapshot-ignored-tracked-directory-symlink :parent agent-duet
  (with-worktree-repository (repo trees)
    (let ((start (ai:snapshot-folder repo "symlink-start")))
      (ai::worktree-git repo "rm" "--cached" "-f" "shared.txt")
      (delete-file (merge-pathnames "shared.txt" repo))
      (ensure-directories-exist (merge-pathnames "target/" repo))
      (write-worktree-file repo "target/inside.txt" "inside")
      (uiop:run-program (list "ln" "-s" "target" (uiop:native-namestring (merge-pathnames "shared.txt" repo))))
      (write-worktree-file repo ".gitignore" (format nil "shared.txt~%target/"))
      ;; Followed, the symlink is a directory; Git records it as a link.
      (let* ((end (ai:snapshot-folder repo "symlink-end"))
             (entry (ai::worktree-git repo "ls-tree" (ai:snapshot-working-tree end) "shared.txt"))
             (changes (ai:snapshot-changes repo start end))
             (change (find "shared.txt" changes :key #'ai:snapshot-change-path :test #'string=)))
        (true (uiop:string-prefix-p "120000" entry))
        (true change)
        (is eq :modified (ai:snapshot-change-status change))
        ;; Diffs wait until they're asked for.
        (false (ai::snapshot-change-%diff change))
        (true (search "+target" (ai:snapshot-change-diff change)))
        (true (ai::snapshot-change-%diff change))))))
