;;;; history.lisp — conversations kept as JSON in the user's state directory

(in-package #:agent-duet)

(defvar *history-directory* nil)

(defstruct (history (:constructor %make-history
                                (&key id directory title created updated handles entries
                                 &aux (folder directory))))
  id folder title created updated handles entries)

(defun history-directory (&optional history)
  "The storage directory, or HISTORY's working directory when supplied."
  (if history
      (history-folder history)
      (uiop:ensure-directory-pathname
       (or *history-directory*
           (merge-pathnames "agent-duet/history/"
                            (let ((state (uiop:getenv "XDG_STATE_HOME")))
                              (if (and state (plusp (length state)))
                                  (uiop:ensure-directory-pathname state)
                                  (merge-pathnames ".local/state/" (user-homedir-pathname)))))))))

(defun make-history (directory)
  "A new, unsaved conversation in DIRECTORY."
  (let ((now (get-universal-time)))
    (%make-history :id (make-uuid) :directory (namestring (uiop:ensure-directory-pathname directory))
                   :created now :updated now :handles (list :claude nil :codex nil))))

(defun history-add-entry (history who kind text)
  "Append an entry and update the conversation's title and timestamp."
  (unless (and (member who '(:claude :codex :middle :user))
               (member kind '(:prompt :text :tool :note :error)) (stringp text))
    (error "Invalid history entry."))
  (let ((now (get-universal-time)))
    (setf (history-entries history)
          (nconc (history-entries history) (list (list :who who :kind kind :text text :time now)))
          (history-updated history) now)
    (when (and (eq kind :prompt) (null (history-title history)))
      (let ((line (string-trim '(#\Space #\Tab)
                               (map 'string (lambda (c) (if (find c '(#\Newline #\Return #\Tab)) #\Space c)) text))))
        (setf (history-title history) (subseq line 0 (min 80 (length line)))))))
  history)

(defun history-handle (history agent-id)
  (getf (history-handles history) agent-id))

(defun (setf history-handle) (value history agent-id)
  (unless (and (member agent-id '(:claude :codex)) (or (null value) (stringp value)))
    (error "Invalid conversation handle."))
  (setf (getf (history-handles history) agent-id) value))

(defun history-file (id)
  ;; IDs are filenames, never paths supplied by a saved document.
  (unless (and (stringp id) (plusp (length id))
               (every (lambda (c) (or (find c "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
                                     (char= c #\-))) id))
    (error "Invalid history ID."))
  (merge-pathnames (make-pathname :name id :type "json") (history-directory)))

(defun history-json (history)
  (jobj "id" (history-id history) "directory" (history-directory history)
        "title" (history-title history) "created" (history-created history)
        "updated" (history-updated history)
        "handles" (jobj "claude" (history-handle history :claude) "codex" (history-handle history :codex))
        "entries" (coerce (loop for entry in (history-entries history)
                               collect (jobj "who" (getf entry :who) "kind" (getf entry :kind)
                                             "text" (getf entry :text) "time" (getf entry :time))) 'vector)))

(defun save-history (history)
  "Atomically save HISTORY; ignore disk errors and empty conversations."
  (when (history-entries history)
    (ignore-errors
      (let* ((file (history-file (history-id history)))
             (temp (merge-pathnames (format nil ".~a-~a.tmp" (history-id history) (make-uuid)) file)))
        (ensure-directories-exist file)
        (unwind-protect
             (progn
               (with-open-file (out temp :direction :output :if-exists :error :external-format :utf-8)
                 (write-string (json-string (history-json history)) out)
                 (terpri out))
               (uiop:rename-file-overwriting-target temp file))
          (when (probe-file temp) (delete-file temp)))))))

(defun history-keyword (value choices)
  (or (and (stringp value)
           (find value choices :key (lambda (key) (string-downcase (symbol-name key))) :test #'string=))
      (error "Unknown history keyword.")))

(defun load-history (id)
  "Read a conversation, or nil if missing or corrupt."
  (ignore-errors
    (let* ((json (parse-json (uiop:read-file-string (history-file id) :external-format :utf-8)))
           (entries (jget json "entries")))
      (unless (and (equal id (jget json "id")) (stringp (jget json "directory"))
                   (or (stringp (jget json "title")) (eq :null (jget json "title"))) (integerp (jget json "created"))
                   (integerp (jget json "updated")) (vectorp entries) (not (stringp entries))
                   (jobject-p (jget json "handles")))
        (error "Invalid history."))
      (%make-history
       :id id :directory (jget json "directory") :title (let ((title (jget json "title"))) (and (stringp title) title))
       :created (jget json "created") :updated (jget json "updated")
       :handles (loop for agent in '(:claude :codex)
                      for value = (jget json "handles" (string-downcase (symbol-name agent)))
                      append (list agent (cond ((stringp value) value)
                                               ((member value '(nil :null)) nil)
                                               (t (error "Invalid history handle.")))))
       :entries (loop for entry in (jlist entries)
                      collect (progn
                                (unless (and (stringp (jget entry "text")) (integerp (jget entry "time")))
                                  (error "Invalid history entry."))
                                (list :who (history-keyword (jget entry "who") '(:claude :codex :middle :user))
                                      :kind (history-keyword (jget entry "kind") '(:prompt :text :tool :note :error))
                                      :text (jget entry "text") :time (jget entry "time"))))))))

(defun list-histories (&key directory (limit 50))
  "Newest summaries: plists with :id, :title, :directory, :updated, :entry-count."
  (let* ((folder (and directory (namestring (uiop:ensure-directory-pathname directory))))
         (summaries
           (loop for file in (ignore-errors (uiop:directory-files (history-directory) "*.json"))
                 for history = (load-history (pathname-name file))
                 when (and history (or (null folder)
                                       (equal folder (namestring (uiop:ensure-directory-pathname
                                                                 (history-directory history))))))
                   collect (list :id (history-id history) :title (history-title history)
                                 :directory (history-directory history) :updated (history-updated history)
                                 :entry-count (length (history-entries history))))))
    (setf summaries (stable-sort summaries #'> :key (lambda (summary) (getf summary :updated))))
    (if limit (subseq summaries 0 (min (max 0 limit) (length summaries))) summaries)))

(defun delete-history (id)
  "Delete a saved conversation, ignoring missing files and disk errors."
  (ignore-errors (delete-file (history-file id))))

(defun prune-histories (&key (keep 200))
  "Delete the oldest files beyond KEEP; use file dates for corrupt histories."
  (let ((files (loop for file in (ignore-errors (uiop:directory-files (history-directory) "*.json"))
                     for history = (load-history (pathname-name file))
                     collect (cons file (or (and history (history-updated history))
                                            (ignore-errors (file-write-date file)) 0)))))
    (setf files (stable-sort files #'> :key #'cdr))
    (dolist (entry (nthcdr (max 0 keep) files))
      (ignore-errors (delete-file (car entry))))))
