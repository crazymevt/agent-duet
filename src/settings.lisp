;;;; settings.lisp — what the app remembers between runs
;;;;
;;;; A plist in ~/.config/agent-duet/settings.lisp (or under
;;;; $XDG_CONFIG_HOME), read without evaluation.

(in-package #:agent-duet)

(defvar *settings* '() "The settings plist.")

(defparameter *recent-folders-kept* 10)

(defun settings-file ()
  (uiop:xdg-config-home "agent-duet/settings.lisp"))

(defun tree-without-cycles-p (object)
  "True if no cons or array is reachable twice from OBJECT, so printing,
walking and copying it all finish. Shared structure counts too: the
settings are saved without it."
  (let ((seen (make-hash-table :test #'eq))
        (pending (list object)))
    (loop while pending
          do (let ((x (pop pending)))
               (loop while (or (consp x) (and (arrayp x) (not (stringp x))))
                     do (when (gethash x seen) (return-from tree-without-cycles-p nil))
                        (setf (gethash x seen) t)
                        (cond ((consp x) (push (car x) pending) (setf x (cdr x)))
                              (t (dotimes (i (array-total-size x)) (push (row-major-aref x i) pending))
                                 (setf x nil))))))
    t))

(defun proper-list-p (object)
  (and (listp object) (handler-case (list-length object) (error () nil)) t))

(defun sanitized-settings (plist)
  "PLIST's entries whose keys are symbols and whose values are free of
cycles and, if lists, proper; nil if PLIST is not a proper, even-length list."
  (let ((length (and (listp plist) (ignore-errors (list-length plist)))))
    (when (and length (evenp length))
      (loop for (key value) on plist by #'cddr
            when (and (symbolp key) (tree-without-cycles-p value)
                      (or (atom value) (proper-list-p value)))
              append (list key value)))))

(defun load-settings (&optional (file (settings-file)))
  "Read the settings from FILE; nil settings if it is missing or unreadable.
Entries with circular values are dropped, keeping the rest."
  (setf *settings*
        (or (ignore-errors
             (with-open-file (in file :if-does-not-exist nil)
               (when in
                 (with-standard-io-syntax
                   (let ((*read-eval* nil)
                         (*package* (find-package '#:agent-duet)))
                     (sanitized-settings (read in nil nil)))))))
            '())))

(defun save-settings (&optional (file (settings-file)))
  (ignore-errors
   (ensure-directories-exist file)
   (with-open-file (out file :direction :output :if-exists :supersede)
     (with-standard-io-syntax
       ;; Not readably: that would write base strings as #A(…) arrays.
       (let ((*package* (find-package '#:agent-duet))
             (*print-readably* nil))
         (prin1 *settings* out)
         (terpri out))))))

(defun setting (key &optional default)
  (getf *settings* key default))

(defun (setf setting) (value key &optional default)
  (declare (ignore default))
  (setf (getf *settings* key) value)
  (save-settings)
  value)

(defun note-recent-folder (directory)
  "Put DIRECTORY first among the recent folders."
  (let ((path (namestring directory))
        (saved (setting :recent-folders)))
    (setf (setting :recent-folders)
          (let ((list (cons path (remove path (if (proper-list-p saved) (remove-if-not #'stringp saved) '())
                                         :test #'equal))))
            (subseq list 0 (min *recent-folders-kept* (length list)))))))

(defun check-commands ()
  "The saved (directory . command) pairs, without malformed entries."
  (let ((saved (setting :check-commands)))
    (when (proper-list-p saved)
      (remove-if-not (lambda (entry) (and (consp entry) (stringp (car entry)) (stringp (cdr entry))))
                     saved))))

(defun folder-check-command (directory)
  "The check command (see the workflow's :check-command) set for DIRECTORY, or nil."
  (cdr (assoc (namestring directory) (check-commands) :test #'equal)))

(defun (setf folder-check-command) (command directory)
  "Remember COMMAND as DIRECTORY's check command; a blank one forgets it."
  (let* ((path (namestring directory))
         (command (and command (string-trim '(#\Space #\Tab #\Newline) command)))
         (others (remove path (check-commands) :key #'car :test #'equal)))
    (unless (equal command (folder-check-command directory))
      (setf (setting :check-commands)
            (if (plusp (length command)) (acons path command others) others)))
    command))

(defun layout-position-key (view)
  (case view
    (:sidebar :sidebar-positions)
    (:agents-left :agents-left-positions)
    (:panes :pane-positions)))

(defun sidebar-view-p (view)
  (not (null (member view '(:sidebar :agents-left)))))

(defun layout-positions (view)
  "Validated divider positions for VIEW; ignore malformed or old preferences."
  (let ((positions (setting (layout-position-key view))))
    (when (and (proper-list-p positions) (eql (list-length positions) 2)
               (every (lambda (n) (and (integerp n) (<= 0 n 100000))) positions))
      positions)))

(defun saved-choice-index (key count)
  "Return a valid dropdown index, tolerating obsolete or malformed settings."
  (let ((index (setting key 0)))
    (if (and (integerp index) (<= 0 index) (< index count)) index 0)))
