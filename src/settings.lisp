;;;; settings.lisp — what the app remembers between runs
;;;;
;;;; A plist in ~/.config/agent-duet/settings.lisp (or under
;;;; $XDG_CONFIG_HOME), read without evaluation.

(in-package #:agent-integration)

(defvar *settings* '() "The settings plist.")

(defparameter *recent-folders-kept* 10)

(defun settings-file ()
  (uiop:xdg-config-home "agent-duet/settings.lisp"))

(defun load-settings (&optional (file (settings-file)))
  "Read the settings from FILE; nil settings if it is missing or unreadable."
  (setf *settings*
        (or (ignore-errors
             (with-open-file (in file :if-does-not-exist nil)
               (when in
                 (with-standard-io-syntax
                   (let ((*read-eval* nil)
                         (*package* (find-package '#:agent-integration)))
                     (let ((plist (read in nil nil)))
                       (and (listp plist) (evenp (length plist)) plist)))))))
            '())))

(defun save-settings (&optional (file (settings-file)))
  (ignore-errors
   (ensure-directories-exist file)
   (with-open-file (out file :direction :output :if-exists :supersede)
     (with-standard-io-syntax
       ;; Not readably: that would write base strings as #A(…) arrays.
       (let ((*package* (find-package '#:agent-integration))
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
  (let ((path (namestring directory)))
    (setf (setting :recent-folders)
          (let ((list (cons path (remove path (setting :recent-folders) :test #'equal))))
            (subseq list 0 (min *recent-folders-kept* (length list)))))))
