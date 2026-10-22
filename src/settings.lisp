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
  "Replace FILE atomically, leaving existing preferences intact on write errors."
  (ignore-errors
    (ensure-directories-exist file)
    (uiop:with-temporary-file (:stream out :pathname temp
                              :directory (uiop:pathname-directory-pathname file)
                              :prefix ".settings-" :type "tmp")
      (with-standard-io-syntax
        ;; Not readably: that would write base strings as #A(…) arrays.
        (let ((*package* (find-package '#:agent-duet))
              (*print-readably* nil))
          (prin1 *settings* out)
          (terpri out)))
      :close-stream
      (uiop:rename-file-overwriting-target temp file))))

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

(defun saved-window-size ()
  "Validated window dimensions, with the original defaults for old settings."
  (let ((size (setting :window-size)))
    (if (and (proper-list-p size) (eql (list-length size) 2)
             (every (lambda (extent) (and (integerp extent) (<= 1 extent 100000))) size))
        size
        '(1440 900))))

(defun sidebar-view-p (view)
  (not (null (member view '(:sidebar :agents-left)))))

(defun layout-positions (view)
  "Validated divider positions for VIEW; ignore malformed or old preferences."
  (let ((positions (setting (layout-position-key view))))
    (when (and (proper-list-p positions) (eql (list-length positions) 2)
               (integerp (first positions)) (<= 0 (first positions) 100000)
               (or (null (second positions))
                   (and (integerp (second positions)) (<= 0 (second positions) 100000))))
      positions)))

(defun equal-agent-divider-position (view available &optional (other-agent-size 0))
  "Divider within AVAILABLE child space (excluding its handle).
In Three panes the first agent is outside this split, with OTHER-AGENT-SIZE.
Sidebar and responsive layouts split the child space in half."
  (max 0 (if (eq view :panes)
             (- available other-agent-size)
             (floor available 2))))

(defun agent-divider-custom-p (position equal-position previous-size current-size)
  "Ignore rounding and allocation-driven changes; retain an unequal stable split."
  (and previous-size (equal previous-size current-size)
       (> (abs (- position equal-position)) 2)))

(defun agent-layout-preference (positions custom-split-p)
  "Keep the outer divider, but persist an inner pixel position only for a custom split."
  (list (first positions) (when custom-split-p (second positions))))

(defun migrate-agent-panel-positions ()
  "Clear legacy inner defaults once, preserving conversation/sidebar widths."
  (unless (eql (setting :equal-agent-panels-version) 2)
    (dolist (view '(:sidebar :agents-left :panes))
      (let ((positions (layout-positions view)))
        (when positions
          (setf (getf *settings* (layout-position-key view))
                (agent-layout-preference positions nil)))))
    (setf (setting :equal-agent-panels-version) 2)))

(defun saved-choice-index (key count)
  "Return a valid dropdown index, tolerating obsolete or malformed settings."
  (let ((index (setting key 0)))
    (if (and (integerp index) (<= 0 index) (< index count)) index 0)))

(defun saved-terminal-height ()
  "The last visible terminal height, ignoring malformed preferences."
  (let ((height (setting :terminal-height)))
    (when (and (integerp height) (<= 1 height 100000)) height)))

(defun update-terminal-layout-settings (visible height restore-pending-p)
  "Update terminal preferences in memory; return true only when they changed."
  (let ((visible (not (null visible)))
        (changed nil))
    (unless (eql visible (setting :terminal-visible))
      (setf (getf *settings* :terminal-visible) visible changed t))
    (when (and visible (not restore-pending-p)
               (integerp height) (<= 1 height 100000)
               (not (eql height (saved-terminal-height))))
      (setf (getf *settings* :terminal-height) height changed t))
    changed))

(defun saved-checklist-width ()
  "The last visible checklist width, ignoring malformed preferences."
  (let ((width (setting :checklist-width)))
    (when (and (integerp width) (<= 1 width 100000)) width)))

(defparameter *panel-shortcuts*
  '((:agents-shortcut "Agents" "a")
    (:checklist-shortcut "Checklist" "l")
    (:terminal-shortcut "Terminal" "t")))

(defun default-panel-shortcut (name)
  (format nil "~a+~a" #+darwin "Cmd+Option" #-darwin "Ctrl+Alt"
          (string-upcase (third (assoc name *panel-shortcuts*)))))

(defun parse-panel-shortcut (text)
  "Return (key-name modifiers), :DISABLED for blank, or NIL for invalid input."
  (when (stringp text)
    (let* ((text (string-trim '(#\Space #\Tab) text))
           (parts (mapcar (lambda (s) (string-downcase (string-trim '(#\Space) s)))
                          (uiop:split-string text :separator "+")))
           (key (car (last parts)))
           (mods (loop for part in (butlast parts)
                       collect (cdr (assoc part '(("cmd" . :meta-mask) ("meta" . :meta-mask)
                                                  ("option" . :alt-mask) ("alt" . :alt-mask)
                                                  ("ctrl" . :control-mask) ("control" . :control-mask)
                                                  ("super" . :super-mask) ("shift" . :shift-mask))
                                           :test #'string=)))))
      (cond ((zerop (length text)) :disabled)
            ((and mods (every #'identity mods)
                  (= (length mods) (length (remove-duplicates mods)))
                  (intersection mods '(:meta-mask :alt-mask :control-mask :super-mask))
                  (or (and (= (length key) 1) (find (char key 0) "abcdefghijklmnopqrstuvwxyz0123456789"))
                      (member key '("grave" "space" "f1" "f2" "f3" "f4" "f5" "f6"
                                    "f7" "f8" "f9" "f10" "f11" "f12") :test #'equal)))
             (list (if (and (> (length key) 1) (char= (char key 0) #\f))
                       (string-upcase key) key)
                   (sort mods #'string< :key #'symbol-name)))))))

(defun panel-shortcut-text (name)
  (let ((saved (setting name)))
    (if (and (stringp saved) (parse-panel-shortcut saved))
        saved (default-panel-shortcut name))))

(defun quit-shortcut-p (key modifiers)
  "Cmd+Q, with or without Super, which the window handles before panel shortcuts."
  (and (string= key "q")
       (member :meta-mask modifiers)
       (not (intersection '(:control-mask :alt-mask :shift-mask) modifiers))))

(defun find-shortcut-p (key modifiers &key terminal-focused (macos (uiop:os-macosx-p)))
  "Use Cmd+F on macOS; preserve terminal Ctrl+F on other platforms."
  (and (string-equal key "f")
       (if macos
           (and (member :meta-mask modifiers) (not (member :control-mask modifiers)))
           (and (member :control-mask modifiers) (not terminal-focused)
                (not (member :meta-mask modifiers))))
       (not (intersection '(:alt-mask :shift-mask) modifiers))))

(defun terminal-grave-shortcut-p (key modifiers)
  "Ctrl+`, with any other modifiers, the fixed terminal toggle."
  (and (member key '("grave" "dead_grave") :test #'string=)
       (member :control-mask modifiers)))

(defun panel-shortcut-error (texts)
  "Validate all three bindings before saving any of them."
  (let ((seen nil))
    (loop for text in texts
          for parsed = (parse-panel-shortcut text)
          do (cond ((null parsed) (return "Use modifiers plus a letter, digit, F1–F12, Space or Grave."))
                   ((eq parsed :disabled))
                   ((or (apply #'quit-shortcut-p parsed)
                        (apply #'terminal-grave-shortcut-p parsed))
                    (return "That shortcut is reserved for Quit or the terminal toggle."))
                   ((member parsed seen :test #'equal) (return "Choose a different shortcut for each action."))
                   (t (push parsed seen))))))

(defun panel-shortcut-action (matches)
  "The first enabled panel shortcut for which MATCHES, called with its key
name and modifiers, returns :EXACT or :PARTIAL (a GdkKeyMatch)."
  (loop for (name) in *panel-shortcuts*
        for parsed = (parse-panel-shortcut (panel-shortcut-text name))
        when (and (consp parsed)
                  (member (funcall matches (first parsed) (second parsed)) '(:exact :partial)))
          return name))
