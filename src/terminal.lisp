;;;; terminal.lisp — the Terminal panel: shells in the folder, below the panes
;;;;
;;;; Each terminal is a VteTerminal (GNOME's terminal widget, libvte for GTK
;;;; 4), loaded when first needed and called through CFFI, as in Cadre: the
;;;; gtk4 bindings have no Vte namespace. VTE's own spawning crashes the child
;;;; on macOS, so the child is started with g_spawn_async, with
;;;; vte_pty_child_setup attaching it to the pty.
;;;;
;;;; The panel opens and closes with the Terminal button or Ctrl+`; closing
;;;; it keeps the shells running. New terminals start in the agents' folder.

(in-package #:agent-duet)

(defparameter *terminal-scrollback* 10000)

;;; libvte

(defparameter *vte-library-names*
  '(#+darwin "libvte-2.91-gtk4.0.dylib" #-darwin "libvte-2.91-gtk4.so.0"))

(defvar *vte-loaded* :unknown)
(defvar *vte-symbols* (make-hash-table :test 'equal))

(defun vte-available-p ()
  "Load libvte (for GTK 4) if it is installed; true if it is."
  (when (eq *vte-loaded* :unknown)
    (setf *vte-loaded*
          (loop for name in *vte-library-names*
                thereis (loop for directory in (append gtk4.runtime:*library-directories* (list nil))
                              thereis (ignore-errors
                                       (cffi:load-foreign-library
                                        (if directory (namestring (merge-pathnames name directory)) name))
                                       t)))))
  *vte-loaded*)

(defun vte-symbol (name)
  (or (gethash name *vte-symbols*)
      (setf (gethash name *vte-symbols*)
            (or (cffi:foreign-symbol-pointer name)
                (error "libvte has no ~a" name)))))

(defmacro vte (name return-type &rest arguments)
  "Call the libvte (or GLib) function NAME: arguments as type, value pairs."
  `(gtk4.runtime:with-gtk-float-traps
    (cffi:foreign-funcall-pointer (vte-symbol ,name) () ,@arguments ,return-type)))

(defun vte-missing-message ()
  (format nil "The terminal needs VTE, GNOME's terminal widget~:[ (libvte for GTK 4, from your distribution)~; (brew install vte3)~]"
          (member :darwin *features*)))

(defun take-gerror (place)
  "The message of the GError at PLACE (a GError**), freeing it; nil if none."
  (let ((error (cffi:mem-ref place :pointer)))
    (unless (cffi:null-pointer-p error)
      (prog1 (cffi:mem-ref error :string 8) ; GError: domain, code, message
        (vte "g_error_free" :void :pointer error)))))

(defun foreign-strv (strings)
  "A NULL-terminated array of new C strings; free it with FREE-STRV."
  (let ((array (cffi:foreign-alloc :pointer :count (1+ (length strings)))))
    (loop for s in strings for i from 0
          do (setf (cffi:mem-aref array :pointer i) (cffi:foreign-string-alloc s :encoding :utf-8)))
    (setf (cffi:mem-aref array :pointer (length strings)) (cffi:null-pointer))
    array))

(defun free-strv (array)
  (loop for i from 0
        for p = (cffi:mem-aref array :pointer i)
        until (cffi:null-pointer-p p)
        do (cffi:foreign-string-free p))
  (cffi:foreign-free array))

;;; What a terminal runs

(defun terminal-shell-command (&key (shell (uiop:getenv "SHELL")) (login (member :darwin *features*)))
  "The user's $SHELL (else /bin/sh); on macOS as a login shell, as
Terminal.app starts it, so the profile files set up PATH."
  (let ((program (if (plusp (length shell)) shell "/bin/sh")))
    (if login (list program "-l") (list program))))

(defparameter *terminal-environment*
  '(("TERM" . "xterm-256color") ("COLORTERM" . "truecolor") ("TERM_PROGRAM" . "AgentDuet")))

(defun terminal-environment (environment &optional (overrides *terminal-environment*))
  "ENVIRONMENT (\"NAME=value\" strings) with OVERRIDES (an alist) in place of
any variables of the same names."
  (append (loop for (name . value) in overrides collect (format nil "~a=~a" name value))
          (remove-if (lambda (entry)
                       (let ((name (subseq entry 0 (or (position #\= entry) (length entry)))))
                         (assoc name overrides :test #'string=)))
                     environment)))

(defun program-name (command)
  (let ((program (first command)))
    (subseq program (1+ (or (position #\/ program :from-end t) -1)))))

;;; Terminals

(defstruct (term (:conc-name term-))
  widget pointer pid command title directory (exited nil) button)

(defvar *terminals* '() "The open terminals, oldest first.")
(defvar *current-terminal* nil)
(defvar *terminal-panel* nil "The panel: tabs, buttons and the terminals' stack.")
(defvar *terminal-stack* nil)
(defvar *terminal-tabs* nil)

(defparameter *terminal-palettes*
  '((:dark "#000000" "#cd3131" "#0dbc79" "#e5e510" "#2472c8" "#bc3fbc" "#11a8cd" "#e5e5e5"
     "#666666" "#f14c4c" "#23d18b" "#f5f543" "#3b8eea" "#d670d6" "#29b8db" "#ffffff")
    (:light "#000000" "#cd3131" "#00bc00" "#949800" "#0451a5" "#bc05bc" "#0598bc" "#555555"
     "#666666" "#cd3131" "#14ce14" "#b5ba00" "#0451a5" "#bc05bc" "#0598bc" "#a5a5a5"))
  "The 16 ANSI colors, for dark and light looks.")

(defun parse-hex-color (string)
  "STRING, #rrggbb or #rrggbbaa, as four floats from 0 to 1."
  (let ((hex (string-left-trim "#" string)))
    (flet ((part (i) (/ (parse-integer hex :start i :end (+ i 2) :radix 16) 255.0)))
      (list (part 0) (part 2) (part 4) (if (>= (length hex) 8) (part 6) 1.0)))))

(defun set-rgba (pointer index color)
  (loop for value in (parse-hex-color color) for j from 0
        do (setf (cffi:mem-aref pointer :float (+ (* 4 index) j)) value)))

(defun style-terminal (term)
  "Colors for the current look; the background transparent, showing the panel's."
  (let ((dark (adw:dark-p)))
    (cffi:with-foreign-objects ((fg :float 4) (bg :float 4) (colors :float (* 4 16)))
      (set-rgba fg 0 (if dark "#e6e6e6" "#2e2e2e"))
      (set-rgba bg 0 "#00000000")
      (loop for color in (cdr (assoc (if dark :dark :light) *terminal-palettes*)) for i from 0
            do (set-rgba colors i color))
      (vte "vte_terminal_set_colors" :void :pointer (term-pointer term) :pointer fg :pointer bg
           :pointer colors :size 16)))
  (vte "vte_terminal_set_scrollback_lines" :void :pointer (term-pointer term) :long *terminal-scrollback*))

(defun restyle-terminals ()
  (dolist (term *terminals*) (style-terminal term)))

(defun spawn-in-terminal (term)
  "Start TERM's command on a new pty in TERM. Returns the pid."
  (cffi:with-foreign-objects ((error :pointer) (pid :int))
    (setf (cffi:mem-ref error :pointer) (cffi:null-pointer))
    (let ((pty (vte "vte_pty_new_sync" :pointer :int 0 :pointer (cffi:null-pointer) :pointer error)))
      (when (cffi:null-pointer-p pty)
        (error "Couldn't make a terminal: ~a" (take-gerror error)))
      (vte "vte_terminal_set_pty" :void :pointer (term-pointer term) :pointer pty)
      (let ((argv (foreign-strv (term-command term)))
            (envp (foreign-strv (terminal-environment (sb-ext:posix-environ)))))
        (unwind-protect
            (let ((ok (vte "g_spawn_async" :int
                           :string (uiop:native-namestring (term-directory term))
                           :pointer argv :pointer envp
                           :int (logior 2 4) ; G_SPAWN_DO_NOT_REAP_CHILD, G_SPAWN_SEARCH_PATH
                           :pointer (vte-symbol "vte_pty_child_setup") :pointer pty
                           :pointer pid :pointer error)))
              (when (zerop ok)
                (vte "g_object_unref" :void :pointer pty)
                (error "Couldn't start ~a: ~a" (first (term-command term)) (take-gerror error))))
          (free-strv argv)
          (free-strv envp)))
      ;; The terminal holds the pty now.
      (vte "g_object_unref" :void :pointer pty)
      (vte "vte_terminal_watch_child" :void :pointer (term-pointer term) :int (cffi:mem-ref pid :int))
      (cffi:mem-ref pid :int))))

(defun start-terminal (&key (command (terminal-shell-command)) (directory *directory*) (focus t))
  "Open a new terminal running COMMAND in DIRECTORY, and show it."
  (let* ((pointer (vte "vte_terminal_new" :pointer))
         (widget (gtk4.runtime:wrap-object pointer))
         (term (make-term :widget widget :pointer pointer :command command
                          :title (program-name command)
                          :directory (uiop:ensure-directory-pathname (or directory (user-homedir-pathname))))))
    (gtk:widget-add-css-class widget "duet-terminal")
    (gtk:widget-set-hexpand widget t)
    (gtk:widget-set-vexpand widget t)
    (vte "vte_terminal_set_audible_bell" :void :pointer pointer :int 0)
    (vte "vte_terminal_set_mouse_autohide" :void :pointer pointer :int 1)
    (vte "vte_terminal_set_scroll_on_keystroke" :void :pointer pointer :int 1)
    (vte "vte_terminal_set_scroll_on_output" :void :pointer pointer :int 0)
    (style-terminal term)
    (gobject:connect widget "child-exited" (lambda (w status) (declare (ignore w)) (terminal-exited term status)))
    (gobject:connect widget "termprop-changed"
                     (lambda (w name) (declare (ignore w))
                       (when (equal name "xterm.title") (terminal-title-changed term))))
    (setup-terminal-keys term)
    (setf (term-pid term) (spawn-in-terminal term))
    (setf *terminals* (append *terminals* (list term)))
    (gtk:stack-add-named *terminal-stack* widget (format nil "term-~d" (term-pid term)))
    (add-terminal-tab term)
    (select-terminal term :focus focus)
    term))

(defun live-terminals () (remove-if #'term-exited *terminals*))

(defun terminal-title-changed (term)
  (let ((title (ignore-errors (vte "vte_terminal_get_window_title" :string :pointer (term-pointer term)))))
    (when (plusp (length title))
      (setf (term-title term) title)
      (update-terminal-tab term))))

(defun wait-status-text (status)
  (if (zerop (logand status #x7f))
      (format nil "exited with code ~d" (ash status -8))
      (format nil "ended by signal ~d" (logand status #x7f))))

(defun feed-terminal-display (term text)
  "Show TEXT in TERM, as if its program had written it."
  (cffi:with-foreign-string ((bytes length) text :encoding :utf-8 :null-terminated-p nil)
    (vte "vte_terminal_feed" :void :pointer (term-pointer term) :pointer bytes :ssize length)))

(defun terminal-exited (term status)
  "TERM's process ended: close it if it ended well, else keep it, saying how it ended."
  (unless (term-exited term)
    (setf (term-exited term) t)
    (if (zerop status)
        (remove-terminal term)
        (progn
          (feed-terminal-display term (format nil "~c[0m~c~%[Process ~a]~c~%" #\Esc #\Return
                                              (wait-status-text status) #\Return))
          (update-terminal-tab term)))))

(defun remove-terminal (term)
  (setf *terminals* (remove term *terminals*))
  (gtk:stack-remove *terminal-stack* (term-widget term))
  (gtk:box-remove *terminal-tabs* (term-button term))
  (when (eq term *current-terminal*)
    (setf *current-terminal* nil)
    (let ((next (car (last *terminals*))))
      (if next
          (select-terminal next :focus t)
          (gtk:stack-set-visible-child-name *terminal-stack* "empty")))))

(defun kill-term (term)
  "End TERM's process (with SIGHUP, as closing a terminal window does) and close it."
  (unless (term-exited term)
    (setf (term-exited term) t)
    (ignore-errors (sb-posix:kill (term-pid term) sb-posix:sighup)))
  (remove-terminal term))

(defun kill-all-terminals ()
  (dolist (term *terminals*)
    (unless (term-exited term)
      (setf (term-exited term) t)
      (ignore-errors (sb-posix:kill (term-pid term) sb-posix:sighup)))))

;;; Keys: ⌘C, ⌘V, ⌘K on macOS (Ctrl+Shift+C, V on others)

(defun terminal-copy (term)
  (when (/= 0 (vte "vte_terminal_get_has_selection" :int :pointer (term-pointer term)))
    (vte "vte_terminal_copy_clipboard_format" :void :pointer (term-pointer term) :int 1))) ; VTE_FORMAT_TEXT

(defun terminal-paste (term)
  (vte "vte_terminal_paste_clipboard" :void :pointer (term-pointer term)))

(defun feed-terminal (term string)
  "Write STRING to TERM's process, as if typed."
  (cffi:with-foreign-string ((bytes length) string :encoding :utf-8 :null-terminated-p nil)
    (vte "vte_terminal_feed_child" :void :pointer (term-pointer term) :pointer bytes :ssize length)))

(defun clear-terminal (term)
  "Forget TERM's scrollback and screen, and have the program draw itself again (Ctrl+L)."
  (vte "vte_terminal_reset" :void :pointer (term-pointer term) :int 1 :int 1)
  (feed-terminal term (string (code-char 12))))

(defun setup-terminal-keys (term)
  (let ((keys (gtk:event-controller-key-new)))
    (gtk:event-controller-set-propagation-phase keys :capture)
    (gobject:connect keys :key-pressed
                     (lambda (c keyval keycode state)
                       (declare (ignore c keycode))
                       (let* ((mods (modifier-list state))
                              (name (gdk:keyval-name (gdk:keyval-to-lower keyval)))
                              (command (if (member :darwin *features*)
                                           (and (member :meta-mask mods) t)
                                           (and (member :control-mask mods) (member :shift-mask mods) t)))
                              (command (or command (and (member :super-mask mods) t))))
                         (cond ((not command) nil)
                               ((string= name "c") (terminal-copy term) t)
                               ((string= name "v") (terminal-paste term) t)
                               ((string= name "k") (clear-terminal term) t)
                               ((string= name "a") (vte "vte_terminal_select_all" :void :pointer (term-pointer term)) t)
                               (t nil)))))
    (gtk:widget-add-controller (term-widget term) keys)))

;;; The panel

(defun terminal-tab-label (term)
  (format nil "~a~:[~; (ended)~]" (term-title term) (term-exited term)))

(defun add-terminal-tab (term)
  (let ((button (make-instance 'gtk:toggle-button :label (terminal-tab-label term)
                               :css-classes '("flat")
                               :tooltip-text (format nil "~{~a~^ ~}~%in ~a"
                                                     (term-command term)
                                                     (uiop:native-namestring (term-directory term))))))
    (let ((other (find-if #'term-button (remove term *terminals*))))
      (when other (gtk:toggle-button-set-group button (term-button other))))
    (gobject:connect button :toggled
                     (lambda (b) (when (and (gtk:toggle-button-get-active b) (not (eq term *current-terminal*)))
                                   (select-terminal term :focus t))))
    (setf (term-button term) button)
    (gtk:box-append *terminal-tabs* button)))

(defun update-terminal-tab (term)
  (when (term-button term)
    (gtk:button-set-label (term-button term) (terminal-tab-label term))))

(defun select-terminal (term &key focus)
  (setf *current-terminal* term)
  (gtk:stack-set-visible-child *terminal-stack* (term-widget term))
  (unless (gtk:toggle-button-get-active (term-button term))
    (gtk:toggle-button-set-active (term-button term) t))
  (when focus (gtk:widget-grab-focus (term-widget term))))

(defun new-terminal ()
  "A new shell in the agents' folder, shown in the panel."
  (if (vte-available-p)
      (handler-case (start-terminal)
        (error (e) (show-terminal-problem (princ-to-string e))))
      (show-terminal-problem (vte-missing-message))))

(defun show-terminal-problem (text)
  (let ((page (gtk:stack-get-child-by-name *terminal-stack* "empty")))
    (gtk:label-set-text (gtk:widget-get-first-child page) text)
    (gtk:stack-set-visible-child-name *terminal-stack* "empty")))

(defun make-terminal-panel ()
  "The panel: tabs and buttons above, the current terminal below. Hidden at first."
  (let ((stack (make-instance 'gtk:stack :vexpand t :hexpand t))
        (tabs (make-instance 'gtk:box :spacing 2)))
    (setf *terminal-stack* stack
          *terminal-tabs* tabs)
    (gtk:stack-add-named stack
                         (gtk:build
                          (gtk:box :orientation :vertical :valign :center :spacing 8
                                   (gtk:label :label "No terminal" :css-classes '("dim-label") :wrap t)
                                   (gtk:button :label "New Terminal" :halign :center :css-classes '("pill")
                                               :on-clicked (lambda (b) (declare (ignore b)) (new-terminal)))))
                         "empty")
    (setf *terminal-panel*
          (gtk:build
           (gtk:box :orientation :vertical :css-classes '("duet-terminal-panel") :visible nil
                    (gtk:box :spacing 4 :margin-start 8 :margin-end 6 :margin-top 2 :margin-bottom 2
                             (gtk:label :label "Terminal" :css-classes '("heading") :margin-end 6)
                             (gtk:scrolled-window :hscrollbar-policy :automatic :vscrollbar-policy :never
                                                  :hexpand t :child tabs)
                             (gtk:button :label "+ New" :css-classes '("flat")
                                         :tooltip-text "A new shell in the agents' folder"
                                         :on-clicked (lambda (b) (declare (ignore b)) (new-terminal)))
                             (gtk:button :label "Clear" :css-classes '("flat") :tooltip-text "Clear this terminal (⌘K)"
                                         :on-clicked (lambda (b) (declare (ignore b))
                                                       (let ((term *current-terminal*))
                                                         (when (and term (not (term-exited term))) (clear-terminal term)))))
                             (gtk:button :label "Kill" :css-classes '("flat") :tooltip-text "End this terminal's shell and close it"
                                         :on-clicked (lambda (b) (declare (ignore b))
                                                       (when *current-terminal* (kill-term *current-terminal*))))
                             (gtk:button :label "Hide" :css-classes '("flat") :tooltip-text "Hide the panel; the shells keep running (Ctrl+`)"
                                         :on-clicked (lambda (b) (declare (ignore b)) (set-terminal-visible nil))))
                    (gtk:separator)
                    stack)))))

(defun terminal-visible-p ()
  (and *terminal-panel* (gtk:widget-get-visible *terminal-panel*)))

(defun set-terminal-visible (visible)
  "Show or hide the Terminal panel. Showing it starts a shell if there's none."
  (gtk:widget-set-visible *terminal-panel* visible)
  (let ((toggle (control :terminal)))
    (when (and toggle (not (eq (gtk:toggle-button-get-active toggle) (and visible t))))
      (gtk:toggle-button-set-active toggle visible)))
  (when visible
    (cond ((live-terminals)
           (when *current-terminal* (gtk:widget-grab-focus (term-widget *current-terminal*))))
          ((null *terminals*) (new-terminal)))))

(defun toggle-terminal ()
  (set-terminal-visible (not (terminal-visible-p))))
