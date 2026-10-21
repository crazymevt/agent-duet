;;;; screenshots.lisp — regenerate the README screenshots in docs/; run with make screenshots
;;;; (needs GTK and a working display):
;;;; sbcl --non-interactive --load scripts/screenshots.lisp
;;;;
;;;; Opens the window on the demo conversation in screenshot-demo.json, adds an
;;;; approval card, and draws each layout to a PNG at twice the window's size.
;;;; Settings and history live in a temporary directory, so the user's own
;;;; aren't read or changed; nothing is sent to either agent.
(require :asdf)
(require :sb-posix)

(defvar *root* (merge-pathnames "../" (uiop:pathname-directory-pathname *load-truename*)))
(defvar *demo-id* "d3m0d3m0-0000-4000-8000-000000000001")
;; The folder the demo conversation names: the header shows it.
(defvar *demo-folder* "/tmp/agent-duet-demo/recipe-box/")
(defvar *shots* '((:sidebar "screenshot") (:panes "screenshot-three-panes")
                  (:conversation "screenshot-conversation-only")))

(push (truename *root*) asdf:*central-registry*)
(ql:quickload :agent-duet)

(let ((temp (uiop:ensure-directory-pathname
             (string-trim '(#\Space #\Newline #\Return)
                          (uiop:run-program '("mktemp" "-d") :output :string)))))
  (setf (uiop:getenv "XDG_CONFIG_HOME") (namestring (merge-pathnames "config/" temp))
        (uiop:getenv "XDG_STATE_HOME") (namestring (merge-pathnames "state/" temp)))
  (let ((settings (merge-pathnames "config/agent-duet/settings.lisp" temp))
        (history (merge-pathnames (format nil "state/agent-duet/history/~a.json" *demo-id*) temp)))
    (ensure-directories-exist settings)
    (with-open-file (out settings :direction :output)
      (prin1 '(:theme :dark :window-size (1440 900) :view :sidebar :permission :ask :claude-model "opus"
               :workflow-progress-expanded nil :workflow-settings-expanded nil
               :terminal-visible nil :checklist-visible nil)
             out))
    (ensure-directories-exist history)
    (uiop:copy-file (merge-pathnames "scripts/screenshot-demo.json" *root*) history))
  (ensure-directories-exist *demo-folder*)
  (unless (uiop:directory-exists-p (merge-pathnames ".git/" *demo-folder*))
    (uiop:run-program (list "git" "-C" *demo-folder* "init" "-q")))
  (defparameter cl-user::*screenshot-temp* temp))

(in-package #:agent-duet)

;; Keep the account's real usage out of the screenshots.
(defmethod agent-usage-windows :around (agent) (declare (ignore agent)) nil)

(defun save-screenshot (name)
  "Draw the window to docs/NAME.png at twice its size."
  (let* ((width (gtk:widget-get-width *window*))
         (height (gtk:widget-get-height *window*))
         (snapshot (gtk:snapshot-new)))
    (gtk:snapshot-scale snapshot 2.0 2.0)
    (gdk:paintable-snapshot (gtk:widget-paintable-new *window*) snapshot (float width 1d0) (float height 1d0))
    (let* ((node (gtk:snapshot-to-node snapshot))
           (texture (gsk:renderer-render-texture (gtk:native-get-renderer *window*) node nil))
           (file (merge-pathnames (format nil "docs/~a.png" name) cl-user::*root*)))
      (gdk:texture-save-to-png texture (namestring file))
      (format t "~&Saved ~a (~dx~d)~%" (enough-namestring file cl-user::*root*) (* 2 width) (* 2 height))
      (finish-output))))

(defun after (ms function)
  (glib:timeout-add glib:+priority-default+ ms (lambda () (funcall function) nil)))

(defun screenshot-views (shots)
  "Show each view, scrolled to the end, and save it; then close the window."
  (if (null shots)
      (after 200 (lambda () (gtk:window-close *window*)))
      (destructuring-bind ((view name) &rest more) shots
        (set-view view)
        (after 1200 (lambda ()
                      (dolist (entry *panes*) (pane-scroll-to-end (cdr entry)))
                      (pane-scroll-to-end *middle*)
                      (after 600 (lambda () (save-screenshot name) (screenshot-views more))))))))

;; Agent checks finish within the first few seconds.
(after 3000
       (lambda ()
         (let ((history (load-history cl-user::*demo-id*)))
           (replay-history history)
           (setf *history* history))
         (pane-approval (agent-pane-of *codex*) "demo-approval" "$ /bin/zsh -lc \"npm test\""
                        "Run the test suite to check the listener fix.")
         (screenshot-views cl-user::*shots*)))

(unwind-protect (main cl-user::*demo-folder*)
  (uiop:delete-directory-tree cl-user::*screenshot-temp* :validate t))
(uiop:quit 0)
