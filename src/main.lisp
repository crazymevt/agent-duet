;;;; main.lisp — A Common Lisp project.

(in-package #:agent-integration)

(defun hello (&optional (who "World"))
  "A greeting for WHO."
  (format nil "Hello, ~a!" who))

;;; Handlers are connected by symbol, so redefining ACTIVATE (or a
;;; function it calls) changes the running program.

(defun activate (app)
  "Open the main window."
  (let ((window (make-instance 'gtk:application-window
                               :application app :title "agent-integration"
                               :default-width 480 :default-height 320)))
    (gtk:window-set-child window (make-instance 'gtk:label :label (hello)))
    (gtk:window-present window)))

(defun main ()
  "Run the application; returns when its last window closes. On macOS it
must run on the first thread: in Cadre, use Run GTK App."
  (let ((app (gtk:application-new "org.example.agent_integration" '(:default-flags))))
    (gobject:connect app :window-removed
                     (lambda (application window)
                       (declare (ignore window))
                       (when (null (gtk:application-get-windows application))
                         (gio:application-quit application))))
    (gobject:connect app :activate 'activate)
    (gio:application-run app nil)))
