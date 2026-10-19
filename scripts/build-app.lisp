;;;; build-app.lisp — save the image the macOS app runs:
;;;;   sbcl --dynamic-space-size 2048 --non-interactive --load scripts/build-app.lisp
;;;; (make app runs this, then scripts/make-app.sh puts it in Agent Duet.app.)
;;;;
;;;; It writes build/app/agent-duet.core, a core without the runtime: the app
;;;; runs it with a copy of this SBCL's runtime, which a launcher starts. The
;;;; core is kept apart from the runtime because macOS can't code-sign an
;;;; executable with an image appended to it. As in Cadre.
(require :asdf)
(push (truename ".") asdf:*central-registry*)
(ql:quickload :agent-duet :silent t)

(let ((others (remove-if (lambda (thread) (search "finalizer" (sb-thread:thread-name thread)))
                         (rest (sb-thread:list-all-threads)))))
  (when others
    (error "Can't save: other threads are running (~{~a~^, ~})" (mapcar #'sb-thread:thread-name others))))

(let ((path (merge-pathnames "build/app/agent-duet.core" (truename "."))))
  (ensure-directories-exist path)
  (format t "~&Saving ~a~%" path)
  (sb-ext:save-lisp-and-die path
                            :toplevel (lambda ()
                                        (let ((status (gtk4.runtime:with-gtk-float-traps
                                                          (agent-duet::app-main))))
                                          (sb-ext:exit :code (if (integerp status) status 0) :abort nil)))))
