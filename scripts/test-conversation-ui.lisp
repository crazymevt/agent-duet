;;;; GTK routing, synchronized attention cards, conversation and layout regression checks; run with make test-ui on a machine with GTK/display support:
;;;; sbcl --non-interactive --load scripts/test-conversation-ui.lisp
(require :asdf)
(require :sb-posix)
;; Isolate every check, including system loading and GTK initialization.
(let* ((config (uiop:ensure-directory-pathname
                (string-trim '(#\Space #\Newline #\Return)
                             (uiop:run-program '("mktemp" "-d") :output :string))))
       (old-config (uiop:getenv "XDG_CONFIG_HOME")))
  (setf (uiop:getenv "XDG_CONFIG_HOME") (namestring config))
  (unwind-protect
       (load (merge-pathnames "test-conversation-ui-body.lisp" *load-truename*))
    (if old-config
        (setf (uiop:getenv "XDG_CONFIG_HOME") old-config)
        (sb-posix:unsetenv "XDG_CONFIG_HOME"))
    (uiop:delete-directory-tree config :validate t)))
(uiop:quit 0)
