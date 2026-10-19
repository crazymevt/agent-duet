;;;; agent-integration.asd

(defsystem "agent-integration"
  :description "A Common Lisp project."
  :author "Jessie Hughart"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("gtk4")
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "main"))))
  :entry-point "agent-integration:main"
  :in-order-to ((test-op (test-op "agent-integration/tests"))))

(defsystem "agent-integration/tests"
  :description "Tests for agent-integration."
  :author "Jessie Hughart"
  :license "MIT"
  :depends-on ("agent-integration" "parachute")
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :agent-integration/tests)))
