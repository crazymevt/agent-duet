;;;; agent-duet.asd

(defsystem "agent-duet/core"
  :description "Claude Code and Codex driven through their CLIs, and the workflows that have them work together. No GTK."
  :author "Jessie Hughart"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("uiop")
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "json")
               (:file "markdown")
               (:file "claude")
               (:file "codex")
               (:file "usage")
               (:file "settings")
               (:file "history")
               (:file "agents")
               (:file "workflow")))

(defsystem "agent-duet"
  :description "Agent Duet: Claude and ChatGPT (Codex) side by side, writing and reviewing each other's code."
  :author "Jessie Hughart"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("agent-duet/core" "gtk4" "gtk4-adwaita" "cffi" "sb-posix")
  :pathname "src/"
  :serial t
  :components ((:file "ui")
               (:file "terminal")
               (:file "preview")
               (:file "app"))
  :entry-point "agent-duet:main"
  :in-order-to ((test-op (test-op "agent-duet/tests"))))

(defsystem "agent-duet/tests"
  :description "Tests for agent-duet (headless: no GTK)."
  :author "Jessie Hughart"
  :license "MIT"
  :depends-on ("agent-duet/core" "parachute")
  :components ((:module "tests"
                :serial t
                :components ((:file "main")
                             (:file "history"))))
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :agent-duet/tests)))
