;;;; tests/main.lisp

(defpackage #:agent-integration/tests
  (:use #:cl #:parachute))

(in-package #:agent-integration/tests)

(define-test agent-integration)

(define-test hello
  :parent agent-integration
  (is string= "Hello, World!" (agent-integration:hello))
  (is string= "Hello, Lisp!" (agent-integration:hello "Lisp")))
