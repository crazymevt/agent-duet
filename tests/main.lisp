;;;; tests/main.lisp

(defpackage #:agent-integration/tests
  (:use #:cl #:parachute)
  (:local-nicknames (#:ai #:agent-integration)))

(in-package #:agent-integration/tests)

(define-test agent-integration)

;;; JSON and the CLIs' events

(define-test json
  :parent agent-integration
  (let ((v (ai:parse-json "{\"a\": [1, 2.5, \"x\\n\"], \"b\": true, \"c\": null}")))
    (is equal 1 (ai:jget v "a" 0))
    (is = 2.5d0 (ai:jget v "a" 1))
    (is string= (format nil "x~%") (ai:jget v "a" 2))
    (is eq t (ai:jget v "b"))
    (is eq :null (ai:jget v "c")))
  (is string= "{\"k\":\"v\",\"n\":[1,2]}" (ai:json-string (ai:jobj "k" "v" "n" #(1 2)))))

(define-test claude-events
  :parent agent-integration
  (let ((events (ai::parse-claude-event
                 "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false,\"result\":\"Done.\",\"session_id\":\"s1\"}")))
    (is eq :result (ai::event-kind (first events)))
    (is string= "Done." (ai::event-text (first events)))
    (false (ai::event-error (first events))))
  (let ((events (ai::parse-claude-event
                 "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_delta\",\"delta\":{\"type\":\"text_delta\",\"text\":\"Hi\"}}}")))
    (is eq :text-delta (ai::event-kind (first events)))
    (is string= "Hi" (ai::event-text (first events)))))

(define-test claude-arguments
  :parent agent-integration
  (let ((args (ai::claude-arguments :session-id "s" :permission-mode "acceptEdits" :model "opus")))
    (true (search '("--permission-mode" "acceptEdits") args :test #'equal))
    (true (search '("--session-id" "s") args :test #'equal))
    (true (search '("--model" "opus") args :test #'equal))))

(define-test codex-items
  :parent agent-integration
  (is string= "$ make test"
      (ai::codex-item-summary (ai:parse-json "{\"type\":\"commandExecution\",\"command\":\"make test\"}")))
  (is string= "Edit a.lisp, b.lisp"
      (ai::codex-item-summary
       (ai:parse-json "{\"type\":\"fileChange\",\"changes\":[{\"path\":\"/x/a.lisp\"},{\"path\":\"/x/b.lisp\"}]}"))))

;;; Reading replies

(define-test verdicts
  :parent agent-integration
  (is eq :approved (ai:review-verdict (format nil "Looks good.~%~%VERDICT: APPROVED")))
  (is eq :approved (ai:review-verdict (format nil "Fine~%**Verdict: approved**")))
  (is eq :changes (ai:review-verdict (format nil "1. Bug~%VERDICT: CHANGES REQUESTED")))
  (is eq :changes (ai:review-verdict "No verdict here"))
  ;; The last verdict counts: a review may quote the format first.
  (is eq :changes (ai:review-verdict (format nil "Reply VERDICT: APPROVED if...~%VERDICT: CHANGES REQUESTED"))))

(define-test plans
  :parent agent-integration
  (multiple-value-bind (lead partner)
      (ai:parse-plan (format nil "Here's the split.~%~%**LEAD TASK:**~%Write the parser.~%~%**PARTNER TASK:**~%Write the tests."))
    (is string= "Write the parser." lead)
    (is string= "Write the tests." partner))
  (multiple-value-bind (lead partner)
      (ai:parse-plan (format nil "LEAD TASK:~%All of it.~%~%PARTNER TASK:~%NONE"))
    (is string= "All of it." lead)
    (false partner))
  (false (nth-value 1 (ai:parse-plan "Just do it."))))

;;; Workflows, with agents that answer from a script

(defclass fake-agent (ai:agent)
  ((replies :initarg :replies :accessor fake-replies
            :documentation "Functions of the prompt that return the reply, used in turn")
   (prompts :initform '() :accessor fake-prompts)))

(defmethod ai:agent-begin-turn ((agent fake-agent) prompt)
  (push prompt (fake-prompts agent))
  (let ((reply (if (functionp (fake-replies agent))
                   (fake-replies agent)
                   (pop (fake-replies agent)))))
    (ai:agent-turn-finished agent (if reply (funcall reply prompt) "out of replies") (null reply))))

(defun says (text) (lambda (prompt) (declare (ignore prompt)) text))

(defun run-fake (mode claude-replies codex-replies &key (rounds 3))
  (let* ((claude (make-instance 'fake-agent :id :claude :name "Claude" :replies claude-replies))
         (codex (make-instance 'fake-agent :id :codex :name "ChatGPT" :replies codex-replies))
         (messages '())
         (workflow (make-instance 'ai:workflow :mode mode :lead claude :partner codex
                                               :request "Add a frobnicate function" :max-rounds rounds
                                               :on-message (lambda (from to text kind)
                                                             (declare (ignore to))
                                                             (push (list (if (symbolp from) from (ai:agent-name from))
                                                                         kind text)
                                                                   messages)))))
    (ai:start-workflow workflow)
    (values workflow (reverse messages) claude codex)))

(define-test review-approved
  :parent agent-integration
  (multiple-value-bind (workflow messages claude codex)
      (run-fake :review
                (list (says "Added frobnicate in a.lisp") (says "Fixed the off-by-one"))
                (list (says (format nil "1. Off by one~%VERDICT: CHANGES REQUESTED"))
                      (says (format nil "Good now.~%VERDICT: APPROVED"))))
    (is eq :finished (ai:workflow-state workflow))
    (is eq :approved (ai::loop-state (first (ai:workflow-loops workflow))))
    (is equal '(:user :request) (subseq (first messages) 0 2))
    (is equal '(("Claude" :work) ("ChatGPT" :review) ("Claude" :work) ("ChatGPT" :review))
        (loop for m in messages unless (member (first m) '(:user :system)) collect (subseq m 0 2)))
    ;; The reviewer saw the author's report; the author saw the review.
    (true (search "Added frobnicate in a.lisp" (second (fake-prompts codex))))
    (true (search "Off by one" (first (fake-prompts claude))))))

(define-test review-gives-up
  :parent agent-integration
  (let ((no (says (format nil "Still wrong~%VERDICT: CHANGES REQUESTED"))))
    (multiple-value-bind (workflow messages)
        (run-fake :review (list (says "v1") (says "v2")) (list no no) :rounds 2)
      (declare (ignore messages))
      (is eq :finished (ai:workflow-state workflow))
      (is eq :unresolved (ai::loop-state (first (ai:workflow-loops workflow))))
      (is = 2 (ai::loop-round (first (ai:workflow-loops workflow)))))))

(defun responder (&rest pairs)
  "A reply function: the reply after the first of PAIRS' strings the prompt
contains; the last of PAIRS (unpaired) otherwise."
  (lambda (prompt)
    (loop for (key reply) on pairs by #'cddr
          when (null reply) return key
          when (search key prompt) return reply)))

(define-test divide-and-cross-review
  :parent agent-integration
  (let ((ok (format nil "Fine.~%VERDICT: APPROVED")))
    (multiple-value-bind (workflow messages claude codex)
        (run-fake :divide
                  (responder "Split the work" (format nil "LEAD TASK:~%The function~%~%PARTNER TASK:~%The tests")
                             "Review the work" ok
                             "Wrote the function")
                  (responder "Review the work" ok
                             "Wrote the tests"))
      (is eq :finished (ai:workflow-state workflow))
      (is = 2 (length (ai:workflow-loops workflow)))
      (true (every (lambda (l) (eq :approved (ai::loop-state l))) (ai:workflow-loops workflow)))
      (is = 2 (count :review messages :key #'second))
      ;; ChatGPT was given its part and told what Claude is doing.
      (let ((task (find "Your part" (fake-prompts codex) :test #'search)))
        (true task)
        (true (search "The tests" task))
        (true (search "The function" task)))
      ;; Claude reviewed ChatGPT's report.
      (true (find "Wrote the tests" (fake-prompts claude) :test #'search)))))

(define-test divide-alone
  :parent agent-integration
  (let ((ok (says (format nil "VERDICT: APPROVED"))))
    (let ((workflow (run-fake :divide
                              (list (says (format nil "LEAD TASK:~%Everything~%PARTNER TASK:~%NONE"))
                                    (says "Did it all"))
                              (list ok))))
      (is eq :finished (ai:workflow-state workflow))
      (is = 1 (length (ai:workflow-loops workflow))))))

(define-test failed-turn
  :parent agent-integration
  ;; Claude has no replies: its turn fails, and so does the workflow.
  (let ((workflow (run-fake :review '() '())))
    (is eq :finished (ai:workflow-state workflow))
    (is eq :failed (ai::loop-state (first (ai:workflow-loops workflow))))))

(define-test queueing
  :parent agent-integration
  ;; A prompt given while a turn runs waits for it.
  (let* ((results '())
         (agent (make-instance 'fake-agent :id :claude :name "Claude"
                                           :replies (list (says "one") (says "two")))))
    (setf (ai::agent-busy agent) t)
    (ai:agent-submit agent "first" (lambda (text error) (declare (ignore error)) (push text results)))
    (is equal '() (fake-prompts agent))
    (setf (ai::agent-busy agent) nil)
    (ai::agent-next agent)
    (is equal '("one") results)))
