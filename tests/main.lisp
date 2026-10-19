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

;;; Approvals

(define-test claude-permission
  :parent agent-integration
  (let ((event (first (ai::parse-claude-event
                       "{\"type\":\"control_request\",\"request_id\":\"r1\",\"request\":{\"subtype\":\"can_use_tool\",\"tool_name\":\"Bash\",\"input\":{\"command\":\"make test\"},\"description\":\"Run the tests\",\"permission_suggestions\":[{\"type\":\"addRules\",\"rules\":[{\"toolName\":\"Bash\",\"ruleContent\":\"make test\"}],\"behavior\":\"allow\",\"destination\":\"localSettings\"}]}}"))))
    (is eq :permission (ai::event-kind event))
    (is string= "r1" (ai::event-id event))
    (is string= "Run the tests" (ai::event-text event))
    (is string= "$ make test" (ai::claude-approval-title "Bash" (ai::event-input event)))
    ;; Allowing always adds the suggested rules for this session only.
    (let ((answer (ai:parse-json (ai::permission-response-line
                                  "r1" t :input (ai::event-input event)
                                  :always-rules (ai::jlist (ai:jget (ai::event-data event) "permission_suggestions"))))))
      (is string= "allow" (ai:jget answer "response" "response" "behavior"))
      (is string= "make test" (ai:jget answer "response" "response" "updatedInput" "command"))
      (is string= "session" (ai:jget answer "response" "response" "updatedPermissions" 0 "destination")))
    (let ((answer (ai:parse-json (ai::permission-response-line "r1" nil))))
      (is string= "deny" (ai:jget answer "response" "response" "behavior"))
      (false (ai:jget answer "response" "response" "updatedPermissions")))))

(define-test approvals
  :parent agent-integration
  (let* ((seen '())
         (answers '())
         (agent (make-instance 'fake-agent :id :claude :name "Claude" :replies '())))
    (setf (ai:agent-listener agent) (lambda (a kind &rest plist) (declare (ignore a)) (push (cons kind plist) seen)))
    (ai::agent-ask-approval agent "a1" "$ make test" nil (lambda (d) (push d answers)))
    (ai::agent-ask-approval agent "a2" "$ rm -rf build" nil (lambda (d) (push d answers)))
    (is eq :approval (car (second seen)))
    (ai::agent-answer-approval agent "a1" :always)
    (is equal '(:always) answers)
    ;; Answered once only.
    (ai::agent-answer-approval agent "a1" :allow)
    (is equal '(:always) answers)
    ;; Cancelling denies what's still waiting.
    (ai:agent-cancel agent)
    (is equal '(:deny :always) answers)))

;;; Codex models and usage

(define-test codex-catalog
  :parent agent-integration
  (let ((model (ai:parse-json "{\"id\":\"gpt-x\",\"supportedReasoningEfforts\":[{\"reasoningEffort\":\"low\"},{\"reasoningEffort\":\"high\"}]}")))
    (is equal '("low" "high") (ai::codex-model-efforts model)))
  (let ((buckets (ai::codex-rate-limits-from-response
                  (ai:parse-json "{\"rateLimits\":{\"limitId\":\"codex\",\"primary\":{\"usedPercent\":28,\"windowDurationMins\":300},\"secondary\":{\"usedPercent\":60.4,\"windowDurationMins\":10080}}}"))))
    (is string= "72% 5h / 40% 7d" (ai::codex-usage-summary buckets))
    (let ((updated (ai::merge-rate-limits buckets (ai:parse-json "{\"limitId\":\"codex\",\"primary\":{\"usedPercent\":50,\"windowDurationMins\":300}}"))))
      (is = 1 (length updated))
      (is string= "50% 5h" (ai::codex-usage-summary updated)))))

;;; Settings

(define-test settings
  :parent agent-integration
  (let ((file (merge-pathnames (format nil "duet-settings-~d.lisp" (random 1000000)) (uiop:temporary-directory)))
        (ai::*settings* '()))
    (unwind-protect
         (progn
           (setf (getf ai::*settings* :theme) :dark
                 (getf ai::*settings* :recent-folders) '("/a/" "/b/"))
           (setf (getf ai::*settings* :directory) (coerce "/c/" 'base-string))
           (ai::save-settings file)
           (false (search "#A" (uiop:read-file-string file)))
           (setf ai::*settings* '())
           (ai::load-settings file)
           (is eq :dark (ai::setting :theme))
           (is equal '("/a/" "/b/") (ai::setting :recent-folders))
           (is equal "/c/" (ai::setting :directory)))
      (ignore-errors (delete-file file)))
    ;; Unreadable settings are no settings, not an error.
    (with-open-file (out file :direction :output :if-exists :supersede) (write-string "(:theme #.(oops" out))
    (unwind-protect (is equal '() (ai::load-settings file))
      (ignore-errors (delete-file file)))))

;;; Markdown

(define-test markdown-blocks
  :parent agent-integration
  (let ((blocks (ai::markdown-blocks (format nil "# Title~%~%Some *text*.~%~%- [x] done~%- [ ] open~%~%| a | b |~%|:-:|--:|~%| 1 | 2 |~%~%```py~%x = 1~%```"))))
    (is equal '(:heading :paragraph :list :table :code) (mapcar #'first blocks))
    (is equal '(:done :open) (mapcar #'first (fourth (third blocks))))
    (is equal '(:center :right) (second (fourth blocks)))
    (is string= "x = 1" (third (fifth blocks))))
  (is string= "risks-and-open-questions" (ai::heading-anchor "Risks and open questions?"))
  (is string= "  ab  " (ai::pad-cell "ab" 4 :center))
  (is string= " ab" (ai::pad-cell "ab" 1 :right))
  (is string= "ab " (ai::pad-cell "ab" 1 :left)))

(define-test markdown-files
  :parent agent-integration
  (let ((root (merge-pathnames (format nil "duet-md-~d/" (random 1000000)) (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (dolist (f '("README.md" "notes.txt" "docs/DESIGN.md" "docs/deep/x.markdown" ".hidden/no.md" "node_modules/p/no.md"))
             (let ((path (merge-pathnames f root)))
               (ensure-directories-exist path)
               (with-open-file (out path :direction :output :if-exists :supersede) (write-line "# x" out))))
           (is equal '("README.md" "docs/deep/x.markdown" "docs/DESIGN.md") (ai::markdown-files root)))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

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
