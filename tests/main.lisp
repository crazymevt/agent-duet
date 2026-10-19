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

(define-test claude-question
  :parent agent-integration
  (let* ((event (first (ai::parse-claude-event
                        "{\"type\":\"control_request\",\"request_id\":\"q1\",\"request\":{\"subtype\":\"can_use_tool\",\"tool_name\":\"AskUserQuestion\",\"input\":{\"questions\":[{\"question\":\"Which database?\",\"header\":\"DB\",\"options\":[{\"label\":\"Postgres\",\"description\":\"Relational\"},{\"label\":\"SQLite\",\"description\":\"Embedded\"}],\"multiSelect\":false}]}}}")))
         (input (ai::event-input event))
         (answer (ai:parse-json (ai::permission-response-line
                                 "q1" t :input (ai::question-answer-input input '(("Which database?" . "SQLite")))))))
    (is string= "AskUserQuestion" (ai::event-name event))
    (is string= "Which database?" (ai:jget (first (ai::jlist (ai:jget input "questions"))) "question"))
    ;; The answers go back with the questions, keyed by question text.
    (is string= "allow" (ai:jget answer "response" "response" "behavior"))
    (is string= "SQLite" (ai:jget answer "response" "response" "updatedInput" "answers" "Which database?"))
    (is string= "DB" (ai:jget answer "response" "response" "updatedInput" "questions" 0 "header"))))

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
  ;; A quote ends where an unquoted line starts another block...
  (is equal '(:quote :heading) (mapcar #'first (ai::markdown-blocks (format nil "> quoted~%# Outside"))))
  (is equal '(:quote :list) (mapcar #'first (ai::markdown-blocks (format nil "> quoted~%- item"))))
  (is equal '(:quote :code) (mapcar #'first (ai::markdown-blocks (format nil "> quoted~%```~%x~%```"))))
  ;; ...but a plain line carries on its paragraph.
  (let ((blocks (ai::markdown-blocks (format nil "> one~%two~%~%after"))))
    (is equal '(:quote :paragraph) (mapcar #'first blocks))
    (is string= "one two" (car (second (first (second (first blocks)))))))
  ;; Not after a heading inside the quote.
  (is equal '(:quote :paragraph) (mapcar #'first (ai::markdown-blocks (format nil "> # Title~%text"))))
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
  (is eq :changes (ai:review-verdict (format nil "Not yet.~%VERDICT: NOT APPROVED")))
  (is eq :changes (ai:review-verdict (format nil "VERDICT: APPROVED WITH CHANGES")))
  (is eq :approved (ai:review-verdict (format nil "VERDICT: APPROVED.")))
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
  (let* ((reply (if (functionp (fake-replies agent))
                    (fake-replies agent)
                    (pop (fake-replies agent))))
         (text (if reply (funcall reply prompt) "out of replies")))
    ;; A reply of :out-of-usage plays the account running out.
    (if (eq text :out-of-usage)
        (ai:agent-turn-finished agent "You've hit your limit" :out-of-usage)
        (ai:agent-turn-finished agent text (null reply)))))

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

(defclass hanging-agent (fake-agent) ()
  (:documentation "Starts turns and never finishes them, until cancelled."))

(defmethod ai:agent-begin-turn ((agent hanging-agent) prompt)
  (push prompt (fake-prompts agent)))

(define-test cancel-ends-workflow-step
  :parent agent-integration
  ;; Stopping an agent mid-workflow (its Stop or New button, or a folder
  ;; change) must not leave the workflow running with nobody working.
  (let* ((claude (make-instance 'hanging-agent :id :claude :name "Claude" :replies '()))
         (codex (make-instance 'fake-agent :id :codex :name "ChatGPT" :replies '()))
         (workflow (make-instance 'ai:workflow :mode :review :lead claude :partner codex :request "x")))
    (ai:start-workflow workflow)
    (is eq :running (ai:workflow-state workflow))
    (ai:agent-cancel claude)
    (is eq :finished (ai:workflow-state workflow))
    (is eq :failed (ai::loop-state (first (ai:workflow-loops workflow))))))

(define-test cancel-ends-queued-steps
  :parent agent-integration
  ;; Queued prompts are workflow steps too.
  (let* ((agent (make-instance 'hanging-agent :id :claude :name "Claude" :replies '()))
         (results '()))
    (ai:agent-submit agent "one" (lambda (text error) (push (list :one text error) results)))
    (ai:agent-submit agent "two" (lambda (text error) (push (list :two text error) results)))
    (ai:agent-cancel agent)
    (is = 2 (length results))
    (true (every #'third results))
    (false (ai::agent-queue agent))))

;;; Running out of usage

(define-test usage-windows
  :parent agent-integration
  (let* ((info (ai:parse-json "{\"status\":\"allowed\",\"unifiedWindows\":{\"five_hour\":{\"utilization\":0.56,\"resetsAt\":1792443000},\"seven_day\":{\"utilization\":0.27,\"resetsAt\":1792947600}}}"))
         (windows (ai::claude-usage-windows info)))
    (is string= "44% 5h / 73% 7d" (ai::usage-summary windows))
    (false (ai::usage-low-p windows))
    (false (ai::exhausted-reset windows)))
  (let* ((info (ai:parse-json "{\"status\":\"rejected\",\"unifiedWindows\":{\"five_hour\":{\"utilization\":1.0,\"resetsAt\":1792443000},\"seven_day\":{\"utilization\":0.95,\"resetsAt\":1792947600}}}"))
         (windows (ai::claude-usage-windows info)))
    (true (ai::usage-low-p windows))
    (is = (ai::unix-to-universal 1792443000) (ai::exhausted-reset windows)))
  (let ((buckets (ai::codex-rate-limits-from-response
                  (ai:parse-json "{\"rateLimits\":{\"primary\":{\"usedPercent\":100,\"windowDurationMins\":300,\"resetsAt\":1792443000},\"secondary\":{\"usedPercent\":40,\"windowDurationMins\":10080,\"resetsAt\":1792947600}}}"))))
    (is = (ai::unix-to-universal 1792443000) (ai::exhausted-reset (ai::codex-usage-windows buckets))))
  (true (ai::codex-limit-error-p (ai:parse-json "{\"status\":\"failed\",\"error\":{\"message\":\"x\",\"codexErrorInfo\":\"usageLimitExceeded\"}}")))
  (false (ai::codex-limit-error-p (ai:parse-json "{\"status\":\"failed\",\"error\":{\"message\":\"x\",\"codexErrorInfo\":\"badRequest\"}}")))
  (true (ai::claude-limit-text-p "You've hit your limit · resets 3pm"))
  (false (ai::claude-limit-text-p "Error: file not found"))
  (is = (ai::unix-to-universal 1792443000) (ai::claude-limit-text-reset "Claude AI usage limit reached|1792443000"))
  (let ((noon (encode-universal-time 0 5 15 19 10 2026)))
    (is string= "3:05 PM" (ai::format-reset-time noon noon))
    (true (search "3:05 PM" (ai::format-reset-time noon (+ noon (* 2 86400)))))))

(define-test usage-exact-percentages
  :parent agent-integration
  ;; 99.6% of the week used is not the week used up: only the five-hour
  ;; window is, so it's that window's reset that counts.
  (let ((windows (ai::claude-usage-windows
                  (ai:parse-json "{\"unifiedWindows\":{\"five_hour\":{\"utilization\":1.0,\"resetsAt\":1792443000},\"seven_day\":{\"utilization\":0.996,\"resetsAt\":1792947600}}}"))))
    (is = (ai::unix-to-universal 1792443000) (ai::exhausted-reset windows))
    (is string= "0% 5h / <1% 7d" (ai::usage-summary windows)))
  (is string= "71% 5h" (ai::usage-summary (ai::claude-usage-windows
                                           (ai:parse-json "{\"unifiedWindows\":{\"five_hour\":{\"utilization\":0.29}}}")))))

(defun claude-result (agent text error)
  "Feed AGENT (a Claude agent, busy with a turn) the turn's result event."
  (setf (ai::agent-busy agent) t)
  (ai::claude-agent-event agent nil (ai::make-claude-event :kind :result :text text :error error)))

(define-test claude-limit-evidence
  :parent agent-integration
  (let ((agent (make-instance 'ai:claude-agent))
        (errors '()))
    (flet ((turn (text)
             (setf (ai::agent-callback agent) (lambda (tx e) (declare (ignore tx)) (push e errors)))
             (claude-result agent text t)
             (first errors)))
      ;; A new limit replaces an expired reset time.
      (setf (ai::agent-out-until agent) (- (get-universal-time) 3600))
      (is eq :out-of-usage (turn "Claude AI usage limit reached|1792443000"))
      (is = (ai::unix-to-universal 1792443000) (ai::agent-out-until agent))
      ;; A limit left over as :unknown doesn't make a sign-in failure one.
      (setf (ai::agent-out-until agent) :unknown)
      (is eq t (turn "Invalid API key · Please run /login"))
      ;; This turn's rejected rate-limit event is evidence, even with plain text.
      (ai::claude-agent-event agent nil (ai::make-claude-event
                                         :kind :rate-limit
                                         :data (ai:parse-json "{\"status\":\"rejected\",\"resetsAt\":1792443000}")))
      (is eq :out-of-usage (turn "Error"))
      ;; ...for that turn only.
      (is eq t (turn "Error")))))

(defun run-pausing (mode claude-replies codex-replies)
  "Like RUN-FAKE, also collecting the paused steps."
  (let* ((claude (make-instance 'fake-agent :id :claude :name "Claude" :replies claude-replies))
         (codex (make-instance 'fake-agent :id :codex :name "ChatGPT" :replies codex-replies))
         (paused '())
         (notes '())
         (workflow (make-instance 'ai:workflow :mode mode :lead claude :partner codex :request "Add frobnicate"
                                               :on-message (lambda (from to text kind)
                                                             (declare (ignore to))
                                                             (when (eq from :system) (push (list kind text) notes)))
                                               :on-pause (lambda (w step) (declare (ignore w)) (push step paused)))))
    (ai:start-workflow workflow)
    (values workflow paused claude codex notes)))

(define-test reviewer-out-then-unreviewed
  :parent agent-integration
  (multiple-value-bind (workflow paused claude codex)
      (run-pausing :review (list (says "Wrote it")) (list (says :out-of-usage)))
    (declare (ignore claude codex))
    (is eq :running (ai:workflow-state workflow))
    (is = 1 (length paused))
    (is eq :review (ai::step-role (first paused)))
    (is string= "ChatGPT's review of Claude's work" (ai::step-description (first paused)))
    (ai::hand-off-step workflow (first paused))
    (is eq :finished (ai:workflow-state workflow))
    (is eq :unreviewed (ai::loop-state (first (ai:workflow-loops workflow))))
    ;; A second click does nothing.
    (ai::hand-off-step workflow (first paused))
    (is eq :finished (ai:workflow-state workflow))))

(define-test out-then-resumed
  :parent agent-integration
  (multiple-value-bind (workflow paused)
      (run-pausing :review (list (says "Wrote it"))
                   (list (says :out-of-usage) (says (format nil "Fine.~%VERDICT: APPROVED"))))
    (ai::resume-step workflow (first paused))
    (is eq :finished (ai:workflow-state workflow))
    (is eq :approved (ai::loop-state (first (ai:workflow-loops workflow))))))

(define-test author-out-then-taken-over
  :parent agent-integration
  (multiple-value-bind (workflow paused claude codex)
      (run-pausing :review (list (says :out-of-usage)) (list (says "Took it over and wrote it")))
    (declare (ignore claude))
    (is eq :work (ai::step-role (first paused)))
    (true (ai::can-hand-off-p workflow (first paused)))
    (ai::hand-off-step workflow (first paused))
    (true (search "taking it over" (first (fake-prompts codex))))
    (true (search "Add frobnicate" (first (fake-prompts codex))))
    (is eq :finished (ai:workflow-state workflow))
    (let ((loop (first (ai:workflow-loops workflow))))
      (is eq :unreviewed (ai::loop-state loop))
      (is eq codex (ai::loop-author loop)))))

(define-test plan-out-then-taken-over
  :parent agent-integration
  (multiple-value-bind (workflow paused claude codex)
      (run-pausing :divide (list (says :out-of-usage)) (list (says "Did the whole thing")))
    (declare (ignore claude))
    (is eq :plan (ai::step-role (first paused)))
    (ai::hand-off-step workflow (first paused))
    (true (search "do all of it yourself" (first (fake-prompts codex))))
    (is eq :finished (ai:workflow-state workflow))
    (is eq :unreviewed (ai::loop-state (first (ai:workflow-loops workflow))))))

(define-test stop-while-paused
  :parent agent-integration
  (multiple-value-bind (workflow paused)
      (run-pausing :review (list (says "Wrote it")) (list (says :out-of-usage)))
    (ai:stop-workflow workflow)
    (is eq :stopped (ai:workflow-state workflow))
    (false (ai::workflow-paused workflow))
    ;; Choices on the old card do nothing now.
    (ai::resume-step workflow (first paused))
    (is eq :stopped (ai:workflow-state workflow))))

(define-test out-agent-turns-prompts-away
  :parent agent-integration
  (let ((agent (make-instance 'fake-agent :id :codex :name "ChatGPT" :replies (list (says "should not run"))))
        (result nil))
    (ai::agent-ran-out agent (+ (get-universal-time) 3600))
    (true (ai::agent-out-of-usage-p agent))
    (ai:agent-submit agent "work" (lambda (text error) (setf result (list text error))))
    (is eq :out-of-usage (second result))
    (true (search "out of usage until" (first result)))
    (false (fake-prompts agent))
    ;; Unknown reset: it still tries (it may have reset).
    (ai::agent-ran-out agent nil)
    (false (ai::agent-out-of-usage-p agent))
    ;; A successful turn clears it.
    (ai:agent-submit agent "work" (lambda (text error) (declare (ignore text error))))
    (false (ai::agent-out-until agent))))

;;; Codex against a fake app-server (tests/fake-codex, which needs python3)

(defun fake-codex-program ()
  (namestring (asdf:system-relative-pathname "agent-integration" "tests/fake-codex")))

(defun wait-until (predicate &optional (seconds 10))
  (loop repeat (* seconds 50)
        until (funcall predicate)
        do (sleep 0.02))
  (funcall predicate))

(defmacro with-fake-codex ((agent directory) &body body)
  `(let* ((,directory (merge-pathnames (format nil "duet-codex-~d/" (random 1000000)) (uiop:temporary-directory)))
          (,agent (make-instance 'ai:codex-agent :program (fake-codex-program) :directory ,directory
                                                 :permission :edits)))
     (ensure-directories-exist ,directory)
     (unwind-protect (progn ,@body)
       (ignore-errors (ai:agent-shutdown ,agent))
       (uiop:delete-directory-tree ,directory :validate t :if-does-not-exist :ignore))))

(defun touch (directory name)
  (with-open-file (out (merge-pathnames name directory) :direction :output :if-exists :supersede)
    (write-line "" out)))

(defun fake-log (directory)
  (let ((file (merge-pathnames "log" directory)))
    (if (probe-file file) (uiop:read-file-lines file) '())))

(define-test codex-stop-while-starting
  :parent agent-integration
  (if (null (ignore-errors (uiop:run-program '("python3" "--version") :output :string)))
      (skip "python3 is needed for the fake Codex" (true t))
      (with-fake-codex (agent directory)
        (let ((result nil))
          ;; Hold the app-server in initialize, stop the turn, then let it go.
          (touch directory "hold")
          (ai:agent-submit agent "do it" (lambda (text error) (setf result (list text error))))
          (true (wait-until (lambda () (member "initialize" (fake-log directory) :test #'string=))))
          (ai:agent-cancel agent)
          (delete-file (merge-pathnames "hold" directory))
          (true (wait-until (lambda () (not (ai:agent-busy agent)))))
          (sleep 0.3)
          (false (member "turn/start" (fake-log directory) :test #'string=))
          (true (second result))))))

(define-test codex-retry-after-failed-start
  :parent agent-integration
  (if (null (ignore-errors (uiop:run-program '("python3" "--version") :output :string)))
      (skip "python3 is needed for the fake Codex" (true t))
      (with-fake-codex (agent directory)
        (let ((results '()))
          (touch directory "fail-thread-once")
          (ai:agent-submit agent "first" (lambda (text error) (push (list text error) results)))
          (true (wait-until (lambda () (= 1 (length results)))))
          (true (second (first results)))       ; failed
          (ai:agent-submit agent "second" (lambda (text error) (push (list text error) results)))
          (true (wait-until (lambda () (= 2 (length results)))))
          (is equal '("hi" nil) (first results))
          (is = 2 (count "thread/start" (fake-log directory) :test #'string=))))))

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
