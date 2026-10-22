;;;; Task measurements: no GTK or authenticated CLIs.
(in-package #:agent-duet/tests)

(define-test summary-metrics-lifecycle :parent agent-duet
  (let* ((lead (make-instance 'fake-agent :id :claude :name "Claude" :replies (list (says :hang))))
         (partner (make-instance 'fake-agent :id :codex :name "ChatGPT" :replies (list (says :hang))))
         (workflow (make-instance 'ai:workflow :lead lead :partner partner :request "Measure"))
         (step (ai::make-step :agent lead :role :work)))
    (ai::workflow-agent-submit workflow step lead "work" (lambda (&rest ignored) (declare (ignore ignored))))
    (ai::agent-add-metrics lead '(:cost-usd 0.2d0 :input-tokens 10 :output-tokens 3) :id "first")
    (ai::agent-add-metrics lead '(:cost-usd 99 :input-tokens 1000) :id "first")
    (ai::agent-add-metrics lead '(:cost-usd 0.1d0 :input-tokens 5) :id "second")
    (ai:agent-turn-finished lead "done" nil)
    (let* ((totals (first (ai::task-metric-totals workflow)))
           (values (getf totals :values)))
      (is = 1 (getf totals :turns))
      (is = 15 (second (assoc :input-tokens values)))
      (is = 3 (second (assoc :output-tokens values)))
      (is = 1 (third (assoc :cost-usd values))))
    ;; An interrupted attempt is published once, even if a late result follows.
    (setf (fake-replies lead) (list (says :hang)))
    (ai::workflow-agent-submit workflow step lead "stop" nil)
    (ai::agent-add-metrics lead '(:input-tokens 7) :id "third")
    (ai:agent-cancel lead)
    (ai:agent-turn-finished lead "late" t)
    (is = 2 (length (ai::workflow-metrics workflow)))
    ;; The other agent is independent; latest Codex snapshots replace, not sum.
    (ai::workflow-agent-submit workflow step partner "parallel" nil)
    (ai::agent-add-metrics partner '(:input-tokens 50 :output-tokens 4) :replace t)
    (ai::agent-add-metrics partner '(:input-tokens 60 :output-tokens 6) :replace t)
    (ai:agent-turn-finished partner "done" nil)
    (is = 60 (second (assoc :input-tokens (getf (second (ai::task-metric-totals workflow)) :values))))
    (setf (ai:workflow-state workflow) :stopped)
    (ai::capture-workflow-completion workflow)
    (let ((lines (ai::task-summary-lines (ai::workflow-completion-data workflow))))
      (true (some (lambda (line) (search "partial" line)) lines))
      (true (some (lambda (line) (search "cost unknown" (string-downcase line))) lines)))
    ;; Restart preserves raw turn records, including unknown interrupted work.
    (let* ((checkpoint (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow))))
           (restored (ai:restore-workflow checkpoint :lead lead :partner partner)))
      (is equalp (ai::task-metric-totals workflow) (ai::task-metric-totals restored))
      (is = (ai::workflow-started-at workflow) (ai::workflow-started-at restored))
      (is = (ai::workflow-ended-at workflow) (ai::workflow-ended-at restored)))))

(define-test summary-unknown-after-restart :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (let ((workflow (presented-workflow lead partner)))
      (setf (fake-replies lead) (list (says :hang)))
      (ai::workflow-agent-submit workflow (ai::make-step :agent lead :role :work) lead "hang" nil)
      (let* ((cp (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow))))
             (restored (ai:restore-workflow cp :lead lead :partner partner)))
        (is = 1 (getf (first (ai::task-metric-totals restored)) :turns))
        (false (second (assoc :cost-usd (getf (first (ai::task-metric-totals restored)) :values)))))
      (ai:agent-cancel lead)
      (ai:agent-turn-finished lead "stopped" t))))

(define-test summary-codex-notification :parent agent-duet
  (let* ((events nil)
         (server (ai::make-codex-app-server :deliver #'funcall :on-event (lambda (event) (push event events)))))
    (ai::codex-handle-notification server
                                 (ai:jobj "method" "thread/tokenUsage/updated"
                                          "params" (ai:jobj "threadId" "t" "turnId" "u"
                                                            "tokenUsage" (ai:jobj "last" (ai:jobj "inputTokens" 42)))))
    (is eq :token-usage (ai::codex-event-kind (first events)))
    (is equal "u" (ai::codex-event-turn-id (first events)))
    (is = 42 (ai:jget (ai::codex-event-data (first events)) "last" "inputTokens"))))

(define-test summary-real-agent-events :parent agent-duet
  (let* ((agent (make-instance 'ai:claude-agent))
         (cp (ai::make-claude-process)) (reports nil))
    (setf (ai::claude-agent-process agent) cp
          (ai::agent-busy agent) t
          (ai::agent-turn-token agent) (list t)
          (ai::agent-metrics-start agent) (ai::clock-seconds)
          (ai::agent-metrics-reporter agent) (lambda (id values) (declare (ignore id)) (push values reports))
          (ai::claude-agent-prompts agent) '("one" "two"))
    (flet ((result (index)
             (first (ai::parse-claude-event
                     (ai:json-string (ai:jobj "type" "result" "subtype" "success" "result" "done"
                                              "result_index" index "duration_ms" 100 "total_cost_usd" (* (1+ index) 0.02d0)
                                              "usage" (ai:jobj "input_tokens" 12 "output_tokens" 4)))))))
      (ai::claude-agent-event agent cp (result 0))
      (ai::claude-agent-event agent cp (result 0))
      (false reports)
      (ai::claude-agent-event agent cp (result 1))
      (is = 1 (length reports))
      (is = 24 (getf (first reports) :input-tokens))
      (is = 200 (getf (first reports) :duration-ms))
      (is = 0.04d0 (getf (first reports) :cost-usd))
      ;; A duplicate result from the preceding turn cannot enter new totals.
      (setf (ai::agent-busy agent) t (ai::agent-metrics agent) nil
            (ai::agent-metrics-seen agent) nil (ai::claude-agent-turn-result-start agent) 2
            (ai::claude-agent-prompts agent) '("next"))
      (ai::claude-agent-event agent cp (result 1))
      (false (ai::agent-metrics agent))
      (ai::claude-agent-event agent cp (result 2))
      (is = 12 (getf (ai::agent-metrics agent) :input-tokens))))
  (let* ((agent (make-instance 'ai:codex-agent))
         (server (ai::make-codex-app-server :turn-id "active" :thread-id "thread")))
    (setf (ai::codex-agent-server agent) server (ai::codex-agent-thread-id agent) "thread"
          (ai::agent-busy agent) t (ai::agent-metrics-start agent) (ai::clock-seconds))
    (dolist (id '("stale" "active" "active"))
      (ai::codex-agent-event agent server
                            (ai::make-codex-event :kind :token-usage :turn-id id :thread-id "thread"
                                                 :data (ai:jobj "last" (ai:jobj "inputTokens" 30 "outputTokens" 8)
                                                                "total" (ai:jobj "inputTokens" 130 "outputTokens" 28)))))
    (is = 30 (getf (ai::agent-metrics agent) :input-tokens))
    (ai::codex-agent-event agent server
                          (ai::make-codex-event :kind :token-usage :turn-id "active" :thread-id "thread"
                                               :data (ai:jobj "last" (ai:jobj "inputTokens" 40 "outputTokens" 5 "totalTokens" 45)
                                                              "total" (ai:jobj "inputTokens" 170 "outputTokens" 33))))
    (is = 45 (ai::agent-context-tokens agent))
    (is = 70 (getf (ai::agent-metrics agent) :input-tokens))
    (is = 13 (getf (ai::agent-metrics agent) :output-tokens))
    (false (getf (ai::agent-metrics agent) :cost-usd))))

(define-test summary-retry-attempts :parent agent-duet
  (let* ((lead (make-instance 'fake-agent :id :claude :name "Claude"
                              :replies (list (says :hang) (says :hang))))
         (partner (make-instance 'fake-agent :id :codex :name "ChatGPT" :replies nil))
         (workflow (make-instance 'ai:workflow :lead lead :partner partner :request "Retry")))
    (ai::workflow-agent-submit workflow (ai::make-step :agent lead :role :work) lead "try" nil)
    (ai::agent-add-metrics lead '(:input-tokens 10))
    (true (ai::agent-retry-turn lead))
    (ai:agent-turn-finished lead "interrupted" t)
    (true (ai:agent-busy lead))
    (ai::agent-add-metrics lead '(:input-tokens 20))
    (ai:agent-turn-finished lead "done" nil)
    (is = 2 (length (ai::workflow-metrics workflow)))
    (let ((totals (first (ai::task-metric-totals workflow))))
      (is = 30 (second (assoc :input-tokens (getf totals :values))))
      (true (getf totals :incomplete-p)))))

(define-test summary-handoff :parent agent-duet
  (multiple-value-bind (workflow paused lead partner)
      (run-pausing :compare (list (says :out-of-usage)) (list (says "Theirs") (says "Did it all")))
    (declare (ignore partner))
    (ai::agent-ran-out lead nil)
    (ai::hand-off-step workflow (first paused))
    (is eq :finished (ai:workflow-state workflow))
    (let ((totals (getf (ai::workflow-completion-data workflow) :metrics)))
      (is = 1 (getf (first totals) :turns))
      (is = 2 (getf (second totals) :turns))
      (true (getf (first totals) :incomplete-p))
      (false (second (assoc :cost-usd (getf (first totals) :values)))))))

(define-test summary-review-corrections :parent agent-duet
  (multiple-value-bind (lead partner) (presented-agents)
    (let* ((workflow (presented-workflow lead partner))
           (lines (ai::task-summary-lines (list :metrics (ai::task-metric-totals workflow)))))
      (true (some (lambda (line) (search "Uncached input tokens" line)) lines))
      (true (some (lambda (line) (search "Input tokens (including cache)" line)) lines))))
  (let* ((files (loop for n from 1 to 30 collect (format nil "file-~d.lisp" n)))
         (text (ai::summary-files-text files t)))
    (true (search "file-10.lisp" text))
    (false (search "file-11.lisp" text))
    (true (search "and 20 more" text))
    (true (search "Changes" text)))
  (let ((agent (make-instance 'ai:claude-agent)))
    (is = 0.25d0 (ai::claude-result-cost agent 0.25d0 0))
    (setf (ai::claude-agent-next-result-index agent) 1)
    ;; Results 1 and 2 were never delivered. Do not attribute their cost to 3.
    (false (ai::claude-result-cost agent 1d0 3))
    (setf (ai::claude-agent-next-result-index agent) 4)
    (is = 0.25d0 (ai::claude-result-cost agent 1.25d0 4))
    (setf (ai::claude-agent-next-result-index agent) 5)
    (false (ai::claude-result-cost agent nil 5))
    (setf (ai::claude-agent-next-result-index agent) 6)
    (false (ai::claude-result-cost agent 1.75d0 6))
    (setf (ai::claude-agent-next-result-index agent) 7)
    (is = 0.25d0 (ai::claude-result-cost agent 2d0 7))))

(define-test prompt-size-stage-metrics :parent agent-duet
  (let* ((author (make-instance 'fake-agent :id :claude :name "Claude" :replies (list (says "Done"))))
         (reviewer (make-instance 'fake-agent :id :codex :name "ChatGPT"))
         (workflow (make-instance 'ai:workflow :lead author :partner reviewer :request "Measure prompts"))
         (step (ai::make-step :agent author :role :work)))
    (ai::workflow-agent-submit workflow step author "12345" nil)
    (is = (length (first (fake-prompts author))) (second (assoc :prompt-characters
                           (getf (first (ai::task-metric-totals workflow :work)) :values))))
    (is = 0 (getf (first (ai::task-metric-totals workflow :review)) :turns))
    (let* ((cp (ai::json-copy (ai:workflow-checkpoint workflow)))
           (restored (ai:restore-workflow cp :lead author :partner reviewer)))
      (is equalp (ai::task-metric-totals workflow :work)
          (ai::task-metric-totals restored :work)))))

(define-test agent-context-usage-and-clear :parent agent-duet
  (let* ((agent (make-instance 'ai:claude-agent))
         (cp (ai::make-claude-process)))
    (setf (ai::claude-agent-process agent) cp)
    (dolist (event (ai::parse-claude-event
                    "{\"type\":\"stream_event\",\"event\":{\"type\":\"message_start\",\"message\":{\"usage\":{\"input_tokens\":12,\"cache_read_input_tokens\":100,\"cache_creation_input_tokens\":8}}}}"))
      (ai::claude-agent-event agent cp event))
    (is = 120 (ai::agent-context-tokens agent))
    (setf (ai::claude-agent-process agent) nil
          (ai::claude-agent-started agent) t
          (ai::claude-agent-session-id agent) "old")
    (ai::agent-new-conversation agent)
    (false (ai::agent-conversation-handle agent))
    (false (ai::agent-context-tokens agent)))
  (let ((agent (make-instance 'ai:codex-agent)))
    (setf (ai::codex-agent-thread-id agent) "keep"
          (ai::agent-context-fallback agent) '(("compact" . "full")))
    (ai::agent-new-conversation agent)
    (is equal '(("compact" . "full")) (ai::agent-context-fallback agent))
    (false (ai::agent-conversation-handle agent))))

(define-test summary-efficiency-feedback :parent agent-duet
  (let ((lines (ai::task-summary-lines
                (list :elapsed-seconds 60 :open-findings 2 :open-suggestions 1 :unanswered-questions 1
                      :efficiency (list :blocked-seconds 30 :repeated-checks 1 :recurring-findings 0
                                        :integration-reworks 2)))))
    (true (member "Open findings: 2 (suggestions: 1, which don't stop approval)" lines :test #'string=))
    (true (member "Unanswered questions: 1" lines :test #'string=))
    (true (some (lambda (line) (search "Repeated checks" line)) lines))
    (true (some (lambda (line) (search "Integration reworks" line)) lines))
    ;; Zero values are left out; defects (open findings) come first.
    (false (some (lambda (line) (search "Recurring findings" line)) lines))
    (is string= "Open findings: 1" (second (ai::task-summary-lines (list :open-findings 1))))))
