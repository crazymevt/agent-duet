;;;; tests/main.lisp

(defpackage #:agent-duet/tests
  (:use #:cl #:parachute)
  (:local-nicknames (#:ai #:agent-duet)))

(in-package #:agent-duet/tests)

(define-test agent-duet)

;;; JSON and the CLIs' events

(define-test json
    :parent agent-duet
  (let ((v (ai:parse-json "{\"a\": [1, 2.5, \"x\\n\"], \"b\": true, \"c\": null}")))
    (is equal 1 (ai:jget v "a" 0))
    (is = 2.5d0 (ai:jget v "a" 1))
    (is string= (format nil "x~%") (ai:jget v "a" 2))
    (is eq t (ai:jget v "b"))
    (is eq :null (ai:jget v "c")))
  (is string= "{\"k\":\"v\",\"n\":[1,2]}" (ai:json-string (ai:jobj "k" "v" "n" #(1 2)))))

(define-test claude-events
    :parent agent-duet
  (let ((events (ai::parse-claude-event
                 "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false,\"result\":\"Done.\",\"session_id\":\"s1\"}")))
    (is eq :result (ai::event-kind (first events)))
    (is string= "Done." (ai::event-text (first events)))
    (false (ai::event-error (first events))))
  (let ((events (ai::parse-claude-event
                 "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_delta\",\"delta\":{\"type\":\"text_delta\",\"text\":\"Hi\"}}}")))
    (is eq :text-delta (ai::event-kind (first events)))
    (is string= "Hi" (ai::event-text (first events)))))

(define-test claude-model-labels
    :parent agent-duet
  (is string= "Sonnet 5.5"
      (ai::claude-model-label
       (ai:jobj "value" "sonnet" "displayName" "Sonnet 5.5"
                "resolvedModel" "claude-sonnet-5-5"
                "description" "Most efficient for simpler tasks")))
  (is string= "Default (recommended) (claude-opus-5-5)"
      (ai::claude-model-label
       (ai:jobj "value" "default" "displayName" "Default (recommended)"
                "resolvedModel" "claude-opus-5-5"
                "description" "Opus 5.5 · Best for everyday, complex tasks")))
  (is string= "claude-sonnet-5-5"
      (ai::claude-model-label (ai:jobj "value" "sonnet" "resolvedModel" "claude-sonnet-5-5")))
  (is string= "Sonnet" (ai::claude-model-label "sonnet")))

(define-test claude-arguments
    :parent agent-duet
  (let ((args (ai::claude-arguments :session-id "s" :permission-mode "acceptEdits" :model "opus")))
    (true (search '("--permission-mode" "acceptEdits") args :test #'equal))
    (true (search '("--session-id" "s") args :test #'equal))
    (true (search '("--model" "opus") args :test #'equal))))

(define-test codex-items
    :parent agent-duet
  (is string= "$ make test"
      (ai::codex-item-summary (ai:parse-json "{\"type\":\"commandExecution\",\"command\":\"make test\"}")))
  (is string= "Edit a.lisp, b.lisp"
      (ai::codex-item-summary
       (ai:parse-json "{\"type\":\"fileChange\",\"changes\":[{\"path\":\"/x/a.lisp\"},{\"path\":\"/x/b.lisp\"}]}"))))

;;; Approvals

(define-test claude-permission
    :parent agent-duet
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
    :parent agent-duet
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
    :parent agent-duet
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
    :parent agent-duet
  (let ((model (ai:parse-json "{\"id\":\"gpt-x\",\"supportedReasoningEfforts\":[{\"reasoningEffort\":\"low\"},{\"reasoningEffort\":\"high\"}]}")))
    (is equal '("low" "high") (ai::codex-model-efforts model)))
  (let ((buckets (ai::codex-rate-limits-from-response
                  (ai:parse-json "{\"rateLimits\":{\"limitId\":\"codex\",\"primary\":{\"usedPercent\":28,\"windowDurationMins\":300},\"secondary\":{\"usedPercent\":60.4,\"windowDurationMins\":10080}}}"))))
    (is string= "72% 5h / 40% 7d" (ai::codex-usage-summary buckets))
    (let ((updated (ai::merge-rate-limits buckets (ai:parse-json "{\"limitId\":\"codex\",\"primary\":{\"usedPercent\":50,\"windowDurationMins\":300}}"))))
      (is = 1 (length updated))
      (is string= "50% 5h" (ai::codex-usage-summary updated)))))

(define-test restored-claude-model
    :parent agent-duet
  (dolist (model '("opus" "default" "claude-sonnet-5-5"))
    (true (member model (ai::claude-startup-models model) :test #'equal)))
  (is equal ai::*claude-models* (ai::claude-startup-models nil)))

;;; Settings

(define-test settings
    :parent agent-duet
  (let ((file (merge-pathnames (format nil "duet-settings-~d.lisp" (random 1000000)) (uiop:temporary-directory)))
        (ai::*settings* '()))
    (unwind-protect
        (progn
          (setf (getf ai::*settings* :theme) :dark
                (getf ai::*settings* :recent-folders) '("/a/" "/b/")
                (getf ai::*settings* :claude-model) "default"
                (getf ai::*settings* :claude-effort) "high"
                (getf ai::*settings* :codex-model) "gpt-5.4"
                (getf ai::*settings* :codex-effort) "xhigh")
          (setf (getf ai::*settings* :directory) (coerce "/c/" 'base-string))
          (ai::save-settings file)
          (false (search "#A" (uiop:read-file-string file)))
          (setf ai::*settings* '())
          (ai::load-settings file)
          (is eq :dark (ai::setting :theme))
          (is equal "default" (ai::setting :claude-model))
          (is equal "high" (ai::setting :claude-effort))
          (is equal "gpt-5.4" (ai::setting :codex-model))
          (is equal "xhigh" (ai::setting :codex-effort))
          (is equal '("/a/" "/b/") (ai::setting :recent-folders))
          (is equal "/c/" (ai::setting :directory)))
      (ignore-errors (delete-file file)))
    ;; Unreadable settings are no settings, not an error.
    (with-open-file (out file :direction :output :if-exists :supersede) (write-string "(:theme #.(oops" out))
    (unwind-protect (is equal '() (ai::load-settings file))
      (ignore-errors (delete-file file)))))

(defmacro finishes-in-time (&body body)
  "BODY's value, or :hung if it runs for more than five seconds."
  `(handler-case (sb-ext:with-timeout 5 ,@body)
     (sb-ext:timeout () :hung)))

(define-test settings-malformed-files
    :parent agent-duet
  ;; A file that reads but isn't a plist is no settings; read-time
  ;; evaluation stays off.
  (let ((file (merge-pathnames (format nil "duet-settings-~d.lisp" (random 1000000)) (uiop:temporary-directory)))
        (ai::*settings* '(:theme :dark)))
    (unwind-protect
         (dolist (text '("42" "\"settings\"" "(:theme :dark :view)" "(:theme :dark . :light)"
                         "#(:theme :dark)" "(:view #.(error \"evaluated\"))" ""
                         ;; Circular top-level lists, which the reader accepts.
                         "#1=(:theme :dark . #1#)" "#1=(:theme :dark :view :sidebar . #1#)"))
           (with-open-file (out file :direction :output :if-exists :supersede) (write-string text out))
           (is equal '() (finishes-in-time (ai::load-settings file)) "~s" text))
      (ignore-errors (delete-file file)))))

(define-test settings-circular-values
    :parent agent-duet
  ;; Circular or dotted values are dropped on load, keeping the other
  ;; settings, so reading, saving and updating them all finish.
  (let* ((config (merge-pathnames (format nil "duet-config-~d/" (random 1000000)) (uiop:temporary-directory)))
         (file (merge-pathnames "agent-duet/settings.lisp" config))
         (old (uiop:getenv "XDG_CONFIG_HOME"))
         (ai::*settings* '()))
    (setf (uiop:getenv "XDG_CONFIG_HOME") (namestring config))
    (unwind-protect
         (progn
           (ensure-directories-exist file)
           (with-open-file (out file :direction :output :if-exists :supersede)
             (write-string "(:sidebar-positions #1=(700 . #1#) :agents-left-positions (480 420)
                             :pane-positions #2=(300 900 . #2#) :view :agents-left
                             :recent-folders #3=(\"/a/\" . #3#) :check-commands ((\"/b/\" . \"make\") . #4=(#4#))
                             :workflow-mode (2 . 3) :shared-notes #5=#2A((#5#)) :theme :dark)" out))
           (is equal '(:agents-left-positions (480 420) :view :agents-left :theme :dark)
               (finishes-in-time (ai::load-settings file)))
           (false (finishes-in-time (ai::layout-positions :sidebar)))
           (is equal '(480 420) (finishes-in-time (ai::layout-positions :agents-left)))
           (false (finishes-in-time (ai::layout-positions :panes)))
           (is = 0 (ai::saved-choice-index :workflow-mode 4))
           (false (finishes-in-time (ai::folder-check-command "/b/")))
           (finishes-in-time (ai::note-recent-folder "/c/"))
           (is equal '("/c/") (ai::setting :recent-folders))
           (finishes-in-time (ai::load-settings file))
           (is equal '("/c/") (ai::setting :recent-folders))
           (is eq :dark (ai::setting :theme))
           (is equal '(480 420) (ai::layout-positions :agents-left)))
      (setf (uiop:getenv "XDG_CONFIG_HOME") (or old ""))
      (uiop:delete-directory-tree config :validate t :if-does-not-exist :ignore)))
  ;; Circular or dotted positions set in memory are ignored too, for each
  ;; layout, without disturbing the others.
  (let ((ai::*settings* '()))
    (dolist (case '((:sidebar :sidebar-positions) (:agents-left :agents-left-positions) (:panes :pane-positions)))
      (destructuring-bind (view key) case
        (dolist (value (list (let ((l (list 700 360))) (setf (cddr l) l) l)
                             (let ((l (list 700))) (setf (cdr l) l) l)
                             '(700 360 . 200)))
          (setf ai::*settings* (list key value :sidebar-positions '(950 400) :agents-left-positions '(480 420)
                                     :pane-positions '(300 900)))
          (false (finishes-in-time (ai::layout-positions view)) "~s" view)
          (dolist (other '((:sidebar (950 400)) (:agents-left (480 420)) (:panes (300 900))))
            (unless (eq (first other) view)
              (is equal (second other) (ai::layout-positions (first other))))))))
    ;; Malformed check commands and recent folders are skipped.
    (setf ai::*settings* (list :check-commands '(("/a/" . "make") 42 ("/b/" . 7) nil) :recent-folders '(3 "/x/")))
    (is equal "make" (ai::folder-check-command "/a/"))
    (false (ai::folder-check-command "/b/"))
    (is equal '(("/a/" . "make")) (ai::check-commands))))

(define-test layout-preferences
    :parent agent-duet
  (let ((ai::*settings* '()))
    (flet ((positions (view key value)
             (setf ai::*settings* (list key value))
             (ai::layout-positions view)))
      ;; Each layout reads its own positions.
      (is equal '(700 360) (positions :sidebar :sidebar-positions '(700 360)))
      (is equal '(300 900) (positions :panes :pane-positions '(300 900)))
      (setf ai::*settings* (list :sidebar-positions '(700 360) :pane-positions '(300 900)))
      (is equal '(700 360) (ai::layout-positions :sidebar))
      (is equal '(300 900) (ai::layout-positions :panes))
      ;; Missing, malformed or obsolete positions are ignored, not used.
      (dolist (value (list nil 700 "700 360" '(700) '(700 360 200) '(700 . 360) #(700 360)
                           '(700.5 360) '(-1 360) '(700 100001) '("700" 360) '(:left :right)))
        (false (positions :sidebar :sidebar-positions value) "~s" value)
        (false (positions :panes :pane-positions value) "~s" value)))
    ;; Dropdown choices fall back to the first when out of range or not a number.
    (dolist (case '((nil 0) (0 0) (3 3) (4 0) (-1 0) (2.0 0) ("2" 0) (:divide 0)))
      (setf ai::*settings* (list :workflow-mode (first case)))
      (is = (second case) (ai::saved-choice-index :workflow-mode 4) "~s" (first case)))
    (setf ai::*settings* '())
    (is = 0 (ai::saved-choice-index :workflow-lead 2))))

(define-test layout-preferences-agents-left
    :parent agent-duet
  ;; Agents on the left keep their own divider positions: switching between
  ;; the layouts never moves another's.
  (let ((ai::*settings* (list :sidebar-positions '(950 400) :agents-left-positions '(480 420)
                              :pane-positions '(300 900))))
    (is equal '(950 400) (ai::layout-positions :sidebar))
    (is equal '(480 420) (ai::layout-positions :agents-left))
    (is equal '(300 900) (ai::layout-positions :panes))
    (false (ai::layout-positions :conversation))
    (remf ai::*settings* :agents-left-positions)
    (false (ai::layout-positions :agents-left))
    (setf ai::*settings* (list :agents-left-positions '(480 . 420) :sidebar-positions '(950 400)))
    (false (ai::layout-positions :agents-left))
    (is equal '(950 400) (ai::layout-positions :sidebar))))

(define-test agent-panels-start-equal
    :parent agent-duet
  ;; EQUAL-AGENT-DIVIDER-POSITION places the inner divider within AVAILABLE,
  ;; the combined extent of its two children (excluding the handle). In Three
  ;; panes Claude's pane lies outside that split, with the given extent.
  (flet ((agents (view available &optional (claude 0))
           ;; The two agent panels' extents that the position produces.
           (let ((position (ai::equal-agent-divider-position view available claude)))
             (true (and (integerp position) (<= 0 position available)) "~s ~s" view position)
             (if (eq view :panes)
                 (list claude (- available position))
                 (list position (- available position))))))
    ;; Both sidebar layouts, wide (stacked agents) or narrow (side by side or
    ;; stacked), take the same split: at even, odd and degenerate extents.
    (dolist (view '(:sidebar :agents-left))
      (dolist (available '(820 821 1 0))
        (destructuring-bind (claude codex) (agents view available)
          (true (<= (abs (- claude codex)) 1) "~s ~d: ~d ~d" view available claude codex))))
    ;; Three panes: Codex matches Claude, the middle pane takes the rest,
    ;; including the first frame's estimate from a 1440 wide window.
    (dolist (case '((1008 432) (1009 300) (600 300) (300 300)))
      (destructuring-bind (available claude) case
        (is equal (list claude claude) (agents :panes available claude) "~s" case)))
    ;; Claude wider than the space left: no negative position.
    (agents :panes 200 432)))

(define-test agent-panel-positions-migrate-once
    :parent agent-duet
  ;; Legacy inner positions cannot distinguish defaults from drags.
  ;; Reset only those positions, and retain all outer divider widths.
  (let* ((config (merge-pathnames (format nil "duet-config-~d/" (random 1000000)) (uiop:temporary-directory)))
         (old (uiop:getenv "XDG_CONFIG_HOME"))
         (ai::*settings* (list :sidebar-positions '(950 400) :agents-left-positions '(480 420)
                               :pane-positions '(300 900) :view :panes :theme :dark)))
    (setf (uiop:getenv "XDG_CONFIG_HOME") (namestring config))
    (unwind-protect
         (progn
           (ai::migrate-agent-panel-positions)
           (dolist (view '(:sidebar :agents-left :panes))
             (is equal (list (case view (:sidebar 950) (:agents-left 480) (:panes 300)) nil)
                 (ai::layout-positions view) "~s" view))
           (is eq :panes (ai::setting :view))
           (is eq :dark (ai::setting :theme))
           ;; The migration is recorded on disk, so a restart keeps new drags.
           (setf ai::*settings* '())
           (ai::load-settings)
           (is eql 2 (ai::setting :equal-agent-panels-version))
           ;; Upgrade settings written by the first equal-sizing fix too.
           (setf (ai::setting :equal-agent-panels-version) 1
                 (ai::setting :sidebar-positions) '(950 400))
           (ai::migrate-agent-panel-positions)
           (is equal '(950 nil) (ai::layout-positions :sidebar))
           (setf (ai::setting :sidebar-positions) '(900 300)
                 (ai::setting :pane-positions) '(320 960))
           (ai::migrate-agent-panel-positions)
           (ai::load-settings)
           (ai::migrate-agent-panel-positions)
           (is equal '(900 300) (ai::layout-positions :sidebar))
           (is equal '(320 960) (ai::layout-positions :panes)))
      (setf (uiop:getenv "XDG_CONFIG_HOME") (or old ""))
      (uiop:delete-directory-tree config :validate t :if-does-not-exist :ignore))))

(define-test agent-divider-custom-geometry
    :parent agent-duet
  (let ((size '(1440 900 :sidebar)))
    (false (ai::agent-divider-custom-p 450 450 size size))
    (false (ai::agent-divider-custom-p 452 450 size size))
    (false (ai::agent-divider-custom-p 448 450 size size))
    (true (ai::agent-divider-custom-p 453 450 size size))
    (true (ai::agent-divider-custom-p 300 450 size size))
    ;; GTK may scale a default divider while the window changes size. It must
    ;; be equalized again, not saved as a deliberate split.
    (false (ai::agent-divider-custom-p 300 450 size '(1440 1000 :sidebar)))
    (false (ai::agent-divider-custom-p 300 450 nil size))))

(define-test default-agent-layout-survives-restart
    :parent agent-duet
  (let ((file (merge-pathnames (format nil "duet-agent-layout-~d.lisp" (random 1000000))
                              (uiop:temporary-directory)))
        (ai::*settings* nil))
    (unwind-protect
         (dolist (view '(:sidebar :agents-left :panes))
           ;; Closing, switching views or opening the checklist uses this same
           ;; preference helper. Automatic allocations must not become a saved
           ;; inner pixel position, even when the outer width is retained.
           (setf (getf ai::*settings* (ai::layout-position-key view))
                 (ai::agent-layout-preference '(600 700)
                   (ai::agent-divider-custom-p 700 700 '(1440 1400) '(1440 1400))))
           (ai::save-settings file)
           (setf ai::*settings* nil)
           (ai::load-settings file)
           (let ((saved (ai::layout-positions view)))
             (is = 600 (first saved))
             ;; The UI checks SECOND, not the presence of the outer position,
             ;; to schedule its second-frame equalization after restart.
             (false (second saved))
             (let ((available 820))
               (is = (if (eq view :panes) 520 410)
                   (ai::equal-agent-divider-position view available 300))))
           ;; A deliberate split still survives serialization and validation.
           (setf (getf ai::*settings* (ai::layout-position-key view))
                 (ai::agent-layout-preference '(600 250)
                   (ai::agent-divider-custom-p 250 700 '(1440 1400) '(1440 1400))))
           (ai::save-settings file)
           (ai::load-settings file)
           (is equal '(600 250) (ai::layout-positions view)))
      (when (probe-file file) (delete-file file)))))

(define-test layout-preferences-survive-restart
    :parent agent-duet
  (let ((file (merge-pathnames (format nil "duet-settings-~d.lisp" (random 1000000)) (uiop:temporary-directory)))
        (ai::*settings* '()))
    (unwind-protect
         (progn
           (setf ai::*settings* (list :view :agents-left :sidebar-positions '(820 410)
                                      :agents-left-positions '(500 380) :pane-positions '(320 960)
                                      :workflow-mode 2 :workflow-lead 1 :workflow-settings-expanded t
                                      :claude-transcript-expanded nil))
           (ai::save-settings file)
           (setf ai::*settings* '())
           (ai::load-settings file)
           (is eq :agents-left (ai::setting :view))
           (is equal '(820 410) (ai::layout-positions :sidebar))
           (is equal '(500 380) (ai::layout-positions :agents-left))
           (is equal '(320 960) (ai::layout-positions :panes))
           (is = 2 (ai::saved-choice-index :workflow-mode 4))
           (is = 1 (ai::saved-choice-index :workflow-lead 2))
           (true (ai::setting :workflow-settings-expanded))
           (false (ai::setting :claude-transcript-expanded t))
           ;; A hand-edited file with bad layout values still loads the rest.
           (with-open-file (out file :direction :output :if-exists :supersede)
             (write-string "(:view :sideways :sidebar-positions (1 2 3) :pane-positions \"x\" :workflow-mode 9 :theme :dark)" out))
           (ai::load-settings file)
           (is eq :dark (ai::setting :theme))
           (false (ai::layout-positions :sidebar))
           (false (ai::layout-positions :panes))
           (is = 0 (ai::saved-choice-index :workflow-mode 4)))
      (ignore-errors (delete-file file)))))

;;; Markdown

(define-test markdown-blocks
    :parent agent-duet
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
  ;; Nor after other blocks that aren't paragraphs.
  (dolist (text (list (format nil "> ```~%> x~%outside") (format nil ">     code~%outside")
                      (format nil "> title~%> ===~%outside") (format nil "> one~%>~%outside")
                      (format nil "> > one~%> >~%outside") (format nil "> | a |~%> |---|~%outside")))
    (let ((blocks (ai::markdown-blocks text)))
      (is equal '(:quote :paragraph) (mapcar #'first blocks))
      (is string= "outside" (car (second (second blocks))))))
  ;; A list item's paragraph inside the quote carries on too, over several lines.
  (let* ((blocks (ai::markdown-blocks (format nil "> - one~%two~%three")))
         (item (first (fourth (first (second (first blocks)))))))
    (is equal '(:quote) (mapcar #'first blocks))
    (is string= "one two three" (car (second (first (second item))))))
  ;; Lazy lines carry on a nested quote's paragraph, and the outer quote's.
  (let ((blocks (ai::markdown-blocks (format nil "> > one~%two~%> three~%four"))))
    (is equal '(:quote) (mapcar #'first blocks))
    (is equal '(:quote) (mapcar #'first (second (first blocks))))
    (is string= "one two three four" (car (second (first (second (first (second (first blocks)))))))))
  ;; Lines alternating with and without > make one paragraph.
  (let ((blocks (ai::markdown-blocks (with-output-to-string (o) (dotimes (k 2000) (format o "> a~%b~%"))))))
    (is = 1 (length blocks))
    (is = 1 (length (second (first blocks)))))
  (is string= "risks-and-open-questions" (ai::heading-anchor "Risks and open questions?"))
  (is string= "  ab  " (ai::pad-cell "ab" 4 :center))
  (is string= " ab" (ai::pad-cell "ab" 1 :right))
  (is string= "ab " (ai::pad-cell "ab" 1 :left)))

(define-test markdown-quote-linear-scanning :parent agent-duet
  ;; Quotes that end early must not read the remaining document each time.
  ;; Count source reads instead of elapsed time so the bound is deterministic.
  (dolist (pattern '("> # heading~%outside~%" "> > one~%> >~%outside~%"
                     "> ```~%> code~%outside~%"))
    (let* ((count 2000)
           (text (with-output-to-string (out)
                   (dotimes (k count) (format out pattern))))
           (lines (coerce (ai::split-text-lines text) 'vector))
           (reads 0)
           (blocks (ai::parse-line-blocks
                    (lambda (k) (incf reads) (aref lines k))
                    (lambda (k) (< k (length lines))) 0)))
      (is = (* 2 count) (length blocks))
      (true (loop for (quote paragraph) on blocks by #'cddr
                  for n from 0
                  always (and (eq :quote (first quote))
                              (eq :paragraph (first paragraph))
                              (string= "outside" (car (second paragraph)))
                              (= (* n (if (search "heading" pattern) 2 3))
                                 (car (last quote))))))
      (true (< reads (* 20 count))))))

(define-test markdown-files
    :parent agent-duet
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
    :parent agent-duet
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
    :parent agent-duet
  (multiple-value-bind (lead partner)
      (ai:parse-plan (format nil "Here's the split.~%~%**LEAD TASK:**~%Write the parser.~%~%**PARTNER TASK:**~%Write the tests."))
    (is string= "Write the parser." lead)
    (is string= "Write the tests." partner))
  (multiple-value-bind (lead partner)
      (ai:parse-plan (format nil "LEAD TASK:~%All of it.~%~%PARTNER TASK:~%NONE"))
    (is string= "All of it." lead)
    (false partner))
  (false (nth-value 1 (ai:parse-plan "Just do it."))))

(define-test plan-orders
    :parent agent-duet
  ;; No ORDER line: the parts go on in parallel.
  (is eq :parallel (nth-value 2 (ai:parse-plan (format nil "LEAD TASK:~%A~%~%PARTNER TASK:~%B"))))
  (multiple-value-bind (lead partner order)
      (ai:parse-plan (format nil "LEAD TASK:~%The parser~%~%PARTNER TASK:~%The tests~%~%ORDER: LEAD FIRST"))
    (is string= "The parser" lead)
    ;; The ORDER line isn't part of the partner's task.
    (is string= "The tests" partner)
    (is eq :lead-first order))
  (multiple-value-bind (lead partner order)
      (ai:parse-plan (format nil "**LEAD TASK:**~%A~%~%**ORDER:** partner-first.~%~%**PARTNER TASK:**~%B"))
    (is string= "A" lead)
    (is string= "B" partner)
    (is eq :partner-first order))
  (is eq :parallel (nth-value 2 (ai:parse-plan (format nil "LEAD TASK:~%A~%PARTNER TASK:~%B~%ORDER: PARALLEL"))))
  (is eq :circular (nth-value 2 (ai:parse-plan (format nil "LEAD TASK:~%A~%PARTNER TASK:~%B~%ORDER: LEAD FIRST, PARTNER FIRST"))))
  (is eq :circular (nth-value 2 (ai:parse-plan (format nil "LEAD TASK:~%A~%PARTNER TASK:~%B~%ORDER: circular"))))
  (is eq :parallel (nth-value 2 (ai:parse-plan (format nil "LEAD TASK:~%A~%PARTNER TASK:~%B~%ORDER: whenever"))))
  ;; A plan that can't be split still parses, its order unused.
  (multiple-value-bind (lead partner order)
      (ai:parse-plan (format nil "LEAD TASK:~%All of it.~%~%PARTNER TASK:~%NONE~%ORDER: PARALLEL"))
    (is string= "All of it." lead)
    (false partner)
    (is eq :parallel order))
  ;; The lead is asked for an order.
  (true (search "ORDER: LEAD FIRST" (ai::plan-form (make-instance 'fake-agent :id :codex :name "ChatGPT")))))

(define-test findings
    :parent agent-duet
  (let ((f (ai::parse-finding-line "- F2 [fixed] `src/a.lisp:42` — Off by one")))
    (is eql 2 (getf f :id))
    (is eq :fixed (getf f :status))
    (is string= "src/a.lisp:42" (getf f :location))
    (is string= "Off by one" (getf f :text)))
  ;; Emphasis, a numbered list, no location, no number.
  (let ((f (ai::parse-finding-line "1. **F3** [Dismissed]: Rename it")))
    (is eql 3 (getf f :id))
    (is eq :dismissed (getf f :status))
    (false (getf f :location))
    (is string= "Rename it" (getf f :text)))
  (let ((f (ai::parse-finding-line "* [open] `b.lisp` No test")))
    (false (getf f :id))
    (is eq :open (getf f :status)))
  (false (ai::parse-finding-line "- Just a remark"))
  (false (ai::parse-finding-line "Some prose about F1"))
  ;; The last FINDINGS section, up to the verdict.
  (multiple-value-bind (parsed found)
      (ai::parse-findings (format nil "Use FINDINGS: like this~%~%FINDINGS:~%- F1 [open] `a.lisp:1` Bug~%~
- F2 [open] Style~%~%VERDICT: CHANGES REQUESTED~%- F9 [open] not one"))
    (true found)
    (is equal '(1 2) (mapcar (lambda (p) (getf p :id)) parsed)))
  (multiple-value-bind (parsed found) (ai::parse-findings (format nil "FINDINGS: NONE~%VERDICT: APPROVED"))
    (true found)
    (false parsed))
  (false (nth-value 1 (ai::parse-findings "No section")))
  ;; Merging: named ones are updated, the rest added, the left-out kept.
  (let* ((one (ai::merge-findings '() (ai::parse-findings (format nil "FINDINGS:~%- F1 [open] `a.lisp:1` Bug~%- F2 [open] Style")) 1))
         (two (ai::merge-findings one (ai::parse-findings (format nil "FINDINGS:~%- F1 [fixed] Bug~%- [open] New one~%- F7 [open] Numbered")) 2)))
    (is equal '(:open :open) (mapcar #'ai::finding-status one))
    (is equal '(1 2 3 7) (mapcar #'ai::finding-id two))
    (is equal '(:fixed :open :open :open) (mapcar #'ai::finding-status two))
    (is string= "a.lisp:1" (ai::finding-location (first two)))
    (is eql 2 (ai::finding-round (third two)))
    ;; The first list wasn't changed.
    (is eq :open (ai::finding-status (first one)))
    (is = 8 (ai::next-finding-id two))
    (true (search "- F1 [fixed] `a.lisp:1` Bug" (ai::findings-lines two)))))

;;; Workflows, with agents that answer from a script

(defclass fake-agent (ai:agent)
  ((replies :initarg :replies :accessor fake-replies
            :documentation "Functions of the prompt that return the reply, used in turn")
   (prompts :initform '() :accessor fake-prompts)
   (handle :initform "conversation-1" :accessor fake-handle)))

(defmethod ai::agent-conversation-handle ((agent fake-agent)) (fake-handle agent))

(defmethod ai:agent-begin-turn ((agent fake-agent) prompt)
  (push prompt (fake-prompts agent))
  (let* ((reply (if (functionp (fake-replies agent))
                    (fake-replies agent)
                    (pop (fake-replies agent))))
         (text (if reply (funcall reply prompt) "out of replies")))
    ;; A reply of :out-of-usage plays the account running out; (:error
    ;; text), a turn that fails; :hang, one that goes on until finished
    ;; by hand (AGENT-TURN-FINISHED).
    (cond ((eq text :hang) nil)
          ((eq text :out-of-usage) (ai:agent-turn-finished agent "You've hit your limit" :out-of-usage))
          ((consp text) (ai:agent-turn-finished agent (second text) t))
          (t (ai:agent-turn-finished agent text (null reply))))))

(defun says (text) (lambda (prompt) (declare (ignore prompt)) text))

(defun run-fake (mode claude-replies codex-replies &key (rounds 3) on-plan (approve-plan (and on-plan t)))
  (let* ((claude (make-instance 'fake-agent :id :claude :name "Claude" :replies claude-replies))
         (codex (make-instance 'fake-agent :id :codex :name "ChatGPT" :replies codex-replies))
         (messages '())
         (workflow (make-instance 'ai:workflow :mode mode :lead claude :partner codex
                                  :request "Add a frobnicate function" :max-rounds rounds
                                  :approve-plan approve-plan :on-plan on-plan
                                  :on-message (lambda (from to text kind)
                                                (declare (ignore to))
                                                (push (list (if (symbolp from) from (ai:agent-name from))
                                                            kind text)
                                                      messages)))))
    (ai:start-workflow workflow)
    (values workflow (reverse messages) claude codex)))

(define-test review-approved
    :parent agent-duet
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
    :parent agent-duet
  (let ((no (says (format nil "Still wrong~%VERDICT: CHANGES REQUESTED"))))
    (multiple-value-bind (workflow messages)
        (run-fake :review (list (says "v1") (says "v2")) (list no no) :rounds 2)
      (declare (ignore messages))
      (is eq :finished (ai:workflow-state workflow))
      (is eq :unresolved (ai::loop-state (first (ai:workflow-loops workflow))))
      (is = 2 (ai::loop-round (first (ai:workflow-loops workflow)))))))

(define-test review-continues-past-last-round
    :parent agent-duet
  (let ((no (says (format nil "Still wrong: ~a~%VERDICT: CHANGES REQUESTED" "needs tests"))))
    (multiple-value-bind (workflow messages claude codex)
        (run-fake :review (list (says "v1") (says "v2") (says "v3"))
                  (list no no (says (format nil "Good.~%VERDICT: APPROVED")))
                  :rounds 2)
      (declare (ignore messages codex))
      (true (ai:workflow-continuable-p workflow))
      (true (ai:continue-workflow workflow 2))
      (is eq :finished (ai:workflow-state workflow))
      (is eq :approved (ai::loop-state (first (ai:workflow-loops workflow))))
      (is = 3 (ai::loop-round (first (ai:workflow-loops workflow))))
      ;; The author picked up from the last review.
      (true (search "needs tests" (first (fake-prompts claude))))
      (false (ai:workflow-continuable-p workflow))
      (false (ai:continue-workflow workflow 1)))))

(define-test review-continues-only-in-same-context
    :parent agent-duet
  (let ((no (says (format nil "Still wrong~%VERDICT: CHANGES REQUESTED"))))
    ;; The folder changed since it finished.
    (multiple-value-bind (workflow messages claude)
        (run-fake :review (list (says "v1") (says "v2")) (list no) :rounds 1)
      (declare (ignore messages))
      (true (ai:workflow-continuable-p workflow))
      (setf (ai:agent-directory claude) #p"/elsewhere/")
      (false (ai:workflow-continuable-p workflow))
      (false (ai:continue-workflow workflow 1))
      (is eq :finished (ai:workflow-state workflow))
      (is = 1 (length (fake-prompts claude))))
    ;; One agent's conversation was reset (or another one opened).
    (multiple-value-bind (workflow messages claude codex)
        (run-fake :review (list (says "v1") (says "v2")) (list no) :rounds 1)
      (declare (ignore messages))
      (setf (fake-handle codex) nil)
      (false (ai:continue-workflow workflow 1))
      (is = 1 (length (fake-prompts claude))))))

(defun responder (&rest pairs)
  "A reply function: the reply after the first of PAIRS' strings the prompt
contains; the last of PAIRS (unpaired) otherwise."
  (lambda (prompt)
    (loop for (key reply) on pairs by #'cddr
          when (null reply) return key
          when (search key prompt) return reply)))

(define-test divide-and-cross-review
    :parent agent-duet
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
    :parent agent-duet
  (let ((ok (says (format nil "VERDICT: APPROVED"))))
    (let ((workflow (run-fake :divide
                              (list (says (format nil "LEAD TASK:~%Everything~%PARTNER TASK:~%NONE"))
                                    (says "Did it all"))
                              (list ok))))
      (is eq :finished (ai:workflow-state workflow))
      (is = 1 (length (ai:workflow-loops workflow)))
      (is equal (ai::workflow-request workflow)
          (ai::loop-task (first (ai:workflow-loops workflow)))))))


(defun checklist-snapshots (messages)
  (mapcar #'third (remove :findings messages :key #'second :test-not #'eq)))

(defun checklist-heading (text)
  (subseq text 0 (position #\Newline text)))

(define-test task-checklist-isolates-task-markdown
    :parent agent-duet
  (let* ((author (make-instance 'fake-agent :name "Claude"))
         (part (ai::make-review-loop :author author
                                    :task (format nil "# Task~%```lisp~%(unfinished")
                                    :findings (list (ai::make-finding :id 1 :text "Keep this visible"))))
         (text (ai::checklist-text part))
         (blocks (ai::markdown-blocks text)))
    (is equal '(:paragraph :quote :paragraph :paragraph :list) (mapcar #'first blocks))
    (true (search "Assigned to: **Claude**" text))
    (true (search "**F1**" text))
    (setf (ai::loop-combined-result part) t)
    (let ((merged (ai::checklist-text part)))
      (true (search "Combine both parts and review the merged result." merged))
      (false (search "unfinished" merged)))
    (setf (ai::loop-state part) :unknown)
    (fail (ai::checklist-text part) type-error)))

(define-test task-checklist-from-assignment
    :parent agent-duet
  (multiple-value-bind (workflow messages)
      (run-fake :review (list (says "Implemented"))
                (list (says (format nil "FINDINGS: NONE~%VERDICT: APPROVED"))))
    (declare (ignore workflow))
    (let ((snapshots (checklist-snapshots messages)))
      (is = 4 (length snapshots))
      (is equal '("**Task checklist**")
          (remove-duplicates (mapcar #'checklist-heading snapshots) :test #'equal))
      (true (every (lambda (text)
                     (and (search "Add a frobnicate function" text)
                          (search "Assigned to: **Claude**" text)))
                   snapshots))
      (loop for status in '("Assigned" "Working" "In review" "Approved")
            for text in snapshots
            do (true (search (format nil "Progress: **~a**" status) text)))
      ;; Assignment is published before any work reply.
      (true (< (position :findings messages :key #'second)
               (position :work messages :key #'second))))))

(define-test split-task-checklist-from-assignment
    :parent agent-duet
  (let ((ok (format nil "FINDINGS: NONE~%VERDICT: APPROVED")))
    (multiple-value-bind (workflow messages)
        (run-fake :divide
                  (responder "Split the work"
                             (format nil "LEAD TASK:~%The function~%PARTNER TASK:~%The tests")
                             "Review the work" ok "Implemented")
                  (responder "Review the work" ok "Tested"))
      (declare (ignore workflow))
      (let* ((snapshots (checklist-snapshots messages))
             (headings (remove-duplicates (mapcar #'checklist-heading snapshots) :test #'equal)))
        (is = 2 (length headings))
        ;; Both assignments exist before either synchronous fake agent works.
        (true (search "The function" (first snapshots)))
        (true (search "Assigned to: **Claude**" (first snapshots)))
        (true (search "The tests" (second snapshots)))
        (true (search "Assigned to: **ChatGPT**" (second snapshots)))
        (true (every (lambda (text) (search "Progress: **Assigned**" text))
                     (subseq snapshots 0 2)))
        (dolist (heading headings)
          (let ((part (remove-if-not (lambda (text) (equal heading (checklist-heading text))) snapshots)))
            (is = 4 (length part))
            (true (search "Progress: **Approved**" (car (last part))))))))))

(define-test split-task-checklist-preserves-findings
    :parent agent-duet
  (multiple-value-bind (workflow messages)
      (run-fake :divide
                (list (says (format nil "LEAD TASK:~%The function~%PARTNER TASK:~%The tests"))
                      (says "v1") (says "v2")
                      (says (format nil "FINDINGS: NONE~%VERDICT: APPROVED")))
                (list (says (format nil "FINDINGS:~%- F1 [open] Missing case~%VERDICT: CHANGES REQUESTED"))
                      (says (format nil "FINDINGS:~%- F1 [fixed] Missing case~%VERDICT: APPROVED"))
                      (says "Tests")))
    (declare (ignore workflow))
    (let* ((snapshots (checklist-snapshots messages))
           (mine (remove-if-not (lambda (text) (search "**Task checklist, Claude's part**" text)) snapshots))
           (theirs (remove-if-not (lambda (text) (search "**Task checklist, ChatGPT's part**" text)) snapshots)))
      (true (find-if (lambda (text) (and (search "Addressing review" text)
                                       (search "**F1** open" text))) mine))
      (true (search "**F1** fixed" (car (last mine))))
      (true (search "Approved" (car (last mine))))
      (false (some (lambda (text) (search "**F1**" text)) theirs)))))

(define-test review-checklist
    :parent agent-duet
  (multiple-value-bind (workflow messages claude codex)
      (run-fake :review
                (list (says "v1") (says "v2") (says "v3"))
                (list (says (format nil "FINDINGS:~%- F1 [open] `a.lisp:3` Off by one~%- F2 [open] Name~%~
VERDICT: CHANGES REQUESTED"))
                      (says (format nil "FINDINGS:~%- F1 [fixed] Off by one~%- F3 [open] `b.lisp` No test~%~
VERDICT: CHANGES REQUESTED"))
                      (says (format nil "FINDINGS:~%- F2 [dismissed] Name~%- F3 [open] No test~%~
VERDICT: CHANGES REQUESTED"))))
    (declare (ignore claude))
    (let ((loop (first (ai:workflow-loops workflow))))
      (is equal '(:fixed :dismissed :open) (mapcar #'ai::finding-status (ai::loop-findings loop))))
    ;; Every progress snapshot retains the accumulated findings.
    (let ((lists (remove :findings messages :key #'second :test-not #'eq)))
      (true (> (length lists) 3))
      (let ((fixing (find "Progress: **Addressing review**" lists :key #'third :test #'search)))
        (true fixing)
        (true (search "**F1** open" (third fixing)))
        (true (search "**F2** open" (third fixing))))
      (true (search "1 open, 1 fixed, 1 dismissed" (third (car (last lists))))))
    ;; The reviewer was shown its earlier findings, and where to number from.
    (let ((third-review (third (reverse (fake-prompts codex)))))
      (false (search "Your earlier findings" (first (last (fake-prompts codex)))))
      (true (search "- F2 [open] Name" (second (reverse (fake-prompts codex)))))
      (true (search "F4 [open]" third-review)))
    (true (search "1 finding open" (third (find "Done:" messages :key #'third :test #'search))))))

(define-test approval-with-findings-open
    :parent agent-duet
  (let ((changes (says (format nil "FINDINGS:~%- F1 [open] `a.lisp:1` Bug~%VERDICT: CHANGES REQUESTED")))
        (loose (says (format nil "FINDINGS: NONE~%VERDICT: APPROVED"))))
    ;; Approved with F1 left open: asked once to settle it, which it does.
    (multiple-value-bind (workflow messages claude codex)
        (run-fake :review (list (says "v1") (says "v2"))
                  (list changes loose (says (format nil "FINDINGS:~%- F1 [fixed] Bug~%VERDICT: APPROVED"))))
      (declare (ignore messages claude))
      (let ((loop (first (ai:workflow-loops workflow))))
        (is eq :approved (ai::loop-state loop))
        (is = 2 (ai::loop-round loop))
        (is equal '(:fixed) (mapcar #'ai::finding-status (ai::loop-findings loop))))
      (true (search "still open" (first (fake-prompts codex))))
      (true (search "- F1 [open]" (first (fake-prompts codex)))))
    ;; Asked, it approves again with it open: not approved, and it can be
    ;; continued, the author told what's still open.
    (multiple-value-bind (workflow messages claude)
        (run-fake :review (list (says "v1") (says "v2") (says "v3"))
                  (list changes loose loose (says (format nil "FINDINGS:~%- F1 [fixed] Bug~%VERDICT: APPROVED"))))
      (is eq :unresolved (ai::loop-state (first (ai:workflow-loops workflow))))
      (true (find "left 1 finding open" messages :key #'third :test #'search))
      (true (ai:workflow-continuable-p workflow))
      (true (ai:continue-workflow workflow 1))
      (let ((fix (first (fake-prompts claude))))
        (true (search "reviewed your work:" fix))
        (true (search "still open:" fix))
        (true (search "- F1 [open]" fix)))
      (is eq :approved (ai::loop-state (first (ai:workflow-loops workflow)))))
    ;; Asked, it requests changes after all: another round.
    (multiple-value-bind (workflow messages claude)
        (run-fake :review (list (says "v1") (says "v2") (says "v3"))
                  (list changes loose changes (says (format nil "FINDINGS:~%- F1 [fixed] Bug~%VERDICT: APPROVED"))))
      (declare (ignore messages))
      (is eq :approved (ai::loop-state (first (ai:workflow-loops workflow))))
      (is = 3 (length (fake-prompts claude))))))

(define-test plan-approval
    :parent agent-duet
  (let* ((ok (format nil "Fine.~%VERDICT: APPROVED"))
         (offered nil)
         (plan (format nil "LEAD TASK:~%The function~%~%PARTNER TASK:~%The tests")))
    (multiple-value-bind (workflow messages claude codex)
        (run-fake :divide
                  (responder "Split the work" plan "Review the work" ok "Wrote the function")
                  (responder "Review the work" ok "Wrote the tests")
                  :on-plan (lambda (workflow lead partner) (declare (ignore workflow)) (setf offered (list lead partner))))
      (declare (ignore messages))
      ;; It waits, with nothing started.
      (is equal '("The function" "The tests") offered)
      (is eq :running (ai:workflow-state workflow))
      (true (ai:workflow-pending-plan workflow))
      (is = 1 (length (fake-prompts claude)))
      (false (fake-prompts codex))
      (false (ai:approve-plan workflow "  " ""))
      ;; Edited: each is given its part as the user left it.
      (true (ai:approve-plan workflow "The function, documented" "The tests"))
      (false (ai:approve-plan workflow "Again" nil))
      (is eq :finished (ai:workflow-state workflow))
      (true (find (format nil "The user edited the plan. Your part is now:~%~%The function, documented")
                  (fake-prompts claude) :test #'search))
      (true (find "The function, documented" (fake-prompts codex) :test #'search))))
  ;; The partner's part cleared: the lead does its part alone.
  (multiple-value-bind (workflow messages claude codex)
      (run-fake :divide
                (responder "Split the work" (format nil "LEAD TASK:~%A~%~%PARTNER TASK:~%B") "Did A")
                (responder "Review the work" (format nil "VERDICT: APPROVED"))
                :on-plan (lambda (&rest args) (declare (ignore args))))
    (declare (ignore messages codex))
    (true (ai:approve-plan workflow "A and B" nil))
    (is = 1 (length (ai:workflow-loops workflow)))
    (true (search "do this yourself" (first (fake-prompts claude))))
    (is eq :finished (ai:workflow-state workflow)))
  ;; The lead's cleared: the partner does its part, the lead reviews.
  (multiple-value-bind (workflow messages claude codex)
      (run-fake :divide
                (responder "Split the work" (format nil "LEAD TASK:~%A~%~%PARTNER TASK:~%B") (format nil "VERDICT: APPROVED"))
                (says "Did B")
                :on-plan (lambda (&rest args) (declare (ignore args))))
    (declare (ignore messages claude))
    (true (ai:approve-plan workflow "" "B"))
    (let ((loop (first (ai:workflow-loops workflow))))
      (is eq codex (ai::loop-author loop))
      (is eq :approved (ai::loop-state loop)))
    (true (search "No one else is working on it" (first (fake-prompts codex))))))

(define-test plan-approval-without-callback
    :parent agent-duet
  ;; Asked to wait for approval, it waits even with no one told.
  (multiple-value-bind (workflow messages claude codex)
      (run-fake :divide (list (says (format nil "LEAD TASK:~%A~%~%PARTNER TASK:~%B"))) '() :approve-plan t)
    (declare (ignore messages))
    (true (ai:workflow-pending-plan workflow))
    (is = 1 (length (fake-prompts claude)))
    (false (fake-prompts codex))))

(define-test stop-while-plan-waits
    :parent agent-duet
  (multiple-value-bind (workflow messages claude)
      (run-fake :divide (list (says (format nil "LEAD TASK:~%A~%~%PARTNER TASK:~%B"))) '()
                :on-plan (lambda (&rest args) (declare (ignore args))))
    (declare (ignore messages))
    (ai:stop-workflow workflow)
    (false (ai:approve-plan workflow "A" "B"))
    (is = 1 (length (fake-prompts claude)))))

(define-test compare-proposals
    :parent agent-duet
  (let ((ok (format nil "Fine.~%VERDICT: APPROVED")))
    (multiple-value-bind (workflow messages claude codex)
        (run-fake :compare
                  (responder "propose your approach" "Claude: a macro"
                             "proposed an approach, independently" (format nil "We differ on macros.~%LEAD TASK:~%The function~%~%PARTNER TASK:~%The tests")
                             "Review the work" ok
                             "Wrote the function")
                  (responder "propose your approach" "ChatGPT: a function"
                             "Review the work" ok
                             "Wrote the tests"))
      (is eq :finished (ai:workflow-state workflow))
      (is = 2 (length (ai:workflow-loops workflow)))
      (is = 2 (count :proposal messages :key #'second))
      ;; Each proposed without seeing the other's; then the lead saw ChatGPT's.
      (false (search "a macro" (first (last (fake-prompts codex)))))
      (let ((reconcile (find "independently:" (fake-prompts claude) :test #'search)))
        (true (search "ChatGPT: a function" reconcile))
        (true (search "Compare it with yours" reconcile))
        (true (search "PARTNER TASK:" reconcile)))
      (true (find "The tests" (fake-prompts codex) :test #'search)))))

(define-test compare-without-one-proposal
    :parent agent-duet
  ;; A failed proposal is preserved for explicit recovery.
  (multiple-value-bind (workflow messages claude)
      (run-fake :compare
                (responder "propose your approach" "Mine"
                           "couldn't make a proposal" (format nil "LEAD TASK:~%All~%~%PARTNER TASK:~%NONE")
                           "Did it")
                (list nil (says (format nil "VERDICT: APPROVED"))))
    (declare (ignore messages))
    (is eq :running (ai:workflow-state workflow))
    (is eq :failed (ai::step-reason (first (ai::workflow-paused workflow)))))
  ;; Neither is available: both proposals stay recoverable.
  (let ((workflow (run-fake :compare '() '())))
    (is eq :running (ai:workflow-state workflow))
    (false (ai:workflow-loops workflow))))

(define-test lead-proposal-out-then-handed-off
    :parent agent-duet
  ;; The lead runs out proposing: going without its proposal, the partner
  ;; carries out its own, unreviewed, rather than waiting on the lead.
  (multiple-value-bind (workflow paused claude codex)
      (run-pausing :compare (list (says :out-of-usage)) (list (says "Theirs") (says "Did it all")))
    (is eq claude (ai::step-agent (first paused)))
    (ai::agent-ran-out claude nil)
    (ai::hand-off-step workflow (first paused))
    (is = 1 (length (fake-prompts claude)))
    (true (search "carry out yours yourself" (first (fake-prompts codex))))
    (is eq :finished (ai:workflow-state workflow))
    (let ((loop (first (ai:workflow-loops workflow))))
      (is eq codex (ai::loop-author loop))
      (is eq :unreviewed (ai::loop-state loop)))))

(define-test lead-proposal-out-with-plan-approval
    :parent agent-duet
  ;; With plan approval, the partner's takeover waits for the user too.
  (let* ((claude (make-instance 'fake-agent :id :claude :name "Claude" :replies (list (says :out-of-usage))))
         (codex (make-instance 'fake-agent :id :codex :name "ChatGPT" :replies (list (says "Theirs") (says "Did it"))))
         (paused '())
         (workflow (make-instance 'ai:workflow :mode :compare :lead claude :partner codex :request "Add frobnicate"
                                  :approve-plan t
                                  :on-pause (lambda (w step) (declare (ignore w)) (push step paused)))))
    (ai:start-workflow workflow)
    (ai::agent-ran-out claude nil)
    (ai::hand-off-step workflow (first paused))
    (true (ai:workflow-pending-plan workflow))
    (is = 1 (length (fake-prompts codex)))
    (true (ai:approve-plan workflow nil "Add frobnicate, documented"))
    (true (search "Add frobnicate, documented" (first (fake-prompts codex))))
    (true (search "carry out yours yourself" (first (fake-prompts codex))))
    (is eq :unreviewed (ai::loop-state (first (ai:workflow-loops workflow))))
    (is = 1 (length (fake-prompts claude)))))

(define-test proposal-out-then-handed-off
    :parent agent-duet
  (multiple-value-bind (workflow paused)
      (run-pausing :compare
                   (list (says "Mine") (says (format nil "LEAD TASK:~%All~%PARTNER TASK:~%NONE")) (says "Did it"))
                   (list (says :out-of-usage)))
    (is eq :propose (ai::step-role (first paused)))
    (ai::hand-off-step workflow (first paused))
    ;; Claude planned from its own, and did it.
    (is = 1 (length (ai:workflow-loops workflow)))
    (is equal "Did it" (ai::loop-summary (first (ai:workflow-loops workflow))))))

(define-test failed-turn
    :parent agent-duet
  ;; A failed turn pauses with its assignment intact.
  (let ((workflow (run-fake :review '() '())))
    (is eq :running (ai:workflow-state workflow))
    (is eq :paused (ai::loop-state (first (ai:workflow-loops workflow))))))

(defclass hanging-agent (fake-agent) ()
  (:documentation "Starts turns and never finishes them, until cancelled."))

(defmethod ai:agent-begin-turn ((agent hanging-agent) prompt)
  (push prompt (fake-prompts agent)))

(define-test cancel-ends-workflow-step
    :parent agent-duet
  ;; Stopping an agent mid-workflow (its Stop or New button, or a folder
  ;; change) must not leave the workflow running with nobody working.
  (let* ((claude (make-instance 'hanging-agent :id :claude :name "Claude" :replies '()))
         (codex (make-instance 'fake-agent :id :codex :name "ChatGPT" :replies '()))
         (workflow (make-instance 'ai:workflow :mode :review :lead claude :partner codex :request "x")))
    (ai:start-workflow workflow)
    (is eq :running (ai:workflow-state workflow))
    (ai:agent-cancel claude)
    (is eq :running (ai:workflow-state workflow))
    (is eq :paused (ai::loop-state (first (ai:workflow-loops workflow))))))

(define-test cancel-ends-queued-steps
    :parent agent-duet
  ;; Queued prompts are workflow steps too.
  (let* ((agent (make-instance 'hanging-agent :id :claude :name "Claude" :replies '()))
         (results '()))
    (ai:agent-submit agent "one" (lambda (text error) (push (list :one text error) results)))
    (ai:agent-submit agent "two" (lambda (text error) (push (list :two text error) results)))
    (ai:agent-cancel agent)
    (is = 2 (length results))
    (true (every #'third results))
    (false (ai::agent-queue agent))))

;;; A turn gone quiet

(defun go-quiet (agent seconds)
  "Make AGENT's last sign of life SECONDS ago."
  (setf (ai::agent-last-activity agent) (- (ai::clock-seconds) seconds)))

(define-test stall-after-silence
    :parent agent-duet
  (let ((agent (make-instance 'hanging-agent :id :claude :name "Claude" :replies '())))
    ;; Idle: nothing to stall.
    (go-quiet agent 1000)
    (false (ai::agent-silence agent))
    (ai:agent-submit agent "work")
    (false (ai::agent-check-stall agent 300))
    (go-quiet agent 299)
    (false (ai::agent-check-stall agent 300))
    (go-quiet agent 301)
    (true (ai::agent-check-stall agent 300))
    ;; Reported once for each silence.
    (false (ai::agent-check-stall agent 300))
    ;; An event starts the silence again.
    (ai::agent-emit agent :delta :text "still here")
    (false (ai::agent-check-stall agent 300))
    (go-quiet agent 301)
    (true (ai::agent-check-stall agent 300))
    ;; Keep waiting: another while.
    (ai::agent-keep-waiting agent)
    (false (ai::agent-check-stall agent 300))
    (go-quiet agent 301)
    (true (ai::agent-check-stall agent 300))
    ;; A turn being stopped isn't stalled.
    (go-quiet agent 1000)
    (ai:agent-cancel agent)
    (false (ai::agent-silence agent))))

(define-test stall-not-while-waiting-for-user
    :parent agent-duet
  ;; A permission or a question waiting doesn't count.
  (let ((agent (make-instance 'hanging-agent :id :claude :name "Claude" :replies '()))
        (answer nil))
    (ai:agent-submit agent "work")
    (ai::agent-ask-approval agent "a1" "$ make test" nil (lambda (d) (setf answer d)))
    (go-quiet agent 1000)
    (false (ai::agent-silence agent))
    (false (ai::agent-check-stall agent 300))
    ;; Answered: the silence starts from there.
    (ai::agent-answer-approval agent "a1" :allow)
    (is eq :allow answer)
    (false (ai::agent-check-stall agent 300))
    (ai::agent-ask-question agent "q1" '() (lambda (d) (declare (ignore d))))
    (go-quiet agent 1000)
    (false (ai::agent-check-stall agent 300)))
  ;; A plan waiting for approval: neither agent is in a turn.
  (multiple-value-bind (workflow messages claude codex)
      (run-fake :divide
                (responder "Split the work" (format nil "LEAD TASK:~%A~%~%PARTNER TASK:~%B") "Did A")
                (says "Did B")
                :on-plan (lambda (&rest args) (declare (ignore args))))
    (declare (ignore messages))
    (true (ai:workflow-pending-plan workflow))
    (dolist (agent (list claude codex))
      (go-quiet agent 1000)
      (false (ai::agent-check-stall agent 300))))
  ;; Paused for want of usage: nor here.
  (multiple-value-bind (workflow paused claude codex)
      (run-pausing :review (list (says "Wrote it")) (list (says :out-of-usage)))
    (true paused)
    (is eq :running (ai:workflow-state workflow))
    (dolist (agent (list claude codex))
      (go-quiet agent 1000)
      (false (ai::agent-check-stall agent 300)))))

(define-test stall-retry-after-turn-ends
    :parent agent-duet
  (let* ((claude (make-instance 'hanging-agent :id :claude :name "Claude" :replies '()))
         (codex (make-instance 'fake-agent :id :codex :name "ChatGPT"
                               :replies (list (says (format nil "Fine.~%VERDICT: APPROVED")))))
         (workflow (make-instance 'ai:workflow :mode :review :lead claude :partner codex :request "Add frobnicate")))
    (ai:start-workflow workflow)
    (go-quiet claude 400)
    (true (ai::agent-check-stall claude 300))
    (true (ai::retry-stalled-turn workflow claude))
    ;; Interrupted, but not over yet: nothing is sent again.
    (is = 1 (length (fake-prompts claude)))
    (true (ai::agent-retry-pending-p claude))
    (false (car (ai::agent-turn-token claude)))
    (false (ai::agent-check-stall claude 0))
    (false (ai::retry-stalled-turn workflow claude))
    ;; The interrupted turn ends: the workflow doesn't hear of it, and the
    ;; same prompt goes again.
    (ai:agent-turn-finished claude "Interrupted." t)
    (is eq :running (ai:workflow-state workflow))
    (is eq :sent (ai::agent-retry claude))
    (is = 2 (length (fake-prompts claude)))
    (is string= (second (fake-prompts claude)) (first (fake-prompts claude)))
    (true (car (ai::agent-turn-token claude)))
    ;; Its reply carries the workflow on.
    (ai:agent-turn-finished claude "Wrote it" nil)
    (is eq :finished (ai:workflow-state workflow))
    (is eq :approved (ai::loop-state (first (ai:workflow-loops workflow))))
    (true (search "Wrote it" (first (fake-prompts codex))))))

(define-test stall-stop-when-turn-never-ends
    :parent agent-duet
  (let* ((claude (make-instance 'hanging-agent :id :claude :name "Claude" :replies '()))
         (codex (make-instance 'fake-agent :id :codex :name "ChatGPT" :replies '()))
         (workflow (make-instance 'ai:workflow :mode :review :lead claude :partner codex :request "x")))
    (ai:start-workflow workflow)
    (true (ai::retry-stalled-turn workflow claude))
    ;; The interrupt went unanswered: Stop ends it all the same.
    (ai::stop-stalled-turn workflow claude)
    (is eq :stopped (ai:workflow-state workflow))
    (false (ai:agent-busy claude))
    (is eq :dropped (ai::agent-retry claude))
    (is = 1 (length (fake-prompts claude)))
    (false (ai::agent-queue claude)))
  ;; Outside a workflow, it ends the agent's turn alone.
  (let ((agent (make-instance 'hanging-agent :id :claude :name "Claude" :replies '()))
        (result nil))
    (ai:agent-submit agent "work" (lambda (text error) (setf result (list text error))))
    (ai::stop-stalled-turn nil agent)
    (false (ai:agent-busy agent))
    (true (second result))))

(define-test stall-cleared-by-every-sign-of-life
    :parent agent-duet
  ;; Events that aren't shown count too, and clear a reported stall (so the
  ;; card for it settles): a Claude subagent's, Codex's stderr and usage.
  (let* ((claude (make-instance 'ai:claude-agent))
         (cp (ai::make-claude-process)))
    (setf (ai::claude-agent-process claude) cp
          (ai::agent-busy claude) t
          (ai::agent-turn-token claude) (list t))
    (go-quiet claude 400)
    (true (ai::agent-check-stall claude 300))
    (ai::claude-agent-event claude cp (ai::make-claude-event :kind :text :text "inner" :subagent t))
    (false (ai::agent-stall-noticed claude))
    (false (ai::agent-check-stall claude 300))
    (setf (ai::agent-busy claude) nil))
  (let ((codex (make-instance 'ai:codex-agent))
        (server (ai::make-codex-app-server)))
    (setf (ai::codex-agent-server codex) server
          (ai::agent-busy codex) t
          (ai::agent-turn-token codex) (list t))
    (dolist (event (list (ai::make-codex-event :kind :stderr :text "working…")
                         (ai::make-codex-event :kind :rate-limits :data nil)))
      (go-quiet codex 400)
      (true (ai::agent-check-stall codex 300))
      (ai::codex-agent-event codex server event)
      (false (ai::agent-stall-noticed codex)))
    (setf (ai::codex-agent-server codex) nil (ai::agent-busy codex) nil)))

;;; What a task changed

(defmacro with-fake-snapshots ((taken deleted &key fail) &body body)
  "BODY with snapshots faked: each is (:snapshot label), pushed onto TAKEN;
deleted ones onto DELETED. While FAIL (a place) is true, taking one fails."
  `(let ((,taken '()) (,deleted '()))
     (let ((ai::*task-snapshot* (lambda (directory label)
                                  (declare (ignore directory))
                                  (when ,fail (error "disk full"))
                                  (let ((snapshot (list :snapshot label)))
                                    (push snapshot ,taken)
                                    snapshot)))
           (ai::*task-snapshot-delete* (lambda (directory snapshot)
                                         (declare (ignore directory))
                                         (push snapshot ,deleted)))
           (ai::*task-snapshot-prune* (lambda (directory) (declare (ignore directory)))))
       ,@body)))

(defun snapshot-label (snapshot)
  (second snapshot))

(define-test task-end-after-agents-stop
    :parent agent-duet
  (with-fake-snapshots (taken deleted)
    ;; Stopped mid-turn: the end waits for the turn to be over.
    (let* ((claude (make-instance 'hanging-agent :id :claude :name "Claude" :replies '()))
           (codex (make-instance 'fake-agent :id :codex :name "ChatGPT" :replies '()))
           (workflow (make-instance 'ai:workflow :mode :review :lead claude :partner codex :request "x"))
           (task (ai::begin-task-changes workflow #p"/repo/")))
      (setf (ai::workflow-on-finish workflow)
            (lambda (w) (declare (ignore w))
              (ai::end-task-changes task)
              ;; As the finish callback runs, the stopped agent is still busy.
              (true (ai:agent-busy claude))
              (false (ai::finish-task-changes task))))
      (ai:start-workflow workflow)
      (is eq :live (ai::task-changes-range task))
      (ai:stop-workflow workflow)
      ;; Interrupted, but not over: no end yet, and Changes is still live.
      (true (ai:agent-busy claude))
      (false (ai::finish-task-changes task))
      (is = 1 (length taken))
      (is eq :live (ai::task-changes-range task))
      (ai:agent-turn-finished claude "Interrupted." t)
      (is eq :taken (ai::finish-task-changes task))
      (is = 2 (length taken))
      (is eq :final (ai::task-changes-range task))
      (false deleted))
    ;; The stall card's Stop: the end comes once the turn is ended.
    (let* ((claude (make-instance 'hanging-agent :id :claude :name "Claude" :replies '()))
           (codex (make-instance 'fake-agent :id :codex :name "ChatGPT" :replies '()))
           (workflow (make-instance 'ai:workflow :mode :review :lead claude :partner codex :request "x"))
           (task (ai::begin-task-changes workflow #p"/repo/"))
           (busy-at-finish :unset))
      (setf (ai::workflow-on-finish workflow)
            (lambda (w) (declare (ignore w))
              (setf busy-at-finish (ai:agent-busy claude))
              (ai::end-task-changes task)
              (ai::finish-task-changes task)))
      (ai:start-workflow workflow)
      (ai::stop-stalled-turn workflow claude)
      (true busy-at-finish)
      (is eq :pending (ai::task-changes-final task))
      (false (ai:agent-busy claude))
      (is eq :taken (ai::finish-task-changes task))
      (true (search "-end-" (snapshot-label (ai::task-changes-end task)))))))

(define-test task-changes-continued-and-failed
    :parent agent-duet
  (let ((fail nil))
    (with-fake-snapshots (taken deleted :fail fail)
      (let ((no (says (format nil "Still wrong~%VERDICT: CHANGES REQUESTED"))))
        (multiple-value-bind (workflow messages)
            (run-fake :review (list (says "v1") (says "v2") (says "v3"))
                      (list no (says "unused")) :rounds 1)
          (declare (ignore messages))
          ;; As the UI does it: the start before the workflow, the end after.
          (let ((task (ai::begin-task-changes workflow #p"/repo/")))
            (ai::end-task-changes task)
            (is eq :taken (ai::finish-task-changes task))
            (let ((first-end (ai::task-changes-end task)))
              (is eq :final (ai::task-changes-range task))
              ;; Continued: live again, not the earlier result.
              (setf (fake-replies (ai::workflow-partner workflow)) (list no))
              (true (ai:continue-workflow workflow 1))
              (is eq :finished (ai:workflow-state workflow))
              (setf (ai:workflow-state workflow) :running)
              (is eq :live (ai::task-changes-range task))
              (setf (ai:workflow-state workflow) :finished)
              ;; Its new end fails: said so, the old end kept, nothing live.
              (ai::end-task-changes task)
              (setf fail t)
              (multiple-value-bind (result why) (ai::finish-task-changes task)
                (is eq :failed result)
                (true (search "disk full" why)))
              (is eq :failed (ai::task-changes-range task))
              (is eq first-end (ai::task-changes-end task))
              (false deleted)
              ;; Ended again and taken: the earlier end goes only now.
              (setf fail nil)
              (ai::end-task-changes task)
              (is eq :taken (ai::finish-task-changes task))
              (isnt eq first-end (ai::task-changes-end task))
              (is equal (list first-end) deleted)
              (is eq :final (ai::task-changes-range task)))))))
    ;; Outside a git repository: no start, nothing to show or end.
    (let ((ai::*task-snapshot* (lambda (directory label) (declare (ignore directory label)) nil)))
      (multiple-value-bind (task why) (ai::begin-task-changes nil #p"/not-git/")
        (false why)
        (is eq :unrecorded (ai::task-changes-range task))
        (false (ai::end-task-changes task))))))

;;; Running out of usage

(define-test usage-windows
    :parent agent-duet
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
    :parent agent-duet
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
    :parent agent-duet
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
    :parent agent-duet
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
    :parent agent-duet
  (multiple-value-bind (workflow paused)
      (run-pausing :review (list (says "Wrote it"))
                   (list (says :out-of-usage) (says (format nil "Fine.~%VERDICT: APPROVED"))))
    (ai::resume-step workflow (first paused))
    (is eq :finished (ai:workflow-state workflow))
    (is eq :approved (ai::loop-state (first (ai:workflow-loops workflow))))))

(define-test author-out-then-taken-over
    :parent agent-duet
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
    :parent agent-duet
  (multiple-value-bind (workflow paused claude codex)
      (run-pausing :divide (list (says :out-of-usage)) (list (says "Did the whole thing")))
    (declare (ignore claude))
    (is eq :plan (ai::step-role (first paused)))
    (ai::hand-off-step workflow (first paused))
    (true (search "do all of it yourself" (first (fake-prompts codex))))
    (is eq :finished (ai:workflow-state workflow))
    (is eq :unreviewed (ai::loop-state (first (ai:workflow-loops workflow))))))

(define-test stop-while-paused
    :parent agent-duet
  (multiple-value-bind (workflow paused)
      (run-pausing :review (list (says "Wrote it")) (list (says :out-of-usage)))
    (ai:stop-workflow workflow)
    (is eq :stopped (ai:workflow-state workflow))
    (true (ai::workflow-paused workflow))
    ;; Stop preserves the step; recovering it is an explicit user action.
    (true (member (first paused) (ai::workflow-paused workflow)))))

(define-test out-agent-turns-prompts-away
    :parent agent-duet
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

;;; Codex against a fake app-server (tests/fake-codex, using SBCL)

(defun fake-codex-program ()
  (namestring (asdf:system-relative-pathname "agent-duet" "tests/fake-codex")))

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
    :parent agent-duet
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
      (true (second result)))))

(define-test codex-retry-after-failed-start
    :parent agent-duet
  (with-fake-codex (agent directory)
    (let ((results '()))
      (touch directory "fail-thread-once")
      (ai:agent-submit agent "first" (lambda (text error) (push (list text error) results)))
      (true (wait-until (lambda () (= 1 (length results)))))
      (true (second (first results)))       ; failed
      (ai:agent-submit agent "second" (lambda (text error) (push (list text error) results)))
      (true (wait-until (lambda () (= 2 (length results)))))
      (is equal '("hi" nil) (first results))
      (is = 2 (count "thread/start" (fake-log directory) :test #'string=)))))

(define-test queueing
    :parent agent-duet
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


(define-test codex-send-while-working
    :parent agent-duet
  (with-fake-codex (agent directory)
    (let ((results nil))
      (touch directory "await-steer")
      (ai:agent-submit agent "original task"
                       (lambda (text error) (push (list text error) results)))
      (true (wait-until (lambda () (let ((server (ai::codex-agent-server agent)))
                                    (and server (ai::codex-turn-id server))))))
      (true (ai:agent-busy agent))
      (ai:agent-send-now agent "focus on tests")
      (true (wait-until (lambda () results)))
      (is equal '(("steered reply" nil)) results)
      (is string= "focus on tests" (uiop:read-file-string (merge-pathnames "steered" directory)))
      (is = 1 (count "turn/start" (fake-log directory) :test #'equal))
      (false (member "turn/interrupt" (fake-log directory) :test #'equal))
      (false (ai::agent-queue agent)))))

(define-test claude-streaming-completion
    :parent agent-duet
  ;; A result with another CLI response queued must not start the reviewer.
  (let* ((agent (make-instance 'ai:claude-agent))
         (cp (ai::make-claude-process))
         (results nil))
    (setf (ai::claude-agent-process agent) cp
          (ai::agent-busy agent) t
          (ai::agent-callback agent) (lambda (text error) (push (list text error) results)))
    (flet ((result (text queued)
             (first (ai::parse-claude-event
                     (ai:json-string (ai:jobj "type" "result" "subtype" "success"
                                              "result" text "queued_turn_count" queued))))))
      (ai::claude-agent-event agent cp (result "first" 1))
      (true (ai:agent-busy agent))
      (false results)
      (ai::claude-agent-event agent cp (result "updated" 0))
      (false (ai:agent-busy agent))
      (is equal '(("updated" nil)) results))))

(define-test send-now-idle-and-stopping
    :parent agent-duet
  (let ((agent (make-instance 'hanging-agent :id :claude :name "Claude" :replies nil)))
    (ai:agent-send-now agent "first")
    (is equal '("first") (fake-prompts agent))
    (ai:agent-cancel agent)
    (fail (ai:agent-send-now agent "too late"))
    (false (ai::agent-queue agent))))

(define-test codex-stale-startup-error
    :parent agent-duet
  (with-fake-codex (agent directory)
    (let ((deliveries nil)
          (lock (sb-thread:make-mutex))
          (results nil))
      ;; Hold GUI deliveries until a failed startup's worker has returned.
      (setf (ai::agent-deliver agent)
            (lambda (thunk)
              (sb-thread:with-mutex (lock) (push thunk deliveries))))
      (touch directory "fail-thread-once")
      (ai:agent-submit agent "old task")
      (true (wait-until (lambda () (sb-thread:with-mutex (lock) (not (null deliveries))))))
      (ai:agent-cancel agent)
      (ai::agent-new-conversation agent)
      (setf (ai::agent-deliver agent) #'funcall)
      (touch directory "await-steer")
      (ai:agent-submit agent "new task" (lambda (text error) (push (list text error) results)))
      (true (wait-until (lambda ()
                          (let ((server (ai::codex-agent-server agent)))
                            (and server (ai::codex-turn-id server))))))
      (dolist (thunk (sb-thread:with-mutex (lock) (reverse deliveries)))
        (funcall thunk))
      (true (ai:agent-busy agent))
      (false results)
      (ai:agent-send-now agent "finish new task")
      (true (wait-until (lambda () results)))
      (is equal '(("steered reply" nil)) results))))

(define-test claude-running-permission
    :parent agent-duet
  (dolist (next-permission '(:edits :full))
    (let* ((agent (make-instance 'ai:claude-agent :permission next-permission))
           (cp (ai::make-claude-process)))
      ;; The process started in Ask mode; a UI change is for its next turn.
      (setf (ai::claude-agent-process agent) cp
            (ai::claude-agent-config agent) (list "sonnet" nil :ask nil))
      (ai::claude-agent-event agent cp
                             (ai::make-claude-event :kind :permission :id "approval"
                                                   :name "Bash" :input (ai:jobj "command" "make test")))
      (true (assoc "approval" (ai::agent-approvals agent) :test #'equal)))))

;;; Resuming a conversation from the history

(define-test claude-resume-conversation
    :parent agent-duet
  (let ((agent (make-instance 'ai:claude-agent)))
    (false (ai::agent-conversation-handle agent))
    (ai::agent-resume-conversation agent "session-1")
    (is equal "session-1" (ai::agent-conversation-handle agent))
    (true (ai::claude-agent-started agent))
    (ai::agent-resume-conversation agent nil)
    (false (ai::agent-conversation-handle agent))))

(defmacro with-fake-claude ((agent directory notes) &body body)
  "Run BODY with AGENT, a Claude agent working in DIRECTORY against
tests/fake-claude, its notes pushed onto NOTES."
  (let ((program (gensym)))
    `(let* ((,directory (merge-pathnames (format nil "duet-claude-~d/" (random 1000000)) (uiop:temporary-directory)))
            (,program ai:*claude-program*)
            (,agent (make-instance 'ai:claude-agent :directory ,directory))
            (,notes '()))
       (ensure-directories-exist ,directory)
       ;; Set, not bound: a retry starts the CLI from the reader thread.
       (setf ai:*claude-program* (namestring (asdf:system-relative-pathname "agent-duet" "tests/fake-claude")))
       (setf (ai:agent-listener ,agent)
             (lambda (agent kind &key text &allow-other-keys)
               (declare (ignore agent))
               (when (eq kind :note) (push text ,notes))))
       (unwind-protect (progn ,@body)
         (setf ai:*claude-program* ,program)
         (ignore-errors (ai:agent-shutdown ,agent))
         (uiop:delete-directory-tree ,directory :validate t :if-does-not-exist :ignore)))))

(define-test claude-resume-missing-session
    :parent agent-duet
  (let ((event (first (ai::parse-claude-event
                       "{\"type\":\"result\",\"subtype\":\"error_during_execution\",\"is_error\":true,\"session_id\":\"gone\",\"errors\":[\"No conversation found with session ID: gone\"]}"))))
    (is string= "No conversation found with session ID: gone" (ai::event-text event)))
  (with-fake-claude (agent directory notes)
    (let ((results nil))
      (ai::agent-resume-conversation agent "gone")
      (ai:agent-submit agent "the task" (lambda (text error) (push (list text error) results)))
      (true (wait-until (lambda () results)))
      ;; It says so, and the same task carries on in a new conversation.
      (is equal '(("fresh reply" nil)) results)
      (true (find-if (lambda (n) (search "Couldn't resume" n)) notes))
      (let ((handle (ai::agent-conversation-handle agent)))
        (true handle)
        (isnt equal "gone" handle))
      (is = 2 (length (uiop:read-file-lines (merge-pathnames "args" directory)))))))

(define-test claude-resume-missing-session-steered
    :parent agent-duet
  (with-fake-claude (agent directory notes)
    (let ((results nil))
      (touch directory "expect-steer")
      (ai::agent-resume-conversation agent "gone")
      (ai:agent-submit agent "the task" (lambda (text error) (push (list text error) results)))
      ;; Sent while the resume is still pending: the new conversation gets it too.
      (ai:agent-send-now agent "and this")
      (true (wait-until (lambda () results)))
      (is equal '(("fresh reply: the task + and this" nil)) results))))

(define-test codex-resume-conversation
    :parent agent-duet
  (with-fake-codex (agent directory)
    (let ((results nil))
      (ai::agent-resume-conversation agent "thread-old")
      (is equal "thread-old" (ai::agent-conversation-handle agent))
      (ai:agent-submit agent "carry on" (lambda (text error) (push (list text error) results)))
      (true (wait-until (lambda () results)))
      (is equal '(("hi" nil)) results)
      (true (member "thread/resume" (fake-log directory) :test #'string=))
      (false (member "thread/start" (fake-log directory) :test #'string=))
      (is equal "thread-old" (ai::agent-conversation-handle agent)))))

(define-test codex-resume-missing-thread
    :parent agent-duet
  (with-fake-codex (agent directory)
    (let ((results nil) (notes nil))
      (setf (ai:agent-listener agent)
            (lambda (agent kind &key text &allow-other-keys)
              (declare (ignore agent))
              (when (eq kind :note) (push text notes))))
      (touch directory "fail-resume")
      (ai::agent-resume-conversation agent "thread-gone")
      (ai:agent-submit agent "carry on" (lambda (text error) (push (list text error) results)))
      (true (wait-until (lambda () results)))
      ;; It says so, and carries on in a new thread.
      (is equal '(("hi" nil)) results)
      (true (find-if (lambda (n) (search "Couldn't resume" n)) notes))
      (is equal "thread-1" (ai::agent-conversation-handle agent)))))

;;; Separate worktrees, in a real repository

(defmacro with-git-folder ((repo) &body body)
  "A temporary git repository with one commit (shared.txt), for BODY."
  `(let ((,repo (merge-pathnames (format nil "duet-folder-~a/" (ai::make-uuid)) (uiop:temporary-directory))))
     (ensure-directories-exist ,repo)
     (unwind-protect
          (progn
            (dolist (args '(("init") ("config" "user.name" "Duet Test") ("config" "user.email" "duet@example.invalid")
                            ("config" "commit.gpgsign" "false")))
              (apply #'ai::worktree-git ,repo args))
            (write-file-text ,repo "shared.txt" "base")
            (ai::worktree-git ,repo "add" "-A")
            (ai::worktree-git ,repo "commit" "-m" "initial")
            ,@body)
       ;; Worktrees kept by a test, then the folder.
       (dolist (line (uiop:split-string (ignore-errors (ai::worktree-git ,repo "worktree" "list" "--porcelain"))
                                        :separator '(#\Newline)))
         (when (uiop:string-prefix-p "worktree " line)
           (let ((tree (uiop:ensure-directory-pathname (subseq line 9))))
             (unless (equal (truename tree) (truename ,repo))
               (ignore-errors (ai::worktree-git ,repo "worktree" "remove" "--force" (namestring tree)))
               (uiop:delete-directory-tree tree :validate t :if-does-not-exist :ignore)))))
       (uiop:delete-directory-tree ,repo :validate t :if-does-not-exist :ignore))))

(defun write-file-text (directory name text)
  (with-open-file (out (merge-pathnames name directory) :direction :output
                       :if-exists :supersede :if-does-not-exist :create)
    (write-string text out)))

(defun read-file-text (directory name)
  (string-trim '(#\Newline) (uiop:read-file-string (merge-pathnames name directory))))

(defun worktree-agents (repo claude-reply codex-reply &key (codex-class 'fake-agent))
  "Claude and ChatGPT in REPO, each answering with its function of (agent prompt)."
  (let ((claude (make-instance 'fake-agent :id :claude :name "Claude" :directory repo))
        (codex (make-instance codex-class :id :codex :name "ChatGPT" :directory repo)))
    (setf (fake-replies claude) (lambda (prompt) (funcall claude-reply claude prompt))
          (fake-replies codex) (lambda (prompt) (funcall codex-reply codex prompt)))
    (values claude codex)))

(defun run-worktrees (claude codex)
  (let* ((messages '())
         (workflow (make-instance 'ai:workflow :mode :divide :lead claude :partner codex :worktrees t
                                  :request "Add frobnicate"
                                  :on-message (lambda (from to text kind)
                                                (declare (ignore to))
                                                (push (list (if (symbolp from) from (ai:agent-name from)) kind text)
                                                      messages)))))
    (ai:start-workflow workflow)
    (values workflow (reverse messages))))

(defparameter *split-plan* (format nil "LEAD TASK:~%The function~%~%PARTNER TASK:~%The tests"))
(defparameter *approve* (format nil "Fine.~%VERDICT: APPROVED"))

(define-test worktree-parts-merged
    :parent agent-duet
  (with-git-folder (repo)
    (multiple-value-bind (claude codex)
        (worktree-agents repo
                         (lambda (agent prompt)
                           (cond ((search "Split the work" prompt) *split-plan*)
                                 ((search "Produce one combined answer" prompt) "Combined answer")
                                 ((search "Review the work" prompt) *approve*)
                                 (t (write-file-text (ai:agent-directory agent) "a.txt" "from Claude") "Wrote a.txt")))
                         (lambda (agent prompt)
                           (cond ((search "Review the work" prompt) *approve*)
                                 (t (write-file-text (ai:agent-directory agent) "b.txt" "from ChatGPT") "Wrote b.txt"))))
      (multiple-value-bind (workflow messages) (run-worktrees claude codex)
        (is eq :finished (ai:workflow-state workflow))
        ;; Each worked in its own worktree, told so, in a new conversation there.
        (let ((part (find "Your part" (fake-prompts claude) :test #'search)))
          (true (search "own git worktree" part))
          (true (search "agent-duet/claude-" part)))
        ;; Each reviewer was shown the other's diff.
        (true (find "from ChatGPT" (fake-prompts claude) :test #'search))
        (true (find "from Claude" (fake-prompts codex) :test #'search))
        ;; Both parts are in the folder, merged; the worktrees are gone.
        (is string= "from Claude" (read-file-text repo "a.txt"))
        (is string= "from ChatGPT" (read-file-text repo "b.txt"))
        (is = 1 (count-if (lambda (l) (uiop:string-prefix-p "worktree " l))
                          (uiop:split-string (ai::worktree-git repo "worktree" "list" "--porcelain")
                                             :separator '(#\Newline))))
        (is string= "" (ai::worktree-git repo "branch" "--list" "agent-duet/*"))
        (is equal repo (ai:agent-directory claude))
        (is equal repo (ai:agent-directory codex))
        ;; Then the merged result was reviewed.
        (is equal '("Claude's part" "ChatGPT's part" "the merged result")
            (mapcar #'ai::loop-label (ai:workflow-loops workflow)))
        (true (every (lambda (l) (eq :approved (ai::loop-state l))) (ai:workflow-loops workflow)))
        (true (search "git diff" (first (fake-prompts codex))))
        (true (find "Merged agent-duet/codex-" messages :key #'third :test #'search))))))

(define-test worktree-conflict-resolved
    :parent agent-duet
  (with-git-folder (repo)
    (multiple-value-bind (claude codex)
        (worktree-agents repo
                         (lambda (agent prompt)
                           (let ((here (ai:agent-directory agent)))
                             (cond ((search "Split the work" prompt) *split-plan*)
                                 ((search "Produce one combined answer" prompt) "Combined answer")
                                   ((search "Review the work" prompt) *approve*)
                                   ((search "conflicted in" prompt)
                                    (true (search "shared.txt" prompt))
                                    (write-file-text here "shared.txt" "both")
                                    (ai::worktree-git here "add" "-A")
                                    (ai::worktree-git here "commit" "--no-edit")
                                    "Kept both")
                                   (t (write-file-text here "shared.txt" "Claude's") "Changed shared.txt"))))
                         (lambda (agent prompt)
                           (cond ((search "Review the work" prompt) *approve*)
                                 (t (write-file-text (ai:agent-directory agent) "shared.txt" "ChatGPT's")
                                    "Changed shared.txt"))))
      (multiple-value-bind (workflow messages) (run-worktrees claude codex)
        (is eq :finished (ai:workflow-state workflow))
        (true (find "conflicts in shared.txt" messages :key #'third :test #'search))
        (true (find "Kept both" messages :key #'third :test #'search))
        (is string= "both" (read-file-text repo "shared.txt"))
        (false (ai::merge-in-progress-p repo))
        (is eq :approved (ai::loop-state (third (ai:workflow-loops workflow))))))))

(define-test worktree-conflict-unresolved
    :parent agent-duet
  ;; The lead doesn't conclude the merge: it ends there, saying so.
  (with-git-folder (repo)
    (multiple-value-bind (claude codex)
        (worktree-agents repo
                         (lambda (agent prompt)
                           (cond ((search "Split the work" prompt) *split-plan*)
                                 ((search "Produce one combined answer" prompt) "Combined answer")
                                 ((search "Review the work" prompt) *approve*)
                                 ((search "conflicted in" prompt) "I looked")
                                 (t (write-file-text (ai:agent-directory agent) "shared.txt" "Claude's") "Done")))
                         (lambda (agent prompt)
                           (cond ((search "Review the work" prompt) *approve*)
                                 (t (write-file-text (ai:agent-directory agent) "shared.txt" "ChatGPT's") "Done"))))
      (multiple-value-bind (workflow messages) (run-worktrees claude codex)
        (is eq :finished (ai:workflow-state workflow))
        (true (find "the merge is still unresolved" messages :key #'third :test #'search))
        (is = 2 (length (ai:workflow-loops workflow)))
        (false (ai:workflow-continuable-p workflow))))))

(define-test worktree-needs-clean-folder
    :parent agent-duet
  (with-git-folder (repo)
    (write-file-text repo "stray.txt" "uncommitted")
    (multiple-value-bind (claude codex)
        (worktree-agents repo (lambda (a p) (declare (ignore a p)) *split-plan*)
                         (lambda (a p) (declare (ignore a p)) "no"))
      (multiple-value-bind (workflow messages) (run-worktrees claude codex)
        (is eq :finished (ai:workflow-state workflow))
        (true (find "uncommitted changes" messages :key #'third :test #'search))
        (false (fake-prompts claude)))))
  (let ((folder (merge-pathnames (format nil "duet-plain-~a/" (ai::make-uuid)) (uiop:temporary-directory))))
    (ensure-directories-exist folder)
    (unwind-protect
         (is string= "the folder isn't a git repository" (ai::worktrees-problem folder))
      (uiop:delete-directory-tree folder :validate t :if-does-not-exist :ignore))))

(define-test worktree-stopped-keeps-work
    :parent agent-duet
  ;; Stop returns agents home but retains worktrees and resumable assignments.
  (with-git-folder (repo)
    (multiple-value-bind (claude codex)
        (worktree-agents repo
                         (lambda (agent prompt)
                           (cond ((search "Split the work" prompt) *split-plan*)
                                 ((search "Produce one combined answer" prompt) "Combined answer")
                                 (t (write-file-text (ai:agent-directory agent) "a.txt" "kept") "Wrote a.txt")))
                         (lambda (a p) (declare (ignore a p)) "never")
                         :codex-class 'hanging-agent)
      (let* ((workflow (run-worktrees claude codex))
             (trees (mapcar #'cdr (ai::workflow-trees workflow)))
             (notes nil))
        (is = 2 (length trees))
        (setf (ai::workflow-on-message workflow)
              (lambda (from to text kind)
                (declare (ignore from to kind))
                (push text notes)))
        (ai:stop-workflow workflow)
        (is = 1 (count "The worktrees are kept" notes :test #'search))
        (false (find "recovery refs" notes :test #'search))
        (false (ai::workflow-recovery-recorded workflow))
        (is equal repo (ai:agent-directory claude))
        (is equal repo (ai:agent-directory codex))
        (true (every #'probe-file trees))
        (true (some (lambda (tree) (probe-file (merge-pathnames "a.txt" tree))) trees))
        (false (probe-file (merge-pathnames "a.txt" repo)))))))

(define-test worktree-merge-aborted
    :parent agent-duet
  ;; The lead aborts the merge instead of concluding it: nothing is taken
  ;; as merged, and the branches stay.
  (with-git-folder (repo)
    (multiple-value-bind (claude codex)
        (worktree-agents repo
                         (lambda (agent prompt)
                           (let ((here (ai:agent-directory agent)))
                             (cond ((search "Split the work" prompt) *split-plan*)
                                 ((search "Produce one combined answer" prompt) "Combined answer")
                                   ((search "Review the work" prompt) *approve*)
                                   ((search "conflicted in" prompt) (ai::worktree-git here "merge" "--abort") "Aborted")
                                   (t (write-file-text here "shared.txt" "Claude's") "Done"))))
                         (lambda (agent prompt)
                           (cond ((search "Review the work" prompt) *approve*)
                                 (t (write-file-text (ai:agent-directory agent) "shared.txt" "ChatGPT's") "Done"))))
      (multiple-value-bind (workflow messages) (run-worktrees claude codex)
        (is eq :finished (ai:workflow-state workflow))
        (true (find "wasn't merged" messages :key #'third :test #'search))
        (is = 2 (length (ai:workflow-loops workflow)))
        (is = 2 (length (uiop:split-string (string-trim '(#\Newline) (ai::worktree-git repo "branch" "--list" "agent-duet/*"))
                                           :separator '(#\Newline))))))))

(define-test worktree-failed-part-not-merged
    :parent agent-duet
  ;; A part whose turn fails after changing files, or whose review fails:
  ;; nothing is committed or merged, the worktrees are kept, and the
  ;; agents are back in the folder.
  (dolist (failing '(:work :review))
    (with-git-folder (repo)
      (multiple-value-bind (claude codex)
          (worktree-agents repo
                           (lambda (agent prompt)
                             (cond ((search "Split the work" prompt) *split-plan*)
                                 ((search "Produce one combined answer" prompt) "Combined answer")
                                   ((search "Review the work" prompt) *approve*)
                                   (t (write-file-text (ai:agent-directory agent) "a.txt" "half done")
                                      (if (eq failing :work) '(:error "It broke") "Wrote a.txt"))))
                           (lambda (agent prompt)
                             (cond ((search "Review the work" prompt)
                                    (if (eq failing :review) '(:error "Review broke") *approve*))
                                   (t (write-file-text (ai:agent-directory agent) "b.txt" "done") "Wrote b.txt"))))
        (multiple-value-bind (workflow messages) (run-worktrees claude codex)
          (true (ai::workflow-paused workflow))
          (false (find "recovery refs" messages :key #'third :test #'search))
          (false (ai::workflow-recovery-recorded workflow))
          (is eq :running (ai:workflow-state workflow))
          (is eq :paused (ai::loop-state (first (ai:workflow-loops workflow))))
          (true (ai::workflow-paused workflow))
          (is = 2 (length (ai:workflow-loops workflow)))
          (false (probe-file (merge-pathnames "a.txt" repo)))
          (false (probe-file (merge-pathnames "b.txt" repo)))
          (is string= "" (ai::worktree-git repo "status" "--porcelain"))
          (let ((trees (mapcar #'cdr (ai::workflow-trees workflow))))
            (true (every #'probe-file trees))
            (true (some (lambda (tree) (probe-file (merge-pathnames "a.txt" tree))) trees)))
          ;; Nothing was committed in them either.
          (true (every (lambda (e) (plusp (length (ai::worktree-git (cdr e) "status" "--porcelain"))))
                       (ai::workflow-trees workflow)))
          (true (member (ai:agent-directory claude) (mapcar #'cdr (ai::workflow-trees workflow)) :test #'equal))
          (true (member (ai:agent-directory codex) (mapcar #'cdr (ai::workflow-trees workflow)) :test #'equal)))))))

(define-test worktree-written-results
    :parent agent-duet
  ;; A part's answer matters even when it leaves no files. Check both a
  ;; wholly text-only task and one with a single part that changes files.
  (dolist (files '(nil t))
    (with-git-folder (repo)
      (let ((base (ai::worktree-git-line repo "rev-parse" "HEAD"))
            (combined "Combined answer: use the function and the test strategy.")
            (reviewed nil))
        (multiple-value-bind (claude codex)
            (worktree-agents
             repo
             (lambda (agent prompt)
               (cond ((search "Split the work" prompt) *split-plan*)
                     ((search "Produce one combined answer" prompt)
                      (true (search "Function explanation" prompt))
                      (true (search "Test strategy" prompt))
                      (true (search "ChatGPT's part (no file changes)" prompt))
                      (true (search (if files "Claude's part (file changes)"
                                       "Claude's part (no file changes)") prompt))
                      combined)
                     ((search "Review the work" prompt) *approve*)
                     (t (when files (write-file-text (ai:agent-directory agent) "a.txt" "function"))
                        "Function explanation")))
             (lambda (agent prompt)
               (declare (ignore agent))
               (cond ((search "Review the work" prompt)
                      (when (search "Both parts' final replies follow" prompt)
                        (setf reviewed t)
                        (true (search combined prompt))
                        (true (search "Function explanation" prompt))
                        (true (search "Test strategy" prompt))
                        (true (search "nonempty Git diff is not required" prompt))
                        ;; Both temporary worktrees and all four refs still
                        ;; exist at the time the merged result is reviewed.
                        (is = 3 (count-if (lambda (line) (uiop:string-prefix-p "worktree " line))
                                          (uiop:split-string (ai::worktree-git repo "worktree" "list" "--porcelain")
                                                             :separator '(#\Newline))))
                        (is = 4 (length (remove "" (uiop:split-string
                                                    (ai::worktree-git repo "for-each-ref" "--format=%(refname)"
                                                                      "refs/agent-duet/tasks/")
                                                    :separator '(#\Newline)) :test #'equal))))
                      *approve*)
                     (t "Test strategy"))))
          (multiple-value-bind (workflow messages) (run-worktrees claude codex)
            (true reviewed)
            (is eq :approved (ai::loop-state (third (ai:workflow-loops workflow))))
            (is string= combined (ai::loop-summary (third (ai:workflow-loops workflow))))
            (true (find combined messages :key #'third :test #'equal))
            (false (ai::worktree-recovery-refs repo (ai::workflow-retention-id workflow)))
            (false (some (lambda (entry) (probe-file (cdr entry))) (ai::workflow-trees workflow)))
            (unless files
              (is string= base (ai::worktree-git-line repo "rev-parse" "HEAD")))
            (is string= "" (ai::worktree-git repo "status" "--porcelain"))))))))

(define-test worktree-unapproved-results-retained
    :parent agent-duet
  (with-git-folder (repo)
    (let ((ai::*default-max-rounds* 1)
          (approve-merged nil))
      (multiple-value-bind (claude codex)
          (worktree-agents
           repo
           (lambda (agent prompt)
             (declare (ignore agent))
             (cond ((search "Split the work" prompt) *split-plan*)
                   ((search "Produce one combined answer" prompt) "Combined written result")
                   ((search "full revised combined answer" prompt) "Full revised answer with both parts")
                   ((search "Review the work" prompt) *approve*)
                   (t "Lead written result")))
           (lambda (agent prompt)
             (declare (ignore agent))
             (if (search "Review the work" prompt)
                 (if (and (search "Both parts' final replies follow" prompt) (not approve-merged))
                     (format nil "Explain the answer more clearly.~%VERDICT: CHANGES REQUESTED")
                     *approve*)
                 "Partner written result")))
        (multiple-value-bind (workflow messages) (run-worktrees claude codex)
          (true (find "worktrees and branches are kept" messages :key #'third :test #'search))
          (true (find (ai::workflow-retention-id workflow) messages :key #'third :test #'search))
          (is eq :unresolved (ai::loop-state (third (ai:workflow-loops workflow))))
          (true (every (lambda (entry) (probe-file (cdr entry))) (ai::workflow-trees workflow)))
          (let ((refs (ai::worktree-recovery-refs repo (ai::workflow-retention-id workflow))))
            (is = 2 (length refs))
            (true (every (lambda (entry) (not (getf entry :changed))) refs)))
          (true (ai:workflow-continuable-p workflow))
          ;; Cleanup must not depend on the display label or the folder existing.
          (setf (ai::loop-label (ai::workflow-merged-loop workflow)) "Combined answer")
          (uiop:delete-directory-tree (cdar (ai::workflow-trees workflow)) :validate t)
          (setf approve-merged t)
          (true (ai:continue-workflow workflow 1))
          (is string= "Full revised answer with both parts"
              (ai::loop-summary (ai::workflow-merged-loop workflow)))
          (is string= "" (ai::worktree-git repo "branch" "--list" "agent-duet/*"))
          (is eq :approved (ai::loop-state (third (ai:workflow-loops workflow))))
          (false (some (lambda (entry) (probe-file (cdr entry))) (ai::workflow-trees workflow)))
          (false (ai::worktree-recovery-refs repo (ai::workflow-retention-id workflow))))))))

(define-test worktree-reconciliation-failure-keeps-results
    :parent agent-duet
  (with-git-folder (repo)
    (multiple-value-bind (claude codex)
        (worktree-agents
         repo
         (lambda (agent prompt)
           (declare (ignore agent))
           (cond ((search "Split the work" prompt) *split-plan*)
                 ((search "Produce one combined answer" prompt) '(:error "Could not combine"))
                 ((search "Review the work" prompt) *approve*)
                 (t "Lead answer preserved")))
         (lambda (agent prompt)
           (declare (ignore agent))
           (if (search "Review the work" prompt) *approve* "Partner answer preserved")))
      (multiple-value-bind (workflow messages) (run-worktrees claude codex)
        (let ((merged (third (ai:workflow-loops workflow))))
          (true (find "worktrees and branches are kept" messages :key #'third :test #'search))
          (true (find (ai::workflow-retention-id workflow) messages :key #'third :test #'search))
          (is eq :running (ai:workflow-state workflow))
          (is eq :paused (ai::loop-state merged))
          (true (search "Lead answer preserved" (ai::loop-summary merged)))
          (true (search "Partner answer preserved" (ai::loop-summary merged)))
          (true (every (lambda (entry) (probe-file (cdr entry))) (ai::workflow-trees workflow)))
          (is = 2 (length (ai::worktree-recovery-refs repo (ai::workflow-retention-id workflow)))))))))


(define-test split-results-reply-limit :parent agent-duet
  (let* ((ai::*review-diff-limit* 12)
         (workflow (make-instance 'ai:workflow))
         (reply "A long written answer that must remain available")
         (part (ai::make-review-loop :label "Part" :tree #p"/tmp/part/" :summary reply)))
    (setf (ai:workflow-loops workflow) (list part))
    (let ((text (ai::split-results-text workflow)))
      (true (search (subseq reply 0 12) text))
      (false (search reply text))
      (true (search "Final reply cut short" text))
      (is string= reply (ai::loop-summary part)))))

(define-test worktree-stopped-after-merge-reports-retention :parent agent-duet
  (with-git-folder (repo)
    (multiple-value-bind (claude codex)
        (worktree-agents repo
                         (lambda (agent prompt)
                           (declare (ignore agent))
                           (cond ((search "Split the work" prompt) *split-plan*)
                                 ((search "Produce one combined answer" prompt) :out-of-usage)
                                 ((search "Review the work" prompt) *approve*)
                                 (t "Lead result")))
                         (lambda (agent prompt)
                           (declare (ignore agent))
                           (if (search "Review the work" prompt) *approve* "Partner result")))
      (let ((workflow (run-worktrees claude codex))
            (notes nil))
        (true (ai::workflow-merged-loop workflow))
        (setf (ai::workflow-on-message workflow)
              (lambda (from to text kind)
                (declare (ignore from to kind))
                (push text notes)))
        (ai:stop-workflow workflow)
        (true (find "worktrees and branches are kept" notes :test #'search))
        (true (find (ai::workflow-retention-id workflow) notes :test #'search))
        (dolist (entry (ai::workflow-trees workflow))
          (true (find (namestring (cdr entry)) notes :test #'search)))
        (is = 2 (length (ai::worktree-recovery-refs repo (ai::workflow-retention-id workflow))))))))

(define-test combined-answer-survives-check-fix :parent agent-duet
  (let* ((author (make-instance 'fake-agent :id :claude :name "Claude"
                                :replies (lambda (prompt)
                                           (if (search "full revised combined answer" prompt)
                                               "Complete combined answer after fixing the check"
                                               "Only an edit summary"))))
         (reviewer (make-instance 'fake-agent :id :codex :name "ChatGPT"
                                  :replies (list (says *approve*))))
         (part (ai::make-review-loop :author author :reviewer reviewer :combined-result t
                                     :task "Combine both answers" :summary "Original combined answer"))
         (workflow (make-instance 'ai:workflow :lead author :partner reviewer))
         (token (list nil)))
    (setf (ai:workflow-loops workflow) (list part)
          (ai::workflow-state workflow) :running
          (ai::loop-check-handle part) token)
    (ai::check-in workflow part token "test" 0 1 "Failure" nil)
    (is string= "Complete combined answer after fixing the check" (ai::loop-summary part))
    (true (search "Complete combined answer after fixing the check" (first (fake-prompts reviewer))))))

;;; Review-only mode: no writing turn, even when changes are requested.

(defun review-only-fixture (repo &key (agents :both) (target :uncommitted) branch (base "HEAD") (count 1) reply)
  (let* ((claude (make-instance 'fake-agent :id :claude :name "Claude" :directory repo))
         (codex (make-instance 'fake-agent :id :codex :name "ChatGPT" :directory repo))
         (workflow (make-instance 'ai:workflow :mode :review-only :lead claude :partner codex
                                  :request "Check regressions" :review-agents agents :review-target target
                                  :review-branch branch :review-base base :review-count count
                                  :check-command "must never run")))
    (dolist (agent (list claude codex))
      (let ((agent agent))
        (setf (ai:agent-permission agent) :full
              (fake-replies agent)
              (list (lambda (prompt)
                      (true (ai::agent-read-only-p agent))
                      (true (search "Review only" prompt))
                      (if reply (funcall reply agent prompt)
                          (format nil "FINDINGS:~%- F1 [open] `shared.txt:1` Fix bug~%VERDICT: CHANGES REQUESTED")))))))
    (values workflow claude codex)))

(define-test review-only-both
    :parent agent-duet
  (with-git-folder (repo)
    (write-file-text repo "untracked.txt" "untracked")
    (let ((status (ai::worktree-git repo "status" "--porcelain")))
      (multiple-value-bind (workflow claude codex) (review-only-fixture repo)
        (ai:start-workflow workflow)
        (is eq :finished (ai:workflow-state workflow))
        (is = 2 (length (ai::workflow-loops workflow)))
        (dolist (agent (list claude codex))
          (is = 1 (length (fake-prompts agent)))
          (false (ai::agent-read-only-p agent)))
        (is string= status (ai::worktree-git repo "status" "--porcelain"))
        (false (ai::workflow-continuable-p workflow))
        (true (search "Claude's review" (ai::review-fix-request workflow)))
        (true (search "ChatGPT's review" (ai::review-fix-request workflow)))
        (true (every (lambda (l) (= 1 (ai::count-findings (ai::loop-findings l) :open)))
                     (ai::workflow-loops workflow)))))))

(define-test review-only-selection-and-failure
    :parent agent-duet
  (with-git-folder (repo)
    (dolist (choice '(:lead :partner))
      (multiple-value-bind (workflow claude codex)
          (review-only-fixture repo :agents choice :reply (lambda (agent prompt) (declare (ignore agent prompt)) '(:error "failed")))
        (ai:start-workflow workflow)
        (is = 1 (length (ai::workflow-loops workflow)))
        (is = (if (eq choice :lead) 1 0) (length (fake-prompts claude)))
        (is = (if (eq choice :partner) 1 0) (length (fake-prompts codex)))
        (is eq (eq choice :lead) (ai::agent-read-only-p claude))
        (is eq (eq choice :partner) (ai::agent-read-only-p codex))
        (is eq :paused (ai::loop-state (first (ai::workflow-loops workflow))))))))

(define-test review-only-pause-stop-restores-access
    :parent agent-duet
  (with-git-folder (repo)
    (multiple-value-bind (workflow claude codex)
        (review-only-fixture repo :agents :lead :reply (lambda (a p) (declare (ignore a p)) :out-of-usage))
      (ai:start-workflow workflow)
      (true (ai::workflow-paused workflow))
      (true (ai::agent-read-only-p claude))
      (ai:stop-workflow workflow)
      (false (ai::agent-read-only-p claude))
      (false (ai::agent-read-only-p codex)))))

(define-test review-only-target-ranges
    :parent agent-duet
  (with-git-folder (repo)
    (let ((base (ai::worktree-git-line repo "rev-parse" "HEAD")))
      (ai::worktree-git repo "branch" "review-topic")
      (write-file-text repo "main.txt" "main change")
      (ai::commit-worktree repo "main change")
      (let ((tip (ai::worktree-git-line repo "rev-parse" "HEAD")))
        (ai::worktree-git repo "checkout" "review-topic")
        (write-file-text repo "topic.txt" "topic change")
        (ai::commit-worktree repo "topic change")
        (dolist (target '(:branch :branch-tips :commits))
          (multiple-value-bind (workflow claude codex)
              (review-only-fixture repo :target target :branch "HEAD" :base tip :agents :lead)
            (declare (ignore claude codex))
            (let ((description (ai::review-target-description workflow)))
              (true (search (if (eq target :branch-tips) tip base) description)))))
        (multiple-value-bind (workflow claude codex)
            (review-only-fixture repo :target :branch :branch "--bad-ref")
          (ai:start-workflow workflow)
          (true (ai::workflow-completion-error workflow))
          (false (fake-prompts claude))
          (false (fake-prompts codex)))))))

(define-test review-only-root-and-approval
    :parent agent-duet
  (with-git-folder (repo)
    (multiple-value-bind (workflow claude codex)
        (review-only-fixture repo :target :commits :agents :lead
                             :reply (lambda (a p) (declare (ignore a p)) "FINDINGS: NONE
VERDICT: APPROVED"))
      (declare (ignore codex))
      (setf (ai::agent-read-only-p claude) t)
      (ai:start-workflow workflow)
      (is eq :approved (ai::loop-state (first (ai::workflow-loops workflow))))
      (true (ai::agent-read-only-p claude))
      (false (ai::review-fix-request workflow)))))

(define-test review-only-independent-delayed-replies
    :parent agent-duet
  (with-git-folder (repo)
    (let* ((claude (make-instance 'hanging-agent :id :claude :name "Claude" :directory repo :replies nil))
           (codex (make-instance 'hanging-agent :id :codex :name "ChatGPT" :directory repo :replies nil))
           (workflow (make-instance 'ai:workflow :mode :review-only :lead claude :partner codex :request "Review")))
      (ai:start-workflow workflow)
      (ai:agent-turn-finished codex (format nil "FINDINGS:~%- F1 [open] Codex issue~%VERDICT: CHANGES REQUESTED") nil)
      (is eq :running (ai:workflow-state workflow))
      (ai:agent-turn-finished claude "FINDINGS: NONE
VERDICT: APPROVED" nil)
      (is eq :finished (ai:workflow-state workflow))
      (is eq :approved (ai::loop-state (first (ai::workflow-loops workflow))))
      (is eq :unresolved (ai::loop-state (second (ai::workflow-loops workflow))))
      (false (ai::agent-read-only-p claude))
      (false (ai::agent-read-only-p codex)))))

;;; Recovery must retain progress and require a deliberate user action.
(defun recovery-fixture ()
  (let* ((lead (make-instance 'hanging-agent :id :claude :name "Claude" :replies nil))
         (partner (make-instance 'fake-agent :id :codex :name "ChatGPT" :replies nil))
         (workflow (make-instance 'ai:workflow :lead lead :partner partner :request "Recover me")))
    (ai:start-workflow workflow)
    (values workflow lead partner)))

(define-test recovery-checkpoint-roundtrip :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (let* ((step (first (ai::workflow-steps workflow)))
           (l (first (ai:workflow-loops workflow))))
      (setf (ai::loop-summary l) "Written progress must survive"
            (ai::loop-round l) 2
            (ai::loop-findings l) (list (ai::make-finding :id 7 :status :open :text "Pending" :location "a:1")))
      (let* ((checkpoint (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow))))
             (restored (ai:restore-workflow checkpoint :lead lead :partner partner)))
        (is eq :paused (ai:workflow-state restored))
        (is = 1 (length (fake-prompts lead)))
        (is = 1 (length (ai::workflow-paused restored)))
        (is string= (ai::step-id step) (ai::step-id (first (ai::workflow-paused restored))))
        (is string= "Written progress must survive" (ai::loop-summary (first (ai:workflow-loops restored))))
        (is = 2 (ai::loop-round (first (ai:workflow-loops restored))))
        (is = 7 (ai::finding-id (first (ai::loop-findings (first (ai:workflow-loops restored))))))
        (fail (ai:restore-workflow checkpoint) error)))))

(define-test recovery-pause-stale-result-and-handoff :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (let ((old-callback (ai::agent-callback lead)))
      (ai:pause-workflow-agent workflow lead)
      (is eq :interrupted (ai::step-reason (first (ai::workflow-paused workflow))))
      (funcall old-callback "Late completed work" nil)
      (is string= "" (ai::loop-summary (first (ai:workflow-loops workflow))))
      (is eq :pending-shutdown (ai:recover-workflow workflow :hand-off))
      (false (fake-prompts partner))
      (ai:agent-turn-finished lead "Stopped" :interrupted)
      (setf (fake-replies partner) (list (says "Recovered implementation")))
      (is eq :started (ai:recover-workflow workflow :hand-off))
      (let ((l (first (ai:workflow-loops workflow))))
        (is eq :unreviewed (ai::loop-state l))
        (is string= "Recovered implementation" (ai::loop-summary l)))
      (true (search "Original assignment" (first (fake-prompts partner))))
      (funcall old-callback "Stale overwrite" nil)
      (is string= "Recovered implementation" (ai::loop-summary (first (ai:workflow-loops workflow)))))))

(define-test recovery-stop-roundtrip-does-not-execute :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (ai:stop-workflow workflow)
    (is eq :stopped (ai:workflow-state workflow))
    (true (ai::workflow-paused workflow))
    (let* ((checkpoint (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow))))
           (restored (ai:restore-workflow checkpoint :lead lead :partner partner)))
      (ai:recover-workflow restored :review)
      (is = 1 (length (fake-prompts lead)))
      (is eq :pending-shutdown (ai:recover-workflow restored :resume))
      (ai:agent-turn-finished lead "Stopped" :interrupted)
      (is eq :started (ai:recover-workflow restored :resume))
      (is = 2 (length (fake-prompts lead)))
      (true (search "Inspect existing files" (first (fake-prompts lead)))))))

(define-test recovery-review-failure-retains-implementation :parent agent-duet
  (multiple-value-bind (workflow messages lead partner)
      (run-fake :review (list (says "Completed implementation")) (list (says '(:error "Disconnected"))))
    (declare (ignore messages))
    (is eq :review (ai::step-role (first (ai::workflow-paused workflow))))
    (is eq :failed (ai::step-reason (first (ai::workflow-paused workflow))))
    (setf (fake-replies partner) (list (says "VERDICT: APPROVED")))
    (is eq :started (ai:recover-workflow workflow :resume))
    (is = 1 (length (fake-prompts lead)))
    (is eq :approved (ai::loop-state (first (ai:workflow-loops workflow))))))

(define-test recovery-both-unavailable :parent agent-duet
  (multiple-value-bind (workflow messages lead partner)
      (run-fake :review (list (says :out-of-usage)) nil)
    (declare (ignore messages))
    (ai::agent-ran-out lead (+ (get-universal-time) 1000))
    (ai::agent-ran-out partner (+ (get-universal-time) 1000))
    (is eq :unavailable (ai:recover-workflow workflow :resume))
    (is eq :unavailable (ai:recover-workflow workflow :hand-off))
    (true (ai::workflow-paused workflow))
    (is = 2 (length (ai:jget (ai:inspect-workflow-recovery workflow) "agents")))))

(define-test recovery-dependencies-and-cycles :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (let* ((prerequisite (first (ai:workflow-loops workflow)))
           (dependent (ai::make-review-loop :author partner :reviewer lead :task "Dependent")))
      (setf (ai:workflow-loops workflow) (list prerequisite dependent))
      (true (ai:set-workflow-dependencies workflow (list (cons dependent (list prerequisite)))))
      (ai::loop-work workflow dependent "Do dependent work")
      (false (fake-prompts partner))
      (is eq :blocked (ai:recover-workflow workflow :resume))
      (fail (ai:set-workflow-dependencies workflow (list (cons prerequisite (list dependent)))) error)
      (false (ai::loop-dependencies prerequisite))
      (is equal (list prerequisite) (ai::loop-dependencies dependent))
      (setf (ai::loop-state prerequisite) :approved
            (fake-replies partner) (list (says :out-of-usage)))
      (ai::check-finished workflow)
      (is = 1 (length (fake-prompts partner)))
      (is eq :out-of-usage (ai::step-reason (first (ai::workflow-paused workflow)))))))

(define-test recovery-preserves-streamed-progress :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (ai::agent-emit lead :delta :text "Partial reasoning and edits")
    (ai:pause-workflow-agent workflow lead)
    (is string= "Partial reasoning and edits" (ai::step-progress (first (ai::workflow-paused workflow))))
    (let ((restored (ai:restore-workflow (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow)))
                                        :lead lead :partner partner)))
      (is string= "Partial reasoning and edits" (ai::step-progress (first (ai::workflow-paused restored)))))))

(define-test recovery-waits-for-editing-child :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (let ((child (sb-ext:run-program "/bin/sleep" '("10") :wait nil)))
      (unwind-protect
           (progn
             (ai:pause-workflow-agent workflow lead)
             (ai:agent-turn-finished lead "Stopped" :interrupted)
             (setf (ai::agent-editing-pids lead) (list (sb-ext:process-pid child)))
             (is eq :pending-shutdown (ai:recover-workflow workflow :hand-off))
             (false (fake-prompts partner))
             (sb-ext:process-kill child 15)
             (sb-ext:process-wait child)
             (true (ai:agent-quiescent-p lead)))
        (when (sb-ext:process-alive-p child) (sb-ext:process-kill child 15))))))

(define-test recovery-changed-prerequisite-invalidates-dependent :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (let* ((prerequisite (first (ai:workflow-loops workflow)))
           (dependent (ai::make-review-loop :author partner :reviewer lead :task "Dependent"
                                            :state :approved :summary "Previously approved")))
      (setf (ai:workflow-loops workflow) (list prerequisite dependent)
            (ai::loop-dependencies dependent) (list prerequisite)
            (ai::loop-revision prerequisite) 1)
      (ai::invalidate-dependent-loops workflow prerequisite)
      (is eq :paused (ai::loop-state dependent))
      (is eq :prerequisite-changed (ai::step-reason (first (ai::workflow-paused workflow))))
      (is eq :blocked (ai:recover-workflow workflow :resume))
      (is string= "Previously approved" (ai::loop-summary dependent)))))

(define-test recovery-old-stop-cannot-cancel-new-work :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (declare (ignore partner))
    (ai:stop-workflow workflow)
    (ai:agent-turn-finished lead "Stopped" :interrupted)
    (ai:agent-submit lead "Unrelated later work")
    (ai:stop-workflow workflow)
    (true (ai:agent-busy lead))
    (true (car (ai::agent-turn-token lead)))))

(define-test recovery-dependency-diamond-invalidates-once :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (let* ((a (first (ai:workflow-loops workflow)))
           (b (ai::make-review-loop :author lead :reviewer partner :task "B" :state :approved :dependencies (list a)))
           (c (ai::make-review-loop :author partner :reviewer lead :task "C" :state :approved :dependencies (list a)))
           (d (ai::make-review-loop :author partner :reviewer lead :task "D" :state :approved :dependencies (list b c))))
      (setf (ai:workflow-loops workflow) (list a b c d))
      (ai::invalidate-dependent-loops workflow a)
      (is = 1 (count d (ai::workflow-paused workflow) :key #'ai::step-loop))
      (is eq :paused (ai::loop-state d))
      ;; Further prerequisite changes update the same paused assignment.
      (ai::invalidate-dependent-loops workflow a)
      (is = 1 (count d (ai::workflow-paused workflow) :key #'ai::step-loop)))))

;;; Review regressions: real process lifetimes, checkpoints and explicit recovery.
(define-test recovery-handoff-resolve-preserves-target :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (ai:pause-workflow-agent workflow lead)
    (ai:agent-turn-finished lead "Stopped" :interrupted)
    (let* ((step (first (ai::workflow-paused workflow)))
           (calls nil))
      (setf (ai::step-role step) :resolve
            (ai::step-target step) "saved-merge-object"
            (ai::step-then step) (lambda (text error) (push (list text error) calls))
            (fake-replies partner) (list (says "Resolved")))
      (is eq :started (ai:recover-workflow workflow :hand-off :step step))
      (is string= "saved-merge-object" (ai::step-target (first (ai::workflow-steps workflow))))
      (is equal '(("Resolved" nil)) calls))))

(define-test recovery-checkpoints-do-not-inspect-repositories :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (declare (ignore lead partner))
    (let ((old (symbol-function 'ai::workflow-repositories))
          (old-ps (symbol-function 'ai::remember-agent-editors))
          (calls 0))
      (unwind-protect
           (progn
             (setf (symbol-function 'ai::workflow-repositories)
                   (lambda (w) (declare (ignore w)) (incf calls) (error "Checkpoint ran Git"))
                   (symbol-function 'ai::remember-agent-editors)
                   (lambda (a) (declare (ignore a)) (incf calls) (error "Checkpoint ran ps")))
             (dotimes (i 5) (ai:json-string (ai:workflow-checkpoint workflow)))
             (is = 0 calls))
        (setf (symbol-function 'ai::workflow-repositories) old
              (symbol-function 'ai::remember-agent-editors) old-ps)))))

(define-test recovery-restores-fix-assignment :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (let* ((l (first (ai:workflow-loops workflow)))
           (checkpoint nil))
      (setf (ai::loop-last-review l) "Fix the boundary condition"
            (ai::loop-round l) 1
            (ai::workflow-steps workflow) nil)
      (setf checkpoint (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow))))
      (let ((restored (ai:restore-workflow checkpoint :lead lead :partner partner)))
        (true (search "Fix the boundary condition" (ai::step-prompt (first (ai::workflow-paused restored)))))
        (true (search "Address the points" (ai::step-prompt (first (ai::workflow-paused restored)))))))))

(defclass steering-observer (hanging-agent)
  ((steers :initform nil :accessor observer-steers)))
(defmethod ai::agent-steer ((agent steering-observer) prompt &optional images)
  (declare (ignore images))
  (push prompt (observer-steers agent)))

(define-test recovery-pause-does-not-steer-peer :parent agent-duet
  (let* ((lead (make-instance 'hanging-agent :id :claude :name "Claude" :replies nil))
         (peer (make-instance 'steering-observer :id :codex :name "ChatGPT" :replies nil))
         (w (make-instance 'ai:workflow :lead lead :partner peer :request "Work")))
    (ai:start-workflow w)
    (ai:agent-submit peer "Independent work")
    (ai:pause-workflow-agent w lead)
    (false (observer-steers peer))
    (ai:stop-workflow w)
    (false (observer-steers peer))))

(defclass process-owning-agent (hanging-agent)
  ((process :initarg :process :accessor owned-process)))
(defmethod ai::agent-cli-process ((agent process-owning-agent)) (owned-process agent))

(define-test recovery-persistent-cli-child-is-not-a-turn-editor :parent agent-duet
  (let* ((root (sb-ext:run-program "/bin/sh" '("-c" "sleep 30 & echo ready; wait") :wait nil :output :stream))
         (agent (make-instance 'process-owning-agent :id :claude :name "Claude" :replies nil :process root)))
    (unwind-protect
         (progn
           (is string= "ready" (read-line (sb-ext:process-output root)))
           (ai::capture-agent-process-baseline agent)
           (true (ai::agent-turn-process-baseline agent))
           (ai:agent-submit agent "Work")
           (ai:agent-cancel agent)
           (ai:agent-turn-finished agent "Stopped" :interrupted)
           (false (ai::agent-editing-processes agent))
           (true (ai:agent-quiescent-p agent)))
      (dolist (record (ai::agent-turn-process-baseline agent))
        (when (ai::recovery-process-alive-p record) (ignore-errors (sb-posix:kill (ai:jget record "pid") 15))))
      (when (sb-ext:process-alive-p root) (sb-ext:process-kill root 15))
      (sb-ext:process-wait root))))

(define-test recovery-process-identity-rejects-reused-pid :parent agent-duet
  (let ((record (ai::recovery-process-record (sb-posix:getpid))))
    (true (ai::recovery-process-alive-p record))
    (setf (cdr (assoc "identity" (cdr record) :test #'equal)) "an earlier process with this PID")
    (false (ai::recovery-process-alive-p record))
    (multiple-value-bind (workflow lead partner) (recovery-fixture)
      (declare (ignore lead partner))
      (setf (ai::workflow-shutdown-processes workflow) (list record))
      (true (ai::workflow-old-processes-stopped-p workflow))
      (let ((checkpoint (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow)))))
        (is = 0 (length (ai:jget checkpoint "shutdown-pids")))
        (is = 1 (length (ai:jget checkpoint "shutdown-processes")))))))

(define-test recovery-turn-owned-child-is-stopped-and-cleared :parent agent-duet
  (let* ((root (sb-ext:run-program "/bin/sh" '("-c" "echo ready; read command; sleep 30 & echo editing; wait")
                                   :wait nil :input :stream :output :stream))
         (agent (make-instance 'process-owning-agent :id :claude :name "Claude" :replies nil :process root)))
    (unwind-protect
         (progn
           (is string= "ready" (read-line (sb-ext:process-output root)))
           (ai::capture-agent-process-baseline agent)
           (false (ai::agent-turn-process-baseline agent))
           (ai:agent-submit agent "Work")
           (write-line "go" (sb-ext:process-input root))
           (finish-output (sb-ext:process-input root))
           (is string= "editing" (read-line (sb-ext:process-output root)))
           (ai:agent-cancel agent)
           (true (ai::agent-editing-processes agent))
           (true (wait-until (lambda () (not (sb-ext:process-alive-p root)))))
           (sb-ext:process-wait root)
           (ai:agent-turn-finished agent "Stopped" :interrupted)
           (true (ai:agent-quiescent-p agent))
           (false (ai::agent-editing-processes agent)))
      (dolist (record (ai::agent-editing-processes agent))
        (when (ai::recovery-process-alive-p record) (ignore-errors (sb-posix:kill (ai:jget record "pid") 15))))
      (when (sb-ext:process-alive-p root) (sb-ext:process-kill root 15))
      (sb-ext:process-wait root))))

(define-test recovery-checkpoint-excludes-finished-steps :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (declare (ignore partner))
    (dotimes (i 100)
      (push (ai::make-step :id (format nil "old-~d" i) :agent lead
                           :role :work :state (if (evenp i) :completed :superseded) :prompt "Past prompt"
                           :progress "Past reply")
            (ai::workflow-steps workflow)))
    (let* ((checkpoint (ai::json-copy (ai:workflow-checkpoint workflow)))
           (steps (ai::jlist (ai:jget checkpoint "steps"))))
      (is = 1 (length steps))
      (false (search "Past prompt" (ai:json-string checkpoint))))))

(define-test recovery-pause-and-stop-snapshot-off-caller-thread :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (declare (ignore partner))
    (let ((old (symbol-function 'ai::workflow-repositories))
          (caller sb-thread:*current-thread*)
          (threads nil))
      (unwind-protect
           (progn
             (setf (symbol-function 'ai::workflow-repositories)
                   (lambda (w)
                     (push sb-thread:*current-thread* threads)
                     (funcall old w)))
             (true (ai:pause-workflow-agent workflow lead))
             (ai:stop-workflow workflow)
             (ai::await-recovery-captures workflow)
             (is eq :stopped (ai:workflow-state workflow))
             (true threads)
             (false (member caller threads)))
        (ai::await-recovery-captures workflow)
        (setf (symbol-function 'ai::workflow-repositories) old)))))

(define-test recovery-precomputed-inspection :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (declare (ignore partner))
    (ai:pause-workflow-agent workflow lead)
    (let ((inspection (ai:inspect-workflow-recovery workflow))
          (old (symbol-function 'ai:inspect-workflow-recovery)))
      (unwind-protect
           (progn
             (setf (symbol-function 'ai:inspect-workflow-recovery)
                   (lambda (w) (declare (ignore w)) (error "Repeated repository inspection")))
             (is eq :pending-shutdown
                 (ai:recover-workflow workflow :resume :inspection inspection)))
        (setf (symbol-function 'ai:inspect-workflow-recovery) old)))))

(define-test recovery-pending-snapshot-does-not-block-checkpoint :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (declare (ignore partner))
    (let ((old (symbol-function 'ai::workflow-repositories))
          (entered (sb-thread:make-semaphore))
          (release (sb-thread:make-semaphore)))
      (unwind-protect
           (progn
             (setf (symbol-function 'ai::workflow-repositories)
                   (lambda (w)
                     (sb-thread:signal-semaphore entered)
                     (sb-thread:wait-on-semaphore release)
                     (funcall old w)))
             (ai:pause-workflow-agent workflow lead)
             (true (sb-thread:wait-on-semaphore entered :timeout 10))
             (is eq t (ai:jget (ai:workflow-checkpoint workflow) "repository-snapshot-pending"))
             (true (ai:json-string (ai:workflow-checkpoint workflow)))
             (is eq :pending-shutdown
                 (ai:recover-workflow workflow :resume :inspection (ai::jobj "repositories" #()))))
        (sb-thread:signal-semaphore release)
        (ai::await-recovery-captures workflow)
        (setf (symbol-function 'ai::workflow-repositories) old)))))

;;; Saved before its snapshot was ready: reopened, the card says the
;;; comparison may include the agents' own edits, until a fresh baseline.
(define-test recovery-pending-snapshot-survives-restart :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (let ((old (symbol-function 'ai::workflow-repositories))
          (entered (sb-thread:make-semaphore))
          (release (sb-thread:make-semaphore))
          (checkpoint nil))
      (unwind-protect
           (progn
             (setf (symbol-function 'ai::workflow-repositories)
                   (lambda (w)
                     (sb-thread:signal-semaphore entered)
                     (sb-thread:wait-on-semaphore release)
                     (funcall old w)))
             (ai:pause-workflow-agent workflow lead)
             (true (sb-thread:wait-on-semaphore entered :timeout 10))
             (ai:agent-turn-finished lead "Stopped" :interrupted)
             (setf checkpoint (ai::json-copy (ai:workflow-checkpoint workflow))))
        (setf (symbol-function 'ai::workflow-repositories) old)
        (sb-thread:signal-semaphore release)
        (ai::await-recovery-captures workflow))
      (is eq t (ai:jget checkpoint "repository-snapshot-pending"))
      (let* ((restored (ai:restore-workflow checkpoint :lead lead :partner partner))
             (inspection (ai:inspect-workflow-recovery restored)))
        (is eq t (ai:jget inspection "repository-snapshot-pending"))
        (true (getf (ai::inspection-summary inspection checkpoint) :approximate))
        ;; Saved again before anything is carried on, it stays marked.
        (is eq t (ai:jget (ai:workflow-checkpoint restored) "repository-snapshot-pending"))
        (is eq :started (ai:recover-workflow restored :resume :acknowledge-changes t))
        (is eq :false (ai:jget (ai:workflow-checkpoint restored) "repository-snapshot-pending"))
        (ai:stop-workflow restored))
      (ai:stop-workflow workflow))))

;;; Transport failures must not depend on EOF and process death arriving together.
(defun exercise-disconnected-workflow (agent directory mode)
  (touch directory "disconnect")
  (when mode (touch directory mode))
  (let* ((peer (make-instance 'hanging-agent :id :peer :name "Peer" :replies nil))
         (workflow (make-instance 'ai:workflow :lead agent :partner peer :request "Keep partial work"))
         (pauses 0))
    (setf (ai::workflow-on-pause workflow) (lambda (w s) (declare (ignore w s)) (incf pauses)))
    (ai:start-workflow workflow)
    (true (wait-until (lambda () (plusp pauses)) 2))
    (let ((step (first (ai::workflow-paused workflow))))
      (true step "A disconnect must preserve the interrupted step")
      (when step
        (is eq :disconnected (ai::step-reason step))
        (true (or (search "23" (ai::step-progress step))
                  (search "connection closed" (ai::step-progress step))) (or mode "exit"))
        (is string= "partial work" (uiop:read-file-string (merge-pathnames "partial.txt" directory)))
        (false (ai:agent-busy agent))
        (true (wait-until (lambda () (ai::agent-quiescent-p agent)) 2))
        (is = 1 pauses)
        (delete-file (merge-pathnames "disconnect" directory))
        (is eq :started (ai:recover-workflow workflow :resume :step step))
        (true (wait-until (lambda () (not (ai:agent-busy agent)))))
        (false (ai::workflow-paused workflow))))
    (ai:stop-workflow workflow)))

(define-test recovery-transport-exit-and-inherited-output :parent agent-duet
  (dolist (mode '(nil "child-output"))
    (with-fake-claude (agent directory notes)
      (exercise-disconnected-workflow agent directory mode))
    (with-fake-codex (agent directory)
      (exercise-disconnected-workflow agent directory mode))))

(define-test recovery-transport-eof-before-exit :parent agent-duet
  (with-fake-claude (agent directory notes)
    (touch directory "close-output")
    (touch directory "disconnect")
    (let ((results nil))
      (ai:agent-submit agent "work" (lambda (text error) (push (list text error) results)))
      (true (wait-until (lambda () results) 2))
      (is eq :disconnected (second (first results)))
      (true (wait-until (lambda () (ai::agent-quiescent-p agent)) 2))))
  (with-fake-codex (agent directory)
    (touch directory "close-output")
    (touch directory "disconnect")
    (let ((results nil))
      (ai:agent-submit agent "work" (lambda (text error) (push (list text error) results)))
      (true (wait-until (lambda () results) 2))
      (is eq :disconnected (second (first results)))
      (true (wait-until (lambda () (ai::agent-quiescent-p agent)) 2)))))

(define-test recovery-disconnect-blocks-dependent-work :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (let* ((prerequisite (first (ai:workflow-loops workflow)))
           (dependent (ai::make-review-loop :author partner :reviewer lead :task "Dependent"
                                            :state :approved :summary "Keep result"
                                            :dependencies (list prerequisite)))
           (notes '()))
      (push dependent (ai:workflow-loops workflow))
      ;; It had started against the prerequisite's first revision.
      (setf (ai::loop-prerequisite-revisions dependent) (list (cons prerequisite (ai::loop-revision prerequisite)))
            (ai::workflow-on-message workflow) (lambda (from to text kind)
                                                (declare (ignore from to kind)) (push text notes)))
      (ai::agent-emit lead :delta :text "Partial reply")
      (ai:agent-turn-finished lead "Connection closed" :disconnected)
      (is eq :paused (ai::loop-state prerequisite))
      (true (find "Partial reply" (ai::workflow-paused workflow)
                  :key #'ai::step-progress :test (lambda (a b) (and b (search a b)))))
      ;; Blocked, not invalidated: its result and state are as they were.
      (is eq :approved (ai::loop-state dependent))
      (false (find dependent (ai::workflow-paused workflow) :key #'ai::step-loop))
      (true (find "is blocked until" notes :test #'search))
      (is string= "Keep result" (ai::loop-summary dependent))
      ;; Its prerequisite's work is redone: then it revalidates.
      (is eq :started (ai:recover-workflow workflow :resume))
      (is eq :paused (ai::loop-state dependent))
      (let ((step (find dependent (ai::workflow-paused workflow) :key #'ai::step-loop)))
        (is eq :prerequisite-changed (ai::step-reason step))
        (is eq :blocked (ai:recover-workflow workflow :resume :step step)))
      (is string= "Keep result" (ai::loop-summary dependent))
      (ai:stop-workflow workflow))))

(define-test recovery-disconnect-clears-permission :parent agent-duet
  (with-fake-codex (agent directory)
    (touch directory "request-approval")
    (let ((results nil))
      (ai:agent-submit agent "work" (lambda (text error) (push (list text error) results)))
      (true (wait-until (lambda () (ai::agent-approvals agent))))
      (sb-ext:process-kill (ai::agent-cli-process agent) 15)
      (true (wait-until (lambda () results)))
      (is eq :disconnected (second (first results)))
      (false (ai::agent-approvals agent))
      (false (ai:agent-busy agent)))))

(define-test recovery-disconnect-fails-queued-assignments :parent agent-duet
  (with-fake-codex (agent directory)
    (touch directory "await-steer")
    (let ((results nil))
      (ai:agent-submit agent "first" (lambda (text error) (declare (ignore text)) (push error results)))
      (true (wait-until (lambda () (member "turn/start" (fake-log directory) :test #'equal))))
      (ai:agent-submit agent "queued" (lambda (text error) (declare (ignore text)) (push error results)))
      (sb-ext:process-kill (ai::agent-cli-process agent) 15)
      (true (wait-until (lambda () (= 2 (length results)))))
      (is equal '(:disconnected :disconnected) results)
      (false (ai::agent-queue agent))
      (is = 1 (count "turn/start" (fake-log directory) :test #'equal)))))

(define-test recovery-disconnect-settles-stall-retry :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (declare (ignore partner))
    (let ((step (first (ai::workflow-steps workflow))))
      (true (ai::retry-stalled-turn workflow lead))
      (true (ai::agent-retry-pending-p lead))
      (ai::agent-transport-lost lead "Connection closed during interrupt")
      (is eq :dropped (ai::agent-retry lead))
      (false (ai::agent-retry-pending-p lead))
      (false (ai:agent-busy lead))
      (false (ai::agent-queue lead))
      (true (member step (ai::workflow-paused workflow)))
      (is eq :disconnected (ai::step-reason step))
      (ai:stop-workflow workflow))))

(define-test codex-exit-disposes-process-streams :parent agent-duet
  (dolist (mode '(nil "child-output" "close-output" :shutdown))
    (with-fake-codex (agent directory)
      (let* ((exits 0)
             (server (ai::start-codex-app-server
                      (fake-codex-program) directory
                      :on-exit (lambda (code) (declare (ignore code)) (incf exits))))
             (process (ai::codex-process server))
             (streams (list (sb-ext:process-input process) (sb-ext:process-output process)
                            (sb-ext:process-error process))))
        (unwind-protect
            (progn
              (ai::codex-start-thread server)
              (if (eq mode :shutdown)
                  (ai::stop-codex-app-server server)
                  (progn
                    (touch directory "disconnect")
                    (when mode (touch directory mode))
                    (ai::codex-start-turn server "exit")
                    (true (wait-until (lambda () (= 1 exits)) 2))
                    ;; EOF can precede process exit by an arbitrary interval.
                    (when (equal mode "close-output")
                      (true (sb-ext:process-alive-p process))
                      (ai::stop-codex-app-server server))))
              (true (wait-until (lambda () (every (lambda (stream) (not (open-stream-p stream))) streams)) 5)
                    "All process streams must close for ~s" mode)
              (is = (if (eq mode :shutdown) 0 1) exits))
          (ai::stop-codex-app-server server))))))

(define-test recovery-idle-transport-loss-is-quiet :parent agent-duet
  (with-fake-claude (agent directory notes)
    (let ((finished nil))
      (ai:agent-submit agent "finish" (lambda (text error) (declare (ignore text error)) (setf finished t)))
      (true (wait-until (lambda () finished)))
      (let ((cp (ai::claude-agent-process agent)))
        (sb-ext:process-kill (ai::cp-process cp) 15)
        (true (wait-until (lambda () (null (ai::claude-agent-process agent)))))
        (false notes)))))

;;; Ordered split plans, changed prerequisites and mutual waiting

(defun run-ordered (claude-replies codex-replies &key (rounds 3) approve-plan)
  "A :divide workflow like RUN-FAKE, also collecting notes, pauses and plans offered."
  (let* ((claude (make-instance 'fake-agent :id :claude :name "Claude" :replies claude-replies))
         (codex (make-instance 'fake-agent :id :codex :name "ChatGPT" :replies codex-replies))
         (log (list :notes '() :paused '() :plans '()))
         (workflow (make-instance 'ai:workflow :mode :divide :lead claude :partner codex
                                  :request "Add a frobnicate function" :max-rounds rounds
                                  :approve-plan approve-plan
                                  :on-plan (lambda (w lead partner) (declare (ignore w))
                                             (push (list lead partner) (getf log :plans)))
                                  :on-pause (lambda (w step) (declare (ignore w)) (push step (getf log :paused)))
                                  :on-message (lambda (from to text kind)
                                                (declare (ignore from))
                                                (when (eq kind :note) (push (list to text) (getf log :notes)))))))
    (ai:start-workflow workflow)
    (values workflow claude codex log)))

(defun noted (log text &optional to)
  "Whether a note containing TEXT was posted (to TO, if given: an agent, or nil for the user)."
  (find-if (lambda (n) (and (search text (second n)) (or (null to) (eq to (first n)))))
           (getf log :notes)))

(defun reviews (&rest verdicts)
  "A reply function for reviews: VERDICTS in turn (:approve or :changes), then approvals."
  (lambda (prompt)
    (declare (ignore prompt))
    (if (eq (pop verdicts) :changes)
        (format nil "Still wrong.~%VERDICT: CHANGES REQUESTED")
        *approve*)))

(defparameter *lead-first-plan* (format nil "LEAD TASK:~%The function~%~%PARTNER TASK:~%The tests~%~%ORDER: LEAD FIRST"))

(define-test ordered-plan-lead-first
    :parent agent-duet
  (multiple-value-bind (workflow claude codex log)
      (run-ordered (responder "Split the work" *lead-first-plan* "Review the work" *approve* "Wrote the function")
                   (responder "Review the work" *approve* "Wrote the tests"))
    (is eq :finished (ai:workflow-state workflow))
    (destructuring-bind (mine theirs) (ai:workflow-loops workflow)
      (is equal (list mine) (ai::loop-dependencies theirs))
      (false (ai::loop-dependencies mine))
      (true (every (lambda (l) (eq :approved (ai::loop-state l))) (list mine theirs)))
      ;; ChatGPT's part started against the first revision of Claude's.
      (is equal (list (cons mine 1)) (ai::loop-prerequisite-revisions theirs)))
    ;; ChatGPT reviewed Claude's part before doing its own, and was given its report.
    (let ((prompts (reverse (fake-prompts codex))))
      (true (search "Review the work" (first prompts)))
      (true (search "Your part builds on work that is now done" (second prompts)))
      (true (search "Wrote the function" (second prompts)))
      (true (search "Your part builds on Claude's part" (second prompts))))
    (true (find "ChatGPT's part builds on yours" (fake-prompts claude) :test #'search))
    (true (noted log "ChatGPT's work on ChatGPT's part waits for Claude's part"))
    ;; A planned wait isn't a pause for the user.
    (false (getf log :paused))))

(define-test ordered-plan-approval
    :parent agent-duet
  (multiple-value-bind (workflow claude codex log)
      (run-ordered (responder "Split the work" (format nil "LEAD TASK:~%A~%~%PARTNER TASK:~%B~%~%ORDER: PARTNER FIRST")
                              "Review the work" *approve* "Did A")
                   (responder "Review the work" *approve* "Did B")
                   :approve-plan t)
    (declare (ignore claude))
    (is = 1 (length (getf log :plans)))
    (is eq :partner-first (ai:pending-plan-order workflow))
    (false (ai:pending-plan-error workflow))
    (let ((checkpoint (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow)))))
      (is string= "partner-first" (ai:jget checkpoint "pending-plan" "order"))
      (false (ai::jtrue-p (ai:jget checkpoint "pending-plan" "error"))))
    ;; Each first, both ways: nothing starts, and the plan says why.
    (false (ai:approve-plan workflow "A" "B" :order :circular))
    (true (ai:pending-plan-error workflow))
    (false (fake-prompts codex))
    (is eq :parallel (ai:pending-plan-order workflow))
    (is string= (ai:pending-plan-error workflow)
        (ai:jget (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow))) "pending-plan" "error"))
    ;; The order chosen on the card wins over the lead's.
    (true (ai:approve-plan workflow "A" "B" :order :lead-first))
    (false (ai:workflow-pending-plan workflow))
    (is eq :finished (ai:workflow-state workflow))
    (destructuring-bind (mine theirs) (ai:workflow-loops workflow)
      (is equal (list mine) (ai::loop-dependencies theirs))
      (false (ai::loop-dependencies mine))))
  ;; Approved as proposed, the plan's order holds.
  (multiple-value-bind (workflow claude codex)
      (run-ordered (responder "Split the work" (format nil "LEAD TASK:~%A~%~%PARTNER TASK:~%B~%~%ORDER: PARTNER FIRST")
                              "Review the work" *approve* "Did A")
                   (responder "Review the work" *approve* "Did B")
                   :approve-plan t)
    (declare (ignore codex))
    (true (ai:approve-plan workflow "A" "B"))
    (destructuring-bind (mine theirs) (ai:workflow-loops workflow)
      (is equal (list theirs) (ai::loop-dependencies mine)))
    (true (find "Did B" (fake-prompts claude) :test #'search))
    (is eq :finished (ai:workflow-state workflow))))

(define-test ordered-plan-circular
    :parent agent-duet
  (let ((circular (format nil "LEAD TASK:~%A~%~%PARTNER TASK:~%B~%~%ORDER: LEAD FIRST AND PARTNER FIRST")))
    ;; Without approval, the lead revises it once.
    (multiple-value-bind (workflow claude codex log)
        (run-ordered (responder "can't be started" *lead-first-plan* "Split the work" circular
                                "Review the work" *approve* "Wrote the function")
                     (responder "Review the work" *approve* "Wrote the tests"))
      (declare (ignore log))
      (true (find "each part would wait for the other" (fake-prompts claude) :test #'search))
      (is eq :finished (ai:workflow-state workflow))
      (is = 2 (length (ai:workflow-loops workflow)))
      (true (fake-prompts codex)))
    ;; Circular again: it waits for the user, saying why, with nothing started.
    (multiple-value-bind (workflow claude codex log)
        (run-ordered (responder "Split the work" circular) nil)
      (is = 2 (length (fake-prompts claude)))
      (false (fake-prompts codex))
      (false (ai:workflow-loops workflow))
      (is = 1 (length (getf log :plans)))
      (true (search "each part would wait" (ai:pending-plan-error workflow)))
      (is eq :parallel (ai:pending-plan-order workflow))
      ;; A restart keeps the order and the reason.
      (let ((restored (ai:restore-workflow (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow)))
                                           :lead claude :partner codex)))
        (true (search "each part would wait" (ai:pending-plan-error restored)))
        (is eq :parallel (ai:pending-plan-order restored))))))

(define-test prerequisite-change-revalidates-dependent
    :parent agent-duet
  (progn
    (multiple-value-bind (workflow claude codex log)
        (run-ordered (responder "Split the work" *lead-first-plan* "Review the work" *approve* "Wrote the function")
                     (responder "Review the work" *approve* "changed after you started" "Revalidated the tests"
                                "Wrote the tests"))
      (destructuring-bind (mine theirs) (ai:workflow-loops workflow)
        (is eq :finished (ai:workflow-state workflow))
        ;; Claude's part is reworked after ChatGPT's was done against it.
        (setf (ai:workflow-state workflow) :running
              (ai::loop-check-handle theirs) (list nil))
        (setf (fake-replies claude)
              (let ((old (fake-replies claude)))
                (lambda (prompt) (if (search "Rework" prompt) :hang (funcall old prompt)))))
        (ai::loop-work workflow mine "Rework the function")
        (is = 2 (ai::loop-revision mine))
        (true (ai::loop-stale-p theirs))
        ;; ChatGPT is told; its check is cancelled; its part waits to revalidate.
        (true (noted log "has changed" codex))
        (false (ai::loop-check-handle theirs))
        (is eq :paused (ai::loop-state theirs))
        (let ((step (find theirs (ai::workflow-paused workflow) :key #'ai::step-loop)))
          (is eq :prerequisite-changed (ai::step-reason step))
          (true (search "Wrote the tests" (ai::step-prompt step)))
          (is eq :blocked (ai:recover-workflow workflow :resume :step step)))
        (is string= "Wrote the tests" (ai::loop-summary theirs))
        ;; Claude's rework is done and approved: ChatGPT revalidates, given the change.
        (ai:agent-turn-finished claude "Reworked the function" nil)
        (let ((prompt (find "changed after you started" (fake-prompts codex) :test #'search)))
          (true (search "Reworked the function" prompt))
          (true (search "changed after you started" prompt)))
        (is string= "Revalidated the tests" (ai::loop-summary theirs))
        (false (ai::loop-stale-p theirs))
        (is eq :approved (ai::loop-state theirs))
        (is eq :finished (ai:workflow-state workflow))))))

(define-test failed-prerequisite-hand-off-prompt
    :parent agent-duet
  (multiple-value-bind (workflow paused claude codex)
      (run-pausing :review (list (says '(:error "Lost the network"))) (list (says "Took it over")))
    (declare (ignore claude))
    (is eq :failed (ai::step-reason (first paused)))
    (is eq :started (ai:recover-workflow workflow :hand-off))
    (let ((prompt (first (fake-prompts codex))))
      (true (search "Claude failed partway through a task" prompt))
      (true (search "Original assignment" prompt))
      (true (search "Add frobnicate" prompt))
      (true (search "Reason: FAILED" prompt :test #'char-equal))
      (true (search "Lost the network" prompt))
      (true (search "Current repository inspection" prompt))
      (true (search "since Claude is unavailable" prompt)))
    (is eq :unreviewed (ai::loop-state (first (ai:workflow-loops workflow))))))

(defun deadlocked-workflow (claude-replies)
  "Claude's part goes first but isn't approved in its one round, so ChatGPT's waits for ever."
  (run-ordered claude-replies
               (let ((review (reviews :changes)))
                 (lambda (prompt)
                   (cond ((search "Review the work" prompt) (funcall review prompt))
                         (t "Wrote the tests"))))
               :rounds 1))

(define-test deadlock-run-independently
    :parent agent-duet
  (multiple-value-bind (workflow claude codex log)
      (deadlocked-workflow (responder "Split the work" *lead-first-plan* "Review the work" *approve* "Wrote the function"))
    (declare (ignore claude))
    (destructuring-bind (mine theirs) (ai:workflow-loops workflow)
      (is eq :running (ai:workflow-state workflow))
      (is eq :unresolved (ai::loop-state mine))
      (let ((step (first (ai::workflow-paused workflow))))
        (is eq :deadlock (ai::step-reason step))
        (is eq theirs (ai::step-loop step))
        ;; The user is asked, once.
        (is equal (list step) (getf log :paused))
        (is eq :blocked (ai:recover-workflow workflow :resume)))
      (true (noted log "No part can go on"))
      (is string= "deadlock" (ai:jget (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow)))
                                      "steps" 0 "reason"))
      (is eq :started (ai:recover-workflow workflow :run-independently))
      (false (ai::loop-dependencies theirs))
      (true (search "go on independently" (first (fake-prompts codex))))
      (is eq :approved (ai::loop-state theirs))
      (is eq :finished (ai:workflow-state workflow))
      (is eq :blocked (ai:recover-workflow workflow :run-independently)))))

(define-test deadlock-revise-order
    :parent agent-duet
  (multiple-value-bind (workflow claude codex log)
      (deadlocked-workflow (responder "no part can go on" (format nil "LEAD TASK:~%The function, fixed~%~%PARTNER TASK:~%The tests~%~%ORDER: PARTNER FIRST")
                                      "Split the work" *lead-first-plan* "Review the work" *approve*
                                      "Wrote the function"))
    (declare (ignore log))
    (destructuring-bind (mine theirs) (ai:workflow-loops workflow)
      (is eq :deadlock (ai::step-reason (first (ai::workflow-paused workflow))))
      (is eq :started (ai:recover-workflow workflow :revise-order))
      ;; The lead was given both parts' progress.
      (let ((prompt (find "no part can go on" (fake-prompts claude) :test #'search)))
        (true (search "Wrote the function" prompt))
        (true (search "Still wrong" prompt))
        (true (search "The tests" prompt)))
      (is equal (list theirs) (ai::loop-dependencies mine))
      (false (ai::loop-dependencies theirs))
      (is string= "The function, fixed" (ai::loop-task mine))
      ;; ChatGPT's went first; then Claude's carried on, given its result.
      (true (find "Wrote the tests" (fake-prompts claude) :test #'search))
      (true (search "your part changed" (first (fake-prompts claude))))
      (true (every (lambda (l) (eq :approved (ai::loop-state l))) (list mine theirs)))
      (is eq :finished (ai:workflow-state workflow))
      (true codex))))

(define-test deadlock-revised-order-still-circular
    :parent agent-duet
  (multiple-value-bind (workflow claude codex log)
      (deadlocked-workflow (responder "no part can go on" (format nil "LEAD TASK:~%A~%~%PARTNER TASK:~%B~%~%ORDER: CIRCULAR")
                                      "Split the work" *lead-first-plan* "Review the work" *approve*
                                      "Wrote the function"))
    (declare (ignore claude codex))
    (is eq :started (ai:recover-workflow workflow :revise-order))
    (true (noted log "The revised plan can't go on"))
    (let ((step (first (ai::workflow-paused workflow))))
      (is eq :deadlock (ai::step-reason step))
      (true (member step (getf log :paused))))
    (destructuring-bind (mine theirs) (ai:workflow-loops workflow)
      (is equal (list mine) (ai::loop-dependencies theirs)))))

(define-test order-recovery-checks-repositories
    :parent agent-duet
  (dolist (repository (list (ai:jobj "path" "/nowhere" "available" :false)
                            (ai:jobj "path" "/merging" "available" t "operation" t)))
    (multiple-value-bind (workflow claude codex)
        (deadlocked-workflow (responder "Split the work" *lead-first-plan* "Review the work" *approve* "Wrote the function"))
      ;; The pause snapshot is done, so only the repository can block it.
      (ai::await-recovery-captures workflow)
      (let ((prompts (length (fake-prompts codex)))
            (inspection (ai:jobj "repositories" (vector repository) "repository-changed" :false)))
        (destructuring-bind (mine theirs) (ai:workflow-loops workflow)
          (dolist (action '(:run-independently :revise-order))
            (is eq :blocked (ai:recover-workflow workflow action :inspection inspection)))
          ;; Nothing changed or started.
          (is equal (list mine) (ai::loop-dependencies theirs))
          (is = prompts (length (fake-prompts codex)))
          (false (find "no part can go on" (fake-prompts claude) :test #'search))
          (is eq :deadlock (ai::step-reason (first (ai::workflow-paused workflow)))))))))

(define-test revised-order-prepares-ready-dependent
    :parent agent-duet
  (multiple-value-bind (workflow claude codex)
      (run-ordered (responder "Split the work" *lead-first-plan* "Review the work" *approve* "Wrote the function")
                   (responder "Review the work" *approve* "Wrote the tests"))
    (declare (ignore claude))
    (destructuring-bind (mine theirs) (ai:workflow-loops workflow)
      (is eq :finished (ai:workflow-state workflow))
      (setf (ai:workflow-state workflow) :running)
      (let ((before (length (fake-prompts codex))))
        ;; Claude's part is unchanged and done; ChatGPT's changes and starts at once.
        (ai::order-revised workflow (format nil "LEAD TASK:~%The function~%~%PARTNER TASK:~%The tests, more of them~%~%ORDER: LEAD FIRST"))
        (let ((prompt (nth (- (length (fake-prompts codex)) before 1) (fake-prompts codex))))
          (true (search "your part changed" prompt))
          (true (search "Your part builds on work that is now done" prompt))
          (true (search "Wrote the function" prompt))))
      (is equal (list (cons mine (ai::loop-revision mine))) (ai::loop-prerequisite-revisions theirs)))))

(define-test revalidation-keeps-partial-reply
    :parent agent-duet
  (multiple-value-bind (workflow claude codex)
      (run-ordered (responder "Split the work" *lead-first-plan* "Review the work" *approve* "Wrote the function")
                   (lambda (prompt)
                     (cond ((search "Review the work" prompt) *approve*)
                           ((search "changed after you started" prompt) "Revalidated the tests")
                           (t :hang))))
    (destructuring-bind (mine theirs) (ai:workflow-loops workflow)
      (is eq :approved (ai::loop-state mine))
      (is eq :working (ai::loop-state theirs))
      (setf (ai::agent-partial-text codex) "UNFINISHED DESIGN DETAILS")
      (setf (fake-replies claude)
            (let ((old (fake-replies claude)))
              (lambda (prompt) (if (search "Rework" prompt) :hang (funcall old prompt)))))
      (ai::loop-work workflow mine "Rework the function")
      ;; ChatGPT's cancelled turn ends, as a real one does once interrupted.
      (ai:agent-turn-finished codex "" :interrupted)
      (let ((step (find theirs (ai::workflow-paused workflow) :key #'ai::step-loop)))
        (is eq :prerequisite-changed (ai::step-reason step))
        (true (search "UNFINISHED DESIGN DETAILS" (ai::step-prompt step)))
        (is string= "UNFINISHED DESIGN DETAILS" (ai::step-progress step)))
      ;; Started again once the rework is in, it's given what it had written.
      (ai:agent-turn-finished claude "Reworked the function" nil)
      (let ((prompt (find "changed after you started" (fake-prompts codex) :test #'search)))
        (true (search "UNFINISHED DESIGN DETAILS" prompt))
        (true (search "Reworked the function" prompt))))))

(defun jset (object key value)
  (setf (cdr (assoc key (cdr object) :test #'string=)) value))

(define-test restore-validates-dependencies
    :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (let* ((prerequisite (first (ai:workflow-loops workflow)))
           (dependent (ai::make-review-loop :author partner :reviewer lead :task "Dependent" :state :approved)))
      (setf (ai:workflow-loops workflow) (list prerequisite dependent))
      (ai:set-workflow-dependencies workflow (list (list dependent prerequisite)))
      (setf (ai::loop-prerequisite-revisions dependent) (list (cons prerequisite 1)))
      (flet ((checkpoint () (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow))))
             (restore (checkpoint) (ai:restore-workflow checkpoint :lead lead :partner partner)))
        (let ((restored (restore (checkpoint))))
          (destructuring-bind (a b) (ai:workflow-loops restored)
            (is equal (list a) (ai::loop-dependencies b))
            (is equal (list (cons a 1)) (ai::loop-prerequisite-revisions b))
            (is = (ai::loop-revision prerequisite) (ai::loop-revision a))))
        ;; Waiting for itself, for each other, or for a part that isn't there.
        (dolist (edges (list (list #(0) #()) (list #(1) #(0)) (list #() #(5))))
          (let ((checkpoint (checkpoint)))
            (jset (ai:jget checkpoint "loops" 0) "dependencies" (first edges))
            (jset (ai:jget checkpoint "loops" 1) "dependencies" (second edges))
            (fail (restore checkpoint) error)))
        ;; A revision recorded for a loop it doesn't depend on.
        (let ((checkpoint (checkpoint)))
          (jset (ai:jget checkpoint "loops" 1) "dependencies" #())
          (fail (restore checkpoint) error))
        ;; An older checkpoint, without revisions, reads as before.
        (let ((checkpoint (checkpoint)))
          (dolist (l (ai::jlist (ai:jget checkpoint "loops")))
            (setf (cdr l) (remove-if (lambda (entry) (member (car entry) '("revision" "prerequisite-revisions")
                                                                :test #'string=))
                                     (cdr l))))
          (setf (cdr checkpoint) (remove-if (lambda (entry) (member (car entry) '("plan-revisions" "prerequisite-merges")
                                                                    :test #'string=))
                                            (cdr checkpoint)))
          (let ((restored (restore checkpoint)))
            (is = 0 (ai::loop-revision (first (ai:workflow-loops restored))))
            (false (ai::loop-prerequisite-revisions (second (ai:workflow-loops restored))))))))))

(define-test stale-dependent-not-integrated
    :parent agent-duet
  (with-git-folder (repo)
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (let* ((prerequisite (first (ai:workflow-loops workflow)))
           (dependent (ai::make-review-loop :author partner :reviewer lead :task "Dependent" :state :approved)))
      (ai:stop-workflow workflow)
      (ai:agent-turn-finished lead "Stopped" :interrupted)
      (setf (ai:workflow-loops workflow) (list prerequisite dependent)
            (ai::workflow-paused workflow) nil
            (ai::loop-state prerequisite) :approved
            (ai::workflow-integration-state workflow) :committing
            (ai::workflow-home workflow) repo)
      (ai:set-workflow-dependencies workflow (list (list dependent prerequisite)))
      (setf (ai::loop-prerequisite-revisions dependent) (list (cons prerequisite 0)))
      (true (ai::loop-stale-p dependent))
      (is eq :blocked (ai::recover-integration workflow))
      (setf (ai::loop-prerequisite-revisions dependent) (list (cons prerequisite (ai::loop-revision prerequisite))))
      (false (ai::loop-stale-p dependent))))))

;;; Ordered parts in their own worktrees

(define-test worktree-dependent-gets-prerequisite
    :parent agent-duet
  (if (not (fboundp 'ai::merge-prerequisite-into-worktree))
      (skip "merge-prerequisite-into-worktree isn't here yet" (true nil))
      (with-git-folder (repo)
        (multiple-value-bind (claude codex)
            (worktree-agents repo
                             (lambda (agent prompt)
                               (cond ((search "Split the work" prompt) *lead-first-plan*)
                                     ((search "Produce one combined answer" prompt) "Combined answer")
                                     ((search "Review the work" prompt) *approve*)
                                     (t (write-file-text (ai:agent-directory agent) "a.txt" "from Claude") "Wrote a.txt")))
                             (lambda (agent prompt)
                               (cond ((search "Review the work" prompt) *approve*)
                                     (t (write-file-text (ai:agent-directory agent) "b.txt"
                                                         (format nil "saw ~a" (ignore-errors (read-file-text (ai:agent-directory agent) "a.txt"))))
                                        "Wrote b.txt"))))
          (multiple-value-bind (workflow messages) (run-worktrees claude codex)
            (declare (ignore messages))
            (is eq :finished (ai:workflow-state workflow))
            (let ((part (find "Your part builds on work that is now done" (fake-prompts codex) :test #'search)))
              (true (search "merged into your worktree" part))
              (true (search "a.txt" part)))
            ;; ChatGPT's part was done with Claude's merged in, recorded once.
            (is string= "saw from Claude" (read-file-text repo "b.txt"))
            (is string= "from Claude" (read-file-text repo "a.txt"))
            (is = 1 (length (ai::workflow-prerequisite-merges workflow)))
            (true (every (lambda (l) (eq :approved (ai::loop-state l))) (ai:workflow-loops workflow))))))))

(define-test worktree-prerequisite-merge-conflict
    :parent agent-duet
  (if (not (fboundp 'ai::merge-prerequisite-into-worktree))
      (skip "merge-prerequisite-into-worktree isn't here yet" (true nil))
      (with-git-folder (repo)
        (let ((paused '()))
          (multiple-value-bind (claude codex)
              (worktree-agents repo
                               (lambda (agent prompt)
                                 (cond ((search "Split the work" prompt) *lead-first-plan*)
                                       ((search "Review the work" prompt) *approve*)
                                       (t (write-file-text (ai:agent-directory agent) "shared.txt" "from Claude")
                                          "Changed shared.txt")))
                               (lambda (agent prompt)
                                 (declare (ignore agent))
                                 (cond ((search "Review the work" prompt) *approve*)
                                       ((search "Explicit recovery" prompt) :hang)
                                       (t "Wrote the tests"))))
            (let ((workflow (make-instance 'ai:workflow :mode :divide :lead claude :partner codex :worktrees t
                                           :request "Add frobnicate"
                                           :on-pause (lambda (w step) (declare (ignore w)) (push step paused)))))
              ;; ChatGPT's worktree already has its own, conflicting, edit when Claude's part is done.
              (setf (fake-replies claude)
                    (let ((old (fake-replies claude)))
                      (lambda (prompt)
                        (unless (search "Split the work" prompt)
                          (write-file-text (ai:agent-directory codex) "shared.txt" "from ChatGPT"))
                        (funcall old prompt))))
              (ai:start-workflow workflow)
              (let* ((theirs (second (ai:workflow-loops workflow)))
                     (step (find theirs (ai::workflow-paused workflow) :key #'ai::step-loop)))
                (is eq :integration-unresolved (ai::step-reason step))
                (true (member step paused))
                (true (search "conflicted in" (ai::step-prompt step)))
                (true (search "shared.txt" (ai::step-prompt step)))
                (true (ai::merge-in-progress-p (ai::loop-tree theirs)))
                (false (ai::workflow-prerequisite-merges workflow))
                ;; Resuming lets its agent resolve it in its worktree.
                (is eq :started (ai:recover-workflow workflow :resume :step step :acknowledge-changes t))
                (true (search "conflicted in" (first (fake-prompts codex)))))
              (ai:stop-workflow workflow)))))))

(define-test worktree-unresolved-prerequisite-merge-not-accepted
    :parent agent-duet
  (with-git-folder (repo)
    (let ((recoveries 0))
      (multiple-value-bind (claude codex)
          (worktree-agents repo
                           (lambda (agent prompt)
                             (cond ((search "Split the work" prompt) *lead-first-plan*)
                                   ((search "Produce one combined answer" prompt) "Combined answer")
                                   ((search "Review the work" prompt) *approve*)
                                   (t (write-file-text (ai:agent-directory agent) "shared.txt" "from Claude")
                                      "Changed shared.txt")))
                           (lambda (agent prompt)
                             (cond ((search "Review the work" prompt) *approve*)
                                   ((search "Explicit recovery" prompt)
                                    (if (= 1 (incf recoveries))
                                        "Could not resolve the merge"
                                        (let ((tree (ai:agent-directory agent)))
                                          (write-file-text tree "shared.txt" "from both")
                                          (ai::worktree-git tree "add" "-A")
                                          (ai::worktree-git tree "commit" "--no-edit")
                                          "Resolved the merge")))
                                   (t "Wrote the tests"))))
        (let ((workflow (make-instance 'ai:workflow :mode :divide :lead claude :partner codex :worktrees t
                                       :request "Add frobnicate")))
          (setf (fake-replies claude)
                (let ((old (fake-replies claude))
                      (written nil))
                  (lambda (prompt)
                    (unless (or written (search "Split the work" prompt))
                      (setf written t)
                      (write-file-text (ai:agent-directory codex) "shared.txt" "from ChatGPT"))
                    (funcall old prompt))))
          (ai:start-workflow workflow)
          (let ((theirs (second (ai:workflow-loops workflow))))
            (flet ((paused-step ()
                     (find theirs (ai::workflow-paused workflow) :key #'ai::step-loop)))
              (is eq :integration-unresolved (ai::step-reason (paused-step)))
              (ai::await-recovery-captures workflow)
              (is eq :started (ai:recover-workflow workflow :resume :step (paused-step) :acknowledge-changes t))
              ;; Its agent didn't conclude the merge: it pauses again, unchecked and unmerged.
              (let ((step (paused-step)))
                (true step)
                (is eq :integration-unresolved (ai::step-reason step))
                (true (search "isn't concluded" (ai::step-prompt step))))
              (is eq :running (ai:workflow-state workflow))
              (false (ai::workflow-prerequisite-merges workflow))
              (true (ai::merge-in-progress-p (ai::loop-tree theirs)))
              ;; Concluded the next time, it's checked, reviewed and merged, with the merge recorded.
              (ai::await-recovery-captures workflow)
              (is eq :started (ai:recover-workflow workflow :resume :step (paused-step) :acknowledge-changes t))
              (is eq :finished (ai:workflow-state workflow))
              (is = 1 (length (ai::workflow-prerequisite-merges workflow)))
              (is string= "from both" (read-file-text repo "shared.txt")))))))))

(define-test dependent-start-waits-for-stopped-checks
    :parent agent-duet
  (multiple-value-bind (workflow claude codex log)
      (run-ordered (responder "Split the work" *lead-first-plan* "Review the work" *approve* :hang)
                   (responder "Review the work" *approve* "Wrote the tests"))
    (let* ((stopped nil)
           (thread (sb-thread:make-thread (lambda () (loop until stopped do (sleep 0.01))))))
      ;; A cancelled check that hasn't stopped yet.
      (push (ai::make-check-handle :thread thread) (ai::workflow-stopping-checks workflow))
      (ai:agent-turn-finished claude "Wrote the function" nil)
      (destructuring-bind (mine theirs) (ai:workflow-loops workflow)
        (is eq :approved (ai::loop-state mine))
        (false (find "Your part builds on" (fake-prompts codex) :test #'search))
        (is eq :dependency (ai::step-reason (find theirs (ai::workflow-paused workflow) :key #'ai::step-loop)))
        (true (noted log "Waiting for stopped work to stop"))
        (setf stopped t)
        (sb-thread:join-thread thread)
        (loop repeat 200 until (eq :finished (ai:workflow-state workflow)) do (sleep 0.05))
        (is eq :finished (ai:workflow-state workflow))
        (true (find "Your part builds on" (fake-prompts codex) :test #'search))
        (is eq :approved (ai::loop-state theirs))))))

(define-test revised-plan-keeps-both-parts
    :parent agent-duet
  ;; Either part given NONE.
  (dolist (plan (list (format nil "LEAD TASK:~%Do the whole task~%~%PARTNER TASK:~%NONE~%~%ORDER: PARALLEL")
                      (format nil "LEAD TASK:~%NONE~%~%PARTNER TASK:~%Do the whole task~%~%ORDER: PARALLEL")))
  (multiple-value-bind (workflow claude codex log)
      (deadlocked-workflow (responder "no part can go on" plan
                                      "Split the work" *lead-first-plan* "Review the work" *approve*
                                      "Wrote the function"))
    (let ((prompts (length (fake-prompts codex))))
      (is eq :started (ai:recover-workflow workflow :revise-order))
      (true (search "neither can be NONE" (find "no part can go on" (fake-prompts claude) :test #'search)))
      (true (noted log "must give both parts a task"))
      (destructuring-bind (mine theirs) (ai:workflow-loops workflow)
        ;; Nothing of it is carried out: the parts wait as they were, for the user.
        (is string= "The function" (ai::loop-task mine))
        (is string= "The tests" (ai::loop-task theirs))
        (is equal (list mine) (ai::loop-dependencies theirs))
        (is eq :deadlock (ai::step-reason (find theirs (ai::workflow-paused workflow) :key #'ai::step-loop))))
      (is = prompts (length (fake-prompts codex)))))))

(define-test claude-indexed-steered-results :parent agent-duet
  ;; The real CLI reports queued_turn_count=0 on BOTH results. The callback
  ;; must remain untouched until the last reply.
  (let ((agent (make-instance 'ai:claude-agent)) (results nil))
    (setf (ai:agent-busy agent) t
          (ai::agent-turn-token agent) (list t)
          (ai::claude-agent-prompts agent) '(("FIRST") ("SECOND"))
          (ai::agent-callback agent) (lambda (text error) (push (list text error) results)))
    (flet ((reply (index text &optional error)
             (first (ai::parse-claude-event
                     (ai:json-string (ai:jobj "type" "result" "subtype" (if error "error" "success")
                                             "result" text "result_index" index "queued_turn_count" 0))))))
      (ai::claude-agent-result agent (reply 0 "FIRST"))
      (true (ai:agent-busy agent))
      (false results)
      ;; A third message may be sent after the first result was received.
      (setf (ai::claude-agent-prompts agent) '(("FIRST") ("SECOND") ("THIRD")))
      (ai::claude-agent-result agent (reply 1 "SECOND"))
      (true (ai:agent-busy agent))
      (false results)
      (ai::claude-agent-result agent (reply 2 "THIRD"))
      (false (ai:agent-busy agent))
      (is equal '(("THIRD" nil)) results)
      ;; The same process continues numbering in the next application turn.
      (setf (ai:agent-busy agent) t
            (ai::agent-turn-token agent) (list t)
            (ai::claude-agent-turn-result-start agent) (ai::claude-agent-next-result-index agent)
            (ai::claude-agent-prompts agent) '(("NEXT") ("STEER"))
            (ai::agent-callback agent) (lambda (text error) (push (list text error) results)))
      (is = 3 (ai::claude-agent-turn-result-start agent))
      (ai::claude-agent-result agent (reply 3 "NEXT"))
      (true (ai:agent-busy agent))
      (is = 1 (length results))
      (ai::claude-agent-result agent (reply 4 "STEER"))
      (false (ai:agent-busy agent))
      (is equal '("STEER" nil) (first results))
      ;; A gap represents a lost result, not a reason to wait forever. The
      ;; next turn's baseline follows the actual index rather than a count.
      (setf (ai:agent-busy agent) t
            (ai::agent-turn-token agent) (list t)
            (ai::claude-agent-turn-result-start agent) (ai::claude-agent-next-result-index agent)
            (ai::agent-callback agent) (lambda (text error) (push (list text error) results)))
      (ai::claude-agent-result agent (reply 6 "AFTER GAP"))
      (false (ai:agent-busy agent))
      (is = 7 (ai::claude-agent-next-result-index agent))
      (is equal '("AFTER GAP" nil) (first results))
      ;; Errors must finish even when more messages were sent.
      (setf (ai:agent-busy agent) t
            (ai::agent-turn-token agent) (list t)
            (ai::agent-callback agent) (lambda (text error) (push (list text error) results)))
      (ai::claude-agent-result agent (reply 7 "failed" t))
      (false (ai:agent-busy agent))
      (is equal '("failed" t) (first results)))))

(define-test claude-indexed-results-across-turns :parent agent-duet
  (with-fake-claude (agent directory notes)
    (touch directory "indexed-results")
    (let ((results nil))
      (ai:agent-submit agent "FIRST" (lambda (text error) (push (list text error) results)))
      (true (wait-until (lambda () (= 1 (length results)))))
      (let ((process (ai::claude-agent-process agent)))
        (ai:agent-submit agent "SECOND" (lambda (text error) (push (list text error) results)))
        (ai:agent-send-now agent "STEER")
        ;; A premature completion would credit STEER to this queued turn.
        (ai:agent-submit agent "NEXT" (lambda (text error) (push (list text error) results)))
        (true (wait-until (lambda () (= 3 (length results)))))
        (is eq process (ai::claude-agent-process agent))
        (is equal '(("NEXT" nil) ("STEER" nil) ("FIRST" nil)) results)
        (is = 4 (ai::claude-agent-next-result-index agent))
        ;; A model/effort change creates a process whose first index is zero.
        (setf (ai::agent-effort agent) "low")
        (ai:agent-submit agent "SECOND" (lambda (text error) (push (list text error) results)))
        (ai:agent-send-now agent "RESTARTED")
        (true (wait-until (lambda () (= 4 (length results)))))
        (isnt eq process (ai::claude-agent-process agent))
        (is = 0 (ai::claude-agent-turn-result-start agent))
        (is = 2 (ai::claude-agent-next-result-index agent))
        (is equal '("RESTARTED" nil) (first results))))))

(define-test claude-indexed-results-after-failed-resume :parent agent-duet
  (with-fake-claude (agent directory notes)
    (touch directory "indexed-results")
    (touch directory "expect-steer")
    (let ((results nil))
      (ai::agent-resume-conversation agent "gone")
      ;; Simulate the previous process having produced many results.
      (setf (ai::claude-agent-next-result-index agent) 20
            (ai::claude-agent-turn-result-start agent) 18)
      (ai:agent-submit agent "RESEND" (lambda (text error) (push (list text error) results)))
      (ai:agent-send-now agent "STEER")
      (true (wait-until (lambda () results)))
      (is equal '(("STEER" nil)) results)
      (is = 0 (ai::claude-agent-turn-result-start agent))
      (is = 2 (ai::claude-agent-next-result-index agent))
      (true (find-if (lambda (note) (search "Couldn't resume" note)) notes)))))

(define-test saved-effort-before-catalog :parent agent-duet
  (let* ((ai::*settings* '(:codex-effort "medium"))
         (agent (make-instance 'ai:codex-agent :effort (ai::setting :codex-effort)))
         (model (ai:jobj "supportedReasoningEfforts" #("low" "medium" "high"))))
    ;; Dropdown construction before discovery offers only Default. Keep the
    ;; loaded preference so catalog arrival can select it, without rereading
    ;; settings over a user's subsequent choice.
    (setf (ai::agent-effort agent) (ai::catalog-effort (ai::agent-effort agent) '(nil) nil))
    (is string= "medium" (ai::agent-effort agent))
    (setf (ai::agent-effort agent)
          (ai::catalog-effort (ai::agent-effort agent) (cons nil (ai::codex-model-efforts model)) t))
    (is string= "medium" (ai::agent-effort agent))
    (false (ai::catalog-effort "max" (cons nil (ai::codex-model-efforts model)) t))
    (false (ai::catalog-effort nil '(nil "medium") t))))
