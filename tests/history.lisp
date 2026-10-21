;;;; history.lisp — the headless conversation store

(in-package #:agent-duet/tests)

(defmacro with-history-directory (&body body)
  `(let ((ai::*history-directory*
           (merge-pathnames (format nil "duet-history-~a/" (ai::make-uuid)) (uiop:temporary-directory))))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ai::*history-directory* :validate t :if-does-not-exist :ignore))))

(defun saved-history (directory title updated)
  (let ((history (ai::make-history directory)))
    (ai::history-add-entry history :user :prompt title)
    (setf (ai::history-updated history) updated)
    (ai::save-history history)
    history))

(define-test history-round-trip
    :parent agent-duet
  (with-history-directory
    (let ((history (ai::make-history "/project/")))
      (setf (ai::history-handle history :claude) "session"
            (ai::history-handle history :codex) "thread")
      (loop for who in '(:user :claude :codex :middle :claude :claude :codex)
            for kind in '(:prompt :text :tool :note :error :activity :activity-error)
            do (ai::history-add-entry history who kind "hello λ"))
      (ai::save-history history)
      ;; Saving again replaces an existing file.
      (ai::save-history history)
      (let ((loaded (ai::load-history (ai::history-id history))))
        (true loaded)
        (is equal (ai::history-entries history) (ai::history-entries loaded))
        (is equal (ai::history-handles history) (ai::history-handles loaded))
        (is string= "/project/" (ai::history-directory loaded))
        (is = (ai::history-created history) (ai::history-created loaded))
        (is = (ai::history-updated history) (ai::history-updated loaded))
        (is string= "session" (ai::history-handle loaded :claude))
        (is string= "thread" (ai::history-handle loaded :codex))))))

(define-test history-title-and-empty
    :parent agent-duet
  (with-history-directory
    (let ((history (ai::make-history "/project"))
          (prompt (concatenate 'string (format nil "first~%line~c" #\Return) (make-string 100 :initial-element #\x))))
      (ai::save-history history)
      (false (probe-file (ai::history-file (ai::history-id history))))
      (ai::history-add-entry history :middle :note "not the title")
      (false (ai::history-title history))
      (ai::history-add-entry history :user :prompt prompt)
      (is = 80 (length (ai::history-title history)))
      (false (find #\Newline (ai::history-title history)))
      (false (find #\Return (ai::history-title history)))
      (true (search "first line" (ai::history-title history)))
      (let ((title (ai::history-title history)))
        (ai::history-add-entry history :user :prompt "later")
        (is string= title (ai::history-title history))))))

(define-test history-list-and-delete
    :parent agent-duet
  (with-history-directory
    (let* ((old (saved-history "/a/" "old" 10))
           (new (saved-history "/a" "new" 30))
           (other (saved-history "/b/" "other" 20)))
      (with-open-file (out (ai::history-file "corrupt") :direction :output)
        (write-string "not JSON" out))
      (false (ai::load-history "corrupt"))
      (false (ai::load-history "missing"))
      (false (ai::load-history "../outside"))
      (is equal (list (ai::history-id new) (ai::history-id other) (ai::history-id old))
          (mapcar (lambda (s) (getf s :id)) (ai::list-histories)))
      (is equal '("new" "old")
          (mapcar (lambda (s) (getf s :title)) (ai::list-histories :directory "/a")))
      (is = 1 (getf (first (ai::list-histories)) :entry-count))
      (is = 1 (length (ai::list-histories :limit 1)))
      (ai::delete-history (ai::history-id new))
      (false (ai::load-history (ai::history-id new)))
      (is = 2 (length (ai::list-histories))))))

(define-test history-prune
    :parent agent-duet
  (with-history-directory
    (saved-history "/a" "old" 10)
    (saved-history "/b" "middle" 20)
    (saved-history "/a" "new" 30)
    (ai::prune-histories :keep 2)
    (is equal '("new" "middle") (mapcar (lambda (s) (getf s :title)) (ai::list-histories)))
    (with-open-file (out (ai::history-file "corrupt") :direction :output)
      (write-string "bad" out))
    (ai::prune-histories :keep 0)
    (false (probe-file (ai::history-file "corrupt")))
    (false (ai::list-histories))))

(define-test history-note-before-prompt
    :parent agent-duet
  (with-history-directory
    (let ((history (ai::make-history "/a")))
      (ai::history-add-entry history :middle :note "starting")
      (ai::save-history history)
      (let ((loaded (ai::load-history (ai::history-id history))))
        (false (ai::history-title loaded))
        (ai::history-add-entry loaded :user :prompt "first prompt")
        (is string= "first prompt" (ai::history-title loaded)))
      (let* ((json (ai::history-json history))
             (entry (aref (ai::jget json "entries") 0)))
        (setf (cdr (assoc "kind" (cdr entry) :test #'equal)) "unknown-kind")
        (with-open-file (out (ai::history-file (ai::history-id history))
                             :direction :output :if-exists :supersede)
          (write-string (ai::json-string json) out))
        (false (ai::load-history (ai::history-id history)))))))

(defun sample-checkpoint (state)
  "A checkpoint shaped as WORKFLOW-CHECKPOINT writes it."
  (ai::jobj "version" 1 "state" state "mode" "divide" "request" "Fix it λ"
            "agents" (vector (ai::jobj "id" "claude" "directory" "/p/" "conversation" "s" "unavailable" :null)
                             (ai::jobj "id" "codex" "directory" "/p/" "conversation" :null "unavailable" 3900000000))
            "steps" (vector (ai::jobj "id" "s1" "agent" "claude" "role" "work" "loop" 0 "state" "completed"
                                      "reason" :null "progress" "done")
                            (ai::jobj "id" "s2" "agent" "codex" "role" "review" "loop" 0 "state" "paused"
                                      "reason" "interrupted" "progress" "half a review"))
            "loops" (vector (ai::jobj "label" "Claude's part" "author" "claude" "reviewer" "codex"
                                      "state" "reviewing" "round" 1 "skip-review" :false)
                            (ai::jobj "label" "ChatGPT's part" "author" "codex" "reviewer" "claude"
                                      "state" "working" "round" 0 "skip-review" t))
            "worktrees" t
            "trees" (vector (ai::jobj "agent" "claude" "path" "/tmp/tree/" "branch" "agent-duet/claude-1"))
            "integration" "merging"
            "integration-journal" (vector (ai::jobj "path" "/tmp/tree/" "head" "0123456789abcdef"))
            "merged-refs" (vector "refs/heads/agent-duet/claude-1")
            "repositories" (vector (ai::jobj "path" "/p/" "available" t "head" "aaa" "branch" "refs/heads/main"
                                             "status" "" "operation" :false))
            "shutdown-pids" (vector 1 2.5d0 :null)))

(defun checkpoint-with (state &rest keys-and-values)
  "SAMPLE-CHECKPOINT in STATE, with KEYS-AND-VALUES replacing its entries."
  (let ((checkpoint (sample-checkpoint state)))
    (loop for (key value) on keys-and-values by #'cddr
          do (setf (cdr (assoc key (cdr checkpoint) :test #'equal)) value))
    checkpoint))

(define-test history-checkpoint-round-trip
    :parent agent-duet
  (with-history-directory
    (let ((history (ai::make-history "/project/")))
      (ai::history-add-entry history :user :prompt "Fix it")
      (ai::record-history-checkpoint history (sample-checkpoint "running"))
      (ai::save-history history)
      (let ((loaded (ai::load-history (ai::history-id history))))
        ;; Kept exactly as the engine gave it, through its JSON.
        (is equal (ai::json-string (sample-checkpoint "running"))
            (ai::json-string (ai::history-checkpoint loaded)))
        (is equalp (ai::history-checkpoint history) (ai::history-checkpoint loaded))
        (is equal (ai::history-entries history) (ai::history-entries loaded))
        (true (ai::history-interrupted-p loaded))
        (true (getf (first (ai::list-histories)) :interrupted))
        ;; Abandoning keeps the checkpoint but stops offering it.
        (ai::abandon-history-task loaded)
        (ai::save-history loaded)
        (let ((again (ai::load-history (ai::history-id history))))
          (true (ai::history-checkpoint again))
          (true (ai::history-abandoned again))
          (false (ai::history-interrupted-p again))
          (false (ai::history-recoverable-p again))
          (false (getf (first (ai::list-histories)) :interrupted))
          ;; A new task's checkpoint is offered again.
          (ai::record-history-checkpoint again (sample-checkpoint "paused"))
          (false (ai::history-abandoned again))
          (true (ai::history-interrupted-p again)))))))

(define-test history-checkpoint-states
    :parent agent-duet
  (let ((history (ai::make-history "/p")))
    (false (ai::history-interrupted-p history))
    (dolist (state '("running" "paused" "interrupted"))
      (ai::record-history-checkpoint history (sample-checkpoint state))
      (true (ai::history-interrupted-p history)))
    ;; Stopped by the user: recoverable when reopened, not offered unprompted.
    (ai::record-history-checkpoint history (sample-checkpoint "stopped"))
    (false (ai::history-interrupted-p history))
    (true (ai::history-recoverable-p history))
    (dolist (state '("finished" "abandoned" "new" "nonsense"))
      (ai::record-history-checkpoint history (checkpoint-with state "integration" :null))
      (false (ai::history-recoverable-p history)))
    (ai::record-history-checkpoint history nil)
    (false (ai::history-checkpoint history))
    ;; Not JSON, or not an object: refused, the old checkpoint kept.
    (ai::record-history-checkpoint history (sample-checkpoint "running"))
    (fail (ai::record-history-checkpoint history (vector 1)))
    (fail (ai::record-history-checkpoint history (ai::jobj "x" (make-hash-table))))
    (true (ai::history-interrupted-p history))))

(define-test history-checkpoint-compatibility
    :parent agent-duet
  (with-history-directory
    (let* ((history (saved-history "/a/" "old" 10))
           (file (ai::history-file (ai::history-id history)))
           (json (ai::parse-json (uiop:read-file-string file))))
      ;; Saved without a checkpoint: no keys added, as older versions wrote.
      (false (assoc "checkpoint" (cdr json) :test #'equal))
      (false (assoc "abandoned" (cdr json) :test #'equal))
      ;; A file from before checkpoints loads, with none.
      (let ((loaded (ai::load-history (ai::history-id history))))
        (true loaded)
        (false (ai::history-checkpoint loaded))
        (false (ai::history-interrupted-p loaded)))
      ;; A malformed checkpoint is dropped, not the conversation.
      (dolist (bad (list "text" 3 (vector 1 2) :null))
        (with-open-file (out file :direction :output :if-exists :supersede)
          (write-string (ai::json-string (append json (list (cons "checkpoint" bad) (cons "abandoned" "x")))) out))
        (let ((loaded (ai::load-history (ai::history-id history))))
          (true loaded)
          (is equal (ai::history-entries history) (ai::history-entries loaded))
          (false (ai::history-checkpoint loaded))
          (false (ai::history-abandoned loaded)))))))

(define-test checkpoint-summary
    :parent agent-duet
  (let* ((summary (ai::checkpoint-summary (sample-checkpoint "paused")))
         (step (getf summary :step)))
    (is equal "paused" (getf summary :state))
    (is equal "Claude" (getf summary :lead))
    (is equal "ChatGPT" (getf summary :partner))
    (is = 3900000000 (getf (second (getf summary :agents)) :unavailable-until))
    (false (getf (first (getf summary :agents)) :unavailable-until))
    ;; The completed step isn't where it stopped.
    (is = 1 (length (getf summary :steps)))
    (is equal "s2" (getf step :id))
    (is equal "ChatGPT's review of Claude's part" (getf step :description))
    (is equal "interrupted" (getf step :reason))
    (is equal "half a review" (getf step :progress))
    (is equal '("ChatGPT's part") (getf summary :unreviewed))
    (is = 2 (length (getf summary :loops)))
    (is equal "/tmp/tree/" (getf (first (getf summary :worktrees)) :path))
    (is equal "merging" (getf (getf summary :integration) :state))
    ;; The journal is what was committed; merged-refs, what was merged.
    (is equal '("/tmp/tree/ committed at 0123456789") (getf (getf summary :integration) :committed))
    (is equal '("agent-duet/claude-1 merged") (getf (getf summary :integration) :merged))
    (false (getf summary :pending-plan)))
  ;; Unknown and malformed pieces read as missing.
  (let ((summary (ai::checkpoint-summary (ai::jobj "version" 9 "state" 4 "steps" "x" "loops" "y"
                                                   "trees" (vector 1 (ai::jobj "agent" "claude"))
                                                   "integration" (vector)))))
    (is = 9 (getf summary :version))
    (false (getf summary :state))
    (false (getf summary :step))
    (false (getf summary :loops))
    (false (getf summary :worktrees))
    (false (getf summary :integration)))
  (false (getf (ai::checkpoint-summary nil) :state))
  (is equal "Claude's plan for splitting the work" (ai::checkpoint-step-description "plan" "Claude" nil))
  (is equal "ChatGPT's work on its part" (ai::checkpoint-step-description "work" "ChatGPT" "its part")))

(define-test inspection-summary
    :parent agent-duet
  (let* ((nul (string (code-char 0)))
         (inspection
           (ai::jobj "state" "paused" "repository-changed" t
                     "repositories" (vector (ai::jobj "path" "/p/" "available" t "head" "bbbbbbbbbbbbbbbb"
                                                      "branch" "refs/heads/other" "operation" t
                                                      "status" (format nil " M a.lisp~aR  new.lisp~aold.lisp~a?? x y~a"
                                                                       nul nul nul nul))
                                            (ai::jobj "path" "/tmp/tree/" "available" :false
                                                      "error" "Repository or worktree unavailable"))
                     "agents" (vector (ai::jobj "id" "claude" "busy" :false "shutdown-confirmed" t "unavailable" :null)
                                      (ai::jobj "id" "codex" "busy" t "shutdown-confirmed" :false "unavailable" 5))
                     "recovery-refs" (vector (ai::jobj "part" "claude" "head" "h" "base" "b"))))
         (summary (ai::inspection-summary inspection (sample-checkpoint "paused")))
         (repo (first (getf summary :repositories)))
         (tree (second (getf summary :repositories))))
    (true (getf summary :changed))
    (is equal '((" M" . "a.lisp") ("R " . "new.lisp") ("??" . "x y")) (getf repo :files))
    (is equal "other" (getf repo :branch))
    (is equal "main" (getf repo :branch-was))
    (is equal "bbbbbbbbbb" (getf repo :head))
    (true (getf repo :new-commits))
    (true (getf repo :operation))
    (true (getf repo :changed-since))
    (false (getf tree :available))
    (is equal "Repository or worktree unavailable" (getf tree :error))
    (false (getf tree :new-commits))
    (is equal '(:agent "Claude" :busy nil :stopped t :unavailable-until nil) (first (getf summary :agents)))
    (is equal '(:agent "ChatGPT" :busy t :stopped nil :unavailable-until 5) (second (getf summary :agents)))
    (is equal '("claude") (getf summary :recovery-refs)))
  ;; The same as at the checkpoint: nothing new.
  (let* ((checkpoint (sample-checkpoint "paused"))
         (repo (first (getf (ai::inspection-summary
                             (ai::jobj "repositories" (ai::jget checkpoint "repositories")) checkpoint)
                            :repositories))))
    (false (getf repo :new-commits))
    (false (getf repo :branch-was))
    (false (getf repo :changed-since))
    (false (getf repo :files)))
  (is equal "detached HEAD" (ai::short-branch "HEAD")))

(define-test recovery-actions-and-choices
    :parent agent-duet
  (let ((paused (ai::checkpoint-summary (sample-checkpoint "paused"))))
    (is equal '(:review :resume :retry :hand-off :stop :abandon) (ai::recovery-actions paused))
    (is equal "Finish unreviewed"
        (second (find :hand-off (ai::recovery-choices (ai::recovery-actions paused)
                                                      :step-role (getf (getf paused :step) :role))
                      :key #'first))))
  ;; Stopped partway through merging, with no step waiting: carry the merge on.
  (let ((checkpoint (sample-checkpoint "stopped")))
    (setf (cdr (assoc "steps" (cdr checkpoint) :test #'equal)) (vector))
    (is equal '(:review :resume :abandon) (ai::recovery-actions (ai::checkpoint-summary checkpoint))))
  (is equal '(:review :stop :abandon) (ai::recovery-actions (ai::checkpoint-summary (ai::jobj "state" "running"))))
  (is equal '(:review :resume :retry :hand-off :stop :abandon)
      (mapcar #'first (ai::recovery-choices '(:abandon :stop :hand-off :retry :resume :review))))
  (is equal '(:resume :abandon) (mapcar #'first (ai::recovery-choices '(:abandon :unknown :resume))))
  (is equal "Hand off" (second (first (ai::recovery-choices '(:hand-off) :step-role "work")))))

(define-test recovery-unfinished-integration
    :parent agent-duet
  ;; Merging stopped partway counts the workflow finished, but it isn't done.
  (let ((history (ai::make-history "/p")))
    (dolist (integration '("committing" "merging" "interrupted"))
      (ai::record-history-checkpoint history (checkpoint-with "finished" "integration" integration))
      (true (ai::history-interrupted-p history))
      (true (ai::history-recoverable-p history)))
    (dolist (integration (list "merged" :null))
      (ai::record-history-checkpoint history (checkpoint-with "finished" "integration" integration))
      (false (ai::history-recoverable-p history))))
  ;; With no step waiting, it is carried on by merging.
  (let ((summary (ai::checkpoint-summary (checkpoint-with "finished" "integration" "interrupted"
                                                          "steps" (vector)))))
    (is equal '(:review :resume :abandon) (ai::recovery-actions summary))
    (is equal "Carry on merging"
        (second (find :resume (ai::recovery-choices (ai::recovery-actions summary)
                                                    :integration (getf (getf summary :integration) :state))
                      :key #'first))))
  ;; Merged, but the combined result not yet begun: carried on too.
  (is equal '(:review :resume :stop :abandon)
      (ai::recovery-actions (ai::checkpoint-summary (checkpoint-with "paused" "integration" "merged"
                                                                     "steps" (vector)))))
  ;; As the engine writes it, where it is loaded.
  (when (and (fboundp 'ai::workflow-checkpoint) (fboundp '(setf ai::workflow-integration-state)))
    (let* ((lead (make-instance 'ai::agent :name "Claude" :id :claude))
           (partner (make-instance 'ai::agent :name "ChatGPT" :id :codex))
           (workflow (make-instance 'ai::workflow :lead lead :partner partner :request "r")))
      ;; As INTEGRATION-FAILED leaves it.
      (setf (ai::workflow-state workflow) :finished)
      (funcall (fdefinition '(setf ai::workflow-integration-state)) :interrupted workflow)
      (let ((history (ai::make-history "/p")))
        (ai::record-history-checkpoint history (ai::workflow-checkpoint workflow))
        (true (ai::history-interrupted-p history))))))

(define-test recovery-pending-plan
    :parent agent-duet
  (let* ((checkpoint (checkpoint-with "paused" "steps" (vector (ai::jobj "id" "p" "agent" "claude" "role" "plan"
                                                                         "state" "completed"))
                                      "integration" :null "integration-journal" (vector) "merged-refs" (vector)))
         (summary (progn (push (cons "pending-plan" (ai::jobj "lead" "Lead part" "partner" "Partner part"
                                                               "takeover" :null))
                               (cdr checkpoint))
                         (ai::checkpoint-summary checkpoint))))
    (is equal '(:lead "Lead part" :partner "Partner part") (getf summary :pending-plan))
    (false (getf summary :step))
    (is equal '(:review :resume :stop :abandon) (ai::recovery-actions summary))
    (is equal "Show the plan" (second (find :resume (ai::recovery-choices '(:resume) :plan (getf summary :pending-plan))
                                            :key #'first)))))

(define-test recovery-restore-worktrees
    :parent agent-duet
  (let* ((summary (ai::checkpoint-summary (sample-checkpoint "paused")))
         (missing (ai::jobj "repositories" (vector (ai::jobj "path" "/tmp/tree/" "available" :false))
                            "recovery-refs" (vector (ai::jobj "part" "claude"))))
         (present (ai::jobj "repositories" (vector (ai::jobj "path" "/tmp/tree/" "available" t))
                            "recovery-refs" (vector (ai::jobj "part" "claude"))))
         (no-refs (ai::jobj "repositories" (vector (ai::jobj "path" "/tmp/tree/" "available" :false)))))
    (true (member :restore-worktrees (ai::recovery-actions summary (ai::inspection-summary missing))))
    (false (member :restore-worktrees (ai::recovery-actions summary (ai::inspection-summary present))))
    (false (member :restore-worktrees (ai::recovery-actions summary (ai::inspection-summary no-refs))))
    ;; Not before the folder has been looked at.
    (false (member :restore-worktrees (ai::recovery-actions summary)))
    (is equal '(:review :restore-worktrees :resume)
        (mapcar #'first (ai::recovery-choices '(:resume :restore-worktrees :review))))))

(define-test recovery-several-steps-waiting
    :parent agent-duet
  (let* ((summary (ai::checkpoint-summary
                   (checkpoint-with "paused" "steps"
                                    (vector (ai::jobj "id" "a" "agent" "claude" "role" "work" "loop" 0 "state" "paused")
                                            (ai::jobj "id" "b" "agent" "codex" "role" "work" "loop" 1 "state" "paused"))))))
    (is equal '("a" "b") (mapcar (lambda (s) (getf s :id)) (getf summary :steps)))
    (is equal '("Claude's work on Claude's part" "ChatGPT's work on ChatGPT's part")
        (mapcar (lambda (s) (getf s :description)) (getf summary :steps)))))

(define-test recovery-selected-step
    :parent agent-duet
  (let* ((checkpoint (checkpoint-with "paused" "steps"
                                      (vector (ai::jobj "id" "c" "agent" "claude" "role" "check" "loop" 0
                                                        "state" "paused" "progress" "check output")
                                              (ai::jobj "id" "w" "agent" "codex" "role" "work" "loop" 1
                                                        "state" "paused" "progress" "half the docs")
                                              (ai::jobj "id" "r" "agent" "claude" "role" "review" "loop" 1
                                                        "state" "running"))))
         (summary (ai::checkpoint-summary checkpoint)))
    ;; The check comes first: only resumed.
    (is equal "c" (getf (getf summary :step) :id))
    (is equal '(:review :resume :stop :abandon) (ai::recovery-actions summary))
    ;; Choosing the work step brings its own actions and progress.
    (let ((work (ai::select-summary-step summary "w")))
      (is equal "w" (getf (getf work :step) :id))
      (is equal "half the docs" (getf (getf work :step) :progress))
      (is equal '(:review :resume :retry :hand-off :stop :abandon) (ai::recovery-actions work))
      ;; And back: the check doesn't offer what it can't do.
      (is equal '(:review :resume :stop :abandon)
          (ai::recovery-actions (ai::select-summary-step work "c"))))
    ;; An id no longer waiting leaves the summary as it was.
    (is eq summary (ai::select-summary-step summary "gone"))
    (is eq summary (ai::select-summary-step summary nil))
    ;; Live, only the steps the workflow still has paused are waiting: one
    ;; resumed (running) is not, whatever the checkpoint said.
    (let ((live (ai::checkpoint-summary checkpoint :waiting '("w"))))
      (is equal '("w") (mapcar (lambda (s) (getf s :id)) (getf live :steps)))
      (is equal "w" (getf (getf live :step) :id))
      (is eq live (ai::select-summary-step live "c")))
    (false (getf (ai::checkpoint-summary checkpoint :waiting '()) :steps))
    ;; Reopened without the engine's say, a step saved running is where it stopped.
    (true (find "r" (getf (ai::checkpoint-summary
                           (checkpoint-with "running" "steps"
                                            (vector (ai::jobj "id" "r" "agent" "claude" "role" "review" "state" "running"))))
                          :steps)
                :key (lambda (s) (getf s :id)) :test #'equal))))

(define-test recovery-reason-wording :parent agent-duet
  (is equal "waiting for prerequisite work" (ai::step-reason-text :dependency))
  (is equal "prerequisite changed; needs revalidation" (ai::step-reason-text "prerequisite-changed"))
  (is equal "could not continue after the step finished" (ai::step-reason-text :continuation-failed)))

(define-test dependency-recovery-summaries :parent agent-duet
  (let* ((loops (vector (ai::jobj "label" "Prepare schema" "state" "working")
                        (ai::jobj "label" "Use schema" "state" "paused" "dependencies" #(0)
                                  "revision" 2 "prerequisite-revisions" #(1))))
         (step (ai::jobj "id" "waiting" "agent" "codex" "role" "author"
                        "loop" 1 "state" "paused" "reason" "dependency"))
         (checkpoint (ai::jobj "state" "paused" "loops" loops "steps" (vector step)))
         (summary (ai::checkpoint-summary checkpoint)))
    (is equal "waiting for Prepare schema" (getf (getf summary :step) :waiting-for))
    (is equal "waiting for Prepare schema" (getf (second (getf summary :loops)) :waiting-for))
    (let ((inspection (ai::inspection-summary (ai::jobj "steps" (vector step)) checkpoint)))
      (is equal "waiting for Prepare schema" (getf (first (getf inspection :steps)) :waiting-for)))
    (setf (cdr (assoc "reason" (cdr step) :test #'equal)) "deadlock")
    (let ((deadlock (ai::checkpoint-summary checkpoint)))
      (is equal "waiting for Prepare schema" (getf (getf deadlock :step) :waiting-for))
      (is equal "waiting for Prepare schema" (getf (second (getf deadlock :loops)) :waiting-for))
      (is equal '(:review :revise-order :run-independently :stop :abandon) (ai::recovery-actions deadlock))
      (is equal '("Revise the order" "Run independently")
          (mapcar #'second (ai::recovery-choices '(:revise-order :run-independently)))))
    ;; Missing dependency and revision keys remain readable.
    (setf (cdr (assoc "loops" (cdr checkpoint) :test #'equal)) (vector (ai::jobj "label" "Old part")))
    (false (getf (getf (ai::checkpoint-summary checkpoint) :step) :waiting-for))))

(define-test dependency-check-failed-summary :parent agent-duet
  (dolist (state '("approved" "unreviewed"))
    (dolist (reason '("dependency" "deadlock"))
      (let* ((checkpoint
               (ai::jobj "loops" (vector (ai::jobj "label" "Prepare schema" "state" state "check-failed" t)
                                        (ai::jobj "label" "Use schema" "state" "paused" "dependencies" #(0)))
                         "steps" (vector (ai::jobj "loop" 1 "state" "paused" "reason" reason))))
             (summary (ai::checkpoint-summary checkpoint)))
        (is equal "waiting for Prepare schema" (getf (second (getf summary :loops)) :waiting-for))
        (is equal "waiting for Prepare schema" (getf (getf summary :step) :waiting-for))))))

(define-test ordered-plan-summary :parent agent-duet
  (let ((checkpoint (ai::jobj "pending-plan" (ai::jobj "lead" "A" "partner" "B"))))
    (false (getf (getf (ai::checkpoint-summary checkpoint) :pending-plan) :order))
    (setf checkpoint (ai::jobj "pending-plan" (ai::jobj "lead" "A" "partner" "B"
                                                                  "order" "lead-first" "error" "Circular prerequisites")))
    (let ((plan (getf (ai::checkpoint-summary checkpoint) :pending-plan)))
      (is equal "lead-first" (getf plan :order))
      (is equal "Circular prerequisites" (getf plan :error))))
  (is equal "each part is waiting on the other" (ai::step-reason-text :deadlock))
  (is equal "each part is waiting on the other" (ai::step-reason-text "deadlock")))

(define-test dependency-labels-only-while-waiting :parent agent-duet
  (dolist (ready '("approved" "unreviewed"))
    (dolist (state '("working" "reviewing" "checking" "paused"))
      (let* ((checkpoint
               (ai::jobj "loops" (vector (ai::jobj "label" "Prepare schema" "state" ready)
                                        (ai::jobj "label" "Use schema" "state" state "dependencies" #(0)))
                         "steps" (vector (ai::jobj "loop" 1 "state" "paused" "reason" "dependency"))))
             (summary (ai::checkpoint-summary checkpoint)))
        (false (getf (second (getf summary :loops)) :waiting-for))
        (false (getf (getf summary :step) :waiting-for)))))
  ;; An unmet edge alone doesn't mean the part is waiting on that edge.
  (dolist (reason '("failed" "out-of-usage" "restart"))
    (let* ((checkpoint
             (ai::jobj "loops" (vector (ai::jobj "label" "Prepare schema" "state" "working")
                                      (ai::jobj "label" "Use schema" "state" "paused" "dependencies" #(0)))
                       "steps" (vector (ai::jobj "loop" 1 "state" "paused" "reason" reason))))
           (summary (ai::checkpoint-summary checkpoint)))
      (false (getf (second (getf summary :loops)) :waiting-for))
      (false (getf (getf summary :step) :waiting-for))))
  ;; Mixed prerequisites name only the unfinished part.
  (let* ((checkpoint
           (ai::jobj "loops" (vector (ai::jobj "label" "Ready" "state" "approved")
                                    (ai::jobj "label" "Pending" "state" "working")
                                    (ai::jobj "label" "Consumer" "state" "paused" "dependencies" #(0 1)))
                     "steps" (vector (ai::jobj "loop" 2 "state" "paused" "reason" "deadlock"))))
         (summary (ai::checkpoint-summary checkpoint)))
    (is equal "waiting for Pending" (getf (third (getf summary :loops)) :waiting-for))
    (is equal "waiting for Pending" (getf (getf summary :step) :waiting-for))))
