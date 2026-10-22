;;;; prompts.lisp — attachments, shared project notes, per-folder check commands

(in-package #:agent-duet/tests)

(defmacro with-temp-folder ((var) &body body)
  `(let ((,var (merge-pathnames (format nil "duet-prompts-~a/" (ai::make-uuid)) (uiop:temporary-directory))))
     (ensure-directories-exist ,var)
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,var :validate t :if-does-not-exist :ignore))))

(defun write-file (path contents)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (write-string contents out))
  path)

(define-test base64
    :parent agent-duet
  (flet ((b64 (string) (ai::base64-encode (map '(vector (unsigned-byte 8)) #'char-code string))))
    (is string= "" (b64 ""))
    (is string= "Zg==" (b64 "f"))
    (is string= "Zm8=" (b64 "fo"))
    (is string= "Zm9v" (b64 "foo"))
    (is string= "Zm9vYmFy" (b64 "foobar"))))

(define-test attachments
    :parent agent-duet
  (with-temp-folder (dir)
    (let ((image (namestring (write-file (merge-pathnames "shot.png" dir) "foo")))
          (text (namestring (write-file (merge-pathnames "notes.txt" dir) "hello")))
          (missing (namestring (merge-pathnames "gone.jpg" dir))))
      ;; Only images that exist (and aren't too big) are shown.
      (is equal (list image) (ai::attachment-images (list image text missing)))
      (let ((ai::*attached-image-limit* 2))
        (false (ai::attachment-images (list image))))
      ;; Every attachment's path follows the prompt.
      (is string= "Hi" (ai::prompt-with-attachments "Hi" '()))
      (let ((prompt (ai::prompt-with-attachments "Hi" (list image text))))
        (true (search image prompt))
        (true (search text prompt)))
      ;; Claude: an image block after the text.
      (let* ((line (ai:parse-json (ai::user-message-line "Look" (list image))))
             (content (ai:jget line "message" "content")))
        (is = 2 (length content))
        (is string= "Look" (ai:jget content 0 "text"))
        (is string= "image" (ai:jget content 1 "type"))
        (is string= "image/png" (ai:jget content 1 "source" "media_type"))
        (is string= "Zm9v" (ai:jget content 1 "source" "data")))
      (is = 1 (length (ai:jget (ai:parse-json (ai::user-message-line "Look")) "message" "content")))
      ;; Codex: local images after the text.
      (let ((input (ai::codex-input "Look" (list image))))
        (is = 2 (length input))
        (is string= "localImage" (ai:jget (aref input 1) "type"))
        (is string= image (ai:jget (aref input 1) "path")))
      (is = 2 (length (ai:jget (ai::codex-steer-params "t" "u" "Look" (list image)) "input")))
      ;; Through an agent: the prompt it gets names them, and the images go with the turn.
      (let ((agent (make-instance 'fake-agent :id :claude :name "Claude" :partner-name "ChatGPT"
                                              :replies (list (says "Seen.") (says "Nothing.")))))
        (ai:agent-send-now agent "What's this?" (list image text))
        (is equal (list image) (ai::agent-turn-images agent))
        (let ((prompt (first (fake-prompts agent))))
          (true (search "What's this?" prompt))
          (true (search text prompt)))
        ;; The next turn has none.
        (ai:agent-submit agent "And now?")
        (is equal '() (ai::agent-turn-images agent))
        (is string= "And now?" (first (fake-prompts agent)))))))

(define-test shared-notes
    :parent agent-duet
  (with-temp-folder (dir)
    (let ((claude (make-instance 'ai:claude-agent :directory dir))
          (codex (make-instance 'ai:codex-agent :directory dir)))
      ;; Nothing to share yet.
      (false (ai::shared-notes claude))
      (false (search "notes for coding agents" (ai::collaboration-instructions claude)))
      (write-file (merge-pathnames "AGENTS.md" dir) "Use tabs.")
      (write-file (merge-pathnames "CLAUDE.md" dir) "Run make test.")
      ;; Each gets the other's file.
      (let ((instructions (ai::collaboration-instructions claude)))
        (true (search "Use tabs." instructions))
        (true (search "`AGENTS.md`" instructions))
        (false (search "Run make test." instructions)))
      (let ((instructions (ai::collaboration-instructions codex)))
        (true (search "Run make test." instructions))
        (true (search "`CLAUDE.md`" instructions))
        (false (search "Use tabs." instructions)))
      ;; Switched off.
      (let ((ai::*shared-notes* nil))
        (false (search "Use tabs." (ai::collaboration-instructions claude))))
      ;; Cut to the limit.
      (let ((ai::*shared-notes-limit* 4))
        (let ((notes (ai::shared-notes claude)))
          (true (uiop:string-prefix-p "Use " notes))
          (false (search "tabs." notes))
          (true (search "cut" notes))))
      ;; Notes an agent already reads aren't repeated: Claude's file imports
      ;; AGENTS.md, and Codex's import line alone is no notes.
      (write-file (merge-pathnames "AGENTS.md" dir) "Use tabs.")
      (write-file (merge-pathnames "CLAUDE.md" dir) (format nil "@AGENTS.md~%"))
      (false (ai::shared-notes claude))
      (false (ai::shared-notes codex))
      (write-file (merge-pathnames "CLAUDE.md" dir) (format nil "@./AGENTS.md~%~%Run make test.~%"))
      (false (ai::shared-notes claude))
      (is string= "Run make test." (ai::shared-notes codex))
      ;; An import line in a fenced example isn't an import.
      (write-file (merge-pathnames "CLAUDE.md" dir)
                  (format nil "Example:~%```~%@AGENTS.md~%```~%~~~~~~~%@./AGENTS.md~%~~~~~~~%"))
      (is string= "Use tabs." (ai::shared-notes claude))
      (true (search "@AGENTS.md" (ai::shared-notes codex)))
      (true (search "@./AGENTS.md" (ai::shared-notes codex)))
      ;; A shorter or annotated fence line doesn't close a longer fence.
      (write-file (merge-pathnames "CLAUDE.md" dir)
                  (format nil "````md~%```~%@AGENTS.md~%```` more~%````~%"))
      (is string= "Use tabs." (ai::shared-notes claude))
      ;; After the fence closes, an import line is an import again.
      (write-file (merge-pathnames "CLAUDE.md" dir)
                  (format nil "````md~%```~%````  ~%@AGENTS.md~%"))
      (false (ai::shared-notes claude))
      ;; Nor are identical copies.
      (write-file (merge-pathnames "CLAUDE.md" dir) (format nil "Use tabs.~%"))
      (false (ai::shared-notes claude))
      (false (ai::shared-notes codex))
      ;; Without its own file, an agent still gets the other's.
      (delete-file (merge-pathnames "CLAUDE.md" dir))
      (is string= "Use tabs." (ai::shared-notes claude))
      ;; A blank file is no notes.
      (write-file (merge-pathnames "AGENTS.md" dir) (format nil "  ~%"))
      (false (ai::shared-notes claude)))))

(define-test check-commands
    :parent agent-duet
  (with-temp-folder (config)
    (let ((old (uiop:getenv "XDG_CONFIG_HOME"))
          (ai::*settings* '()))
      (setf (uiop:getenv "XDG_CONFIG_HOME") (namestring config))
      (unwind-protect
           (progn
             (false (ai::folder-check-command "/a/"))
             (setf (ai::folder-check-command "/a/") "  make test ")
             (setf (ai::folder-check-command #p"/b/") "npm test")
             (is string= "make test" (ai::folder-check-command #p"/a/"))
             (is string= "npm test" (ai::folder-check-command "/b/"))
             ;; Kept between runs.
             (ai::load-settings)
             (is string= "make test" (ai::folder-check-command "/a/"))
             ;; A blank command forgets it.
             (setf (ai::folder-check-command "/a/") "   ")
             (false (ai::folder-check-command "/a/"))
             (is string= "npm test" (ai::folder-check-command "/b/")))
        (setf (uiop:getenv "XDG_CONFIG_HOME") (or old ""))))))

(define-test shell-quote
    :parent agent-duet
  (is string= "/tmp/a-b_c.1.txt" (ai::shell-quote "/tmp/a-b_c.1.txt"))
  (is string= "'/tmp/a b'" (ai::shell-quote "/tmp/a b"))
  (is string= "'/tmp/a\\b.txt'" (ai::shell-quote "/tmp/a\\b.txt"))
  (is string= (format nil "'/tmp/a~cb'" #\Tab) (ai::shell-quote (format nil "/tmp/a~cb" #\Tab)))
  (is string= (format nil "'/tmp/a~%b'") (ai::shell-quote (format nil "/tmp/a~%b")))
  (is string= "'it'\\''s'" (ai::shell-quote "it's"))
  (is string= "''" (ai::shell-quote ""))
  ;; What the shell makes of them: the paths as they were.
  (dolist (path (list "/tmp/a\\b.txt" "it's $HOME" (format nil "a~cb~%c" #\Tab) "plain"))
    (is string= path (uiop:run-program (list "/bin/sh" "-c" (format nil "printf %s ~a" (ai::shell-quote path)))
                                       :output :string))))

(defun big-image (dir)
  (let ((image (merge-pathnames "big.png" dir)))
    (with-open-file (o image :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (write-sequence (make-array (* 1024 1024) :element-type '(unsigned-byte 8) :initial-element 7) o))
    (namestring image)))

(defmacro with-slow-claude ((agent cp out dir) &body body)
  "AGENT, busy with a turn, its CLI CP a process that doesn't read its input
until RECEIVED-LINES releases it, then copies it to the file OUT."
  (let ((process (gensym)))
    `(with-temp-folder (,dir)
       (let* ((,out (merge-pathnames "received" ,dir))
              (,process (sb-ext:run-program "/bin/sh" (list "-c" (format nil "while [ ! -f ~a ]; do sleep 0.02; done; cat > ~a"
                                                                                (ai::shell-quote (concatenate 'string (namestring ,out) ".ready"))
                                                                                (ai::shell-quote (namestring ,out))))
                                            :wait nil :input :stream :output nil :error nil))
              (,cp (ai::make-claude-process :process ,process))
              (,agent (make-instance 'ai:claude-agent :directory ,dir)))
         (setf (ai::claude-agent-process ,agent) ,cp
               (ai:agent-busy ,agent) t
               (ai::agent-turn-token ,agent) (list t))
         (unwind-protect (progn ,@body)
           (when (sb-ext:process-alive-p ,process) (sb-ext:process-kill ,process 9)))))))

(defun received-lines (agent cp out)
  "What the slow CLI got, once everything sent has been written."
  (with-open-file (ready (concatenate 'string (namestring out) ".ready")
                         :direction :output :if-exists :supersede)
    (write-line "ready" ready))
  (let ((sender (ai::claude-agent-sender agent)))
    (when sender (sb-thread:join-thread sender :default nil)))
  ;; The interrupt has its own writer thread. Wait for the reader to receive
  ;; both complete lines before closing the pipe, rather than guessing a delay.
  (unless (wait-until (lambda ()
                        (let ((lines (ignore-errors (uiop:read-file-lines out))))
                          (and (= 2 (length lines))
                               (plusp (length (second lines)))
                               (char= #\} (char (second lines) (1- (length (second lines)))))))))
    (error "The fake Claude CLI did not receive both messages"))
  (close (sb-ext:process-input (ai::cp-process cp)))
  (sb-ext:process-wait (ai::cp-process cp))
  (uiop:read-file-lines out))

(defun seconds-since (start)
  (/ (- (get-internal-real-time) start) internal-time-units-per-second))

(define-test claude-image-send-in-background
    :parent agent-duet
  (with-slow-claude (agent cp out dir)
    (let ((image (big-image dir))
          (errors '())
          (start (get-internal-real-time)))
      (ai::claude-agent-send agent cp "first" (list image) :on-error (lambda (why) (push why errors)))
      ;; Sent after it, while it's still being written: it waits its turn.
      (ai::claude-agent-send agent cp "second" '())
      (true (< (seconds-since start) 0.5))
      (let ((lines (received-lines agent cp out)))
        (is = 2 (length lines))
        (true (search "\"image\"" (first lines)))
        (true (search "first" (first lines)))
        (true (search "second" (second lines))))
      (false errors))))

(define-test claude-stop-while-sending
    :parent agent-duet
  (with-slow-claude (agent cp out dir)
    (let ((image (big-image dir)))
      (ai::claude-agent-send agent cp "first" (list image))
      ;; Queued behind it.
      (ai::claude-agent-send agent cp "second" (list image))
      (ai::claude-agent-send agent cp "third" '())
      ;; Wait until the first message owns stdin and is blocked in the
      ;; gated reader's pipe. Encoding the image can take longer on CI.
      (true (wait-until (lambda () (sb-thread:mutex-owner (ai::cp-lock cp)))))
      ;; Stop doesn't wait for the pipe.
      (let ((start (get-internal-real-time)))
        (ai:agent-cancel agent)
        (true (< (seconds-since start) 0.5)))
      ;; The message being written goes out, then the interrupt; the
      ;; queued ones, for the stopped turn, don't.
      (let ((lines (received-lines agent cp out)))
        (is = 2 (length lines))
        (true (search "first" (first lines)))
        (true (search "interrupt" (second lines)))
        (false (find-if (lambda (l) (or (search "second" l) (search "third" l))) lines))))))

(define-test compact-review-prompts :parent agent-duet
  (let* ((author (make-instance 'fake-agent :name "Claude"))
         (reviewer (make-instance 'fake-agent :name "ChatGPT"))
         (task (make-string 4000 :initial-element #\x))
         (part (ai::make-review-loop
                :author author :reviewer reviewer :task task :summary "Latest report"
                :check-report "Checks passed"
                :findings (list (ai::make-finding :id 1 :text "Fix this")))))
    ;; Round one and unknown history always carry the complete assignment.
    (true (search task (ai::review-prompt part)))
    (setf (ai::loop-round part) 1)
    (true (search task (ai::review-prompt part)))
    (setf (ai::loop-review-history part) (list reviewer (fake-handle reviewer)))
    (let ((short (ai::review-prompt part)))
      (false (search task short))
      (true (< (length short) 1200))
      (dolist (text '("Latest report" "Checks passed" "F1 [open]" "[fixed]" "[dismissed]"
                      "FINDINGS: NONE" "VERDICT: APPROVED" "VERDICT: CHANGES REQUESTED"))
        (true (search text short)))
      ;; Failed resume expands the exact prompt being replayed.
      (true (search task (ai::prompt-without-history reviewer short)))
      (is string= "Unrelated" (ai::prompt-without-history reviewer "Unrelated")))
    (true (search task (ai::review-prompt part :full t)))
    (setf (fake-handle reviewer) "new-conversation")
    (true (search task (ai::review-prompt part)))
    (setf (fake-handle reviewer) nil)
    (true (search task (ai::review-prompt part)))
    (setf (ai::loop-reviewer part) (make-instance 'fake-agent :name "Replacement"))
    (true (search task (ai::review-prompt part)))
    (false (search "summarize what you changed" (ai::work-reply-instruction part)))))

(define-test compact-commit-suggestion :parent agent-duet
  (let* ((author (make-instance 'fake-agent :name "Claude"))
         (reviewer (make-instance 'fake-agent :name "ChatGPT"))
         (part (ai::make-review-loop :author author :reviewer reviewer :task "Implement colors"))
         (instruction ai::*commit-suggestion-instruction*)
         (reminder ai::*commit-suggestion-reminder*))
    ;; A new conversation gets the instruction.
    (let ((prompt (ai::author-work-prompt part "Implement colors")))
      (true (search instruction prompt))
      (false (search reminder prompt)))
    ;; The same one, having reported, only a reminder; lost, the full prompt.
    (ai::work-done part "Implemented colors.")
    (let ((prompt (ai::author-work-prompt part (ai::fix-prompt part "Changes requested"))))
      (false (search instruction prompt))
      (true (search reminder prompt))
      (let ((full (ai::prompt-without-history author prompt)))
        (true (search instruction full))
        (false (search reminder full)))
      ;; A takeover or restored step gets the instruction back, once.
      (let ((takeover (ai::with-commit-suggestion-instruction (ai::takeover-prompt author prompt))))
        (true (search instruction takeover))
        (false (search reminder takeover))
        (is eql (search "Suggested commit:" takeover) (search "Suggested commit:" takeover :from-end t)))
      (is string= (ai::prompt-without-history author prompt) (ai::expand-commit-suggestion prompt))
      ;; Wrapped by a recovery or follow-up, it is still expanded.
      (let ((wrapped (format nil "Recover this:~%~%~a~%~%Then go on." prompt)))
        (is string= (format nil "Recover this:~%~%~a~%~%Then go on." (ai::expand-commit-suggestion prompt))
            (ai::prompt-without-history author wrapped)))
      (let* ((workflow (make-instance 'ai:workflow :lead author :partner reviewer :request "Implement colors"))
             (step (ai::make-step :agent author :prompt prompt :role :work :loop part :reason :disconnected))
             (recovery (ai::recovery-progress-prompt workflow step "{}")))
        (true (search instruction recovery))
        (false (search reminder recovery))))
    (setf (fake-handle author) "new-conversation")
    (true (search instruction (ai::author-work-prompt part "Fix it")))
    (setf (fake-handle author) nil)
    (true (search instruction (ai::author-work-prompt part "Fix it")))))

(define-test review-worktree-stat :parent agent-duet
  (with-git-folder (repo)
    (let* ((author (make-instance 'fake-agent :name "Claude"))
           (base (ai::worktree-git-line repo "rev-parse" "HEAD"))
           (part (ai::make-review-loop :author author :tree repo)))
      (ai::worktree-git repo "config"
                        (ai::worktree-base-key (ai::worktree-branch repo)) base)
      (write-file (merge-pathnames "review-added.txt" repo) "Unique diff contents")
      (let ((note (ai::worktree-note part)))
        (true (search (namestring repo) note))
        (true (search base note))
        (true (search "git diff --cached" note))
        (true (search "review-added.txt" note))
        (false (search "Unique diff contents" note))))))

(define-test recovered-review-restores-full-prompt :parent agent-duet
  (multiple-value-bind (workflow lead partner) (recovery-fixture)
    (let* ((step (first (ai::workflow-steps workflow)))
           (part (first (ai:workflow-loops workflow))))
      (setf (ai::loop-summary part) "Latest reviewable report"
            (ai::loop-round part) 1
            (ai::loop-findings part) (list (ai::make-finding :id 1 :text "Pending"))
            (ai::loop-review-history part) (list partner (fake-handle partner))
            (ai::loop-state part) :reviewing
            (ai::step-agent step) partner
            (ai::step-role step) :review
            (ai::step-prompt step) (ai::review-prompt part))
      (false (search "Recover me" (ai::step-prompt step)))
      (let* ((checkpoint (ai::json-copy (ai:workflow-checkpoint workflow)))
             (restored (ai:restore-workflow checkpoint :lead lead :partner partner))
             (prompt (ai::step-prompt (first (ai::workflow-paused restored)))))
        (true (search "Recover me" prompt))
        (true (search "Latest reviewable report" prompt))
        (true (search "F1 [open]" prompt))))))

(define-test correction-preserves-unquoted-findings :parent agent-duet
  (let* ((author (make-instance 'fake-agent :name "Claude"))
         (reviewer (make-instance 'fake-agent :name "ChatGPT"))
         (part (ai::make-review-loop
                :author author :reviewer reviewer
                :findings (list (ai::make-finding :id 1 :text "Current issue")
                                (ai::make-finding :id 2 :text "Older issue")))))
    (let ((prompt (ai::fix-prompt part (format nil "FINDINGS:~%- F1 [open] Current issue~%VERDICT: CHANGES REQUESTED"))))
      (is eql (search "Current issue" prompt) (search "Current issue" prompt :from-end t))
      (true (search "F2 [open] Older issue" prompt)))
    ;; A changed finding or status must not hide the saved open finding.
    (dolist (review (list (format nil "FINDINGS:~%- F1 [open] Changed issue")
                         (format nil "FINDINGS:~%- F1 [fixed] Current issue")
                         "Malformed reply"))
      (true (search "F1 [open] Current issue" (ai::fix-prompt part review))))))

(define-test retained-long-reply-and-fallback :parent agent-duet
  (let* ((directory (merge-pathnames (format nil "duet-artifacts-~a/" (ai::make-uuid))
                                    (uiop:temporary-directory)))
         (ai::*history-directory* directory)
         (ai::*split-reply-limit* 1500)
         (author (make-instance 'fake-agent :permission :full))
         (reviewer (make-instance 'fake-agent :permission :full))
         (workflow (make-instance 'ai:workflow :lead author :partner reviewer))
         (reply (concatenate 'string (make-string 2000 :initial-element #\x) "END")))
    (unwind-protect
         (progn
           (let* ((text (ai::long-part-reply workflow reply))
                  (path (subseq text (+ (search "Full reply: " text) 12))))
             (true (< (length text) (length reply)))
             (is string= reply (uiop:read-file-string path))
             ;; Indexed as an artifact for the reviewers.
             (is eq :artifact (ai::evidence-kind (first (ai::workflow-task-evidence workflow))))
             (true (search path (ai::evidence-summary (first (ai::workflow-task-evidence workflow))))))
           (setf (ai::agent-permission reviewer) :edits)
           (true (search "cut short" (ai::long-part-reply workflow reply)))
           (setf (ai::agent-permission reviewer) :full)
           ;; A file in place of the storage directory forces a write failure.
           (let ((blocked (merge-pathnames "blocked" directory)))
             (write-file blocked "file")
             (let ((ai::*history-directory* (uiop:ensure-directory-pathname blocked)))
               (true (search "cut short" (ai::long-part-reply workflow reply))))))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(define-test medium-reply-falls-back-without-loss :parent agent-duet
  (let* ((workflow (make-instance 'ai:workflow :lead (make-instance 'fake-agent :permission :edits)
                                  :partner (make-instance 'fake-agent :permission :edits)))
         (reply (make-string 9000 :initial-element #\x)))
    (is string= reply (ai::long-part-reply workflow reply))))

(define-test medium-reply-stays-inline :parent agent-duet
  (with-temp-folder (directory)
    (let* ((ai::*history-directory* directory)
           (workflow (make-instance 'ai:workflow
                                   :lead (make-instance 'fake-agent :permission :full)
                                   :partner (make-instance 'fake-agent :permission :full)))
           (reply (make-string 9000 :initial-element #\x))
           (part (ai::make-review-loop :label "Part" :tree directory :summary reply)))
      (setf (ai:workflow-loops workflow) (list part))
      (true (search reply (ai::split-results-text workflow)))
      (false (directory (merge-pathnames "artifacts/*.txt" directory))))))

(define-test shared-task-contract-and-recovery :parent agent-duet
  (multiple-value-bind (workflow messages lead partner)
      (run-fake :review (list (says "A useful substantive answer"))
                (list (says (format nil "FINDINGS: NONE~%VERDICT: APPROVED"))))
    (declare (ignore messages))
    ;; Missing metadata is visible to reviewers, without losing the answer
    ;; or changing existing approval behavior.
    (is eq :approved (ai::loop-state (first (ai:workflow-loops workflow))))
    (true (search "A useful substantive answer" (first (fake-prompts partner))))
    (true (search "verification not reported" (first (fake-prompts partner))))
    (true (search "SHARED TASK CONTRACT" (first (fake-prompts lead))))
    (ai::record-task-direction workflow "Preserve the public interface.")
    (is = 2 (ai::workflow-contract-revision workflow))
    (let* ((cp (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow))))
           (restored (ai:restore-workflow cp :lead lead :partner partner)))
      (is string= (ai::task-contract workflow) (ai::task-contract restored))
      (is string= (ai::loop-handoff-advisory (first (ai:workflow-loops workflow)))
          (ai::loop-handoff-advisory (first (ai:workflow-loops restored)))))
    ;; Old checkpoints default to revision 1 without inventing direction.
    (let ((cp (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow)))))
      (setf (cdr cp) (remove-if (lambda (entry) (member (car entry) '("contract-revision" "contract-directions") :test #'string=)) (cdr cp)))
      (let ((restored (ai:restore-workflow cp :lead lead :partner partner)))
        (is = 1 (ai::workflow-contract-revision restored))
        (false (ai::workflow-contract-directions restored))))))

(define-test handoff-evidence-advisories :parent agent-duet
  (let* ((lead (make-instance 'fake-agent :id :claude :name "Claude"))
         (partner (make-instance 'fake-agent :id :codex :name "ChatGPT"))
         (workflow (make-instance 'ai:workflow :lead lead :partner partner :request "Keep API compatibility"))
         (step (ai::make-step :agent lead :role :work :contract-revision 1))
         (report (format nil "The full answer.~%TASK REVISION: 1~%DECISIONS: Keep the API to preserve callers.~%VERIFICATION: NOT RUN because this is a design task.")))
    (false (ai::handoff-advisory workflow step report))
    (true (search "verification not reported"
                  (ai::handoff-advisory workflow step (format nil "TASK REVISION: 1~%DECISIONS: NONE~%VERIFICATION: "))))
    (true (search "task revision missing or different"
                  (ai::handoff-advisory workflow step (format nil "TASK REVISION: nonsense~%DECISIONS: NONE~%VERIFICATION: passed"))))
    (ai::record-task-direction workflow "Use the new API instead.")
    (true (search "direction changed during this turn" (ai::handoff-advisory workflow step report)))
    (let ((loop (ai::make-review-loop :author lead :reviewer partner :task "Implement API")))
      (setf (ai:workflow-loops workflow) (list loop))
      (true (search "Assignment for Claude (reviewer: ChatGPT)" (ai::task-contract workflow)))
      (true (search "Use the new API instead." (ai::task-contract workflow))))))


(define-test task-contract-context-and-retry :parent agent-duet
  (let* ((lead (make-instance 'fake-agent :id :claude :name "Claude"
                              :replies (loop repeat 7 collect (says "Done"))))
         (partner (make-instance 'fake-agent :id :codex :name "ChatGPT"))
         (workflow (make-instance 'ai:workflow :lead lead :partner partner :request "Preserve API compatibility"))
         (step (ai::make-step :agent lead :role :work)))
    (ai::workflow-agent-submit workflow step lead "Implement" nil)
    (true (search "SHARED TASK CONTRACT" (first (fake-prompts lead))))
    (ai::workflow-agent-submit workflow step lead "Fix" nil)
    (let ((compact (first (fake-prompts lead))))
      (false (search "SHARED TASK CONTRACT" compact))
      (true (search "SHARED TASK CONTRACT" (ai::prompt-without-history lead compact))))
    ;; Nested compact review history must expand alongside the shared brief.
    (ai::remember-context-fallback lead "Short review" "Full review assignment")
    (ai::workflow-agent-submit workflow step lead "Short review" nil)
    (let ((expanded (ai::prompt-without-history lead (first (fake-prompts lead)))))
      (true (search "Full review assignment" expanded))
      (true (search "SHARED TASK CONTRACT" expanded)))
    (ai::record-task-direction workflow "Keep existing callers working.")
    (ai::workflow-agent-submit workflow step lead "Continue" nil)
    (true (search "Keep existing callers working." (first (fake-prompts lead))))
    ;; A changed session cannot rely on its predecessor's contract.
    (setf (fake-handle lead) "new-session")
    (ai::workflow-agent-submit workflow step lead "Continue" nil)
    (true (search "SHARED TASK CONTRACT" (first (fake-prompts lead))))
    ;; Failed delivery/turns must not make a retry assume context was read.
    (setf (fake-replies lead) (list (says '(:error "Disconnected")) (says "Recovered")))
    (ai::workflow-agent-submit workflow step lead "Fail" nil)
    (ai::workflow-agent-submit workflow step lead "Retry" nil)
    (true (search "SHARED TASK CONTRACT" (first (fake-prompts lead))))))

(define-test contract-preserves-step-format-and-request-size :parent agent-duet
  (let* ((lead (make-instance 'fake-agent :id :claude :name "Claude"
                              :replies (loop repeat 4 collect (says "Done"))))
         (partner (make-instance 'fake-agent :id :codex :name "ChatGPT"))
         (request (concatenate 'string "UNIQUE-LARGE-REQUEST" (make-string 20000 :initial-element #\x)))
         (workflow (make-instance 'ai:workflow :lead lead :partner partner :request request))
         (part (ai::make-review-loop :author lead :reviewer partner :task request))
         (plan (ai::plan-prompt partner request))
         (review (ai::full-review-prompt part)))
    (setf (ai:workflow-loops workflow) (list part))
    (false (search request (ai::task-contract workflow)))
    (loop for role in '(:plan :review) for raw in (list plan review) do
      (ai::workflow-agent-submit workflow (ai::make-step :agent lead :role role) lead raw nil)
      (let ((actual (first (fake-prompts lead))))
        ;; The step's own formatting instructions remain the exact suffix,
        ;; on both the first full send and the next compact send.
        (is string= raw (subseq actual (- (length actual) (length raw))))
        (false (search request actual :start2 (1+ (search request actual))))
        (true (< (- (length actual) (length raw)) 1000))))))

(define-test contract-second-turn-after-session-creation :parent agent-duet
  (let* ((lead (make-instance 'fake-agent :id :claude :name "Claude"
                              :replies (list (says :hang) (says "Done"))))
         (partner (make-instance 'fake-agent :id :codex :name "ChatGPT"))
         (workflow (make-instance 'ai:workflow :lead lead :partner partner :request "Implement"))
         (step (ai::make-step :agent lead :role :work)))
    (setf (fake-handle lead) nil)
    (ai::workflow-agent-submit workflow step lead "First" nil)
    (true (search "SHARED TASK CONTRACT" (first (fake-prompts lead))))
    ;; Simulate a CLI announcing its new session before successful completion.
    (setf (fake-handle lead) "created-during-first-turn")
    (ai:agent-turn-finished lead "Done" nil)
    (ai::workflow-agent-submit workflow step lead "Second" nil)
    (false (search "SHARED TASK CONTRACT" (first (fake-prompts lead))))
    (true (search "revision 1 is unchanged" (first (fake-prompts lead))))))

(define-test unchanged-handoff-gaps-are-not-repeated :parent agent-duet
  (multiple-value-bind (workflow messages lead partner)
      (run-fake :review (list (says "First") (says "Second") (says "Third"))
                (list (says (format nil "FINDINGS: NONE~%VERDICT: CHANGES REQUESTED"))
                      (says (format nil "FINDINGS: NONE~%VERDICT: CHANGES REQUESTED"))
                      (says (format nil "FINDINGS: NONE~%VERDICT: APPROVED"))))
    (is eq :approved (ai::loop-state (first (ai:workflow-loops workflow))))
    (is = 1 (count-if (lambda (m) (and (eq :note (second m)) (search "Handoff advisory" (third m)))) messages))
    (is = 1 (count-if (lambda (prompt) (search "Handoff advisory" prompt)) (fake-prompts partner)))
    (let* ((checkpoint (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow))))
           (restored (ai:restore-workflow checkpoint :lead lead :partner partner)))
      (false (ai::loop-handoff-notice (first (ai:workflow-loops restored))))
      (true (ai::loop-handoff-advisory (first (ai:workflow-loops restored)))))))

(define-test refused-plan-keeps-contract-revision :parent agent-duet
  (let* ((lead (make-instance 'fake-agent :id :claude :name "Claude" :replies (list (says :hang))))
         (partner (make-instance 'fake-agent :id :codex :name "ChatGPT" :replies (list (says :hang))))
         (workflow (make-instance 'ai:workflow :lead lead :partner partner :request "Split" :worktrees t))
         (original (symbol-function 'ai::open-worktrees)))
    (setf (ai:workflow-state workflow) :running)
    (true (ai::start-parts workflow "Lead part" "Partner part" :order :circular))
    (is = 1 (ai::workflow-contract-revision workflow))
    (unwind-protect
         (progn
           (setf (symbol-function 'ai::open-worktrees) (lambda (w) (declare (ignore w)) nil))
           (ai::start-parts workflow "Lead part" "Partner part" :order :parallel)
           (is = 1 (ai::workflow-contract-revision workflow))
           (false (fake-prompts lead))
           (false (fake-prompts partner)))
      (setf (symbol-function 'ai::open-worktrees) original))
    (let ((started (make-instance 'ai:workflow :lead lead :partner partner :request "Split")))
      (setf (ai:workflow-state started) :running)
      (ai::start-parts started "Lead part" "Partner part" :order :parallel)
      (is = 2 (ai::workflow-contract-revision started))
      (true (search "SHARED TASK CONTRACT — revision 2" (first (fake-prompts lead))))
      (true (search "SHARED TASK CONTRACT — revision 2" (first (fake-prompts partner)))))))

(define-test review-snapshots-and-deltas :parent agent-duet
  (with-git-folder (repo)
    (let* ((author (make-instance 'fake-agent :name "Claude" :directory repo))
           (reviewer (make-instance 'fake-agent :name "ChatGPT" :directory repo))
           (workflow (make-instance 'ai:workflow :lead author :partner reviewer :request "Do it"))
           (part (ai::make-review-loop :author author :reviewer reviewer :task "Do it" :summary "Report"
                                      :findings (list (ai::make-finding :id 1 :text "Fix this")))))
      (setf (ai::workflow-repository-start workflow) (ai::repository-start-state repo))
      (write-file (merge-pathnames "first.txt" repo) "one")
      ;; The first review is pointed at the task's changes, untracked files included.
      (let ((full (ai::review-prompt part :workflow workflow)))
        (true (search "since the task started" full))
        (true (search "first.txt" full))
        (true (search "Approve only if none" full)))
      ;; The same conversation's next review sees only what changed since.
      (ai::reviewed part)
      (setf (ai::loop-round part) 1
            (ai::loop-review-history part) (list reviewer (fake-handle reviewer)))
      (write-file (merge-pathnames "second.txt" repo) "two")
      (let ((compact (ai::review-prompt part :workflow workflow)))
        (true (search "since your last review" compact))
        (true (search "second.txt" compact))
        (false (search "first.txt" compact))
        (true (search "Approve only if none" compact))
        ;; Lost history replays the full review, with all of the task's changes.
        (let ((expanded (ai::prompt-without-history reviewer compact)))
          (true (search "first.txt" expanded))
          (true (search "second.txt" expanded))))
      (ai::reviewed part)
      (true (search "Nothing has changed" (ai::review-prompt part :workflow workflow)))
      ;; A new conversation hasn't seen the earlier changes: no delta.
      (ai::reviewed part)
      (setf (fake-handle reviewer) "conversation-2")
      (let ((full (ai::review-prompt part :workflow workflow)))
        (false (search "since your last review" full))
        (true (search "first.txt" full))
        (true (search "second.txt" full)))
      ;; Only the pending snapshot and the baseline keep refs.
      (ai::reviewed part)
      (is = 1 (length (remove "" (uiop:split-string
                                  (ai::worktree-git repo "for-each-ref" "--format=%(refname)"
                                                    "refs/agent-duet/snapshots/")
                                  :separator '(#\Newline))
                              :test #'string=))))))

(define-test codex-light-turn-effort :parent agent-duet
  (let ((agent (make-instance 'ai:codex-agent)))
    (setf (ai::agent-model agent) nil
          (ai::codex-agent-thread-id agent) "thread-1"
          (ai::codex-agent-models agent)
          (list (ai:parse-json "{\"id\":\"m\",\"isDefault\":true,\"defaultReasoningEffort\":\"medium\",\"supportedReasoningEfforts\":[{\"reasoningEffort\":\"low\"},{\"reasoningEffort\":\"medium\"}]}")))
    ;; Without a chosen effort the effective one is unknown: never lowered,
    ;; whatever the catalog's default.
    (setf (ai::agent-turn-light agent) t)
    (false (ai::codex-turn-effort agent))
    (false (ai::codex-agent-lowered agent))
    (setf (ai::agent-effort agent) "medium")
    (is string= "low" (ai::codex-turn-effort agent))
    (is string= "low" (ai::codex-turn-effort agent))
    ;; Turn efforts carry on in a Codex thread: the next turn restores it,
    ;; even if the choice was cleared meanwhile.
    (setf (ai::agent-turn-light agent) nil (ai::agent-effort agent) nil)
    (is string= "medium" (ai::codex-turn-effort agent))
    (false (ai::codex-turn-effort agent))
    ;; Never raised.
    (setf (ai::agent-effort agent) "low" (ai::agent-turn-light agent) t)
    (is string= "low" (ai::codex-turn-effort agent))
    (false (ai::codex-agent-lowered agent))
    ;; Another thread, whether a failed resume's replacement or a new or
    ;; resumed conversation, never gets the replaced effort.
    (setf (ai::agent-effort agent) "medium")
    (is string= "low" (ai::codex-turn-effort agent))
    (setf (ai::agent-turn-light agent) nil (ai::agent-effort agent) nil)
    (false (ai::codex-turn-effort agent :thread "thread-2"))
    (setf (ai::agent-effort agent) "medium" (ai::agent-turn-light agent) t)
    (is string= "low" (ai::codex-turn-effort agent))
    (ai::agent-new-conversation agent)
    (false (ai::codex-agent-lowered agent))
    (ai::agent-resume-conversation agent "thread-1")
    (setf (ai::agent-turn-light agent) nil (ai::agent-effort agent) nil)
    (false (ai::codex-turn-effort agent))))

(define-test confirm-turns-are-light :parent agent-duet
  (let* ((lights '())
         (author (make-instance 'fake-agent :id :claude :name "Claude"
                                :replies (list (says "Done"))))
         (reviewer (make-instance 'fake-agent :id :codex :name "ChatGPT"))
         (workflow (make-instance 'ai:workflow :lead author :partner reviewer :request "Do it")))
    (flet ((reply (text)
             (lambda (prompt) (declare (ignore prompt))
               (push (ai::agent-turn-light reviewer) lights)
               text)))
      (setf (fake-replies reviewer)
            (list (reply (format nil "FINDINGS:~%- F1 [open] Fix this~%VERDICT: APPROVED"))
                  (reply (format nil "FINDINGS:~%- F1 [dismissed] Fine~%VERDICT: APPROVED")))))
    (ai:start-workflow workflow)
    (is eq :approved (ai::loop-state (first (ai:workflow-loops workflow))))
    (is equal '(t nil) lights)))

(define-test context-heavy-offer :parent agent-duet
  (let ((agent (make-instance 'fake-agent :name "Claude")))
    (false (ai::agent-context-heavy-p agent))
    (setf (ai::agent-context-tokens agent) 40000 (ai::agent-context-limit agent) 200000)
    (false (ai::agent-context-heavy-p agent))
    (setf (ai::agent-context-tokens agent) 100000)
    (true (ai::agent-context-heavy-p agent))
    (setf (ai::agent-context-limit agent) 0)
    (false (ai::agent-context-heavy-p agent))))

(define-test worktree-delta-review-omits-full-diff :parent agent-duet
  (with-git-folder (repo)
    (let* ((author (make-instance 'fake-agent :name "Claude"))
           (reviewer (make-instance 'fake-agent :name "ChatGPT"))
           (workflow (make-instance 'ai:workflow :lead author :partner reviewer :request "Do it"))
           (part (ai::make-review-loop :author author :reviewer reviewer :task "Do it" :summary "Report"
                                      :tree repo :findings (list (ai::make-finding :id 1 :text "Fix this")))))
      (ai::worktree-git repo "config" (ai::worktree-base-key (ai::worktree-branch repo))
                        (ai::worktree-git-line repo "rev-parse" "HEAD"))
      (write-file (merge-pathnames "first.txt" repo) "one")
      (true (search "git diff --cached" (ai::review-prompt part :workflow workflow)))
      (ai::reviewed part)
      (setf (ai::loop-round part) 1
            (ai::loop-review-history part) (list reviewer (fake-handle reviewer)))
      (write-file (merge-pathnames "second.txt" repo) "two")
      (let ((compact (ai::review-prompt part :workflow workflow)))
        (true (search "since your last review" compact))
        (true (search (namestring repo) compact))
        (false (search "git diff --cached" compact))
        (false (search "first.txt" compact))
        ;; The fallback keeps the full worktree note.
        (true (search "git diff --cached" (ai::prompt-without-history reviewer compact)))))))

;;; Task records: questions between turns, review convergence, decisions,
;;; evidence and work allocation

(define-test blocking-question-holds-work-until-answered :parent agent-duet
  (multiple-value-bind (workflow messages claude codex)
      (run-fake :review
                (list (says (format nil "Started.~%QUESTIONS:~%- Q1 [blocking] Which API version?"))
                      (says "Done with v2."))
                (list (says (format nil "ANSWERS:~%- Q1 Use v2."))
                      (says (format nil "FINDINGS: NONE~%VERDICT: APPROVED"))))
    (is eq :approved (ai::loop-state (first (ai:workflow-loops workflow))))
    (let ((question (first (ai::workflow-task-questions workflow)))
          (asked (car (last (fake-prompts codex)))))
      (is string= "Use v2." (ai::task-question-answer question))
      (is eq :claude (ai::task-question-from question))
      (is eq :codex (ai::task-question-to question))
      ;; The partner gets its own turn at the boundary, with the question.
      (true (search "Questions for you from Claude" asked))
      (true (search "Which API version?" asked))
      (true (search "Don't change any files" asked)))
    ;; The asker goes on with the answer linked to its question.
    (let ((resumed (first (fake-prompts claude))))
      (true (search "blocking question has been answered" resumed))
      (true (search "Q1 [blocking] Which API version? Answer: Use v2." resumed)))
    (true (find "Claude asks ChatGPT (Q1, blocking)" messages :key #'third :test #'search))
    (true (find "waits for the answer to Q1" messages :key #'third :test #'search))
    ;; The hold is blocked time, opened and closed once.
    (is equal '(:blocked-start :blocked-end)
        (mapcar (lambda (e) (getf e :kind))
                (remove-if-not (lambda (e) (member (getf e :kind) '(:blocked-start :blocked-end)))
                               (ai::workflow-efficiency-events workflow))))
    (false (ai::workflow-question-holds workflow))))

(define-test non-blocking-question-waits-for-next-turn :parent agent-duet
  (multiple-value-bind (workflow messages claude codex)
      (run-fake :review
                (list (says (format nil "Done.~%QUESTIONS:~%- Q1 [non-blocking] Is frob a good name?"))
                      (says "Renamed."))
                (list (says (format nil "ANSWERS:~%- Q1 Prefer frobnicate.~%FINDINGS:~%- F1 [open] Rename it~%VERDICT: CHANGES REQUESTED"))
                      (says (format nil "FINDINGS:~%- F1 [fixed] Rename it~%VERDICT: APPROVED"))))
    (declare (ignore messages))
    (is eq :approved (ai::loop-state (first (ai:workflow-loops workflow))))
    ;; No extra turn: the reviewer sees it with its review, the asker the answer with its fix.
    (is = 2 (length (fake-prompts codex)))
    (true (search "Is frob a good name?" (car (last (fake-prompts codex)))))
    (true (search "Answers to your questions" (first (fake-prompts claude))))
    (true (search "Prefer frobnicate." (first (fake-prompts claude))))
    ;; Given once: the next review doesn't repeat the answered question.
    (false (search "Questions for you" (first (fake-prompts codex))))))

(define-test unanswered-blocking-question-is-given-up :parent agent-duet
  (multiple-value-bind (workflow messages claude codex)
      (run-fake :review
                (list (says (format nil "Started.~%QUESTIONS:~%- Q1 [blocking] Which API?"))
                      (says "Done anyway."))
                (list (says "I don't know.")
                      (says (format nil "FINDINGS: NONE~%VERDICT: APPROVED"))))
    (declare (ignore messages codex))
    (is eq :approved (ai::loop-state (first (ai:workflow-loops workflow))))
    (is string= "No answer was given." (ai::task-question-answer (first (ai::workflow-task-questions workflow))))
    (true (search "No answer was given." (first (fake-prompts claude))))))

(define-test held-work-presentation :parent agent-duet
  (multiple-value-bind (workflow messages claude codex)
      (run-fake :review (list (says (format nil "Started.~%QUESTIONS:~%- Q1 [blocking] Which API?")))
                (list (says :hang)))
    (declare (ignore messages))
    (is string= "Waiting for an answer" (getf (ai::workflow-agent-presentation workflow claude) :label))
    (is string= "Answering questions" (getf (ai::workflow-agent-presentation workflow codex) :label))
    (is string= "Waiting for an answer"
        (getf (ai::workflow-progress-presentation workflow) :label))
    ;; Restart keeps the hold and the answer turn, rather than redoing the work.
    (let* ((cp (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow))))
           (restored (ai:restore-workflow cp :lead claude :partner codex)))
      (is = 1 (length (ai::workflow-question-holds restored)))
      (is equal '(:answer) (mapcar #'ai::step-role (ai:workflow-paused restored)))
      (false (ai::task-question-answer (first (ai::workflow-task-questions restored)))))
    (ai:agent-cancel codex)))

(define-test finding-severity-and-suggestions :parent agent-duet
  (let ((f (ai::parse-finding-line "- F2 [open] [suggestion] `a.lisp:4` Rename it")))
    (is eq :suggestion (getf f :severity))
    (is eq :open (getf f :status))
    (is string= "a.lisp:4" (getf f :location))
    (is string= "Rename it" (getf f :text)))
  (is eq :blocker (getf (ai::parse-finding-line "- F1 [blocker] [open] Bug") :severity))
  (false (getf (ai::parse-finding-line "- F1 [open] Bug") :severity))
  ;; An unknown second tag stays in the text.
  (is string= "[later] Bug" (getf (ai::parse-finding-line "- F1 [open] [later] Bug") :text))
  (let ((merged (ai::merge-findings '() (ai::parse-findings (format nil "FINDINGS:~%- F1 [open] Bug~%- F2 [open] [suggestion] Style")) 1)))
    (is equal '(:blocker :suggestion) (mapcar #'ai::finding-severity merged))
    (true (search "- F2 [open] [suggestion] Style" (ai::findings-lines merged)))
    (is = 1 (ai::open-blockers merged)))
  ;; Approval with only suggestions open needs no settling turn.
  (multiple-value-bind (workflow messages claude codex)
      (run-fake :review (list (says "Done"))
                (list (says (format nil "FINDINGS:~%- F1 [open] [suggestion] Nicer name~%VERDICT: APPROVED"))))
    (declare (ignore claude))
    (is eq :approved (ai::loop-state (first (ai:workflow-loops workflow))))
    (is = 1 (length (fake-prompts codex)))
    (true (search "[suggestion]" (first (fake-prompts codex))))
    (true (find "1 suggestion open" messages :key #'third :test #'search))
    (true (some (lambda (line) (search "suggestions: 1" line))
                (ai::task-summary-lines (ai::workflow-completion-data workflow))))))

(define-test disputed-blocker-escalates-to-user :parent agent-duet
  (let ((review (format nil "FINDINGS:~%- F1 [open] `a.lisp:1` Use a hash table~%VERDICT: CHANGES REQUESTED")))
    (multiple-value-bind (workflow messages claude codex)
        (run-fake :review
                  (list (says "Done")
                        (says (format nil "A list is fine here.~%DISPUTED: F1"))
                        (says (format nil "Still fine.~%DISPUTED: F1")))
                  (list (says review) (says review) (says review)))
      (declare (ignore codex))
      (let* ((loop (first (ai:workflow-loops workflow)))
             (step (first (ai:workflow-paused workflow))))
        (is eq :disagreement (ai::step-reason step))
        (is eq :paused (ai::loop-state loop))
        (is = 3 (length (fake-prompts claude)))
        (true (search "DISPUTED: F2, F5" (first (fake-prompts claude))))
        (true (find "Claude disputes F1 and ChatGPT keeps it open" messages :key #'third :test #'search))
        (true (search "disputed" (getf (ai::workflow-progress-presentation workflow) :label)))
        ;; Saved and restored with its disputes.
        (let* ((cp (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow))))
               (restored (ai:restore-workflow cp :lead claude :partner (ai::workflow-partner workflow))))
          (is = 2 (ai::finding-disputes (first (ai::loop-findings (first (ai:workflow-loops restored))))))
          (is eq :disagreement (ai::step-reason (first (ai:workflow-paused restored)))))
        ;; The user dismisses it: with no blocker left, it's approved.
        (true (ai::decide-disagreement workflow step :dismiss))
        (is eq :dismissed (ai::finding-status (first (ai::loop-findings loop))))
        (is eq :approved (ai::loop-state loop))
        (is eq :finished (ai:workflow-state workflow))))))

(define-test disputed-blocker-kept-by-user :parent agent-duet
  (let ((review (format nil "FINDINGS:~%- F1 [open] Use a hash table~%VERDICT: CHANGES REQUESTED")))
    (multiple-value-bind (workflow messages claude codex)
        (run-fake :review
                  (list (says "Done") (says (format nil "No.~%DISPUTED: F1")) (says (format nil "No.~%DISPUTED: F1"))
                        (says "Switched to a hash table."))
                  (list (says review) (says review) (says review)
                        (says (format nil "FINDINGS:~%- F1 [fixed] Use a hash table~%VERDICT: APPROVED"))))
      (declare (ignore messages codex))
      (let ((step (find :disagreement (ai:workflow-paused workflow) :key #'ai::step-reason)))
        (true step)
        (true (ai::decide-disagreement workflow step :keep))
        (true (search "decided that these findings stand" (first (fake-prompts claude))))
        (is = 0 (ai::finding-disputes (first (ai::loop-findings (first (ai:workflow-loops workflow))))))
        (is eq :approved (ai::loop-state (first (ai:workflow-loops workflow))))))))

(defun disputed-workflow ()
  "A review workflow (default rounds) paused for the user: F1 disputed twice."
  (let ((review (format nil "FINDINGS:~%- F1 [open] Use a hash table~%VERDICT: CHANGES REQUESTED")))
    (run-fake :review
              (list (says "Done") (says (format nil "No.~%DISPUTED: F1")) (says (format nil "No.~%DISPUTED: F1"))
                    (says "Fixed it."))
              (list (says review) (says review) (says review)))))

(define-test dismissed-dispute-with-failing-check-not-approved :parent agent-duet
  (multiple-value-bind (workflow messages claude) (disputed-workflow)
    (declare (ignore messages))
    (let ((loop (first (ai:workflow-loops workflow)))
          (step (first (ai:workflow-paused workflow))))
      (setf (ai::loop-check-failed loop) t)
      (true (ai::decide-disagreement workflow step :dismiss))
      (is eq :dismissed (ai::finding-status (first (ai::loop-findings loop))))
      (false (eq :approved (ai::loop-state loop)))
      (true (search "The check still fails" (first (fake-prompts claude)))))))

(define-test restored-dispute-asks-again :parent agent-duet
  (multiple-value-bind (workflow messages claude codex) (disputed-workflow)
    (declare (ignore messages))
    (let* ((cp (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow))))
           (asked nil)
           (restored (ai:restore-workflow cp :lead claude :partner codex
                                             :on-pause (lambda (w s) (declare (ignore w)) (push s asked))))
           (prompts (length (fake-prompts claude))))
      (is eq :started (ai:recover-workflow restored :resume))
      ;; No fix work replayed: the decision is asked for again.
      (is = prompts (length (fake-prompts claude)))
      (is eq :disagreement (ai::step-reason (first asked)))
      (true (ai::decide-disagreement restored (first asked) :keep))
      (true (search "decided that these findings stand" (first (fake-prompts claude))))
      (is = 0 (ai::finding-disputes (first (ai::loop-findings (first (ai:workflow-loops restored)))))))))

(define-test decisions-steering-and-records-restart :parent agent-duet
  (multiple-value-bind (workflow messages claude codex)
      (run-fake :review
                (list (says (format nil "Done.~%TASK REVISION: 1~%DECISIONS:~%- D1 Keep the list — because callers rely on order; rejected: hash table~%VERIFICATION: make test passed")))
                (list (says (format nil "FINDINGS: NONE~%VERDICT: APPROVED"))))
    (declare (ignore messages))
    ;; A decision section counts as reported decisions.
    (false (search "decisions not reported" (first (fake-prompts codex))))
    (let ((decision (first (ai::workflow-task-decisions workflow))))
      (is string= "Keep the list" (ai::task-decision-text decision))
      (is equal '("hash table") (ai::task-decision-rejected decision))
      (true (search "D1 Keep the list" (ai::task-contract workflow)))
      (ai::record-task-direction workflow "Use a hash table after all.")
      (true (ai::task-decision-stale decision))
      (true (search "[stale]" (ai::task-contract workflow))))
    ;; The author's verification is indexed as its own claim.
    (let ((evidence (first (ai::workflow-task-evidence workflow))))
      (is eq :discovery (ai::evidence-kind evidence))
      (is eq :claude (ai::evidence-source evidence))
      (true (search "make test passed" (ai::evidence-summary evidence))))
    (let* ((cp (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow))))
           (restored (ai:restore-workflow cp :lead claude :partner codex)))
      (is string= (ai::task-contract workflow) (ai::task-contract restored))
      (is = 1 (length (ai::workflow-task-evidence restored)))
      (is equal (ai::workflow-efficiency-events workflow) (ai::workflow-efficiency-events restored)))
    ;; Older checkpoints have no records.
    (let ((cp (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow)))))
      (setf (cdr cp) (remove "task-records" (cdr cp) :key #'car :test #'string=))
      (let ((restored (ai:restore-workflow cp :lead claude :partner codex)))
        (false (ai::workflow-task-decisions restored))
        (false (ai::workflow-task-questions restored))))))

(define-test review-evidence-current-or-stale :parent agent-duet
  (with-git-folder (repo)
    (let* ((author (make-instance 'fake-agent :name "Claude" :directory repo))
           (reviewer (make-instance 'fake-agent :name "ChatGPT" :directory repo))
           (workflow (make-instance 'ai:workflow :lead author :partner reviewer :request "Do it"))
           (part (ai::make-review-loop :author author :reviewer reviewer :task "Do it" :summary "Report")))
      (write-file (merge-pathnames "first.txt" repo) "one")
      (let ((state (ai::repository-state-id repo)))
        (true state)
        ;; The same content is the same state, and leaves no snapshot ref behind.
        (is string= state (ai::repository-state-id repo))
        (ai::add-evidence workflow :check "Check `make test` passed (exit code 0)." state :check))
      (let ((prompt (ai::review-prompt part :workflow workflow)))
        (true (search "Evidence so far" prompt))
        (true (search "[current]" prompt))
        (true (search "verify independently" prompt)))
      (ai::reviewed part)
      (write-file (merge-pathnames "first.txt" repo) "changed")
      (true (search "[stale]" (ai::review-prompt part :workflow workflow))))))

(define-test plan-order-reason :parent agent-duet
  (multiple-value-bind (lead partner order reason)
      (ai:parse-plan (format nil "LEAD TASK:~%The parser~%~%PARTNER TASK:~%The tests~%~%ORDER: LEAD FIRST~%ORDER REASON: the tests need the parser's interface"))
    (is string= "The parser" lead)
    (is string= "The tests" partner)
    (is eq :lead-first order)
    (is string= "the tests need the parser's interface" reason))
  (is eq nil (nth-value 3 (ai:parse-plan (format nil "LEAD TASK:~%A~%PARTNER TASK:~%B~%ORDER: PARALLEL"))))
  (let ((form (ai::plan-form (make-instance 'fake-agent :id :codex :name "ChatGPT"))))
    (dolist (text '("ORDER REASON:" "depends on" "interfaces" "integrating and reviewing"))
      (true (search text form))))
  (multiple-value-bind (workflow messages)
      (run-fake :divide
                (lambda (prompt)
                  (cond ((search "Review the work" prompt) *approve*)
                        ((search "Split the work" prompt)
                         (format nil "LEAD TASK:~%A~%~%PARTNER TASK:~%B~%~%ORDER: PARALLEL~%ORDER REASON: independent files"))
                        (t "A done")))
                (lambda (prompt) (if (search "Review the work" prompt) *approve* "B done")))
    (declare (ignore workflow))
    (true (find "Order: parallel: independent files" messages :key #'third :test #'search))))

(define-test answer-repeated-after-failed-turn :parent agent-duet
  (multiple-value-bind (workflow messages claude codex)
      (run-fake :review
                (list (says (format nil "Done.~%QUESTIONS:~%- Q1 [non-blocking] Is frob a good name?"))
                      (says '(:error "Disconnected")))
                (list (says (format nil "ANSWERS:~%- Q1 Prefer frobnicate.~%FINDINGS:~%- F1 [open] Rename it~%VERDICT: CHANGES REQUESTED"))))
    (declare (ignore messages))
    ;; The turn carrying the answer failed: it doesn't count as given.
    (true (search "Prefer frobnicate." (first (fake-prompts claude))))
    (false (ai::workflow-delivered-answers workflow))
    (let* ((cp (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow))))
           (restored (ai:restore-workflow cp :lead claude :partner codex)))
      (setf (fake-replies claude) (list (says "Renamed."))
            (fake-replies codex) (list (says (format nil "FINDINGS:~%- F1 [fixed] Rename it~%VERDICT: APPROVED"))))
      (is eq :started (ai:recover-workflow restored :resume))
      (true (search "Prefer frobnicate." (first (fake-prompts claude))))
      (is equal '(1) (ai::workflow-delivered-answers restored)))))

(define-test review-only-open-suggestions-approve :parent agent-duet
  (with-git-folder (repo)
    (let ((approved (format nil "FINDINGS:~%- F1 [open] [suggestion] `a.lisp:1` Nicer name~%VERDICT: APPROVED")))
      (multiple-value-bind (workflow claude codex)
          (review-only-fixture repo :agents :lead :reply (lambda (a p) (declare (ignore a p)) approved))
        (declare (ignore codex))
        (ai:start-workflow workflow)
        (is eq :approved (ai::loop-state (first (ai::workflow-loops workflow))))
        (true (search "[suggestion]" (first (fake-prompts claude)))))
      ;; The restored continuation decides the same way.
      (multiple-value-bind (workflow claude codex)
          (review-only-fixture repo :agents :lead :reply (lambda (a p) (declare (ignore a p)) '(:error "failed")))
        (ai:start-workflow workflow)
        (is eq :paused (ai::loop-state (first (ai::workflow-loops workflow))))
        (let* ((cp (ai:parse-json (ai:json-string (ai:workflow-checkpoint workflow))))
               (restored (ai:restore-workflow cp :lead claude :partner codex)))
          (setf (fake-replies claude) (list (says approved)))
          (is eq :started (ai:recover-workflow restored :resume))
          (is eq :approved (ai::loop-state (first (ai::workflow-loops restored)))))))))
