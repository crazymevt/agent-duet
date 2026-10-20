;;;; read-only.lisp — review-only turns: enforced on both agents, whatever the permission

(in-package #:agent-duet/tests)

(defun python-missing-p ()
  (null (ignore-errors (uiop:run-program '("python3" "--version") :output :string))))

(defun argument-after (flag args)
  (second (member flag args :test #'string=)))

;;; What's read-only

(define-test read-only-commands
    :parent agent-duet
  (dolist (command '("git diff" "git diff main...feature" "git log --oneline -5" "git show HEAD~1:src/a.lisp"
                     "git status --short" "git merge-base main HEAD" "git log --format='%h %s'"
                     "ls -la src" "cat README.md" "grep -n defun src/agents.lisp" "  git  diff  "))
    (true (ai::read-only-command-p command) "~a" command))
  (dolist (command '("rm -rf build" "git commit -am x" "git checkout -- ." "git branch -D main"
                     "git -c core.pager=sh diff" "git diff > out.patch" "git diff --output=out.patch"
                     "git log | tee log.txt" "cat a; rm a" "git status && make" "ls $(rm x)"
                     "cat `rm x`" (format nil "ls~%rm x") "sed -i s/a/b/ f" "find . -delete"
                     "git" "" nil))
    (false (ai::read-only-command-p command) "~a" command)))

(define-test claude-read-only-requests
    :parent agent-duet
  (true (ai::claude-read-only-request-p "Read" (ai:jobj "file_path" "/etc/hosts")))
  (true (ai::claude-read-only-request-p "Grep" (ai:jobj "pattern" "x")))
  (true (ai::claude-read-only-request-p "Bash" (ai:jobj "command" "git diff HEAD")))
  (false (ai::claude-read-only-request-p "Bash" (ai:jobj "command" "make test")))
  (false (ai::claude-read-only-request-p "Bash" (ai:jobj)))
  (dolist (tool '("Edit" "MultiEdit" "Write" "NotebookEdit" "WebFetch"))
    (false (ai::claude-read-only-request-p tool (ai:jobj "file_path" "a")) "~a" tool)))

;;; Claude's command line

(define-test claude-read-only-arguments
    :parent agent-duet
  (dolist (permission ai::*permissions*)
    (let ((agent (make-instance 'ai:claude-agent :permission permission)))
      ;; Writable: as the permission says, nothing taken away.
      (let ((args (ai::claude-agent-arguments agent :session-id "s")))
        (false (member "--disallowedTools" args :test #'string=) "~a" permission)
        (is string= (if (eq permission :full) "bypassPermissions" "acceptEdits")
            (argument-after "--permission-mode" args)))
      ;; Read-only: the same whatever the permission.
      (setf (ai:agent-read-only-p agent) t)
      (let ((args (ai::claude-agent-arguments agent :resume "s")))
        (is string= "default" (argument-after "--permission-mode" args) "~a" permission)
        (let ((disallowed (uiop:split-string (argument-after "--disallowedTools" args) :separator ",")))
          (dolist (tool '("Edit" "Write" "NotebookEdit"))
            (true (member tool disallowed :test #'string=) "~a disallowed with ~a" tool permission)))
        ;; Nothing that runs commands goes unasked: those questions are answered by rule.
        (false (search "Bash" (argument-after "--allowedTools" args)) "~a" permission)
        (is string= "" (argument-after "--setting-sources" args))
        (true (member "--strict-mcp-config" args :test #'string=))
        (is string= "{\"mcpServers\":{}}" (argument-after "--mcp-config" args))
        (true (member "mcp__*" (uiop:split-string (argument-after "--disallowedTools" args) :separator ",") :test #'string=))
        (false (search "Agent" (argument-after "--tools" args)))
        (true (ai:jget (ai:parse-json (argument-after "--settings" args)) "hooks" "PreToolUse" 0 "hooks" 0 "command"))
        (is string= "stdio" (argument-after "--permission-prompt-tool" args))
        (is string= "s" (argument-after "--resume" args))))))

;;; Claude's permission requests, against a running process

(define-test claude-read-only-permissions
    :parent agent-duet
  (if (python-missing-p)
      (skip "python3 is needed for the fake Claude" (true t))
      (dolist (case '(("Bash" "rm -rf build" "deny")
                      ("Bash" "git diff HEAD" "allow")
                      ("Read" nil "allow")
                      ("Write" nil "deny")))
        (destructuring-bind (tool command behavior) case
          (with-fake-claude (agent directory notes)
            (let ((results nil) (approvals nil))
              ;; Ask mode would ask the user: read-only decides by itself.
              (setf (ai:agent-permission agent) :ask
                    (ai:agent-read-only-p agent) t
                    (ai:agent-listener agent)
                    (lambda (agent kind &rest plist)
                      (declare (ignore agent plist))
                      (when (eq kind :approval) (push kind approvals))))
              (write-file (merge-pathnames "permission-request" directory)
                          (ai:json-string (ai:jobj "tool_name" tool
                                                   "input" (if command
                                                               (ai:jobj "command" command)
                                                               (ai:jobj "file_path" "x")))))
              (ai:agent-submit agent "review" (lambda (text error) (push (list text error) results)))
              (true (wait-until (lambda () results)))
              (is equal (list (format nil "fresh reply (~a)" behavior) nil) (first results) "~a ~a" tool command)
              (false approvals)
              (false (ai::agent-approvals agent))))))))

(define-test claude-ask-mode-still-asks
    :parent agent-duet
  ;; Once read-only is cleared, Ask mode asks the user again.
  (if (python-missing-p)
      (skip "python3 is needed for the fake Claude" (true t))
      (with-fake-claude (agent directory notes)
        (let ((results nil))
          (setf (ai:agent-permission agent) :ask
                (ai:agent-read-only-p agent) t)
          (ai:agent-submit agent "review" (lambda (text error) (push (list text error) results)))
          (true (wait-until (lambda () results)))
          (setf (ai:agent-read-only-p agent) nil)
          (write-file (merge-pathnames "permission-request" directory)
                      (ai:json-string (ai:jobj "tool_name" "Bash" "input" (ai:jobj "command" "make test"))))
          (ai:agent-submit agent "fix" (lambda (text error) (push (list text error) results)))
          (true (wait-until (lambda () (ai::agent-approvals agent))))
          (ai::agent-answer-approval agent (car (first (ai::agent-approvals agent))) :allow)
          (true (wait-until (lambda () (= 2 (length results)))))
          (is equal '("fresh reply (allow)" nil) (first results))))))

;;; Claude's process across read-only and back

(define-test claude-read-only-session-transitions
    :parent agent-duet
  (if (python-missing-p)
      (skip "python3 is needed for the fake Claude" (true t))
      (with-fake-claude (agent directory notes)
        (let ((results nil))
          (setf (ai:agent-permission agent) :full)
          (flet ((turn (prompt)
                   (let ((n (length results)))
                     (ai:agent-submit agent prompt (lambda (text error) (push (list text error) results)))
                     (true (wait-until (lambda () (> (length results) n))))
                     (is equal '("fresh reply" nil) (first results))))
                 (args (n)
                   (uiop:split-string (nth n (uiop:read-file-lines (merge-pathnames "args" directory)))
                                      :separator " ")))
            (turn "write")
            (let ((session (ai::agent-conversation-handle agent))
                  (writable (ai::claude-agent-process agent)))
              (setf (ai:agent-read-only-p agent) t)
              (turn "review")
              ;; A new process, carrying on the conversation, without edit tools.
              (isnt eq writable (ai::claude-agent-process agent))
              (is string= session (argument-after "--resume" (args 1)))
              (true (search "Edit,MultiEdit,Write,NotebookEdit" (argument-after "--disallowedTools" (args 1))))
              (is string= "default" (argument-after "--permission-mode" (args 1)))
              ;; Another read-only turn keeps that process.
              (let ((read-only (ai::claude-agent-process agent)))
                (turn "review again")
                (is eq read-only (ai::claude-agent-process agent))
                (is = 2 (length (uiop:read-file-lines (merge-pathnames "args" directory))))
                ;; Cleared: back to the permission, in the same conversation.
                (setf (ai:agent-read-only-p agent) nil)
                (turn "fix")
                (isnt eq read-only (ai::claude-agent-process agent))
                (is string= session (argument-after "--resume" (args 2)))
                (false (member "--disallowedTools" (args 2) :test #'string=))
                (is string= "bypassPermissions" (argument-after "--permission-mode" (args 2)))
                (is string= session (ai::agent-conversation-handle agent))
                (false notes))))))))

(define-test claude-read-only-refuses-steering-writable-turn
    :parent agent-duet
  ;; A turn started writable can't be given more once read-only is set.
  (let ((agent (make-instance 'ai:claude-agent :permission :full))
        (cp (ai::make-claude-process)))
    (setf (ai::claude-agent-process agent) cp
          (ai::claude-agent-config agent) (list "sonnet" nil :full nil t nil)
          (ai::agent-busy agent) t
          (ai:agent-read-only-p agent) t)
    (fail (ai::agent-steer agent "now review it"))
    (false (ai::claude-agent-prompts agent))))

;;; Codex

(defun fake-requests (directory method)
  "The params of each METHOD request the fake Codex in DIRECTORY received."
  (let ((file (merge-pathnames "requests" directory)))
    (loop for line in (and (probe-file file) (uiop:read-file-lines file))
          for request = (ai:parse-json line)
          when (equal (ai:jget request "method") method)
            collect (ai:jget request "params"))))

(define-test codex-read-only-policy
    :parent agent-duet
  (dolist (permission ai::*permissions*)
    (let ((agent (make-instance 'ai:codex-agent :permission permission :read-only t)))
      (is string= "read-only" (ai::codex-agent-sandbox agent) "~a" permission)
      (is string= "never" (ai::codex-agent-approval-policy agent) "~a" permission)
      (setf (ai:agent-read-only-p agent) nil)
      (isnt string= "read-only" (ai::codex-agent-sandbox agent) "~a" permission))))

(define-test codex-read-only-session-transitions
    :parent agent-duet
  (if (python-missing-p)
      (skip "python3 is needed for the fake Codex" (true t))
      (with-fake-codex (agent directory)
        (let ((results nil))
          (setf (ai:agent-permission agent) :ask)
          (flet ((turn (prompt)
                   (let ((n (length results)))
                     (ai:agent-submit agent prompt (lambda (text error) (push (list text error) results)))
                     (true (wait-until (lambda () (> (length results) n))))
                     (is equal '("hi" nil) (first results)))))
            (turn "write")
            (let ((writable (ai::codex-agent-server agent)))
              (is string= "workspace-write" (ai:jget (first (fake-requests directory "thread/start")) "sandbox"))
              (is string= "untrusted" (ai:jget (first (fake-requests directory "turn/start")) "approvalPolicy"))
              (setf (ai:agent-read-only-p agent) t)
              (turn "review")
              ;; A new server, resuming the thread in the read-only sandbox, never asking.
              (isnt eq writable (ai::codex-agent-server agent))
              (let ((resume (first (fake-requests directory "thread/resume"))))
                (is string= "thread-1" (ai:jget resume "threadId"))
                (is string= "read-only" (ai:jget resume "sandbox"))
                (is string= "never" (ai:jget resume "approvalPolicy")))
              (is string= "never" (ai:jget (second (fake-requests directory "turn/start")) "approvalPolicy"))
              ;; Cleared: back to the permission's sandbox, same thread.
              (let ((read-only (ai::codex-agent-server agent)))
                (setf (ai:agent-read-only-p agent) nil)
                (turn "fix")
                (isnt eq read-only (ai::codex-agent-server agent))
                (let ((resume (second (fake-requests directory "thread/resume"))))
                  (is string= "thread-1" (ai:jget resume "threadId"))
                  (is string= "workspace-write" (ai:jget resume "sandbox"))
                  (is string= "untrusted" (ai:jget resume "approvalPolicy")))
                (is string= "untrusted" (ai:jget (third (fake-requests directory "turn/start")) "approvalPolicy"))
                (is = 1 (length (fake-requests directory "thread/start"))))))))))

(define-test codex-read-only-declines-approvals
    :parent agent-duet
  (if (python-missing-p)
      (skip "python3 is needed for the fake Codex" (true t))
      (with-fake-codex (agent directory)
        (let ((results nil) (approvals nil))
          (setf (ai:agent-permission agent) :ask
                (ai:agent-read-only-p agent) t
                (ai:agent-listener agent)
                (lambda (agent kind &rest plist)
                  (declare (ignore agent plist))
                  (when (eq kind :approval) (push kind approvals))))
          (touch directory "request-approval")
          (ai:agent-submit agent "review" (lambda (text error) (push (list text error) results)))
          (true (wait-until (lambda () results)))
          (is string= "decline" (uiop:read-file-string (merge-pathnames "approval-decision" directory)))
          (false approvals)
          (false (ai::agent-approvals agent))))))

(define-test codex-read-only-refuses-steering-writable-turn
    :parent agent-duet
  (let ((agent (make-instance 'ai:codex-agent :permission :full)))
    (setf (ai::codex-agent-config agent) (list "danger-full-access" nil t)
          (ai::agent-busy agent) t
          (ai:agent-read-only-p agent) t)
    (fail (ai::agent-steer agent "now review it"))))

;;; Regression cases from the review: shell normalization and Git helpers.

(define-test read-only-shell-and-git-options
    :parent agent-duet
  (dolist (command '("git diff --out'put'=review.patch" "git diff --out=review.patch"
                     "git diff --out\"put\"=review.patch" "git diff --ext-diff"
                     "git show --textconv HEAD:README.md" "git cat-file --filters HEAD:README.md"
                     "git cat-file --textc HEAD:README.md" "git log --show-signature"
                     "git diff --submodule=diff" "git diff *" "git diff 'unterminated"))
    (false (ai::read-only-command-p command) "~a" command))
  (is equal '("git" "log" "--format=%h %s")
      (ai::read-only-command-words "git log --format='%h %s'")))

(defun run-review-hook (name input)
  (let* ((settings (ai:parse-json (ai::claude-read-only-settings)))
         (command (ai:jget settings "hooks" "PreToolUse" 0 "hooks" 0 "command")))
    (multiple-value-bind (out errors code)
        (uiop:run-program (list "/bin/sh" "-c" command)
                          :input (make-string-input-stream
                                  (ai:json-string (ai:jobj "tool_name" name "tool_input" input)))
                          :output :string :error-output :string :ignore-error-status t)
      (declare (ignore errors))
      (values (and (zerop code) (ai:parse-json out)) code))))

(define-test read-only-hook-enforcement
    :parent agent-duet
  (unless (python-missing-p)
    (dolist (name '("Write" "Agent" "Task" "Skill" "mcp__files__write"))
      (is string= "deny" (ai:jget (run-review-hook name (ai:jobj))
                                 "hookSpecificOutput" "permissionDecision") "~a" name))
    (dolist (command '("git diff --out'put'=review.patch" "git diff --out=review.patch"
                       "git diff --ext-diff" "git show --textconv HEAD:README.md"
                       "git cat-file --filters HEAD:README.md" "rm -rf build"
                       "git log --show-signature" "git diff --submodule=diff" "git diff *"))
      (is string= "deny" (ai:jget (run-review-hook "Bash" (ai:jobj "command" command))
                                 "hookSpecificOutput" "permissionDecision") "~a" command))
    (let* ((result (run-review-hook "Bash" (ai:jobj "command" "git diff HEAD -- shared.txt")))
           (output (ai:jget result "hookSpecificOutput"))
           (command (ai:jget output "updatedInput" "command")))
      (is string= "allow" (ai:jget output "permissionDecision"))
      (true (search "--no-ext-diff" command))
      (true (search "--no-textconv" command))
      (true (search "core.fsmonitor=false" command))
      (true (search "GIT_NO_LAZY_FETCH=1" command)))
    ;; Questions still reach our UI, not an auto-approval from the hook.
    (false (ai:jget (run-review-hook "AskUserQuestion" (ai:jobj)) "hookSpecificOutput"))
    (is = 2 (nth-value 1 (run-review-hook "Bash" (ai:jobj "command" "git diff 'unterminated"))))))

(define-test claude-review-overrides-inherited-allow
    :parent agent-duet
  (unless (python-missing-p)
    (dolist (tool '("Bash" "mcp__files__write"))
      (with-fake-claude (agent directory notes)
        (let ((results nil))
          (setf (ai:agent-read-only-p agent) t (ai:agent-permission agent) :full)
          (touch directory "inherited-allow")
          (touch directory "execute-command")
          (write-file (merge-pathnames "permission-request" directory)
                      (ai:json-string (ai:jobj "tool_name" tool "input"
                                               (ai:jobj "command" "git diff --out'put'=escaped.patch"))))
          (ai:agent-submit agent "review" (lambda (text error) (push (list text error) results)))
          (true (wait-until (lambda () results)))
          (is equal '("fresh reply (deny)" nil) (first results))
          (false (probe-file (merge-pathnames "escaped.patch" directory)))
          (is string= "deny" (ai:jget (ai:parse-json (first (uiop:read-file-lines (merge-pathnames "hook-decisions" directory))))
                                     "permissionDecision")))))))

(define-test claude-review-disables-configured-git-helpers
    :parent agent-duet
  (unless (python-missing-p)
    (with-fake-claude (agent directory notes)
      (let ((results nil))
        (ai::worktree-git directory "init")
        (ai::worktree-git directory "config" "user.name" "Test")
        (ai::worktree-git directory "config" "user.email" "test@example.invalid")
        (ai::worktree-git directory "config" "commit.gpgsign" "false")
        (write-file (merge-pathnames "file.txt" directory) "before")
        (write-file (merge-pathnames ".gitattributes" directory) "file.txt diff=review-test")
        (ai::worktree-git directory "add" "file.txt" ".gitattributes")
        (ai::worktree-git directory "commit" "-m" "initial")
        (write-file (merge-pathnames "file.txt" directory) "after")
        ;; These helpers would create marker files if Git ran them.
        (ai::worktree-git directory "config" "diff.external" "touch external-ran")
        (ai::worktree-git directory "config" "diff.review-test.textconv" "touch textconv-ran")
        (ai::worktree-git directory "config" "core.fsmonitor" "touch fsmonitor-ran")
        (setf (ai:agent-read-only-p agent) t)
        (touch directory "inherited-allow")
        (touch directory "execute-command")
        (write-file (merge-pathnames "permission-request" directory)
                    (ai:json-string (ai:jobj "tool_name" "Bash" "input"
                                             (ai:jobj "command" "git diff HEAD -- file.txt"))))
        (ai:agent-submit agent "review" (lambda (text error) (push (list text error) results)))
        (true (wait-until (lambda () results)))
        (is equal '("fresh reply (allow)" nil) (first results))
        (true (search "+after" (uiop:read-file-string (merge-pathnames "command-output" directory))))
        (dolist (name '("external-ran" "textconv-ran" "fsmonitor-ran"))
          (false (probe-file (merge-pathnames name directory)) "~a" name))))))
