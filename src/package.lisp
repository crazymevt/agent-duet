;;;; package.lisp

(defpackage #:agent-duet
  (:use #:cl)
  (:export #:main
   ;; agents
   #:agent #:claude-agent #:codex-agent #:agent-name #:agent-id #:agent-busy
   #:agent-submit #:agent-send-now #:agent-cancel #:agent-shutdown #:agent-begin-turn #:agent-turn-finished
   #:agent-listener #:agent-directory #:agent-permission
   ;; workflows
   #:workflow #:start-workflow #:stop-workflow #:workflow-state #:workflow-loops
   #:workflow-continuable-p #:continue-workflow #:approve-plan #:workflow-pending-plan
   #:review-verdict #:parse-plan #:workflow-completion #:commit-workflow-changes
   ;; settings
   #:*claude-program* #:*claude-model* #:*codex-program* #:*codex-model* #:*default-max-rounds*
   ;; snapshots and changes window
   #:snapshot #:snapshot-label #:snapshot-head #:snapshot-index #:snapshot-working-tree #:snapshot-time
   #:snapshot-change #:snapshot-change-path #:snapshot-change-status #:snapshot-change-old-path #:snapshot-change-diff
   #:snapshot-folder #:snapshot-changes #:delete-snapshot #:prune-snapshots #:show-changes-window
   ;; JSON
   #:parse-json #:json-string #:jobj #:jget))
