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
   #:review-verdict #:parse-plan
   ;; settings
   #:*claude-program* #:*claude-model* #:*codex-program* #:*codex-model* #:*default-max-rounds*
   ;; JSON
   #:parse-json #:json-string #:jobj #:jget))
