# Recovery API (engine ↔ history and UI)

How the workflow engine (`workflow.lisp`, `agents.lisp`, `worktree.lisp`)
and the history store and window (`history.lisp`, `ui.lisp`) share restart
recovery, failure recovery and resuming after an interruption. All in the
`agent-duet` package.

## Checkpoints

`(workflow-checkpoint workflow)` → a JSON-compatible object (`jobj`). It is
data only: never closures. Keywords in it are written as strings, so the UI
reads a checkpoint as it comes back from JSON (`json-copy`).

`:on-checkpoint` (initarg; `workflow-on-checkpoint`) is called with the
workflow whenever its checkpoint changes: a step starts, ends or pauses, a
loop changes state, integration moves on. The UI's `save-workflow-checkpoint`
keeps it with the history (`record-history-checkpoint`) and saves at once.

The history keeps the checkpoint whole (`history-checkpoint`), under
`"checkpoint"` in the conversation's file, plus `"abandoned"` (a time) once
the user abandons the task. Both are left out when absent, so older files
read as before, and a malformed checkpoint is dropped without losing the
conversation. The keys the UI reads (`checkpoint-summary`):

| key | value |
| --- | --- |
| `"version"` | `1` |
| `"state"` | `"running"`, `"paused"`, `"stopped"`, `"finished"`, `"abandoned"` |
| `"mode"`, `"request"` | strings |
| `"started-at"`, `"ended-at"` | task creation and completion times (Common Lisp universal time), or null when unknown/still running |
| `"metrics"` | `[{"id", "agent", "role", "incomplete", "values"}]`; submitted turn records, with numeric measurements or null for unknown fields |
| `"agents"` | `[{"id", "directory", "conversation", "unavailable"}]`, lead first; `unavailable` the out-of-usage time, or null |
| `"loops"` | `[{"label", "author", "reviewer", "state", "round", "skip-review", "dependencies", "revision", "prerequisite-revisions", …}]`; `dependencies` loop indices, `prerequisite-revisions` `[{"loop", "revision"}]` |
| `"steps"` | `[{"id", "agent", "role", "loop", "state", "reason", "progress", "prompt"}]` |
| `"trees"` | `[{"agent", "path", "branch"}]` |
| `"integration"`, `"integration-journal"` | `"committing"`, `"merging"`, `"merged"`, `"interrupted"` or null; `[{"path", "head"}]` merged so far |
| `"prerequisite-merges"` | `[{"path", "from", "head"}]`: prerequisite work (`head`, from the worktree `from`) merged into a dependent part's worktree (`path`) |
| `"repositories"` | each folder and worktree as it was (`inspect-recovery-repository`) |
| `"task-records"` | `{"questions", "evidence", "decisions", "efficiency-events"}` (`task-records-to-json`, read back by `task-records-from-json`) |
| `"delivered-answers"`, `"answer-turns"` | question ids whose answer the asker has been given; blocking question ids already given an answer turn |
| `"question-holds"` | `[{"loop", "step", "questions"}]`: a part's work held (loop index, the held step's id) until those blocking questions are answered |

Metric `values` use `elapsed-seconds`, `duration-ms`, `cost-usd`,
`input-tokens`, `output-tokens`, `cache-read-tokens`, `cache-write-tokens`,
`reasoning-tokens` and `total-tokens`. Empty records preserve pending attempts
across a crash. Reported turn IDs replace the pending entry once; duplicate
reports are ignored. `incomplete` marks interrupted/failed measurements. Task
aggregates retain known-field coverage, so missing turns remain visibly partial.
Older checkpoints load with unknown start time and no recorded metrics.

Loops also save `"disputed"` (finding ids the author's last report disputed),
and findings save `"severity"` (`"blocker"` or `"suggestion"`) and `"disputes"`
(reviews in a row a disputed blocker stayed open). Steps can have the role
`"answer"`, with `"target"` the question ids it answers, and the reason
`"disagreement"`, for work waiting for the user to settle disputed findings
(`decide-disagreement` with `:keep` or `:dismiss`). After a restart, **Resume**
or **Retry** asks again (`:on-pause`) instead of replaying the fix work; without
an `:on-pause` callback it carries on as `:keep`.
Older checkpoints have no task records, holds or disputes. Their findings
read as blockers. On restore, a loop held for answers isn't given a restart
step. If no answer turn was saved for its questions, a paused `"answer"` step
is added to recover.

`history-interrupted-p`: the state is `"running"`, `"paused"` or
`"interrupted"`, or `"finished"` with integration still `"committing"`,
`"merging"` or `"interrupted"` (as `integration-failed` leaves it), and not
abandoned. `history-recoverable-p` also takes `"stopped"`. Reopening a
recoverable history shows the recovery card. Starting another task while
the current conversation's task is still recoverable starts a new history,
so the earlier checkpoint isn't overwritten.

`"pending-plan"` (`{"lead", "partner", "order", "error"}`) is a plan waiting
for approval: `order` `"parallel"`, `"lead-first"` or `"partner-first"`;
`error` why it can't start as it is (a string), or null. Older checkpoints
without them read as `"parallel"` and no error.
The journal lists parts as committed; `"merged-refs"` those merged.

## Restoring

`(restore-workflow checkpoint &key lead partner on-message on-finish on-pause
on-plan on-checkpoint)` → a workflow in state `:paused`, with the agents and
callbacks given (the UI's `workflow-callbacks`). Steps that were running come
back paused, reason `:restart`. Nothing runs, and the agents aren't moved.
It signals an error for a checkpoint it can't use; the card then offers only
*Abandon*.

## Inspecting

`(inspect-workflow-recovery workflow)`, also `(recover-workflow workflow
:review)` → JSON: `"repositories"` as now, `"repository-changed"` against the
checkpoint's, `"repository-snapshot-pending"` when that snapshot may predate
the agents' last edits, `"recovery-refs"`, the paused `"steps"`, and `"agents"`
(`"busy"`, `"shutdown-confirmed"`, `"unavailable"`). It runs git: the UI
calls it off the GTK thread. `inspection-summary` reads it for the card,
each repository against the checkpoint's (changed files, new commits, a
changed branch, an operation in progress, missing).

## Acting

`(recover-workflow workflow action &key step acknowledge-changes inspection)`, STEP a
step or its id, by default the first paused:

| action | result |
| --- | --- |
| `:review` | the inspection |
| `:restore-worktrees` | `:restored`, `:blocked` or `:pending-shutdown`: missing worktrees recreated from recovery refs |
| `:stop` | `:stopped` (still recoverable) |
| `:abandon` | `:abandoned` |
| `:resume`, `:retry`, `:hand-off` | `:started`, or `:repository-changed` (again with `:acknowledge-changes t` to go on), `:pending-shutdown`, `:blocked`, `:unavailable` |

`:resume` gives the agent the step with what's been done and the repository
now; `:retry` gives it the step as first given; `:hand-off` gives the other
agent the first. `:pending-shutdown` means the stopped turn, or a process it
started, isn't confirmed gone: nothing has started. The UI tries again every
2 seconds for 30 (`attempt-recovery`), then says it couldn't confirm. *Wait until …* on
a pause card resumes through the card's own *Resume*, so a changed folder,
a block or a shutdown not confirmed shows on that card, with *Carry on
anyway* where it applies.

With no step, `:resume` shows a pending plan again (`:awaiting-plan`, through
`:on-plan`) or carries integration on.

`recovery-actions` (history.lisp) chooses the card's actions from the
checkpoint and the inspection: a stopped step can be resumed, redone or
handed off (a check only resumed); with none, a pending plan shown again
(*Show the plan*) or integration carried on (*Carry on merging*, also from
`"merged"` before the combined result began); *Restore worktrees* when one
is missing and recovery refs exist; *Stop task* unless already stopped;
*Review what's there* and *Abandon* always. With several steps waiting, the
card has a menu to choose which the buttons act on, and stays until none is
left.

`(pause-workflow-agent workflow agent)` pauses one agent: its steps pause,
reason `:interrupted`. `(stop-workflow workflow)` stops both, keeping the
task recoverable. `:on-pause` is called `(workflow step)` for every pause the user is to act
on, not for a step waiting for its prerequisites (`:dependency`,
`:prerequisite-changed`), which starts by itself;
`(step-reason step)` is `:out-of-usage`, `:interrupted`, `:failed`,
`:restart`, `:disconnected`, `:integration-unresolved`, `:dependency`,
`:prerequisite-changed` or `:deadlock` (`step-reason-text` says it in words).

Checkpoint step records include only new, running and paused steps. Completed
results remain in loop summaries and conversation history, not old step prompts.
Completed steps and new pauses refresh the repository baseline on a worker and
publish it through
the agent delivery callback. Recovery inspection waits for this snapshot off GTK;
checkpoint serialization never waits. A pending snapshot is marked in the saved
checkpoint (`"repository-snapshot-pending"`), and ready snapshots replace the
cached repositories and paused-step baselines. A workflow restored from a
checkpoint so marked keeps the mark (`recovery-snapshot-pending-p`, and
`"repository-snapshot-pending"` in the inspection) until a fresh baseline
replaces the old one: changes are still reported against the older snapshot,
so carrying on still asks first, but the card says they may include the
agents' own last edits. Each paused step keeps its own snapshot, even if a peer pauses later or the
whole task is stopped. Stop captures only steps without a snapshot (or with a
failed capture), preserving user edits made between Pause and Stop.
Superseded captures cannot overwrite newer recovery state. Recovery actions in
the UI inspect repositories on a worker, then apply the action on the GTK thread using
that inspection. Pending shutdown retries inspect again on a worker.

## Ordered parts

The lead's plan may end with `ORDER: PARALLEL`, `ORDER: LEAD FIRST` or
`ORDER: PARTNER FIRST`. `(parse-plan text)` → lead task, partner task, and
the order: `:parallel` (also without the line, or for anything unknown),
`:lead-first`, `:partner-first`, or `:circular` when each part is to go
first. `(start-parts workflow lead partner &key order)` and
`(approve-plan workflow lead partner &key order)` set the parts' edges with
`set-workflow-dependencies` before any work starts; ORDER defaults to the
plan's. A circular order, or an error setting the edges, starts nothing:
without plan approval the lead is asked once to revise the plan
(`"plan-revisions"`), and after that it waits for the user as with approval.
With approval `approve-plan` returns false and the plan stays pending, with
`(pending-plan-error workflow)` saying why; `(pending-plan-order workflow)`
is its order (a circular one reads `:parallel`, with the error). Both are
exported. A revised order (`:revise-order`, below) applies at once, without
the plan card.

A dependent part's step pauses with reason `:dependency` until its
prerequisites are approved or done unreviewed with their check passing. Then
`check-finished` starts it, its prompt extended with each prerequisite's
report; in worktrees each prerequisite is first committed on its branch and
merged into the dependent's worktree (`merge-prerequisite-into-worktree`,
the dependent's own work committed first), recorded in
`"prerequisite-merges"` so it isn't merged twice. A conflict pauses the step
with `:integration-unresolved`, the merge left open in that worktree and the
prompt saying how to resolve it; `:resume` may then go on with that worktree
mid-merge. Before a dependent part's work is checked or reviewed, its
worktree must have no merge or other operation in progress, each
prerequisite's head in its history, and no conflict markers added since that
head; otherwise the work isn't accepted and pauses again with
`:integration-unresolved`, saying what's left. A merge found concluded is
recorded then. `:resume` or `:retry` of a `:dependency` or
`:prerequisite-changed` step whose prerequisites are ready starts it the
same way.

A dependent part starts, by itself or on `:resume`/`:retry`, only once
cancelled checks, old processes and its agent's turns have stopped, and, in
worktrees, its prerequisites' authors' (since their worktrees are committed
and merged). Until then the step stays paused (`:resume` and `:retry` are
`:pending-shutdown`) and `check-finished` tries again shortly, on the
workflow's thread.

Each part has a revision, bumped whenever its work starts (first, a fix, a
retry, a hand-off, more rounds), and the revisions of its prerequisites its
work last started against (`loop-prerequisite-revisions`). Work starting on
a part that others had started against invalidates them, and those that
build on them: each is told, its checks are cancelled, its steps superseded,
and its author gets a revalidation step (`:prerequisite-changed`) with what
it had reported and written so far, which starts with the prerequisite's new
report once it's ready again. Integration doesn't merge a part whose
recorded revisions are stale: it revalidates it, and `recover-integration`
is `:blocked`. `restore-workflow` signals an error for dependencies on
unknown parts, circular ones, or revisions recorded for a part not depended
on.

A prerequisite that fails, is interrupted, disconnects or runs out of usage
pauses as before; its dependents are blocked (a note says so), not
invalidated, until its work is redone. A hand-off's prompt has the original
assignment, how the agent stopped, its partial reply, the written result and
the repository inspection.

When no unfinished part can go on (each is waiting for a prerequisite not
approved, failed, or waiting in turn, and nothing runs), the waiting steps
get reason `:deadlock` and `:on-pause` is called for each. Two more actions,
`(recover-workflow workflow action)`:

| action | result |
| --- | --- |
| `:revise-order` | `:started`: the lead revises the plan (a `:replan` step) from both parts' tasks, progress and last reviews; its tasks and order go to the parts, and those it changes, or that aren't done, carry on from their progress. A revised plan that still can't go on, or that leaves a part without a task (`NONE`), changes nothing and pauses the unfinished parts with `:deadlock` again |
| `:run-independently` | `:started`: every part's prerequisites are dropped and the waiting steps start |

Both first check the repositories and pause snapshots as the other actions
do (`:repository-changed` unless `:acknowledge-changes`, `:blocked` for an
unavailable repository or an operation in progress, `:pending-shutdown` for
an unfinished snapshot). Either is `:blocked` with no waiting step,
`:pending-shutdown` while an agent or check is still stopping, and
`:revise-order` `:unavailable` while the lead is out of usage. A part the
revised plan restarts whose prerequisites are already ready starts as a
dependent part does, given their report and work. `:resume` on a deadlocked step is `:blocked`.

The optional `:inspection` argument is a fresh engine inspection prepared by
the UI worker for this attempt; callers must not reuse it across attempts.

## Lost agent connections

Both CLI adapters detect stdout EOF/read failure independently of process exit.
An exit watcher allows a short bounded stdout drain, then reports exit even if a
child keeps the pipe open. Each transport reports loss once; events from retired
transports are ignored. A quiet live transport still uses the stall card.

A lost transport is retired before the running turn reports `:disconnected`.
Pending permissions are cleared and queued assignments receive the same failure
instead of starting automatically. The workflow preserves partial reply text and
files, pauses the failed step and blocks declared dependent loops, keeping
written results for explicit recovery. The existing process-identity and shutdown
gates still apply to resume and handoff. No automatic retry occurs.

A pending stall retry is marked dropped if its transport disconnects; its queued
assignment receives `:disconnected` and uses explicit recovery instead. The stall
card therefore stops waiting for a turn that has already ended. Like other
failures and usage pauses, a disconnect blocks dependent loops without
invalidating them (Ordered parts, above).

Codex resource disposal runs on the exit watcher after the process is reaped and
the stdout reader finishes, independently of the once-only loss notification.
Inherited output pipes can delay disposal, but cannot delay the recovery card.
