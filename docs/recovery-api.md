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
| `"agents"` | `[{"id", "directory", "conversation", "unavailable"}]`, lead first; `unavailable` the out-of-usage time, or null |
| `"loops"` | `[{"label", "author", "reviewer", "state", "round", "skip-review", …}]` |
| `"steps"` | `[{"id", "agent", "role", "loop", "state", "reason", "progress", "prompt"}]` |
| `"trees"` | `[{"agent", "path", "branch"}]` |
| `"integration"`, `"integration-journal"` | `"committing"`, `"merging"`, `"merged"`, `"interrupted"` or null; `[{"path", "head"}]` merged so far |
| `"repositories"` | each folder and worktree as it was (`inspect-recovery-repository`) |

`history-interrupted-p`: the state is `"running"`, `"paused"` or
`"interrupted"`, or `"finished"` with integration still `"committing"`,
`"merging"` or `"interrupted"` (as `integration-failed` leaves it), and not
abandoned. `history-recoverable-p` also takes `"stopped"`. Reopening a
recoverable history shows the recovery card. Starting another task while
the current conversation's task is still recoverable starts a new history,
so the earlier checkpoint isn't overwritten.

`"pending-plan"` (`{"lead", "partner"}`) is a plan waiting for approval.
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
checkpoint's, `"recovery-refs"`, the paused `"steps"`, and `"agents"`
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
2 seconds for 30 (`attempt-recovery`), then says it couldn't confirm.

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
task recoverable. `:on-pause` is called `(workflow step)` for every pause;
`(step-reason step)` is `:out-of-usage`, `:interrupted`, `:failed`,
`:restart`, `:disconnected` or `:integration-unresolved` (`step-reason-text` says it in words).

Checkpoint step records include only new, running and paused steps. Completed
results remain in loop summaries and conversation history, not old step prompts.
Completed steps and new pauses refresh the repository baseline on a worker and
publish it through
the agent delivery callback. Recovery inspection waits for this snapshot off GTK;
checkpoint serialization never waits. A pending snapshot is marked in the saved
checkpoint, and ready snapshots replace the cached repositories and paused-step
baselines. Each paused step keeps its own snapshot, even if a peer pauses later or the
whole task is stopped. Stop captures only steps without a snapshot (or with a
failed capture), preserving user edits made between Pause and Stop.
Superseded captures cannot overwrite newer recovery state. Recovery actions in
the UI inspect repositories on a worker, then apply the action on the GTK thread using
that inspection. Pending shutdown retries inspect again on a worker.

Dependency APIs are groundwork for the unchecked prerequisite/dependency TODO
items. The current split-plan UI does not declare dependency edges; it schedules
independent parts. These APIs do not yet implement automatic dependency discovery.

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
files, pauses the failed step and invalidates declared dependent loops, keeping
written results for explicit recovery. The existing process-identity and shutdown
gates still apply to resume and handoff. No automatic retry occurs.

A pending stall retry is marked dropped if its transport disconnects; its queued
assignment receives `:disconnected` and uses explicit recovery instead. The stall
card therefore stops waiting for a turn that has already ended. Automatic
dependent-loop invalidation here applies to `:disconnected`; other failure and
usage pauses retain their existing dependency handling.

Codex resource disposal runs on the exit watcher after the process is reaped and
the stdout reader finishes, independently of the once-only loss notification.
Inherited output pipes can delay disposal, but cannot delay the recovery card.
