# TODO

For the current redesign checkpoint, acceptance checks, and remaining milestone
sequence, see [the implementation plan](docs/implementation-plan.md).

- [ ] **Redesign milestone 1 acceptance:** Live GTK layout, narrow-width,
  divider restoration, draft continuity and preference-application checks were
  performed on 2026-10-21; the screenshot is current. Real CLI delivery,
  authentication/usage transitions, physical divider drags and final combined
  review/checks remain before sign-off. See the plan for evidence and limits.

- [x] **Structured review checklist:** Track each review finding as open, fixed, or dismissed, with file locations, so unresolved findings remain visible across review rounds.
- [x] **Plan approval before coding:** Offer an optional pause after the lead proposes a plan, allowing the user to edit the scope or assignments before implementation starts.
- [x] **Completion and attention notifications:** Notify the user when both agents finish, a permission request needs attention, or usage limits pause a task.
- [x] **Separate worktrees for split tasks:** Give each agent its own worktree, then integrate and review their changes, allowing tasks with overlapping files to run independently.
- [x] **Compare proposals mode:** Have both agents independently propose an approach, then reconcile their differences before implementation.
- [x] **Check command before review:** Set a per-folder check command (`make test`, `npm test`, …) that runs after each work round. A failure goes back to the author with its output before the reviewer is asked, and the output is attached to the review prompt, so approval also means the checks pass.
- [x] **Shared project notes:** A per-folder notes file (or the project's `AGENTS.md` / `CLAUDE.md`) included in both agents' prompts, so project conventions don't have to be restated each task.
- [x] **Drag files and images into prompts:** Drop a file or image onto a pane's message box (or the middle request box) to attach it to the prompt sent to that agent, or to both.

- [x] **Preserve split-task results and recovery refs:** Keep each part's written result (its final reply) for the merged-result review, naming parts that changed no files, so a text-only task still ends in a combined answer without needing a nonempty diff. Keep each part's committed state under `refs/agent-duet/tasks/<task>/<part>/{head,base}` (`retain-worktree`, `remove-worktree :recovery-task`), so its worktree and branch can go before the merged result is approved; delete the refs (`delete-worktree-recovery-refs`) only after approval.

## Next (agreed by Claude and ChatGPT)

Agreed priorities now, in order:

1. **Review-only mode** (below).
2. **Restart recovery** (below), using the recovery refs for parts whose worktrees are gone.
3. **End-of-task summary card** (below), with whatever usage and time data the CLIs report.
4. **Commit at the end**, then **Undo a task** (below).

The earlier top five, and the rest:

- [x] **CI:** A GitHub Actions workflow running `make test` on macOS and Linux (the tests need no GTK or CLIs), plus a Linux build check, before workflow behavior grows further.
- [x] **Task changes view:** Record the starting state when a task begins (`HEAD`, the index, and the working tree's contents, tracked files including unstaged changes and untracked ones, e.g. as commits under `refs/agent-duet/<task>`), and the end state the same way when it finishes, and show what changed between the two, per file, in a window like Documents. Viewing only; undo is a separate item below.
- [x] **Review-only mode:** One or both agents review uncommitted changes, a branch or the last N commits, producing the same `F1`… checklist; **Fix these** turns it into a *Write & review* task. Enforced read-only, not just asked: Claude without edit tools (`--disallowedTools Edit,Write,NotebookEdit`, commands limited to read-only ones), Codex in the `read-only` sandbox, whatever the permission setting. A branch is reviewed as the changes it introduces (`git diff base...branch`, from the merge base), with comparing branch tips (`base..branch`) as a separate choice.
- [x] **Stall detection:** A turn with no events for N minutes gets a card: *Keep waiting* / *Interrupt and retry* / *Stop*. Time spent waiting on the user (a permission card, a question, plan approval, a usage pause) doesn't count. *Interrupt and retry* only resubmits once the interrupted turn has ended (its turn token cleared); if it doesn't end, it says so and offers *Stop*.
- [x] **Restart recovery:** Save the workflow's state (mode, loops, rounds, findings, the step under way, worktrees and merge status) with the history. On reopening an interrupted task, show where it stopped and what the repository looks like now (changed files, new commits, a merge in progress, worktrees still present), and offer explicit choices — *Review what's there*, *Redo the step*, *Abandon* — rather than replaying the step automatically.
- [x] **Agent failure and dependent-work recovery:** When an agent's work or review fails, notify the other agent and pause any work that depends on the missing result. Offer retry, reassignment to the surviving agent, or stop, preserving partial edits, written results, and worktrees for recovery. Give a takeover the original assignment, failure details, and available progress; resume dependent work only after its prerequisite is complete. Keep failed or incomplete parts out of integration, and clearly mark work completed without independent review.
- [x] **Resume after a user interruption:** Distinguish pausing one agent from stopping the whole collaboration, and record user interruptions as interrupted rather than failed. Preserve the current step, partial edits, written results, and worktrees. Pausing one agent pauses dependent steps while independent work can continue; offer **Resume**, **Retry step**, **Hand off**, or **Stop task**. Stopping the task interrupts both agents but keeps it resumable later. Before resuming, inspect the repository and available progress and show where execution stopped, so continuation accounts for partial work rather than blindly replaying the last prompt. Share saved state and recovery handling with restart and failure recovery.

### Recovery scenarios to cover

- [x] **Late results after interruption or reassignment:** Reject stale results and callbacks from superseded turns so they cannot advance a resumed step or overwrite its state.
- [x] **Safe handoff while an agent is stopping:** Confirm the original turn and its editing processes have stopped before the replacement agent edits the same work; keep the handoff pending if shutdown cannot be confirmed.
- [x] **Repository changes during a pause:** Before resuming, detect user edits, new commits, branch changes, and missing worktrees; reconcile available progress with the current repository before continuing.
- [x] **Interrupted integration:** Record completed commits and merges and recover from partial integration or unresolved conflicts without repeating completed operations or losing either part's work.
- [x] **Hung or disconnected agents:** Quiet live turns retain stall handling; stdout EOF/read failure and process exit independently trigger recovery, including when a child holds stdout open. Preserve partial work, clear permissions, pause dependent work, and offer explicit resume/retry/handoff.
- [x] **Both agents unavailable:** Keep the task paused and recoverable, show each agent's reason for being unavailable, and allow recovery when either becomes available.
- [x] **Changed prerequisites:** If a prerequisite changes after dependent work starts, notify the dependent agent and revalidate affected implementation, checks, and reviews before completion. Split plans declare an order (`ORDER: PARALLEL | LEAD FIRST | PARTNER FIRST`, editable on the plan card); a dependent part starts with its prerequisite's report and, in worktrees, its work merged in. Each part's work has a revision; redoing a prerequisite has the parts started against it revalidate, and stale parts aren't merged.
- [x] **Circular dependencies or mutual waiting:** Detect when agents are blocked on each other and request a revised plan or dependency order. A circular plan doesn't start (the lead revises it once, then the user decides); when no part can go on, the waiting step pauses as a deadlock, with *Revise the order* and *Run independently*.
- [x] **Review unavailable after successful implementation:** Preserve completed work as unreviewed and allow review to resume later without repeating implementation.

Later:

- [ ] **Undo a task:** Revert what changed in the folder during the task, building on the changes view: apply the reverse of the task's diff (start → end snapshot) to the current tree, as a three-way merge, so edits made after the task are kept. Files where they conflict are listed and left untouched, not overwritten. Shows the reverse diff before applying. The start → end diff can't tell the agents' changes from edits the user made in the folder while the task ran, so those are reversed too: the confirmation says so, and lets files be left out. (Telling them apart would need per-turn snapshots or the agents' own edit events, which is not planned here.)
- [ ] **Commit at the end:** After an approved task outside *Own worktrees*, offer **Commit**: the task's changed files (from the changes view) are preselected, files that were already changed before the task are unselected and marked, the user can change the selection, and the staged diff is shown with a message drafted by the lead (editable) before committing.
- [ ] **Cost and time per task:** Claude's result carries `total_cost_usd` and duration; Codex reports token usage. Per-turn and per-task totals in the status lines and the end-of-task note.
- [ ] **End-of-task summary card:** Files changed (linked to the changes view), rounds per loop, check result, open findings, time and cost, in one card.
- [ ] **Search** in a pane (⌘F) and in the conversation list.
- [ ] **Export a task as Markdown:** request, plans, reviews and checklist, for a PR description or notes.
- [ ] **Artifact creation and viewing:** Support deliverables created by either CLI as local files (HTML, Markdown, images, PDFs, spreadsheets, presentations, and other documents), using available tools and dependencies rather than assuming a shared native artifact protocol. Define how agents report artifact paths and metadata; discover outputs from structured events where supported and explicit file references otherwise. Show per-task artifact cards with creator, type, creation/update status, preview, open externally, and save/export actions. Extend the existing Markdown Documents viewer with suitable previews and an external-app fallback; isolate active HTML content. Preserve artifacts and their history links across restart, handoff, and worktree cleanup, and make them available to the other agent for review. Detect supported CLI output types and capabilities by version, including generated-image events where available, and clearly report missing generation tools or unsupported preview formats.
- [ ] **Prompt templates:** per-folder saved requests in the request box.
- [ ] **Keyboard shortcuts and a help sheet:** Start/Stop, mode, focus each pane, checklist, terminal.
- [ ] **UI tests:** `src/ui.lisp` is the largest file and untested; smoke-test the window and card flows where GTK is available.

Not planned for now: a third agent (Gemini etc.). The two-agent assumption runs through `workflow.lisp` and the layout, and the items above help more for less.
