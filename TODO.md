# TODO

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
- [ ] **Restart recovery:** Save the workflow's state (mode, loops, rounds, findings, the step under way, worktrees and merge status) with the history. On reopening an interrupted task, show where it stopped and what the repository looks like now (changed files, new commits, a merge in progress, worktrees still present), and offer explicit choices — *Review what's there*, *Redo the step*, *Abandon* — rather than replaying the step automatically.

Later:

- [ ] **Undo a task:** Revert what changed in the folder during the task, building on the changes view: apply the reverse of the task's diff (start → end snapshot) to the current tree, as a three-way merge, so edits made after the task are kept. Files where they conflict are listed and left untouched, not overwritten. Shows the reverse diff before applying. The start → end diff can't tell the agents' changes from edits the user made in the folder while the task ran, so those are reversed too: the confirmation says so, and lets files be left out. (Telling them apart would need per-turn snapshots or the agents' own edit events, which is not planned here.)
- [ ] **Commit at the end:** After an approved task outside *Own worktrees*, offer **Commit**: the task's changed files (from the changes view) are preselected, files that were already changed before the task are unselected and marked, the user can change the selection, and the staged diff is shown with a message drafted by the lead (editable) before committing.
- [ ] **Cost and time per task:** Claude's result carries `total_cost_usd` and duration; Codex reports token usage. Per-turn and per-task totals in the status lines and the end-of-task note.
- [ ] **End-of-task summary card:** Files changed (linked to the changes view), rounds per loop, check result, open findings, time and cost, in one card.
- [ ] **Search** in a pane (⌘F) and in the conversation list.
- [ ] **Export a task as Markdown:** request, plans, reviews and checklist, for a PR description or notes.
- [ ] **Prompt templates:** per-folder saved requests in the request box.
- [ ] **Keyboard shortcuts and a help sheet:** Start/Stop, mode, focus each pane, checklist, terminal.
- [ ] **UI tests:** `src/ui.lisp` is the largest file and untested; smoke-test the window and card flows where GTK is available.

Not planned for now: a third agent (Gemini etc.). The two-agent assumption runs through `workflow.lisp` and the layout, and the items above help more for less.
