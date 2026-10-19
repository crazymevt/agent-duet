# TODO

- [x] **Structured review checklist:** Track each review finding as open, fixed, or dismissed, with file locations, so unresolved findings remain visible across review rounds.
- [x] **Plan approval before coding:** Offer an optional pause after the lead proposes a plan, allowing the user to edit the scope or assignments before implementation starts.
- [x] **Completion and attention notifications:** Notify the user when both agents finish, a permission request needs attention, or usage limits pause a task.
- [x] **Separate worktrees for split tasks:** Give each agent its own worktree, then integrate and review their changes, allowing tasks with overlapping files to run independently.
- [x] **Compare proposals mode:** Have both agents independently propose an approach, then reconcile their differences before implementation.
- [x] **Check command before review:** Set a per-folder check command (`make test`, `npm test`, …) that runs after each work round. A failure goes back to the author with its output before the reviewer is asked, and the output is attached to the review prompt, so approval also means the checks pass.
- [x] **Shared project notes:** A per-folder notes file (or the project's `AGENTS.md` / `CLAUDE.md`) included in both agents' prompts, so project conventions don't have to be restated each task.
- [x] **Drag files and images into prompts:** Drop a file or image onto a pane's message box (or the middle request box) to attach it to the prompt sent to that agent, or to both.
