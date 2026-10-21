# Agent Duet implementation plan and handoff

This file records the current implementation and the remaining work so the
next session can continue without reconstructing the conversation. Existing
feature scope comes from `TODO.md`; ordering beyond its agreed priorities is
a proposed implementation sequence, not a new product commitment.

## Current checkpoint: interface redesign

Implemented, still uncommitted in `/Users/jessie/projects/lisp/agent-duet`:

- Shared conversation on the left and stacked agent panels in a resizable
  sidebar; optional three-pane and conversation-only layouts.
- Saved layout/divider preferences, transcript expansion, workflow settings,
  mode, lead, and round count; invalid saved choices are checked before use.
- Larger “Task for both agents” composer, compact workflow controls,
  expandable settings, separate Start collaboration and Stop buttons.
- Workspace menu for terminal, documents, changes, checklist, layout,
  permissions, appearance, and shared project instructions; history remains
  directly accessible.
- Pure workflow presentation helpers for agent activity and task progress,
  independent concurrent stages and rounds, attention states, and recovery.
  Restored handoffs with insufficient evidence do not claim plan approval.
- Approval/question arrivals open the transcript. Pane minimum sizes protect
  panels from collapsing. Conversation-only status retains CLI readiness and
  direct-message status when no relevant workflow exists.
- Allocation-driven sidebar adaptation replaces the 250 ms polling timer.
  Checklist notices name Workspace › Checklist; duplicate checkpoint refresh
  and unnecessary helper availability checks were removed.

Files: `src/ui.lisp`, `src/settings.lisp`, new `src/ui-presentation.lisp`,
new `src/workflow-presentation.lisp`, new `tests/workflow-presentation.lisp`,
`agent-duet.asd`, and `CHANGELOG.md`.

Latest checks: `make load` and `git diff --check` passed. The full parallel
suite selected 262 tests and passed 2,221 assertions across eight workers,
with zero failures. The last worker's “289 passed” is not the suite total.
Live GTK allocation checks toggled the checklist at requested window widths
of 1440, 1000, and 720 pixels. Panels remained nonzero; minimum sizes can
force a narrow window larger than its requested width.

## Milestone 1: finish redesign acceptance and commit

- [ ] Review the final combined diff again, including the last review fixes.
- [ ] Verify readable content and actual on-screen controls at narrow widths,
  rather than treating nonzero allocations alone as sufficient acceptance.
  At a requested 720px with the checklist open, the conversation measured
  337px wide and agent panels 168px each; after closing it the window grew
  to 845px. Decide whether that minimum-width behavior is acceptable.
- [ ] Verify checklist divider restoration and user-dragged positions. At
  1000px the conversation measured 660px before opening the checklist and
  692px after closing it, so exact position restoration is not established.
- [ ] Check approvals, questions, direct messages, missing/signed-out CLIs,
  model/effort and usage controls in every layout, including while switching
  layouts during a run. Preserve message delivery and recovery semantics.
- [ ] Verify layout and workflow preferences survive a restart, including
  malformed/obsolete preferences and transitions between narrow/wide views.
- [ ] Update README layout/control descriptions and screenshot to match the
  redesigned UI. The current README still describes the old middle pane.
- [ ] Re-run `make load`, `make test-parallel`, and `git diff --check` after
  substantive fixes. Full-suite tests must use `make test-parallel`.
- [ ] Check `CHANGELOG.md` under Unreleased, then commit when requested.

## Milestone 2: task summary, usage, and time

The existing agreed priority after review-only and restart recovery is an
end-of-task summary. Those preceding features are already marked complete
in `TODO.md`.

- [ ] Inventory actual CLI usage, duration, and cost events; retain missing
  values as unknown instead of deriving unsupported prices or totals.
- [ ] Store per-turn and per-task totals in history/checkpoints as needed.
- [ ] Add one end-of-task card with changed files linked to Changes, rounds
  per loop, recorded check outcome, open findings, duration, and available
  usage/cost data. Include text-only tasks and incomplete/unreviewed results.
- [ ] Test success, failure, stop, restart, handoff, parallel loops, and absent
  metrics without double counting repeated or stale events.

## Milestone 3: finish the commit-at-end flow

Some completion/commit UI already exists; inspect it before implementing
anything new. Reconcile it with the unchecked entry in `TODO.md`.

- [ ] Compare existing commit behavior with task snapshots and Changes.
- [ ] Preselect task-changed files; leave files already modified before the
  task unselected and visibly marked. Let the user edit the selection.
- [ ] Show the staged diff and an editable lead-drafted message before the
  explicit commit action. Handle existing staged edits deliberately.
- [ ] Verify dirty repositories, untracked files, text-only results, failed
  checks, worktree integrations, and changes made after task completion.

## Milestone 4: undo a task

- [ ] Build a preview from the reverse start-to-end task snapshot diff.
- [ ] Allow excluding files; explain that user edits made during the task
  are included in that snapshot diff too.
- [ ] Apply with three-way reconciliation against the current tree to keep
  later edits. List conflicting files and leave them untouched.
- [ ] Test tracked/untracked files, adds/deletes/renames, later edits,
  conflicts, and interrupted application before exposing the action.

## Remaining backlog

These scopes are recorded in `TODO.md`; the order below is provisional.

- [ ] Search within transcripts and the conversation/history list.
- [ ] Export a task as Markdown, including request, plans, reviews, checklist,
  and the combined written result for text-only tasks.
- [ ] Artifact discovery and cards: local output paths and metadata, creator,
  type, status, previews, external open/save/export, and cross-agent review.
  Preserve links/files across restart, handoff, and worktree cleanup. Isolate
  active HTML and show unsupported previews or missing tools honestly.
- [ ] Per-folder prompt templates in the shared composer.
- [ ] Keyboard shortcuts and a discoverable help sheet for workflow actions,
  pane focus, checklist, and terminal.
- [ ] GTK UI smoke coverage for layouts and attention/recovery card flows,
  on machines where GTK is available.

A third agent remains outside the current plan.

## Resuming work

1. Start in `/Users/jessie/projects/lisp/agent-duet`, not the sibling
   `lisp-edit` repository. The earlier supplied `b15fdae` base belonged to
   `lisp-edit` and is not the base for these changes.
2. Read `AGENTS.md`, this file, `TODO.md`, and the working-tree diff/status.
   Preserve existing uncommitted changes; do not re-copy old worktree files.
3. Finish milestone 1 before adding more workflow or product scope.
4. Update this file and `TODO.md` as milestones are completed. Add user-visible
   changes to `CHANGELOG.md` under Unreleased before committing.
