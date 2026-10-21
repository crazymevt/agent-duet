# Agent Duet implementation plan and handoff

This file records the current implementation and the remaining work so the
next session can continue without reconstructing the conversation. Existing
feature scope comes from `TODO.md`; ordering beyond its agreed priorities is
a proposed implementation sequence, not a new product commitment.

## Current checkpoint: interface redesign

Committed as `bf9eb61` ("UI Resign"); milestone 1 continues on top of it:

- Shared conversation on the left and stacked agent panels in a resizable
  sidebar; optional three-pane and conversation-only layouts.
- Implemented (`src/ui.lisp`, `src/ui-presentation.lisp`,
  `src/settings.lisp`): a fourth layout, *Agents on left*, with the stacked
  agent panels on the left and the conversation on the right, its own saved
  divider positions (`:agents-left-positions`), and the same narrow-window
  adaptation (conversation first). The layout menu labels become *Agents on
  right*, *Agents on left*, *Three panes*, *Conversation only*.
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

Automated coverage added in this step (headless; GTK widgets are untested):

- `tests/main.lisp`: `settings-malformed-files` (non-plist, odd-length,
  dotted, vector, `#.` read-eval and empty settings files load as no
  settings); `layout-preferences` (each layout's divider positions read
  independently; missing, wrong-length, dotted, non-integer, negative and
  out-of-range positions ignored; out-of-range or non-integer dropdown
  choices fall back to the first); `layout-preferences-agents-left`
  (*Agents on left* positions independent of the others);
  `layout-preferences-survive-restart`
  (layout, positions, workflow choices and expansion states round-trip a
  save and load, and a hand-edited file with bad layout values still loads
  its other settings); `settings-circular-values` (circular nested lists and
  arrays of any rank are dropped while independent valid preferences survive
  loading, updating/saving, and reloading; dotted divider values are ignored).
- `tests/workflow-presentation.lisp`: every recoverable pause reason shown on
  the panel and stage with attention; both agents out of usage; a failed
  task; work finished unreviewed (never shown as approved, with and without
  a failing check); a stopped task stays pending and recoverable, while a
  finished one shows its unfinished part as skipped; a paused step restored
  from its checkpoint still asks for the user, with its round.

Resolved in Claude's reviewed settings changes (pending integration here):
circular top-level settings are rejected, and circular nested values,
including arrays of any rank, are dropped while other valid entries survive.
`settings-malformed-files` and `settings-circular-values` cover these cases
with timeout guards; divider validation also safely ignores dotted values.

### Live GTK acceptance, 2026-10-21 (Codex worktree)

The window was exercised in an isolated SBCL/GTK process with temporary
`XDG_CONFIG_HOME` and `XDG_DATA_HOME`, synthetic approval/question cards,
and CLI discovery and desktop notifications disabled. No real CLI command
was sent. Rendered images were captured with GtkWidgetPaintable/GtkSnapshot
and inspected, rather than relying only on nonzero allocations.

Fixes made after observing the renders:

- GtkPaned rescaled divider requests against the previous orientation. A
  narrow/wide cycle changed requested `(450 380)` positions to `(679 256)`.
  Responsive divider requests now apply after the orientation's allocation;
  both sidebar layouts returned to `(450 380)` in the repeated live check.
  Pending requests are discarded on layout changes and protected from saving
  transitional positions.
- At 720px, Three panes gave the conversation only 187px, clipping its
  composer. Minimum requests now reserve 320px for the conversation and
  300px for each agent. Three panes holds the window at 925px when asked for
  720px or 640px. This minimum-width policy is intentional; use a sidebar
  layout or Conversation only for a smaller window.
- Below 680px the responsive sidebar stacks agents vertically and gives
  them 60% of the available height. At 720px it gives the conversation 60%
  and places agents side by side. Divider requests apply after allocation
  in both directions. Checklist restoration no longer depends on an
  allocation-size change being observed in that frame.

Verified in that fixture:

- [x] All four layouts rendered at 1440 × 1000 and requested widths of 720
  and 640. The conversation, composer, model/effort controls and attention
  buttons remained reachable. Short transcript viewports scroll; whole
  question cards do not always fit vertically at once.
- [x] Agents on left rendered agents left/conversation right when wide,
  conversation first when narrow, and mirrored again when widened. Agents
  on right retained its own order. Seeded wide positions were `(900 350)`
  and `(420 370)` respectively at initial application.
- [x] Checklist opening/closing after programmatic divider moves restored
  `(450 380)` in both sidebar layouts and `(450 373)` in Three panes (the
  latter was GTK's accepted position before opening). These were read back
  after settled allocations. Physical pointer dragging was not exercised.
- [x] Existing approval/question cards survived layout changes. The same
  widgets and agent instances were retained; an unsent agent draft was
  asserted unchanged in each layout. Conversation only showed the mirrored
  cards and model/effort controls. Approval/question arrival expanded the
  transcript. This verifies UI continuity, not real protocol delivery.
- [x] Workflow mode, lead, rounds and collapsed workflow settings applied
  from seeded settings. A second Lisp process loaded the saved fixture file,
  asserted the Conversation only layout, lead and rounds, then switched to
  each sidebar and asserted its independent saved divider positions applied.
- [x] README layout/control descriptions and minimum-width guidance updated;
  `docs/screenshot.png` now shows the redesigned Agents on right layout
  with a neutral `/tmp/agent-duet-demo/example/` folder and task-like demo
  content. Its synthetic content is identified in the README caption.
- [x] `CHANGELOG.md` Unreleased reviewed and updated.
- [x] Local `make load`, `make test-parallel` (274 selected tests, 2,363
  passed assertions across eight workers, zero failed/skipped), and
  `git diff --check` passed after the UI fixes.

Still required before milestone 1 sign-off:

- [ ] Review the combined diff with Claude's settings robustness work and
  repeat `make load`, `make test-parallel`, and `git diff --check` after
  integration. The checks above apply only to this worktree's combined base
  and UI changes; Claude's separate worktree was not imported here.
- [ ] Exercise physical divider drags, including checklist toggles at 1000px
  and minimum widths. Current evidence uses GtkPaned position setters.
- [ ] Exercise real streaming/direct-message delivery while switching all
  layouts, real approval/question answers, missing and signed-out CLIs,
  model/effort changes and usage transitions. The fixture displayed these
  controls but did not validate authenticated CLI behavior or recovery.
- [ ] Recheck saved preferences with real CLI catalogs arriving asynchronously.
  The process restart above deliberately disabled CLI discovery.
- [ ] Commit only when requested; no later milestone work was started.

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
