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

## Milestone 1: redesign acceptance (signed off 2026-10-21)

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

Resolved in the integrated settings changes:
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

Historical outstanding checks before the continuations below (resolved by the
2026-10-21 sign-off unless explicitly retained as an evidence limit):

- [x] Reviewed the combined diff, settings regression coverage, and refreshed
  screenshot after integration. `make load` and `make test-parallel` (eight
  workers) passed on the merged result; both the working-tree diff and the
  full diff against `9b69940` passed `git diff --check`.
- [ ] Exercise physical divider drags, including checklist toggles at 1000px
  and minimum widths. Current evidence uses GtkPaned position setters.
- [ ] Exercise real streaming/direct-message delivery while switching all
  layouts, real approval/question answers, missing and signed-out CLIs,
  model/effort changes and usage transitions. The fixture displayed these
  controls but did not validate authenticated CLI behavior or recovery.
- [ ] Recheck saved preferences with real CLI catalogs arriving asynchronously.
  The process restart above deliberately disabled CLI discovery.
- [ ] Commit only when requested; no later milestone work was started.

### Acceptance continuation, 2026-10-21 (Codex, before integration)

Fresh-process GTK verification loaded this worktree with temporary
`XDG_CONFIG_HOME` and `XDG_DATA_HOME`; CLI discovery, notifications and history
pruning were disabled. An earlier hot-reload run briefly reenabled local CLI
catalog/readiness probes; those results are not protocol acceptance evidence. The computer-use target was a temporary *Divider
Acceptance* app, separate from the user's running Agent Duet. No CLI task
was sent and no real user preferences were changed.

- Native pointer drag calls were attempted on the Agents on right divider,
  the Agents on left divider, the Three panes inner divider and the checklist
  edge at 1000px. Settled
  GtkPaned readbacks did **not** establish movement caused by a drag.
  A temporary wider-handle experiment also did not establish movement and
  was reverted in the fixture. This is inconclusive physical-input evidence,
  not a successful drag check or proof of an application defect. Physical
  divider acceptance remains open; position setters cannot close it.
- A separate settled-allocation matrix repeats checklist opening and closing
  for all four layouts at requested widths 1440, 1000, 720 and 640px, at
  1000px height. These are programmatic layout, resize and visibility calls.
  Sidebar restoration at 1440/1000px is asserted against positions read
  before opening; the 1000px cases cross the narrow-layout threshold while
  the checklist is open.
- That matrix exposed a Three panes bug: opening the checklist at 1000px
  grew the window to 1183px; closing it changed inner position 398 to 441.
  `watch-conversation-allocation` now restores the outer split first and
  queues an allocation before restoring the inner split in the next frame.
  Its temporary phase marker resets on visibility and layout changes.
  A fresh-process repeat preserved `(300 398)` exactly in the same 1000px
  case, and `(300 320)` at the Three panes minimum width. Both sidebars
  preserved their 1440/1000px positions too.
- GTK can retain the enlarged width after checklist closing: a requested
  720px sidebar reached 862–869px with the checklist, and Three panes held at
  922px without it and 1183px with it in this fixture. Those are actual
  allocations, not a claim that Three panes fits at 640px. README now
  explains that closing the checklist may leave a larger window to resize.
- The final fresh-process matrix completed all 16 layout/width combinations
  without assertion errors. Every visible conversation/agent panel retained
  positive width and height with the checklist open; Three panes restoration
  was also asserted at requested 720/640px (actual minimum width 922px).
  This harness is manual GTK evidence, not a new checked-in headless test.
- Local `make load`, `make test-parallel` (275 selected tests, 2,405 passed
  assertions across eight workers, zero failed/skipped) and `git diff --check`
  passed after the fix. These are pre-integration checks for this worktree.
- The existing README screenshot still represents the current layout; no
  replacement is needed for this divider-restoration fix. No successful
  physical-input check is claimed from unsuccessful pointer automation.

### Combined acceptance review, 2026-10-21

Both worktrees are integrated. The earlier evidence above remains historical;
this section reconciles the continuation with Claude's final report. The
review inspected the merged source and tests. CLI observations below are
reported from Claude's runs, not independently repeated during integration.
The assigned `src/ui-agents.lisp` and `src/ui-events.lisp` do not exist here;
Claude changed only workflow presentation and its tests.

Merged fixes:

- Checklist closing restores the outer divider before the inner divider,
  preserving Three panes positions when opening enlarged the window.
- Direct messages during a task show "Answering your message", with queued
  work described next; pauses and failed tasks retain precedence.
- Idle/waiting agents expose exhausted usage in presentation detail, including
  Conversation only. Two regression tests cover the presentation changes.

Real CLI evidence (Claude Code 2.1.289, Codex 0.163.0-alpha.10.2) used
`/tmp/ad-accept/proj` and temporary configuration/data/state directories.
It consumed real usage and created CLI history for that temporary folder.

- Both CLIs streamed. Headless Claude accepted an AskUserQuestion answer
  ("Red"), retained context across a sonnet/medium restart, and reported usage
  windows. Headless Codex retained context across a thinking-level change,
  refreshed usage, folded a mid-turn message into its turn, and executed an
  approved command.
- Missing CLIs failed immediately with the expected missing-tool messages.
  Signed-out CLIs failed as authentication errors rather than exhausted usage.
  Codex retried five times, taking roughly 16 seconds to report its 401.
- In the valid second live-window run, Claude streaming retained the same
  widget through all four layouts; a draft survived two layout cycles.
  A Codex approval card survived a layout cycle, showed "Waiting for you",
  and Allow in Conversation only executed its command. Both catalogs arrived
  within about one second.
- Saved Claude model/effort (haiku/low) and Codex model (gpt-6-luna) restored.
  Saved Codex effort (medium) did not: `refresh-effort-dropdown` resets it to
  nil before the catalog arrives, and catalog arrival does not restore it.

Findings from the live runs and remaining acceptance limits:

- Claude mid-turn direct messages misattributed later replies in the live run.
  `claude-agent-result` previously finished on a result with `queued_turn_count: 0` even
  when another submitted message's response is still pending. The live report
  observed PINEAPPLE credited to the SECOND request, then SECOND to the next
  turn. This was fixed during review in `src/agents.lisp` (see below).
- Saved Codex effort restoration was fixed during review in `src/ui.lisp`
  (see below).
- Physical divider drags remain inconclusive; all 16 layout/width combinations
  passed programmatic allocation/checklist checks, not physical drag checks.
- Claude approval was not cleanly verified: the headless touch command was
  auto-allowed. The live-window question stalled and required a manual click,
  so that scenario remains unverified.
- Live-window model/effort changes, out-of-usage display, and saved layout after
  closing were not completed. Real account exhaustion was not induced;
  exhausted-usage presentation is covered by unit tests only. Codex draft and
  streaming observations from the first live run were invalid because the
  harness accidentally submitted the draft.

Milestone 1 remains open. No later milestone work or commit is authorized by
this continuation. The existing screenshot was retained.

Post-integration `make load` and `make test-parallel` passed (277 selected
tests across eight workers). Working-tree and full baseline diffs passed
`git diff --check` after documentation reconciliation.

### Review fixes for F1 and F2

F1, F2 and F5 are accepted and fixed. The first F1 patch incorrectly treated
`result_index` as turn-local; Claude's review established that it counts across
the CLI process. The corrected code tracks the next process result index and
captures it as each turn's starting point. Indexed successful results finish
the application turn only after the last message submitted in that turn has a
result relative to that starting point. The next index advances before callbacks
can start another turn and follows the received index even across gaps.
Starting a new process resets both values, including failed-resume resends. Errors and cancellation still
finish promptly. CLI versions without indexes retain the queue-count fallback.
Regression coverage reproduces zero queued turns on intermediate results,
including another message submitted between results, error completion, a
second turn on the same process, and a gap in numbering. Fake-CLI integration
checks exercise actual turn startup, a queued next task, process reuse, an
effort-triggered restart, and failed-resume resending with indexes restarting
at zero. These fixtures do not constitute live authenticated CLI acceptance.

The effort dropdown preserves the agent's loaded effort before Codex's model
catalog is available, then validates it against the model's supported levels.
Preserving the preference avoids rereading saved settings over a newer user
choice. A headless regression covers delayed catalog arrival and unsupported
levels. This implements F2's intended behavior by retaining the saved value
rather than discarding and reapplying it. F3 and F4 remain dismissed.

These are source/test fixes; real CLI reply attribution and the live dropdown
have not been rerun since these changes. The physical-input and live-window
acceptance limits above remain open.

After the initial review fixes, `make load` and `make test-parallel` passed:
279 selected tests, 2,443 passed assertions across eight workers. After the
process-wide-index correction, both passed again: 281 selected tests, 2466
passed assertions across eight workers, zero failed or skipped. Working-tree and full baseline `git diff --check` also passed.

### Sign-off continuation, 2026-10-21 (current checkout)

Acceptance was rerun at `7b6e0cf` in a separate **Divider acceptance —
isolated** app using `/tmp/duet-signoff/project/` and temporary XDG config
and data directories. Real authenticated CLI calls consumed usage; normal
application preferences and conversations were not used. Test drivers lived
under `/tmp/duet-signoff/` rather than adding a permanent integration harness.

- `make test-parallel` passed across eight workers: 2,789 assertions, zero
  failures or skips. `make test-ui` passed, including layout restart,
  checklist, shortcut and conversation checks.
- Both real model catalogs arrived. The seeded Codex `gpt-6-luna` / `medium`
  preference survived discovery, including both dropdown instances.
- Both CLIs streamed while layouts changed; a second, longer Codex run
  remained active through all four layouts with the same stream widget.
  An unsent Codex draft survived the first two layout cycles.
- Claude accepted a mid-turn PINEAPPLE instruction. After that turn became
  idle, the next request received SECOND; a later question received Blue.
  Rendered replies showed the expected subsequent answers. The existing
  process-index regression tests remain the more exhaustive attribution
  coverage.
- A real Claude AskUserQuestion card survived all layouts. Selecting Blue
  and emitting the visible Submit button's clicked signal completed the
  request with Blue. This was programmatic GTK interaction, not a native
  pointer test of the question controls.
- A real Claude approval card requested a Python command writing only
  `/tmp/duet-signoff/project/approval.txt`. A native pointer click on Allow
  executed it, the file appeared, and both rendered cards showed Allowed.
  Earlier combined acceptance already verified Codex's real approval path
  and missing/signed-out CLI behavior; those checks were not repeated here.
- Changing the real dropdowns selected Claude sonnet and Codex high effort.
  Subsequent requests completed; Claude retained the earlier Blue answer
  across its model-triggered process restart.
- Calling `agent-ran-out` on the live Codex agent changed its status and
  Conversation only strip to Out of usage; `agent-has-usage` restored Ready
  and real usage percentages. Exhaustion itself was simulated, not induced
  on the account.
- The isolated window was closed through its normal close-request handler
  and reopened in a fresh process. Agents on left, Claude sonnet and Codex
  high effort restored with real catalogs enabled.

**Milestone 1 signed off, 2026-10-21.** The user confirmed that physical
divider dragging has been working. This supplies the manual physical-input
evidence missing from automation; the inconclusive automated attempts above
are retained as historical limits. Checklist restoration at 1000px and minimum
widths is covered by the earlier programmatic allocation matrix, rather than
a new claim of user verification of every layout/width combination. Combined
with the automated and live CLI checks, this closes redesign acceptance.
No later milestone work or commit was started.

## Milestone 2: task summary, usage, and time

Implemented for review, extending the existing completion card:

- [x] Inventory actual CLI usage, duration, and cost events; retain missing
  values as unknown instead of deriving unsupported prices or totals.
- [x] Store per-turn records and task aggregates in workflow checkpoints,
  saved through the existing history checkpoint path. Pending submissions
  remain unknown after a crash instead of disappearing from coverage. Start
  and completion timestamps preserve elapsed time for completed tasks; an
  explicit continuation clears the completion timestamp.
- [x] Extend the end-of-task card with changed files linked to Changes, rounds
  per loop, recorded check outcome, open findings, elapsed wall time, and
  per-agent usage/cost data. Text-only and incomplete/unreviewed results use
  the same card. Elapsed time includes pauses and time between restarts.
- [x] Cover lifecycle, interruption, retry, restart, handoff, parallel agents,
  unknown metrics, duplicate Claude result indexes and stale/repeated Codex
  usage snapshots. GTK checks exercise the text-only completion card.

Claude result events provide cost, duration, input/output and cache tokens.
Indexed results are counted once per process; distinct results from mid-turn
messages accumulate. A two-turn live probe confirmed Claude cost is cumulative
within its CLI process, so cost deltas are taken against that process's previous
result, while duration and token usage remain per result. A new process resets
the cost baseline; absent cost fields and result-index gaps leave affected
cost deltas unknown. Indexless CLI results cannot establish whether a result
was missed. Missing fields make that turn's field unknown. Codex's
`thread/tokenUsage/updated` schema was generated from the installed CLI:
`last` describes the most recent model request, so thread `total` differences
from the first update's pre-request baseline accumulate a tool-using turn.
Thread and turn IDs reject stale usage updates; repeated snapshots replace
rather than add. Codex cost and CLI duration are not reported and remain
unknown. Cancellation/failure records are marked incomplete; late results
cannot publish them twice. Raw turn records are checkpoint data, not billing
records. The separate backlog item for per-turn metrics in status lines is
not included in this card change.

A live authenticated smoke test in `/tmp/duet-signoff/project/` returned OK
from both agents and captured real metrics: Claude cost/duration/token fields,
Codex token fields. It consumed real usage without editing project files.
No model prices were inferred. Final validation: `make load`,
`make test-parallel` (2,838 assertions across eight workers, zero failures or
skips), `make test-ui` (including task summary checks), and `git diff --check`
passed.

Review corrections (F1, F2, F3 and F6): pending-snapshot polling now waits
without querying Git, then reads the final file list once and updates only
its label and Changes button. The summary expander and selectable labels
retain their widget identities. The preview shows at most ten paths plus an
omitted count, with Changes retaining the full list. Claude input is labelled
uncached; Codex input is labelled as including cache. Cost index gaps reset
the cumulative baseline and make the affected cost unknown; a subsequent
contiguous result can be measured again. Regression checks cover these
behaviors, including the GTK polling path. F4 and F5 remain dismissed.

## Milestone 3: finish the commit-at-end flow

Implemented for review. Writing tasks now inspect uncommitted changes before
starting and offer commit-and-start, continue without committing, or cancel.
Review-only mode does not prompt. Partial pre-task commits recheck remaining
changes before starting. The completion action opens file selection with all
changes and task-changed presets, previously dirty file markers, exact diff
preview, and an editable suggested message. Selected files include all their
edits; excluded files retain their staging. Preview uses a temporary index;
committing holds the real index lock while preserving unrelated staged entries.
Git hooks run with the temporary index, and failures preserve the original index.
Repository and preview freshness are rechecked before committing.

Review corrections F1–F7: Continue uses the preflight's opening state,
independent of preview success or refresh. A failed pre-task preview shows
Continue/Cancel with commit controls hidden, so conflict-resolution tasks can
start during a merge or rebase. The task-changed preset restores the default
selection, excluding previously dirty files. Fallback start snapshots delete
only their own refs; task-owned snapshot refs remain intact. Text-only tasks
explain that no files changed instead of enabling Commit. The completion card
has no message entry; the preview contains the sole editable message.

Deliberate limits: changes made after task completion still invalidate the
commit action rather than being silently included in an approved result.
Existing `completion-commit-refusals` tests cover later edits, new untracked
content, staging and branch changes. Own worktrees continues to require a
clean start; its part commits and integration merges are handled by the existing
workflow, not this dialog. The dialog commits only remaining uncommitted changes
in the home repository, never inside individual part worktrees. A clean,
already-integrated task has no additional commit to offer. This milestone does
not add partial integration or part-worktree commit actions.

Regression coverage includes pre-task preview failure, Continue during refresh,
stale opening-state rejection, preset exclusion of earlier edits, no duplicate
message box, conflicted merge startup, fallback ref ownership/cleanup,
text-only dirty tasks, and a dirty baseline before the first repository commit.
Final validation: `make test-parallel` passed 2,869 assertions across eight
workers, with zero failures or skips. `make test-ui` passed, including preview
failure/Continue, pending refresh/Continue, preset exclusion, and the single
message-entry checks. GTK emitted frame-timing warnings during layout checks.
`make load` and `git diff --check` passed. Changes remain uncommitted for review.

- [x] Compare existing commit behavior with task snapshots and Changes.
- [x] Preselect task-changed files; leave files already modified before the
  task unselected and visibly marked. Let the user edit the selection.
- [x] Show the staged diff and an editable lead-drafted message before the
  explicit commit action. Handle existing staged edits deliberately.
- [x] Verify dirty repositories, untracked files, text-only results, failed
  checks, worktree integrations, and changes made after task completion.

## Milestone 4: search

Implemented for review: Cmd+F / Ctrl+F or **Find in pane…** opens a pane
selector and literal, case-insensitive search with occurrence counts,
Previous/Next, Enter/Shift+Enter, and Escape to close. Navigation highlights the
matching rendered text in yellow even while Find has focus and expands containing
details. History filters titles and saved message text within the current folder,
across all saved conversations
rather than only the recent 30. Pane search matches within individual rendered
labels; it does not span separate Markdown blocks or formatting labels.

- [x] Search within transcripts and the conversation/history list.

Validation: `make test-parallel` passed 3,268 assertions across eight workers
with zero failures or skips. `make test-ui` passed, including occurrence counts,
Unicode offsets, collapsed details, navigation wrapping, no-match feedback,
visible scrolling and preserving search position during new replies.

Review corrections F1–F4: Find uses Cmd+F on macOS, leaving Ctrl+F to the
Terminal; elsewhere Ctrl+F is ignored while terminal focus is active. Configured
panel shortcuts take precedence. Automatic following pauses only after match
navigation and resumes on query/pane changes or closing Find. History summaries
retain metadata only; non-empty body searches read messages on demand. Selector
names and panes are built from the same filtered pairs.

### Deferred: task export

- [ ] Export a task as Markdown, including request, plans, reviews, checklist,
  and the combined written result for text-only tasks.

Export has no scheduled milestone; deferred at the user's request until needed.

## Remaining backlog

These scopes are recorded in `TODO.md`; the order below is provisional.

- [ ] Artifact discovery and cards: local output paths and metadata, creator,
  type, status, previews, external open/save/export, and cross-agent review.
  Preserve links/files across restart, handoff, and worktree cleanup. Isolate
  active HTML and show unsupported previews or missing tools honestly.
- [ ] Per-folder prompt templates in the shared composer.
- [ ] Keyboard shortcuts and a discoverable help sheet for workflow actions,
  pane focus, checklist, and terminal.
- [ ] GTK UI smoke coverage for layouts and attention/recovery card flows,
  on machines where GTK is available.

### Deferred: undo a task

- [ ] Build a preview from the reverse start-to-end task snapshot diff.
- [ ] Allow excluding files; explain that user edits made during the task
  are included in that snapshot diff too.
- [ ] Apply with three-way reconciliation against the current tree to keep
  later edits. List conflicting files and leave them untouched.
- [ ] Test tracked/untracked files, adds/deletes/renames, later edits,
  conflicts, and interrupted application before exposing the action.

Undo is deferred until repeated use shows a need beyond task commits and Git
reverts. It has no scheduled milestone; the scope above is retained for later
reassessment.

A third agent remains outside the current plan.

## Resuming work

1. Start in `/Users/jessie/projects/lisp/agent-duet`, not the sibling
   `lisp-edit` repository. The earlier supplied `b15fdae` base belonged to
   `lisp-edit` and is not the base for these changes.
2. Read `AGENTS.md`, this file, `TODO.md`, and the working-tree diff/status.
   Preserve existing uncommitted changes; do not re-copy old worktree files.
3. Milestone 1 is signed off and milestone 2 is implemented for review.
   Milestone 3 is implemented for review; review the current diff before starting milestone 4.
4. Update this file and `TODO.md` as milestones are completed. Add user-visible
   changes to `CHANGELOG.md` under Unreleased before committing.
