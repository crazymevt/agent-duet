# Changelog

## Unreleased

- Sidebar divider positions now restore after GTK allocates an orientation
  change, preserving the wide layout when narrowing and widening. Checklist
  restoration also runs when a visibility toggle leaves the allocation unchanged.
- The conversation reserves 320px and each agent panel 300px so Three panes
  keeps its composer and attention controls readable. Below 680px, responsive
  sidebar layouts stack the agents and reserve more vertical room for them.
- Refreshed the README screenshot from the redesigned window with explicitly
  synthetic approval/question cards.
- Settings loading rejects malformed top-level lists and drops circular or
  dotted values while preserving independent valid preferences. Circular
  nested lists and arrays of any rank no longer reach the settings printer;
  malformed divider positions, recent folders and check commands are ignored.
- A redesigned window: the shared conversation is the large pane on the
  left, with both agents' panels stacked in a resizable sidebar on the
  right. Workspace › Layout offers four layouts: *Agents on right*,
  *Agents on left* (the agents' panels stacked on the left, the conversation
  on the right), *Three panes*, and *Conversation only*. The layout is
  remembered, each with its own divider positions, and switching keeps
  running replies, approvals and questions where they are. Transcripts
  expand, the shared composer is larger ("Task for both agents"), and the
  mode, lead and round controls are compact, with the workflow settings
  expandable. *Start collaboration* starts a task;
  *Stop collaboration* stays within reach while it runs. Mode, lead, and
  rounds are remembered; malformed or obsolete saved choices fall back to
  the defaults.
- Header tools and appearance, layout, and permission controls are grouped
  in the Workspace menu. The checklist starts hidden; progress notices point
  to Workspace › Checklist. Opening it reserves a resizable width, and the
  conversation adapts to the space left. All layouts protect pane minimum
  sizes; responsive sidebar changes follow rendered allocations without a
  polling timer. Conversation-only status retains CLI readiness and direct
  message activity.
- Each agent's panel says what it is doing in the task (planning, working,
  reviewing, fixing, running the check, waiting, paused, out of usage) and
  marks what needs you: a plan to approve, a question, a pause. A progress
  strip shows the task's stages; a split plan's parts show side by side,
  each with its own round, rather than as one line. A plan shows as approved
  only once you've approved it, and a check is never shown as passed.

## 1.1.0 - 2026-10-20

- Split plans can be ordered: the lead says whether the parts go on at the
  same time or one first (`ORDER:` in its plan), and the plan card has an
  order menu. A dependent part waits, then starts with its prerequisite's
  report and, in worktrees, its work merged in (a conflict pauses it for its
  agent to resolve). A plan where each part waits for the other doesn't
  start: the lead revises it once, then it's yours to fix on the card.
- A prerequisite redone after its dependent started (a retry, a hand-off,
  more rounds) has the dependent stop, told why, and revalidate its work
  against the change, keeping what it had done; parts done against an
  earlier version aren't merged. A prerequisite that fails or runs out of
  usage only holds its dependents up. When no part can go on, the task
  pauses with *Revise the order* (the lead re-plans from both parts'
  progress) and *Run independently*. A revised plan must keep both parts.
- A dependent part's work isn't checked, reviewed or merged until the merge
  of its prerequisite into its worktree is concluded, without conflict
  markers; and it doesn't start until stopped checks and turns have stopped.
- Handing off a failed or stopped step tells the other agent how it stopped,
  not only running out of usage.

- Split parts with unresolved reviews or failing checks stay in their worktrees
  without merging. Continue offers more review rounds; failed or incomplete
  work cannot be forced into integration.

- Recovering a task: its state is saved with the conversation as it goes.
  Reopening one that was interrupted (the app quit mid-task) or stopped
  shows where it stopped, what was done and written, each agent's
  availability, unreviewed work, and what the folder and worktrees look like
  now; nothing runs until *Review what's there*, *Resume*, *Redo the step*,
  *Hand off*, *Stop task* or *Abandon* is chosen. Histories from earlier
  versions open as before.
- During a task, an agent pane's *Stop* is *Pause*: only that agent's part
  pauses. Pause cards say why a step paused (out of usage, failed, paused by
  you) and offer *Resume*, *Retry step*, *Hand off* and *Stop task*; a
  hand-off waits until the stopped turn is confirmed gone. The middle pane's
  *Stop* keeps the task recoverable.
- Split worktree tasks combine both parts' written replies, including parts
  with no file changes, before reviewing the merged result. Worktrees and
  task-scoped recovery refs are kept until approval; unfinished tasks report
  their recovery locations.
- The conversation-only view has each agent's model and thinking-level
  dropdowns under its status, kept in step with the ones in its pane.
  During a task, each agent's status there shows its usage left again.

## 1.0.0 - 2026-10-20

- Automated builds for macOS (Apple Silicon and Intel) and Linux x86_64;
  version tags publish archives and SHA-256 checksums on GitHub Releases.
- Long prompts in the agent panes (reviews quote whole diffs) show their
  first 2,000 characters, with *Show all*. Resizing the window rewrapped
  them in full every frame, which made it lag.
- A turn with no events for 5 minutes (`:stall-minutes` in the settings)
  gets a card: *Keep waiting*, *Interrupt and retry* (the same prompt is
  sent again only once the interrupted turn has ended; if it doesn't end,
  the card says so and offers *Stop*), or *Stop*. Time waiting on the user
  (approvals, questions, plan approval, usage pauses) doesn't count.
- *Changes*: the folder's state is recorded (as git commits under
  `refs/agent-duet/snapshots/`, without touching the index, files or stash)
  when a task starts and finishes, and **Changes** in the header shows what
  the latest task changed, file by file, with each file's diff. Git runs off
  the interface's thread: the file list loads after the window opens, and
  each diff when its file is selected.
- *Check* command per folder (`make test`, `npm test`, …): it runs after
  each round of work, before the review. A failure goes back to the author
  with its output (up to twice a round), and the review prompt says how the
  check went. Work whose check still fails after the fix attempts can't be
  approved, and work taken over unreviewed is checked too. The conversation
  shows each run and its result.
- *Shared notes*: Claude is given the folder's `AGENTS.md`, and ChatGPT its
  `CLAUDE.md`, with their instructions, so both follow the project's
  conventions. Read when a conversation starts; can be switched off in the
  header.
- Drop files or images on an agent's message box to attach them to the next
  message: their paths are listed, and images are shown to the agent. On the
  middle request box, dropped files' paths are typed in.
- The panes no longer leave a new message, card or question just below the
  visible area until you scroll: following the end now waits until the
  content has been laid out.
- Review checklist: each review lists its findings (`F1`, `F2`, … with file
  locations), and the next marks each one fixed, dismissed or still open.
  The checklist shows after every review; approving with findings still
  open asks the reviewer once to settle them, and leaves the part not
  approved (to continue) if it doesn't; the end of a task says how many
  are still open.
- *Approve plan*: the plan in *Split the work* or *Compare proposals* waits
  for you, so you can edit either agent's part before work starts.
- *Compare proposals* mode: both agents propose an approach on their own,
  then the lead reconciles the two into a plan and splits the work.
- *Own worktrees* for split tasks: each agent does its part in its own git
  worktree; the parts are then committed, merged into the current branch
  (the lead resolving any conflicts), and the merged result reviewed.
- Desktop notifications, when the window isn't in front: both agents
  finished, an approval or question is waiting, an agent ran out of usage,
  a plan is waiting for approval.
- A task that runs out of review rounds without approval can be continued:
  a card offers *Continue* for more rounds, and the author picks up from the
  last review.
- Markdown blockquotes keep unquoted text only when it continues an open
  paragraph, including paragraphs inside nested quotes and list items.
  Text after quoted code, headings, tables or blank lines stays outside.
- Markdown previews read quote contents on demand so documents with many
  quotes ending at unquoted text do not repeatedly scan the remaining lines.
