# Changelog

## Unreleased

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
