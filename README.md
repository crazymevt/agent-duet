# agent-duet (Agent Duet)

Claude and ChatGPT working on the same code, side by side, through their
command-line tools: Claude Code (`claude -p`, stream-json) and Codex
(`codex app-server`, the CLI bundled with the ChatGPT app or on PATH).

The window has three panes:

- **Left: Claude.** Everything Claude does: the prompts it gets, its reply as it
  streams, its tool calls (expand one to see its output).
- **Middle: the conversation between them.** Your request, each plan, each
  report of work done, and each review, passed from one agent to the other.
- **Right: ChatGPT.** The same, for Codex.

Each side pane also has its own box for talking to that agent alone.

## Working together

Type a request in the middle and press Return. Choose how they work:

- **Write & review.** The lead does the work. The other reviews it, reading the
  files and `git diff`, and ends with `VERDICT: APPROVED` or
  `VERDICT: CHANGES REQUESTED`. The lead addresses the review, and so on, until
  it's approved or *Rounds* reviews have been done.
- **Split the work.** The lead splits the request into its part and the
  other's (parts that don't touch the same files). Both work at once; then each
  reviews the other's part, and each part goes round its own review loop.

*Claude leads* / *ChatGPT leads* picks the lead. Start becomes Stop while they
work.

## Running out of usage

When an agent's account runs out partway through a task, its step pauses
instead of failing, and a card in the middle pane offers:

- **Wait until 3:05 PM** (when the reset time is known): it's sent to the
  same agent again a minute after its usage resets. Leave the app open.
- **Try again**: send it again now.
- **Let ChatGPT take over** (or Claude): the other agent picks up the work
  where it stopped. It won't be reviewed, since the reviewer is the one out,
  and the conversation says so. If it was a review that paused, this button is
  **Finish unreviewed**: that part is done, marked as not reviewed.
- **Stop**.

Nothing done so far is lost. While an agent is out, with a known reset time,
it turns prompts away without trying; its status line says until when.

## Folder, models and look

- **Folder** (top left): where both agents work. Its menu lists recent folders
  and *Choose folder…*. Switching starts new conversations with both, in the
  new folder.
- **Model and thinking** (under each agent's name): Claude's model (sonnet,
  opus, haiku) and effort; ChatGPT's models as Codex lists them, with the
  thinking levels each supports. A change applies from that agent's next
  message, carrying on the same conversation.
- **Remaining usage** for both shows in their status lines (for example
  `65% 5h / 56% 7d left`; hover for the reset times), turning orange when
  under 10% is left.
- **Dark / Light / System** (top right).

These, the permission setting and the recent folders are remembered in
`~/.config/agent-duet/settings.lisp`.

## Terminal

**Terminal** in the header (or Ctrl+`) opens a panel below the three panes with
a shell in the agents' folder. *+ New* opens another (each gets a tab), *Clear*
(⌘K) clears one, *Kill* ends one, and *Hide* closes the panel while the shells
keep running. ⌘C and ⌘V copy and paste. New shells start in the current folder;
ones already open stay where they are. It needs VTE, GNOME's terminal widget:
`brew install vte3` on macOS.

## Documents

**Documents** in the header opens a window listing the folder's Markdown files
(type to filter) with a read-only preview of the one you pick: headings,
emphasis, code, lists and task lists, quotes, tables, rules, local images and
links. Links to other Markdown files open in the preview, `#anchors` scroll to
their heading, and web links open in the browser. The preview redraws itself
when the file changes, so you can watch a design document as an agent writes
it. Hidden folders and build folders (`node_modules`, `build`, …) are skipped.

## What they may do

The menu at the top right:

- **Ask before commands** (the default): both may edit files in the folder.
  Before running a command — tests, builds, scripts — they ask, with a card in
  their pane: *Allow*, *Always allow* (for the rest of that conversation) or
  *Deny*. During a workflow, the middle pane says who is waiting. (Read-only
  commands such as `git diff` or `ls` run without asking.)
- **Edits only, no prompts**: both edit without asking. Claude runs no commands
  except read-only git; ChatGPT runs commands in Codex's `workspace-write`
  sandbox (no network) without asking.
- **Full access**: Claude with `bypassPermissions`, Codex with
  `danger-full-access`. Use it only in a folder you're happy for them to change.

Both must be signed in first: `claude auth login` and `codex login`.

## Running

On macOS, GTK must run on the process's first thread. In Cadre, **Run GTK App**
starts it there and keeps the REPL. From a terminal:

```sh
make run      # the agents work in the last folder used, or the current directory
make dev      # with a Swank server on port 4005 for another editor's REPL
make build    # bin/agent-duet
```

Or `(agent-duet:main "/path/to/project/")` to choose the folder.

### As a Mac app

```sh
make app           # build/Agent Duet.app
make install-app   # build it and put it in /Applications
```

The app carries its own Lisp image and SBCL runtime; GTK, libadwaita and VTE
still come from Homebrew (`brew install gtk4 libadwaita vte3`). Opened from the
Finder or the Dock, it takes `PATH` from your login shell, so `claude`, `codex`
and your tools are found as in a terminal, and it starts in the last folder you
used (your home folder the first time). Its output goes to
`~/.local/state/agent-duet/agent-duet.log`. It's signed ad hoc, or with
`CODESIGN_IDENTITY` if that's set.

## Layout of the code

- `src/json.lisp`, `src/claude.lisp`, `src/codex.lisp`, `src/markdown.lisp`
  (inline and block Markdown parsing):
  taken from Cadre, with its fixes (UTF-8 replacement on Codex's output, polled
  JSON-RPC waits, draining stderr when Codex exits, resuming Claude sessions).
- `src/agents.lisp`: both CLIs behind one `agent` class that takes one prompt at
  a time and queues the rest, and asks the user for approvals (Claude through
  `--permission-prompt-tool stdio`, Codex through the app-server's approval
  requests).
- `src/settings.lisp`: what's remembered between runs.
- `src/usage.lisp`: both accounts' usage windows, and recognizing a turn that
  failed because one ran out.
- `src/workflow.lisp`: the two ways of working together. No GTK.
- `src/ui.lisp`: the window.
- `src/terminal.lisp`: the Terminal panel (VTE through CFFI, from Cadre).
- `src/preview.lisp`: the Documents window (Cadre's Markdown preview, read-only).

## Tests

```sh
make test
```

They run without GTK or the CLIs: the workflows are driven by scripted agents.
