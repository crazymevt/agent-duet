# agent-integration

A Common Lisp project.

## Usage

```lisp
(ql:quickload :agent-integration)
(agent-integration:hello "Lisp")   ; => "Hello, Lisp!"
```

## Running

On macOS, GTK must run on the process's first thread. In Cadre, **Run GTK App**
starts it there and keeps the REPL; while it runs, what you evaluate runs on the
GTK thread. From a terminal:

```sh
make run      # just the program
make dev      # with a Swank server on port 4005 for another editor's REPL
make build    # bin/agent-integration
```

With `make dev`, wrap GTK calls you evaluate in `(glib:in-main-thread (:wait t) …)`.

## Tests

```sh
make test
```

Or, in a Lisp: `(asdf:test-system :agent-integration)`.
