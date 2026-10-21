# Makefile for agent-duet. Needs Quicklisp, loaded from your Lisp's init file.

LISP ?= sbcl
TEST_JOBS ?= 8
# Compiling the generated GTK bindings exceeds Ubuntu SBCL's default heap.
LISP_FLAGS ?= --dynamic-space-size 2048
RUN = $(LISP) $(LISP_FLAGS) --non-interactive --eval '(require :asdf)' --eval '(push (truename ".") asdf:*central-registry*)'

.PHONY: load test test-ui test-parallel screenshots run dev build app install-app clean

# Load the system, fetching its dependencies
load:
	$(RUN) --eval '(ql:quickload :agent-duet)'

# Run the tests; exits 1 if one fails
test:
	$(RUN) --eval '(ql:quickload :agent-duet/tests)' \
		--eval '(uiop:quit (if (eq :passed (parachute:status (parachute:test :agent-duet/tests))) 0 1))'

# GTK conversation checks (requires GTK and a working display).
test-ui:
	$(LISP) $(LISP_FLAGS) --non-interactive --load scripts/test-conversation-ui.lisp

# Regenerate the README screenshots in docs/ (requires GTK and a working display).
screenshots:
	$(LISP) $(LISP_FLAGS) --non-interactive --load scripts/screenshots.lisp

# Warm the compilation cache before starting independent Lisp workers.
test-parallel:
	$(RUN) --eval '(ql:quickload :agent-duet/tests)'
	sh scripts/test-parallel.sh $(TEST_JOBS) $(RUN) --eval '(ql:quickload :agent-duet/tests)'

# Run the program (on the first thread, as GTK needs on macOS)
run:
	$(RUN) --eval '(ql:quickload :agent-duet)' --eval '(agent-duet:main)'

# Run it with a Swank server on port 4005, for an editor's REPL (in Cadre, use Run GTK App)
dev:
	$(LISP) $(LISP_FLAGS) --eval '(require :asdf)' --eval '(push (truename ".") asdf:*central-registry*)' \
		--eval '(ql:quickload (quote (:swank :agent-duet)))' --eval '(swank:create-server :port 4005 :dont-close t)' \
		--eval '(agent-duet:main)'

# Build the program: bin/agent-duet (it needs GTK installed where it runs)
build:
	mkdir -p bin
	$(RUN) --eval '(ql:quickload :agent-duet)' --eval '(gtk4:save-executable "bin/agent-duet" (function agent-duet:main))'

# The macOS app: build/Agent Duet.app (GTK, libadwaita and VTE come from Homebrew)
app:
	$(LISP) $(LISP_FLAGS) --non-interactive --load scripts/build-app.lisp
	SBCL=$(LISP) scripts/make-app.sh

# Build the app and put it in /Applications
install-app: app
	rm -rf "/Applications/Agent Duet.app"
	ditto "build/Agent Duet.app" "/Applications/Agent Duet.app"
	@echo "Installed /Applications/Agent Duet.app"

clean:
	find . -name '*.fasl' -delete
	rm -rf bin build
