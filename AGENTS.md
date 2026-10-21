# Project test preference

When running the full test suite, use `make test-parallel` instead of `make test`.
Concurrency is controlled by `TEST_JOBS`; see the Makefile for its default.
Override it when needed, for example `make test-parallel TEST_JOBS=4`.
Use `make test` when debugging a failure that requires sequential execution.

For changes to conversation rendering or command activity, also run `make test-ui`
on a machine with GTK and a working display. The headless suite does not load GTK.
