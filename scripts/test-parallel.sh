#!/bin/sh
# Run isolated Lisp workers, collecting output without interleaving it.
set -eu
jobs=$1
shift
case "$jobs" in
    ''|*[!0-9]*) echo 'TEST_JOBS must be a positive integer' >&2; exit 2 ;;
esac
if ! [ "$jobs" -gt 0 ] 2>/dev/null; then
    echo 'TEST_JOBS must be a positive integer' >&2
    exit 2
fi
work=$(mktemp -d "${TMPDIR:-/tmp}/duet-tests.XXXXXXXX")
trap 'rm -rf "$work"' EXIT
pids=''
# Stop workers before deleting their temporary directories on interruption.
trap 'trap - INT TERM; for pid in $pids; do kill "$pid" 2>/dev/null || :; done; wait || :; exit 130' INT TERM
i=0
while [ "$i" -lt "$jobs" ]; do
    mkdir "$work/$i"
    TEST_SHARD=$i TEST_SHARDS=$jobs TMPDIR="$work/$i/" \
        "$@" --load scripts/test-shard.lisp > "$work/$i.log" 2>&1 &
    pids="$pids $!"
    i=$((i + 1))
done
status=0
for pid in $pids; do
    if wait "$pid"; then :; else status=1; fi
done
i=0
while [ "$i" -lt "$jobs" ]; do
    cat "$work/$i.log"
    i=$((i + 1))
done
exit "$status"
