#!/bin/sh
# Invoke with /bin/sh quiet-command.sh -- command [args...].
# Keep the full output; never change the command's exit status.
[ "${1:-}" = -- ] && shift
[ "$#" -gt 0 ] || { echo 'Usage: quiet-command.sh -- command [args...]' >&2; exit 2; }
log=$(mktemp "${TMPDIR:-/tmp}/agent-duet-command.XXXXXXXX") || exit 125
"$@" >"$log" 2>&1
status=$?
printf 'Command exit: %s\nFull log: %s\n' "$status" "$log"
if [ "$status" -ne 0 ]; then
    # Bounded bytes as well as lines, including for a single enormous line.
    printf '\nFirst diagnostics (up to 2000 bytes):\n'
    LC_ALL=C awk 'tolower($0) ~ /error|fatal|fail|exception/ { print; count++; if (count == 8) exit }' "$log" | head -c 2000
    printf '\nFinal output (up to 2000 bytes):\n'
    tail -c 2000 "$log"
    printf '\n'
fi
exit "$status"
