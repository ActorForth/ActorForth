#!/bin/bash
# run.sh - launch the talk deck.
#
# Requires:
#   ~/.local/bin/presenterm  (or `presenterm` in PATH)
#
# Run from the repo root so the embedded shell snippets can find
# samples/hos/elevator/comparison/exploits/... etc.

set -uo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$HERE/../../../.." && pwd)"
cd "$REPO"

# -X enables `+exec_replace` snippets (the live checker / FAT runs).
exec presenterm -X "$HERE/talk.md" "$@"
