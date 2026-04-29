#!/usr/bin/env bash
# Usage: run.sh [pattern] [WxH]
# Defaults to a glider on a 24x16 grid, which animates smoothly at ~5 fps
# on this interpreted actor implementation. Bigger grids work but pace
# slows linearly with cell count.
set -e
cd "$(dirname "$0")/../.."
exec ./samples/gol/standalone.escript "${1:-glider}" "${2:-20x12}"
