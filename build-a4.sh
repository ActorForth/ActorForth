#!/bin/bash
# Build the standalone a4 binary.
# Usage: ./build-a4.sh [--target x86_64-linux-gnu]
set -euo pipefail

zig-wrapper/build.sh "$@"

mkdir -p ~/.local/bin
cp zig-wrapper/zig-out/bin/a4 ~/.local/bin/a4
rm -rf ~/.cache/a4c/
echo "Installed: ~/.local/bin/a4"
