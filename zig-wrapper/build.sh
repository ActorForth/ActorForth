#!/bin/bash
set -euo pipefail

# Build a standalone a4c binary.
#
# Usage:
#   ./build.sh              # Build for current platform
#   ./build.sh --target x86_64-linux-gnu
#   ./build.sh --target aarch64-linux-gnu
#   ./build.sh --target x86_64-macos
#   ./build.sh --target aarch64-macos
#   ./build.sh --target x86_64-windows
#
# Prerequisites:
#   - rebar3 (Erlang build tool)
#   - zig (0.13+ recommended)
#   - zstd (compression)

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ZIG_DIR="$PROJECT_ROOT/zig-wrapper"
TARGET=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --target) TARGET="$2"; shift 2 ;;
        *) echo "Unknown option: $1"; exit 1 ;;
    esac
done

echo "=== Step 1: Building rebar3 release ==="
cd "$PROJECT_ROOT"
rebar3 as prod release

echo "=== Step 2: Compressing release ==="
RELEASE_DIR="_build/prod/rel/actorforth"
if [ ! -d "$RELEASE_DIR" ]; then
    echo "Error: Release directory not found at $RELEASE_DIR"
    exit 1
fi

# Create tar then compress with zstd
tar cf - -C "$RELEASE_DIR" . | zstd -19 -q -f -o "$ZIG_DIR/src/release.tar.zst"

COMPRESSED_SIZE=$(du -h "$ZIG_DIR/src/release.tar.zst" | cut -f1)
echo "   Compressed release: $COMPRESSED_SIZE"

echo "=== Step 3: Generating content hash ==="
sha256sum "$ZIG_DIR/src/release.tar.zst" | cut -c1-16 > "$ZIG_DIR/src/release.hash"
echo "   Hash: $(cat "$ZIG_DIR/src/release.hash")"

echo "=== Step 4: Building Zig binary ==="
cd "$ZIG_DIR"
ZIG_ARGS="-Doptimize=ReleaseSafe"
if [ -n "$TARGET" ]; then
    ZIG_ARGS="$ZIG_ARGS -Dtarget=$TARGET"
    echo "   Cross-compiling for $TARGET"
fi

zig build $ZIG_ARGS

OUTPUT="$ZIG_DIR/zig-out/bin/a4"
if [ -n "$TARGET" ] && [[ "$TARGET" == *"windows"* ]]; then
    OUTPUT="$OUTPUT.exe"
fi

if [ -f "$OUTPUT" ]; then
    BINARY_SIZE=$(du -h "$OUTPUT" | cut -f1)
    echo ""
    echo "=== Done ==="
    echo "   Binary: $OUTPUT ($BINARY_SIZE)"
    echo "   Usage:  $OUTPUT compile myfile.a4"
    echo "           $OUTPUT repl"
else
    echo "Error: Binary not found at $OUTPUT"
    exit 1
fi
