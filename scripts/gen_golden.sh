#!/usr/bin/env bash
# Regenerate golden fixtures from the Rust oracle.
#
# For each test/golden/NAME.mmd, render with the oracle and write
# test/golden/NAME.expected. An optional test/golden/NAME.width file overrides
# the default width of 120.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ORACLE="$ROOT/oracle/target/release/termaid-oracle"
DIR="$ROOT/test/golden"

if [ ! -x "$ORACLE" ]; then
  echo "oracle not built; run: (cd '$ROOT/oracle' && cargo build --release)" >&2
  exit 1
fi

shopt -s nullglob
for mmd in "$DIR"/*.mmd; do
  base="${mmd%.mmd}"
  width=120
  [ -f "$base.width" ] && width="$(tr -d '[:space:]' < "$base.width")"
  "$ORACLE" --width "$width" < "$mmd" > "$base.expected"
  echo "generated $(basename "$base").expected (width $width)"
done
