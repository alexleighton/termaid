#!/usr/bin/env bash
# Check whether upstream mermaid.rs has changed since the version vendored into
# the oracle.
#
# Compares the pinned git blob hash (oracle/UPSTREAM) against the current file on
# the upstream default branch via `gh`. On a change it lists the commits that
# touched the file since the pinned commit and writes a unified diff to a tmp dir
# for inspection. Read-only: it never modifies the vendored copy.
#
# Note: GitHub's blob SHA is a plain `git hash-object` of the file, so the pinned
# hash can be verified locally with `git hash-object oracle/src/mermaid.rs`.
#
# Exit status: 0 = up to date, 1 = upstream changed, 2 = usage/error.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
PIN="$ROOT/oracle/UPSTREAM"
VENDORED="$ROOT/oracle/src/mermaid.rs"

[ -f "$PIN" ] || { echo "missing provenance file: $PIN" >&2; exit 2; }
[ -f "$VENDORED" ] || { echo "missing vendored file: $VENDORED" >&2; exit 2; }
command -v gh >/dev/null || { echo "gh (GitHub CLI) is required" >&2; exit 2; }

val() { grep -E "^$1=" "$PIN" | head -1 | cut -d= -f2-; }
REPO=$(val repo)
FP=$(val path)
PIN_COMMIT=$(val commit)
PIN_BLOB=$(val blob)

# Detect local drift of the vendored copy from the pin.
LOCAL_BLOB=$(git hash-object "$VENDORED")
if [ "$LOCAL_BLOB" != "$PIN_BLOB" ]; then
  echo "WARNING: vendored file has drifted from the pin in oracle/UPSTREAM"
  echo "  oracle/src/mermaid.rs : $LOCAL_BLOB"
  echo "  pinned blob           : $PIN_BLOB"
  echo
fi

DEF=$(gh api "repos/$REPO" --jq .default_branch)
UP_BLOB=$(gh api "repos/$REPO/contents/$FP?ref=$DEF" --jq .sha)

printf 'repo             %s\n' "$REPO"
printf 'path             %s\n' "$FP"
printf 'pinned commit    %s\n' "$PIN_COMMIT"
printf 'pinned blob      %s\n' "$PIN_BLOB"
printf 'upstream branch  %s\n' "$DEF"
printf 'upstream blob    %s\n' "$UP_BLOB"
echo

if [ "$UP_BLOB" = "$PIN_BLOB" ]; then
  echo "UP TO DATE — upstream mermaid.rs is identical to the vendored copy."
  exit 0
fi

echo "CHANGED — upstream mermaid.rs differs from the vendored copy."
echo

PIN_DATE=$(gh api "repos/$REPO/commits/$PIN_COMMIT" --jq '.commit.committer.date')
echo "Commits touching the file since the pinned commit ($PIN_DATE):"
gh api --paginate "repos/$REPO/commits?path=$FP&sha=$DEF&since=$PIN_DATE" \
  --jq '.[] | "  \(.sha[0:12])  \(.commit.committer.date)  \(.commit.message | split("\n")[0])"'
echo

TMP=$(mktemp -d "${TMPDIR:-/tmp}/termaid-upstream.XXXXXX")
OUT="$TMP/mermaid.rs"
gh api "repos/$REPO/contents/$FP?ref=$DEF" -H "Accept: application/vnd.github.raw" > "$OUT"
echo "Downloaded upstream file: $OUT"
echo "  upstream blob (recomputed): $(git hash-object "$OUT")"
echo
echo "Unified diff (vendored -> upstream):"
diff -u "$VENDORED" "$OUT" | sed 's/^/  /' || true
echo
echo "To adopt the new version:"
echo "  cp '$OUT' '$VENDORED'"
echo "  # update oracle/UPSTREAM:  commit=<new sha>  blob=$UP_BLOB"
echo "  (cd '$ROOT/oracle' && cargo build --release)   # rebuild the oracle"
echo "  ./scripts/gen_golden.sh                          # regenerate fixtures, then review + run tests"

exit 1
