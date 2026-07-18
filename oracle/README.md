# Oracle — upstream reference renderer

This directory builds the upstream Rust `mermaid.rs` into a small stdin→box-art
filter used as the **reference oracle** for the OCaml port: its output is the
ground truth the golden tests diff against. It is a development tool only — not
part of the opam package, and excluded from the published tarball (see
`../.gitattributes`).

## Layout

- `src/mermaid.rs` — the vendored upstream renderer, verbatim from
  [xai-org/grok-build](https://github.com/xai-org/grok-build) (Apache-2.0).
  Provenance and the pinned git blob hash are in [`UPSTREAM`](UPSTREAM).
- `ratatui/` — a no-op shim crate standing in for the real `ratatui`. The
  upstream file pulls ratatui in only to build styled output; the oracle
  consumes the plain-text lines, so the shim lets `mermaid.rs` compile
  unmodified without ratatui's dependency tree.
- `src/main.rs` — reads a diagram on stdin and prints the plain box-art lines.
  `unicode-width` is the real crate, pinned to the version the upstream
  workspace uses so terminal cell widths match byte-for-byte.

## Build

Requires a Rust toolchain (`cargo`). On macOS: `brew install rust`.

```console
$ cargo build --release
```

Produces `target/release/termaid-oracle`.

## Use

```console
$ printf 'flowchart LR\n  A --> B\n' | ./target/release/termaid-oracle --width 120
```

`--width N` caps output at N columns; `--width none` disables the cap; the
default is 120 (matching the upstream test helper).

### Regenerate the golden fixtures

`../scripts/gen_golden.sh` runs the oracle over every `../test/golden/*.mmd`
input and writes the matching `.expected` box art. An optional `NAME.width`
sidecar overrides the default width per fixture.

```console
$ (cd .. && ./scripts/gen_golden.sh)
```

`dune runtest` then diffs the OCaml renderer against those fixtures.

### Track upstream changes

`../scripts/check_upstream.sh` compares the vendored `src/mermaid.rs` against the
current upstream file via `gh` and reports any drift. It exits `0` when up to
date, `1` when upstream changed (printing the changed commits and a diff), and
`2` on error. A weekly GitHub Action runs it and opens a tracking issue on drift.

## Updating the vendored file

When upstream changes, `check_upstream.sh` prints the new commit/blob and a diff.
To adopt:

1. Replace `src/mermaid.rs` with the new upstream version.
2. Update `commit` and `blob` in [`UPSTREAM`](UPSTREAM) (the script prints the
   new blob hash; verify locally with `git hash-object src/mermaid.rs`).
3. `cargo build --release` — rebuild the oracle.
4. `(cd .. && ./scripts/gen_golden.sh)` — regenerate fixtures.
5. Review the fixture diff and run `dune runtest`; port any behavior changes
   into the OCaml modules until the golden suite is green again.
