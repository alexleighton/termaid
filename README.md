# termaid

Render [Mermaid](https://mermaid.js.org/) diagrams as Unicode box-drawing art,
suitable for a terminal.

`termaid` parses a subset of Mermaid — flowcharts, state diagrams, class
diagrams, entity-relationship diagrams, and sequence diagrams — and lays them
out as box-drawing art. Output is plain UTF-8 lines plus a TUI-agnostic,
class-tagged span model for callers that want to colorize. It depends on no
terminal framework; unsupported diagram types fall back to a framed raw box.

## Example

Pipe a Mermaid diagram into `termaid` and it prints box-drawing art — here it is
rendering its own pipeline:

```console
$ termaid <<'EOF'
flowchart TD
  Src[Mermaid text] --> R{render}
  R -->|recognized| P[parse]
  R -->|no match| F[fallback]
  P --> L[layout]
  L --> Cv[canvas]
  Cv --> Art[box art]
  F --> Art
EOF
   ┌──────────────┐
   │ Mermaid text │
   └───────┬──────┘
           │
           ▼
      ╭────────╮
      │ render │
      ╰────┬───╯
     ┌─────┴──────┐
     ▼recognized  ▼no match
 ┌───────┐  ┌──────────┐
 │ parse │  │ fallback ├─────┐
 └───┬───┘  └──────────┘     │
     └─────┐                 │
           ▼                 │
      ┌────────┐             │
      │ layout │             │
      └────┬───┘             │
           │                 │
           ▼                 │
      ┌────────┐             │
      │ canvas │             │
      └────┬───┘             │
           │                 │
           ▼                 │
      ┌─────────┐            │
      │ box art │◄───────────┘
      └─────────┘
```

## API

```ocaml
type cls = Empty | Border | Text | Edge | Edge_label | Title
type span = { text : string; cls : cls }
type line = span list
type t = { plain_lines : string list; styled_lines : line list }

val render : ?max_width:int -> string -> t option
```

`render` returns `None` only for empty input; unrecognized or too-wide diagrams
render as a framed fallback. Map `cls` to your own colors (ANSI, notty, …).

## Development

A standard dune project. With a local opam switch:

```console
$ opam install . --deps-only --with-test --with-doc   # first-time setup
$ dune build                     # library, CLI, and examples
$ dune runtest                   # invariant + golden suites
$ dune build @doc                # odoc API docs
$ dune exec examples/demo.exe    # render a few sample diagrams
$ printf 'graph TD\n A-->B\n' | dune exec bin/main.exe    # run the CLI on stdin
```

The golden suite in `test/golden/` diffs the renderer, byte-for-byte, against a
reference "oracle" — the upstream Rust renderer built under `oracle/`.
`scripts/gen_golden.sh` regenerates those fixtures from the oracle, and
`scripts/check_upstream.sh` watches upstream `mermaid.rs` for changes; see
`oracle/README.md`. These dev-only tools live in the repo but are excluded from
the published package tarball.

## Attribution & license

`termaid` is licensed under the **Apache License 2.0** (see [`LICENSE`](LICENSE)).

It is an OCaml port of the self-contained Rust `mermaid.rs` terminal renderer
from [xai-org/grok-build](https://github.com/xai-org/grok-build), which is also
Apache-2.0. The original diagram parsing, layout, and box-drawing logic are the
work of SpaceXAI; see [`NOTICE`](NOTICE) for details and a list of
modifications.
