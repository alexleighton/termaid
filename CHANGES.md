# Changelog

## 1.0.0 (2026-07-18)

First release — an OCaml port of the self-contained Rust `mermaid.rs` terminal
renderer from [xai-org/grok-build](https://github.com/xai-org/grok-build)
(Apache-2.0; see `NOTICE`).

- Render five Mermaid diagram types to Unicode box-drawing art — flowchart /
  graph, state, class, entity-relationship, and sequence — including subgraphs,
  self-loops, thick/dotted edges, arrowhead variants, and diagram-specific
  features (choice pseudo-states, class members/annotations/generics, ER
  cardinalities, sequence notes/blocks/autonumber). Unrecognized or too-wide
  input renders as a framed fallback box.
- Public API: `Termaid.render : ?max_width:int -> string -> t option`, returning
  plain UTF-8 lines plus a TUI-agnostic, class-tagged span model (map the `cls`
  of each run to your own colors). No terminal-framework dependency; Unicode
  width via `uucp`/`uutf`.
- Layout engine: Sugiyama-style rank assignment, barycenter crossing reduction,
  coordinate relaxation, and greedy track packing, painted onto a box-drawing
  canvas with junction-aware glyph resolution.
- Verified byte-for-byte against the upstream renderer across 53 golden fixtures
  (every diagram type plus wide/tall/dense layout stress cases); 145 invariant
  tests faithfully port the upstream test suite. CI covers Linux and macOS on
  OCaml 4.14 and 5.
