(lang dune 3.0)

(name termaid)

(generate_opam_files true)

(source
 (github alexleighton/termaid))

(license Apache-2.0)

(authors "Alex Leighton <axlelonghorn@gmail.com>")

(maintainers "Alex Leighton <axlelonghorn@gmail.com>")

(package
 (name termaid)
 (synopsis "Render Mermaid diagrams as Unicode box-drawing art")
 (description
  "termaid renders a subset of Mermaid diagrams — flowcharts, state, class,\nentity-relationship, and sequence diagrams — into Unicode box-drawing art\nsuitable for a terminal. Output is plain UTF-8 lines plus a TUI-agnostic\nclass-tagged span model for callers that want to colorize; it depends on no\nterminal framework. Unsupported diagram types fall back to a framed raw box.\n\nThis is an OCaml port of the self-contained Rust `mermaid.rs` renderer from\nxai-org/grok-build, re-implemented and repackaged for opam.")
 (tags
  ("mermaid" "diagram" "terminal" "tui" "box-drawing" "flowchart" "unicode"))
 (depends
  (ocaml
   (>= "4.14.0"))
  (uutf
   (>= "1.0.0"))
  (uucp
   (>= "14.0.0"))
  (alcotest :with-test)
  (odoc :with-doc)))
