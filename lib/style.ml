(* Public styled-output model, shared by the canvas/fallback renderers and
   re-exported by {!Termaid}. Deliberately TUI-agnostic: a per-run semantic
   class the caller maps to its own colors. *)

type cls =
  | Empty
  | Border
  | Text
  | Edge
  | Edge_label
  | Title

type span =
  { text : string
  ; cls : cls
  }

type line = span list

type t =
  { plain_lines : string list
  ; styled_lines : line list
  }
