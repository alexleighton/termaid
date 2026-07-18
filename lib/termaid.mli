(** Render a subset of Mermaid diagrams as Unicode box-drawing art.

    Supported diagram types: flowchart/graph, state, class, entity-relationship,
    and sequence diagrams. Unsupported types fall back to the raw source framed
    in a box.

    This is an OCaml port of the self-contained Rust [mermaid.rs] renderer from
    xai-org/grok-build (Apache-2.0); see the project NOTICE for attribution. *)

(** Semantic class of a run of cells, for callers that want to colorize output.
    Deliberately TUI-agnostic: map these to your own styles (ANSI, notty, …). *)
type cls =
  | Empty  (** background / whitespace *)
  | Border  (** box and frame borders *)
  | Text  (** node and label text *)
  | Edge  (** edge/connector lines and arrowheads *)
  | Edge_label  (** text labels attached to edges *)
  | Title  (** diagram title *)

(** A maximal run of adjacent cells sharing one {!type:cls}. *)
type span =
  { text : string
  ; cls : cls
  }

(** One rendered row as a list of styled spans (concatenating [text] yields the
    corresponding {!field:plain_lines} entry, modulo trailing whitespace). *)
type line = span list

(** A rendered diagram. *)
type t =
  { plain_lines : string list
      (** Rows of box art as plain UTF-8, with trailing whitespace trimmed. *)
  ; styled_lines : line list  (** The same rows carrying per-run {!type:cls} tags. *)
  }

(** [render ?max_width src] renders the Mermaid source [src].

    Returns [None] only when [src] is empty or whitespace. Diagram types that
    are recognized but too wide to fit, or not recognized at all, render as a
    framed fallback box rather than [None].

    @param max_width soft cap on output width in terminal cells (columns). *)
val render : ?max_width:int -> string -> t option

(** Internal building blocks, exposed for the test suites only. Not subject to
    semantic versioning — do not depend on this from outside the package. *)
module Internal : sig
  module Ucore : module type of Ucore
  module Util : module type of Util
  module Vec : module type of Vec
  module Const : module type of Const
  module Text : module type of Text
  module Ir : module type of Ir
  module Parse_graph : module type of Parse_graph
  module Parse_state : module type of Parse_state
  module Parse_class : module type of Parse_class
  module Parse_sequence : module type of Parse_sequence
  module Seq_layout : module type of Seq_layout
  module Style : module type of Style
  module Canvas : module type of Canvas
  module Wrap : module type of Wrap
  module Rank : module type of Rank
  module Layout : module type of Layout
  module Fallback : module type of Fallback
  module Render : module type of Render
end
