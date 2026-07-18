(* termaid — render Mermaid diagrams as Unicode box-drawing art.

   OCaml port of the Rust `mermaid.rs` renderer from xai-org/grok-build
   (Apache-2.0). See NOTICE for attribution. *)

type cls = Style.cls =
  | Empty
  | Border
  | Text
  | Edge
  | Edge_label
  | Title

type span = Style.span =
  { text : string
  ; cls : cls
  }

type line = Style.line

type t = Style.t =
  { plain_lines : string list
  ; styled_lines : line list
  }

let render = Render.render

(* Unstable surface exposed for the test suites; not part of the public API. *)
module Internal = struct
  module Ucore = Ucore
  module Util = Util
  module Vec = Vec
  module Const = Const
  module Text = Text
  module Ir = Ir
  module Parse_graph = Parse_graph
  module Parse_state = Parse_state
  module Parse_class = Parse_class
  module Parse_sequence = Parse_sequence
  module Seq_layout = Seq_layout
  module Style = Style
  module Canvas = Canvas
  module Wrap = Wrap
  module Rank = Rank
  module Layout = Layout
  module Fallback = Fallback
  module Render = Render
end
