(* Invariant tests — faithful port of the upstream `mod tests` structural
   assertions. These check parse-level facts and box-art invariants (counts,
   labels, substring presence/absence, widths, alignment) rather than exact art;
   exact art is pinned by the golden suite.

   This file currently covers the parser/text layer (flowchart). Layout- and
   render-level assertions are added as those modules land. *)

module I = Termaid.Internal
module Ir = I.Ir
module Vec = I.Vec

(* --- accessors (typed to keep record-field resolution unambiguous) --- *)
let n_label (n : Ir.node) = n.label
let n_shape (n : Ir.node) = n.shape
let e_label (e : Ir.edge) = e.label
let e_line (e : Ir.edge) = e.line
let nodes (g : Ir.graph) = Vec.to_list g.nodes
let edges (g : Ir.graph) = Vec.to_list g.edges
let nth = List.nth
let contains hay needle = I.Ucore.find_str hay needle <> None

let parse src =
  match I.Parse_graph.parse_graph src with
  | Some g -> g
  | None -> Alcotest.fail "expected Some graph, got None"

let decode = I.Text.decode_html_entities

(* --- parse: structure --- *)

let parses_nodes_edges_and_direction () =
  let g = parse "flowchart LR\n  A[Start] --> B[End]" in
  Alcotest.(check int) "nodes" 2 (List.length (nodes g));
  Alcotest.(check int) "edges" 1 (List.length (edges g));
  Alcotest.(check string) "node0 label" "Start" (n_label (nth (nodes g) 0));
  Alcotest.(check string) "node1 label" "End" (n_label (nth (nodes g) 1));
  Alcotest.(check bool) "dir Right" true (g.dir = Ir.Right)

let non_flowchart_returns_none_from_parse () =
  Alcotest.(check bool) "sequence is None" true
    (I.Parse_graph.parse_graph "sequenceDiagram\n  A->>B: hi" = None)

(* --- labels: HTML / markdown / entities --- *)

let html_tags_are_stripped_from_labels () =
  let g = parse "flowchart TD\n  A[\"<b>Bold</b> and <i>italic</i>\"] --> B" in
  Alcotest.(check string) "label" "Bold and italic" (n_label (nth (nodes g) 0))

let br_tag_becomes_a_space () =
  let g = parse "flowchart TD\n  A[\"Line1<br/>Line2<br>Line3\"]" in
  Alcotest.(check string) "label" "Line1 Line2 Line3" (n_label (nth (nodes g) 0))

let markdown_string_strips_bold_italic_and_code () =
  let g =
    parse
      "flowchart TD\n  A[\"`**Start** here`\"] --> B[\"`Save to **database**`\"]\n  B --> C[\"`**Done!**`\"]"
  in
  Alcotest.(check string) "n0" "Start here" (n_label (nth (nodes g) 0));
  Alcotest.(check string) "n1" "Save to database" (n_label (nth (nodes g) 1));
  Alcotest.(check string) "n2" "Done!" (n_label (nth (nodes g) 2))

let markdown_string_preserves_snake_case_and_strips_inline_code () =
  let g = parse "flowchart TD\n  A[\"`_italic_ uses `vocab_size` with __all__`\"]" in
  Alcotest.(check string) "label" "italic uses vocab_size with all"
    (n_label (nth (nodes g) 0))

let markdown_string_edge_label_is_stripped () =
  let g = parse "flowchart TD\n  A -->|\"`**yes**`\"| B\n  A -->|\"`__no__`\"| C" in
  Alcotest.(check (option string)) "e0" (Some "yes") (e_label (nth (edges g) 0));
  Alcotest.(check (option string)) "e1" (Some "no") (e_label (nth (edges g) 1))

let plain_label_keeps_literal_text_and_underscores () =
  let g = parse "flowchart TD\n  A[\"[ 464, 3797 ] seq_len d_model\"]" in
  Alcotest.(check string) "label" "[ 464, 3797 ] seq_len d_model"
    (n_label (nth (nodes g) 0))

let code_and_span_tags_are_stripped () =
  let g =
    parse
      "flowchart TD\n  A[\"<code>vocab_size</code> <span style=\\\"color:red\\\">x</span>\"]"
  in
  Alcotest.(check string) "label" "vocab_size x" (n_label (nth (nodes g) 0))

let bare_angle_brackets_are_kept () =
  let g = parse "flowchart TD\n  A[\"a < b and c > d\"]" in
  Alcotest.(check string) "label" "a < b and c > d" (n_label (nth (nodes g) 0))

let generic_types_are_not_stripped_as_html () =
  let g =
    parse "flowchart TD\n  A[\"Returns Vec<String>\"] --> B[\"Option<i32> for <id>\"]"
  in
  Alcotest.(check string) "n0" "Returns Vec<String>" (n_label (nth (nodes g) 0));
  Alcotest.(check string) "n1" "Option<i32> for <id>" (n_label (nth (nodes g) 1))

let decode_html_entities_covers_named_numeric_and_double_escape () =
  Alcotest.(check string) "named" "<a> & \"x\" 'y'"
    (decode "&lt;a&gt; &amp; &quot;x&quot; &apos;y&apos;");
  Alcotest.(check string) "numeric" "it's <ok>" (decode "it&#39;s &#60;ok&#62;");
  Alcotest.(check string) "hex" "<tag> 'q'" (decode "&#x3c;tag&#X3E; &#x27;q&#x27;");
  (* &amp;lt; must yield the literal &lt;, never < *)
  Alcotest.(check string) "double-escape" "&lt;" (decode "&amp;lt;");
  Alcotest.(check string) "unknown" "a &foo; b & c" (decode "a &foo; b & c");
  (* Control chars (NUL collides with CONT, ESC injects ANSI) never decode. *)
  Alcotest.(check string) "controls-dec" "a&#27;b&#0;c" (decode "a&#27;b&#0;c");
  Alcotest.(check string) "controls-hex" "x&#x1b;y" (decode "x&#x1b;y")

(* --- quoting / bracket edge cases --- *)

let quoted_label_with_inner_brackets_is_one_node () =
  let g =
    parse "flowchart TD\n  IDs[\"<b>Token IDs</b><br/>[ 464, 3797 ]<br/><i>indices</i>\"]"
  in
  Alcotest.(check int) "one node" 1 (List.length (nodes g));
  Alcotest.(check int) "no edges" 0 (List.length (edges g));
  Alcotest.(check string) "label" "Token IDs [ 464, 3797 ] indices"
    (n_label (nth (nodes g) 0))

let unquoted_label_with_embedded_quote_closes_at_bracket () =
  let g = parse "flowchart TD\n  A[5\" pipe] --> B[24\" display]" in
  Alcotest.(check int) "nodes" 2 (List.length (nodes g));
  Alcotest.(check int) "edges" 1 (List.length (edges g));
  Alcotest.(check string) "n0" "5\" pipe" (n_label (nth (nodes g) 0));
  Alcotest.(check string) "n1" "24\" display" (n_label (nth (nodes g) 1))

let quoted_label_with_inner_parens_is_one_node () =
  let g = parse "flowchart TD\n  A[\"Tokenizer (BPE / WordPiece)\"] --> B[Done]" in
  Alcotest.(check int) "nodes" 2 (List.length (nodes g));
  Alcotest.(check int) "edges" 1 (List.length (edges g));
  Alcotest.(check string) "n0" "Tokenizer (BPE / WordPiece)" (n_label (nth (nodes g) 0))

(* --- render/layout helpers --- *)

let plain_lines src =
  match Termaid.render ~max_width:120 src with
  | Some a -> a.Termaid.plain_lines
  | None -> Alcotest.fail "expected Some art, got None"

let plain src = String.concat "\n" (plain_lines src)
let width = I.Ucore.width_string

let count_sub hay needle =
  let nl = String.length needle and hl = String.length hay in
  if nl = 0 then 0
  else begin
    let c = ref 0 and i = ref 0 in
    while !i + nl <= hl do
      if String.sub hay !i nl = needle then begin
        incr c;
        i := !i + nl
      end
      else incr i
    done;
    !c
  end

let index_of hay needle =
  match I.Ucore.find_str hay needle with Some i -> i | None -> max_int

let ends_with_char s c = String.length s > 0 && s.[String.length s - 1] = c
let ends_with_break s = List.exists (ends_with_char s) I.Const.label_break_chars
let butlast l = match List.rev l with _ :: t -> List.rev t | [] -> []
let last l = List.nth l (List.length l - 1)

(* --- more shared helpers for the diagram-type / layout tests --- *)

let some_art w src =
  match Termaid.render ~max_width:w src with
  | Some a -> a
  | None -> Alcotest.fail "expected Some art, got None"

let plain_lines_at w src = (some_art w src).Termaid.plain_lines
let plain_at w src = String.concat "\n" (plain_lines_at w src)
let str_lines s = String.split_on_char '\n' s
let nlines s = List.length (str_lines s)
let count_char s c = count_sub s c
let max_width_of lines = List.fold_left (fun a l -> max a (width l)) 0 lines

let starts_with_str s p =
  String.length s >= String.length p && String.sub s 0 (String.length p) = p

let ends_with_str s p =
  let n = String.length s and pl = String.length p in
  n >= pl && String.sub s (n - pl) pl = p

let pos_pred lines pred =
  let rec go i = function
    | [] -> Alcotest.fail "position: not found"
    | l :: t -> if pred l then i else go (i + 1) t
  in
  go 0 lines

let rpos_pred lines pred =
  let r = ref (-1) in
  List.iteri (fun i l -> if pred l then r := i) lines;
  if !r < 0 then Alcotest.fail "rposition: not found" else !r

let pos_of lines needle = pos_pred lines (fun l -> contains l needle)
let rpos_of lines needle = rpos_pred lines (fun l -> contains l needle)
let find_line lines needle = List.find (fun l -> contains l needle) lines

let gidx (g : Ir.graph) id = Hashtbl.find g.index id
let e_from (e : Ir.edge) = e.from_
let e_to (e : Ir.edge) = e.to_
let e_head_to (e : Ir.edge) = e.head_to
let e_head_from (e : Ir.edge) = e.head_from

let parse_class src =
  match I.Parse_class.parse_class src with
  | Some x -> x
  | None -> Alcotest.fail "expected class diagram"

let parse_er src =
  match I.Parse_class.parse_er src with
  | Some x -> x
  | None -> Alcotest.fail "expected ER diagram"

let parse_state src =
  match I.Parse_state.parse_state src with
  | Some x -> x
  | None -> Alcotest.fail "expected state diagram"

let pc src = fst (parse_class src)
let pe src = fst (parse_er src)

(* Rank ordering harness mirroring the upstream test helper. *)
let ordered_ranks src =
  let g = parse src in
  let e = Array.of_list (edges g) in
  let ranks = I.Rank.compute_ranks e (List.length (nodes g)) in
  let max_rank = Array.fold_left max 0 ranks in
  let rows = Array.make (max_rank + 1) [] in
  Array.iteri (fun idx r -> rows.(r) <- rows.(r) @ [ idx ]) ranks;
  let by_rank = Array.map Array.of_list rows in
  I.Rank.order_ranks by_rank e ranks;
  (g, ranks, by_rank, e)

let pos_from by_rank n =
  let pos = Array.make n 0 in
  Array.iter (fun row -> Array.iteri (fun i v -> pos.(v) <- i) row) by_rank;
  pos

(* --- ranks --- *)

let ranks_ignore_back_edges () =
  let g = parse "graph TD\n A-->B\n B-->C\n C-->A" in
  let e = Array.of_list (edges g) in
  let r = I.Rank.compute_ranks e (List.length (nodes g)) in
  let idx id = Hashtbl.find g.Ir.index id in
  Alcotest.(check int) "A" 0 r.(idx "A");
  Alcotest.(check int) "B" 1 r.(idx "B");
  Alcotest.(check int) "C" 2 r.(idx "C")

(* --- rendering invariants --- *)

let td_render_has_boxes_labels_and_arrow () =
  let out = plain "graph TD\n A[Start] --> B[End]" in
  Alcotest.(check bool) "Start" true (contains out "Start");
  Alcotest.(check bool) "End" true (contains out "End");
  Alcotest.(check bool) "corner" true (contains out "┌" || contains out "╭");
  Alcotest.(check bool) "arrow" true (contains out "▼")

let edge_label_is_rendered () =
  Alcotest.(check bool) "yes" true (contains (plain "graph TD\n A-->|yes| B") "yes")

let lr_is_shorter_than_td_for_a_chain () =
  let count src = List.length (plain_lines src) in
  let td = count "graph TD\n A --> B --> C --> D" in
  let lr = count "flowchart LR\n A --> B --> C --> D" in
  Alcotest.(check bool) (Printf.sprintf "lr(%d) < td(%d)" lr td) true (lr < td)

let unsupported_diagram_uses_fallback_box () =
  let out = plain "gantt\n title Plan\n section A\n task :a1, 2024-01-01, 30d" in
  Alcotest.(check bool) "header" true (contains out "mermaid: gantt");
  Alcotest.(check bool) "body" true (contains out "Plan")

let blank_source_returns_none () =
  Alcotest.(check bool) "none" true (Termaid.render ~max_width:80 "   \n  " = None)

let inline_label_with_x_or_o_letters () =
  let g = parse "graph TD\n A -- no exit --> B" in
  Alcotest.(check int) "nodes" 2 (List.length (nodes g));
  Alcotest.(check int) "edges" 1 (List.length (edges g));
  Alcotest.(check (option string)) "label" (Some "no exit") (e_label (nth (edges g) 0))

let wide_glyph_box_stays_aligned () =
  let ls = plain_lines "graph TD\n A[日本語ab]" in
  let ws = List.filter_map (fun l -> if String.trim l = "" then None else Some (width l)) ls in
  let uniform = match ws with [] -> true | x :: r -> List.for_all (fun w -> w = x) r in
  Alcotest.(check bool) "rows share width" true uniform;
  Alcotest.(check bool) "no CONT sentinel" true
    (not (List.exists (fun l -> String.contains l '\000') ls))

let merge_has_single_arrowhead () =
  let out = plain "graph TD\n A[aaa] --> D[ddddddd]\n B[bb] --> D\n C[ccccc] --> D" in
  Alcotest.(check int) "one arrowhead" 1 (count_sub out "▼");
  Alcotest.(check bool) "no stacking" true (not (contains out "▼▼"))

let long_label_wraps_without_truncation () =
  let out =
    plain "graph TD\n A[Check if the user has permission to access resource] --> B[Done]"
  in
  Alcotest.(check bool) "permission" true (contains out "permission");
  Alcotest.(check bool) "resource" true (contains out "resource");
  Alcotest.(check bool) "no ellipsis" true (not (contains out "…"))

let very_long_label_truncates_after_max_lines () =
  let long = String.trim (String.concat "" (List.init 40 (fun _ -> "alpha "))) in
  let out = plain (Printf.sprintf "graph TD\n A[%s] --> B[x]" long) in
  Alcotest.(check bool) "ellipsis" true (contains out "…")

(* --- wrap_label unit tests --- *)

let wrap src = I.Wrap.wrap_label src I.Const.wrap_width I.Const.max_lines

let wrap_label_breaks_long_identifier_on_boundary () =
  let ls = wrap "mark_filter_restore_context" in
  Alcotest.(check bool) "first ends on _" true (ends_with_char (List.nth ls 0) '_');
  List.iter
    (fun l -> Alcotest.(check bool) "break on boundary" true (ends_with_break l))
    (butlast ls);
  Alcotest.(check string) "lossless" "mark_filter_restore_context" (String.concat "" ls)

let wrap_label_token_without_break_char_falls_back_per_char () =
  let token = String.make 40 'a' in
  let ls = wrap token in
  Alcotest.(check bool) "hard-break" true (List.length ls >= 2);
  Alcotest.(check string) "lossless" token (String.concat "" ls)

let flowchart_long_identifier_breaks_on_boundary_not_mid_segment () =
  let out = plain "graph TD\n A[mark_filter_restore_context] --> B[Done]" in
  Alcotest.(check bool) "prefix" true (contains out "mark_filter_restore_");
  Alcotest.(check bool) "tail" true (contains out "context")

let wrap_label_mixed_boundary_then_no_boundary_tail () =
  let token = "ab_" ^ String.make 40 'c' in
  let ls = wrap token in
  Alcotest.(check bool) "first on boundary" true (ends_with_char (List.nth ls 0) '_');
  Alcotest.(check bool) "later per-char break" true
    (List.exists (fun l -> not (ends_with_break l)) (List.tl ls));
  Alcotest.(check string) "lossless" token (String.concat "" ls)

let wrap_label_boundary_breaking_still_truncates_at_max_lines () =
  let id = String.concat "_" (List.init 20 (fun _ -> "segment")) in
  let ls = wrap id in
  Alcotest.(check int) "max lines" I.Const.max_lines (List.length ls);
  Alcotest.(check bool) "keeps ellipsis" true (contains (last ls) "…")

(* --- orientation --- *)

let bt_flips_orientation () =
  let ls = plain_lines "flowchart BT\n A[first] --> B[second] --> C[third]" in
  let row needle =
    let rec go i = function
      | [] -> max_int
      | l :: t -> if contains l needle then i else go (i + 1) t
    in
    go 0 ls
  in
  Alcotest.(check bool) "third above first" true (row "third" < row "first")

let rl_flips_orientation () =
  let line = List.find (fun l -> contains l "first") (plain_lines "flowchart RL\n A[first] --> B[second] --> C[third]") in
  Alcotest.(check bool) "third left of first" true (index_of line "third" < index_of line "first")

let undirected_piped_label_has_no_arrowhead () =
  let out = plain "graph TD\n A ---|maybe| B" in
  Alcotest.(check bool) "label" true (contains out "maybe");
  Alcotest.(check bool) "no arrow" true (not (contains out "▼"))

let chain_edges_are_straight () =
  List.iter
    (fun line ->
      Alcotest.(check bool) "no jog" true (not (contains line "└" && contains line "┐")))
    (plain_lines "graph TD\n A[aaaa] --> B[b] --> C[cccccccc]")

let adversarial_chain_falls_back () =
  let buf = Buffer.create (1 lsl 18) in
  Buffer.add_string buf "graph TD\n";
  for i = 0 to 9999 do
    Buffer.add_string buf (Printf.sprintf " N%d --> N%d\n" i (i + 1))
  done;
  Alcotest.(check bool) "fallback" true (contains (plain (Buffer.contents buf)) "mermaid: graph")

(* --- fallback / oversize --- *)

let single_statement_chain_over_cap_falls_back () =
  let buf = Buffer.create (1 lsl 18) in
  Buffer.add_string buf "graph LR\n ";
  for i = 0 to 9999 do Buffer.add_string buf (Printf.sprintf "N%d-->" i) done;
  Buffer.add_string buf "N10000";
  Alcotest.(check bool) "fallback" true (contains (plain (Buffer.contents buf)) "mermaid: graph")

let deep_chain_within_caps_renders () =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "graph TD\n";
  for i = 0 to 99 do Buffer.add_string buf (Printf.sprintf " N%d --> N%d\n" i (i + 1)) done;
  let joined = plain_at 200 (Buffer.contents buf) in
  Alcotest.(check bool) "N0" true (contains joined "N0");
  Alcotest.(check bool) "N100" true (contains joined "N100");
  Alcotest.(check bool) "arrow" true (contains joined "▼")

let fallback_styled_and_plain_widths_match () =
  let art = some_art 120 "gantt\n title Plan\n a\n" in
  Alcotest.(check int) "lens match" (List.length art.Termaid.plain_lines)
    (List.length art.Termaid.styled_lines);
  let frame_w = width (List.hd art.Termaid.plain_lines) in
  List.iter2
    (fun styled plain ->
      let styled_w =
        List.fold_left (fun a (sp : Termaid.span) -> a + width sp.Termaid.text) 0 styled
      in
      Alcotest.(check int) "styled=plain width" (width plain) styled_w;
      Alcotest.(check int) "rectangular" frame_w (width plain))
    art.Termaid.styled_lines art.Termaid.plain_lines

let over_wide_src =
  "flowchart LR\n A[aaaaaaaaaaaaaaaaaaaa] --> B[bbbbbbbbbbbbbbbbbbbb] --> C[cccccccccccccccccccc]"

let over_wide_diagram_falls_back () =
  let out = plain_lines_at 40 over_wide_src in
  Alcotest.(check bool) "fallback" true (contains (String.concat "\n" out) "mermaid: flowchart");
  let fits = plain_lines_at 120 over_wide_src in
  Alcotest.(check bool) "fits has arrow" true (List.exists (fun l -> contains l "▶") fits);
  Alcotest.(check bool) "width bounded" true (max_width_of out <= String.length over_wide_src)

let too_wide_fallback_appends_hint_below_box () =
  let out = plain_lines_at 40 over_wide_src in
  let joined = String.concat "\n" out in
  Alcotest.(check bool) "header" true (contains joined "mermaid: flowchart");
  Alcotest.(check bool) "no (too wide) in header" true (not (contains joined "(too wide)"));
  Alcotest.(check bool) "raw source" true (contains joined "flowchart LR");
  let bottom = pos_of out "╰" and note = pos_of out "too wide" in
  Alcotest.(check bool) "note below box" true (note > bottom);
  Alcotest.(check bool) "points at image" true (contains joined "open the image");
  Alcotest.(check bool) "fits 40" true (List.for_all (fun l -> width l <= 40) out)

let unsupported_diagram_fallback_not_flagged_too_wide () =
  let out = plain "gantt\n title Plan\n section A\n task :a1, 2024-01-01, 30d" in
  Alcotest.(check bool) "header" true (contains out "mermaid: gantt");
  Alcotest.(check bool) "not too wide" true (not (contains out "too wide"))

let fitting_diagram_has_no_width_warning () =
  let out = plain "flowchart LR\n A[Start] --> B[End]" in
  Alcotest.(check bool) "no warning" true (not (contains out "too wide"));
  Alcotest.(check bool) "not a box" true (not (contains out "mermaid: flowchart"));
  Alcotest.(check bool) "edges" true (contains out "▶")

let fallback_wraps_long_lines_to_max_width () =
  let out =
    plain_lines_at 40
      "gantt\n title a very long line that should wrap inside the fallback box nicely"
  in
  Alcotest.(check bool) "fits 40" true (List.for_all (fun l -> width l <= 40) out);
  List.iter
    (fun l ->
      Alcotest.(check bool) "body borders" true (starts_with_str l "│" && ends_with_str l "│"))
    (butlast (List.tl out));
  Alcotest.(check bool) "nicely" true (contains (String.concat "\n" out) "nicely")

(* --- crossing reduction / bus + lane packing --- *)

let order_ranks_removes_avoidable_crossing () =
  let g, ranks, by_rank, e = ordered_ranks "graph TD\n C[ccc]\n D[ddd]\n A --> D\n B --> C" in
  let pos = pos_from by_rank (List.length (nodes g)) in
  Alcotest.(check int) "no crossings" 0 (I.Rank.count_crossings e ranks pos);
  Alcotest.(check bool) "D before C" true (pos.(gidx g "D") < pos.(gidx g "C"))

let order_ranks_keeps_crossing_free_order () =
  let g, ranks, by_rank, e = ordered_ranks "graph TD\n A --> C\n B --> D" in
  Alcotest.(check (list int)) "row0" [ gidx g "A"; gidx g "B" ] (Array.to_list by_rank.(0));
  Alcotest.(check (list int)) "row1" [ gidx g "C"; gidx g "D" ] (Array.to_list by_rank.(1));
  let pos = pos_from by_rank (List.length (nodes g)) in
  Alcotest.(check int) "no crossings" 0 (I.Rank.count_crossings e ranks pos)

let crossing_edges_render_untangled () =
  let out = plain "graph TD\n C[ccc]\n D[ddd]\n A --> D\n B --> C" in
  let row = find_line (str_lines out) "ccc" in
  Alcotest.(check bool) "ddd left of ccc" true (index_of row "ddd" < index_of row "ccc");
  Alcotest.(check bool) "no plus" true (not (contains out "┼"))

let three_layer_weave_untangles () =
  let g, ranks, by_rank, e =
    ordered_ranks "graph TD\n X[x]\n Y[y]\n A --> Y\n B --> X\n X --> Q\n Y --> P\n P[p]\n Q[q]"
  in
  let pos = pos_from by_rank (List.length (nodes g)) in
  Alcotest.(check int) "no crossings" 0 (I.Rank.count_crossings e ranks pos)

let unavoidable_crossing_gets_separate_bus_rows () =
  let crossing = plain "graph TD\n A --> D[ddd]\n A --> C[ccc]\n B --> C\n B --> D" in
  let parallel = plain "graph TD\n A --> C[ccc]\n B --> D[ddd]" in
  Alcotest.(check bool) "crossing renders" true (contains crossing "┼");
  Alcotest.(check int) "one extra row" (nlines parallel + 1) (nlines crossing);
  Alcotest.(check int) "two arrows" 2 (count_char crossing "▼")

let fan_out_keeps_single_bus_row () =
  let out = plain "graph TD\n A --> C[ccc]\n A --> D[ddd]" in
  let baseline = plain "graph TD\n A --> C[ccc]" in
  Alcotest.(check int) "same rows" (nlines baseline) (nlines out);
  Alcotest.(check bool) "no plus" true (not (contains out "┼"))

let shared_target_back_edges_share_one_lane () =
  let two = plain "graph TD\n A --> B\n B --> C\n B --> A\n C --> A" in
  let one = plain "graph TD\n A --> B\n B --> C\n C --> A" in
  Alcotest.(check int) "same max width" (max_width_of (str_lines one)) (max_width_of (str_lines two));
  Alcotest.(check int) "one back arrow" 1 (count_char two "◄")

let distinct_back_edges_get_separate_lanes () =
  let split = plain "graph TD\n A --> B\n B --> C\n B --> A\n C --> B" in
  let single = plain "graph TD\n A --> B\n B --> C\n C --> B" in
  Alcotest.(check int) "two back arrows" 2 (count_char split "◄");
  Alcotest.(check bool) "wider" true
    (max_width_of (str_lines split) > max_width_of (str_lines single))

(* --- edges: heads, endings, styles, fan-out --- *)

let bidirectional_link_draws_both_arrowheads () =
  let lr = plain "flowchart LR\n A <--> B" in
  Alcotest.(check bool) "lr heads" true (contains lr "◄" && contains lr "▶");
  let td = plain "graph TD\n A <--> B" in
  Alcotest.(check bool) "td heads" true (contains td "▲" && contains td "▼")

let reversed_arrow_swaps_edge_direction () =
  let g = parse "graph TD\n A <-- B" in
  Alcotest.(check int) "edges" 1 (List.length (edges g));
  let e0 = nth (edges g) 0 in
  Alcotest.(check int) "from B" (gidx g "B") (e_from e0);
  Alcotest.(check int) "to A" (gidx g "A") (e_to e0);
  Alcotest.(check bool) "head_to Arrow" true (e_head_to e0 = Ir.Arrow);
  Alcotest.(check bool) "head_from None" true (e_head_from e0 = Ir.No_head);
  let ls = str_lines (plain "graph TD\n A <-- B") in
  Alcotest.(check bool) "B above A" true (pos_of ls "B" < pos_of ls "A")

let reversed_arrow_with_end_marker_swaps_direction () =
  let g = parse "graph TD\n A <--o B\n C <--x D" in
  let e0 = nth (edges g) 0 and e1 = nth (edges g) 1 in
  Alcotest.(check int) "e0 from B" (gidx g "B") (e_from e0);
  Alcotest.(check int) "e0 to A" (gidx g "A") (e_to e0);
  Alcotest.(check bool) "e0 head_to Arrow" true (e_head_to e0 = Ir.Arrow);
  Alcotest.(check bool) "e0 head_from Circle" true (e_head_from e0 = Ir.Circle);
  Alcotest.(check int) "e1 from D" (gidx g "D") (e_from e1);
  Alcotest.(check bool) "e1 head_from Cross" true (e_head_from e1 = Ir.Cross);
  let ls = str_lines (plain "graph TD\n A <--o B") in
  Alcotest.(check bool) "B above A" true (pos_of ls "B" < pos_of ls "A")

let both_end_markers_parse () =
  let g = parse "graph TD\n A o--o B\n C x--x D" in
  let e0 = nth (edges g) 0 and e1 = nth (edges g) 1 in
  Alcotest.(check bool) "e0 circles" true (e_head_from e0 = Ir.Circle && e_head_to e0 = Ir.Circle);
  Alcotest.(check bool) "e1 crosses" true (e_head_from e1 = Ir.Cross && e_head_to e1 = Ir.Cross);
  Alcotest.(check int) "nodes" 4 (List.length (nodes g))

let circle_and_cross_endings_create_no_phantom_nodes () =
  let g = parse "graph TD\n A --o B\n C --x D" in
  Alcotest.(check int) "nodes" 4 (List.length (nodes g));
  Alcotest.(check bool) "no o node" true (not (Hashtbl.mem g.Ir.index "o"));
  Alcotest.(check bool) "no x node" true (not (Hashtbl.mem g.Ir.index "x"));
  Alcotest.(check bool) "e0 Circle" true (e_head_to (nth (edges g) 0) = Ir.Circle);
  Alcotest.(check bool) "e1 Cross" true (e_head_to (nth (edges g) 1) = Ir.Cross);
  Alcotest.(check bool) "circle glyph" true (contains (plain "graph TD\n A --o B") "o")

let left_endings_decorate_without_reversing () =
  let g = parse "graph TD\n A o-- B\n C x-- D" in
  let e0 = nth (edges g) 0 in
  Alcotest.(check int) "e0 from A" (gidx g "A") (e_from e0);
  Alcotest.(check int) "e0 to B" (gidx g "B") (e_to e0);
  Alcotest.(check bool) "e0 head_from Circle" true (e_head_from e0 = Ir.Circle);
  Alcotest.(check bool) "e1 head_from Cross" true (e_head_from (nth (edges g) 1) = Ir.Cross);
  Alcotest.(check bool) "e0 head_to None" true (e_head_to e0 = Ir.No_head)

let fan_out_creates_cross_product_edges () =
  let g = parse "graph TD\n A & B --> C & D" in
  Alcotest.(check int) "nodes" 4 (List.length (nodes g));
  Alcotest.(check int) "edges" 4 (List.length (edges g));
  let has f t = List.exists (fun e -> e_from e = gidx g f && e_to e = gidx g t) (edges g) in
  Alcotest.(check bool) "cross product" true
    (has "A" "C" && has "A" "D" && has "B" "C" && has "B" "D");
  Alcotest.(check int) "two arrows" 2 (count_char (plain "graph TD\n A & B --> C & D") "▼")

let fan_out_in_chain () =
  Alcotest.(check int) "edges" 3 (List.length (edges (parse "graph LR\n A & B --> C --> D")))

let fan_out_with_reversed_arrow () =
  let g = parse "graph TD\n A & B <-- C" in
  Alcotest.(check int) "edges" 2 (List.length (edges g));
  Alcotest.(check bool) "all from C" true (List.for_all (fun e -> e_from e = gidx g "C") (edges g));
  Alcotest.(check bool) "all Arrow" true (List.for_all (fun e -> e_head_to e = Ir.Arrow) (edges g))

let dotted_and_thick_lines_render_distinctly () =
  Alcotest.(check bool) "dotted" true (contains (plain "graph TD\n A -.-> B") "╎");
  Alcotest.(check bool) "thick" true (contains (plain "graph TD\n A ==> B") "┃");
  let solid = plain "graph TD\n A --> B" in
  Alcotest.(check bool) "solid" true (not (contains solid "╎") && not (contains solid "┃"))

let dotted_label_form_renders_dashed () =
  let out = plain "graph LR\n A -. maybe .-> B" in
  Alcotest.(check bool) "dashed" true (contains out "╌");
  Alcotest.(check bool) "label" true (contains out "maybe")

let thick_jog_uses_thick_corners () =
  let out = plain "graph TD\n A[aaaaaaa] ==> B\n A ==> C[ccccccc]" in
  Alcotest.(check bool) "thick corner" true
    (contains out "┏" || contains out "┓" || contains out "┳")

let mixed_solid_and_dotted_bus_stays_light () =
  let out = plain "graph TD\n A --> C\n B -.-> C" in
  Alcotest.(check bool) "dotted" true (contains out "╌");
  Alcotest.(check bool) "solid" true (contains out "─");
  Alcotest.(check bool) "light merge" true (contains out "┬")

let box_borders_stay_light_next_to_styled_edges () =
  let out = plain "graph TD\n A ==> B" in
  Alcotest.(check bool) "light corners" true (contains out "┌" && contains out "└");
  Alcotest.(check bool) "not restyled" true (not (contains out "┏"))

let skip_edge_routes_around_intermediate_boxes () =
  let out = plain "graph TD\n A --> B\n B --> C\n A --> C" in
  Alcotest.(check bool) "no plus" true (not (contains out "┼"));
  Alcotest.(check bool) "lane arrow" true (contains out "◄")

let inline_o_word_label_still_parses_as_label () =
  let g = parse "graph TD\n A -- or else --> B" in
  Alcotest.(check int) "nodes" 2 (List.length (nodes g));
  Alcotest.(check (option string)) "label" (Some "or else") (e_label (nth (edges g) 0))

(* --- comments --- *)

let semicolon_and_comment_survive_inside_quoted_label () =
  let g = parse "graph TD\n A[\"wait; 50%% done\"] --> B" in
  Alcotest.(check int) "nodes" 2 (List.length (nodes g));
  Alcotest.(check string) "label" "wait; 50%% done" (n_label (nth (nodes g) 0))

let comment_outside_quotes_is_stripped () =
  let g = parse "graph TD %% main flow\n A --> B %% trailing\n %% full line\n" in
  Alcotest.(check int) "nodes" 2 (List.length (nodes g));
  Alcotest.(check int) "edges" 1 (List.length (edges g))

(* --- self loops --- *)

let self_loop_renders_below_box () =
  let out = plain "graph TD\n A --> A" in
  Alcotest.(check bool) "corners" true (contains out "╰" && contains out "╯");
  Alcotest.(check bool) "returns" true (contains out "▲")

let self_loop_label_renders () =
  Alcotest.(check bool) "again" true (contains (plain "graph TD\n A -->|again| A") "again")

let self_loop_coexists_with_forward_edge () =
  let out = plain "graph TD\n A --> A\n A --> B" in
  Alcotest.(check bool) "up" true (contains out "▲");
  Alcotest.(check bool) "down" true (contains out "▼");
  Alcotest.(check bool) "B" true (contains out "B");
  Alcotest.(check bool) "no plus" true (not (contains out "┼"))

let self_loop_flips_with_bt () =
  let out = plain "flowchart BT\n A --> A\n A --> B" in
  Alcotest.(check bool) "down head" true (contains out "▼");
  Alcotest.(check bool) "round" true (contains out "╭" || contains out "╮")

let self_loop_in_lr () =
  let out = plain "flowchart LR\n A --> A\n A --> B" in
  Alcotest.(check bool) "up" true (contains out "▲");
  Alcotest.(check bool) "right" true (contains out "▶")

(* --- subgraphs --- *)

let subgraph_renders_titled_frame () =
  let out =
    plain "graph TD\n S[Start] --> one\n subgraph one [Group One]\n A --> B\n end\n one --> E[End]"
  in
  Alcotest.(check bool) "title" true (contains out " Group One ");
  let ls = str_lines out in
  let title = pos_of ls "Group One" and a = pos_of ls "│ A │" and b = pos_of ls "│ B │" in
  let close = rpos_pred ls (fun l -> starts_with_str (I.Ucore.trim_start l) "└") in
  Alcotest.(check bool) "nesting order" true (title < a && a < b && b <= close);
  Alcotest.(check bool) "start/end" true (contains out "Start" && contains out "End");
  Alcotest.(check int) "three arrows" 3 (count_char out "▼")

let subgraph_edge_between_groups () =
  let out =
    plain "graph TD\n subgraph api [API]\n A1 --> A2\n end\n subgraph db [Storage]\n B1\n end\n api --> db"
  in
  Alcotest.(check bool) "api" true (contains out " API ");
  Alcotest.(check bool) "storage" true (contains out " Storage ");
  let ls = str_lines out in
  Alcotest.(check bool) "api above db" true (pos_of ls "API" < pos_of ls "Storage")

let subgraph_nested_frames () =
  let out =
    plain "graph TD\n subgraph outer [Outer]\n subgraph inner [Inner]\n X --> Y\n end\n W --> X\n end\n S --> outer"
  in
  Alcotest.(check bool) "outer" true (contains out " Outer ");
  Alcotest.(check bool) "inner" true (contains out " Inner ");
  let ls = str_lines out in
  Alcotest.(check bool) "outer above inner" true (pos_of ls "Outer" < pos_of ls "Inner")

let subgraph_cross_member_edge_attaches_to_frame () =
  let out = plain "graph LR\n S --> A\n subgraph g [Workers]\n A --> B\n end\n B --> T" in
  Alcotest.(check bool) "workers" true (contains out " Workers ");
  Alcotest.(check bool) "S and T" true (contains out "S" && contains out "T");
  Alcotest.(check int) "three arrows" 3 (count_char out "▶");
  let row = find_line (str_lines out) "│ A ├" in
  Alcotest.(check bool) "A outside group" true (index_of row "S" < index_of row "A")

let subgraph_id_referenced_before_declaration () =
  let g = parse "graph TD\n X --> two\n subgraph two\n C --> D\n end" in
  Alcotest.(check int) "groups" 1 (Vec.length g.Ir.groups);
  let out = plain "graph TD\n X --> two\n subgraph two\n C --> D\n end" in
  Alcotest.(check bool) "titled by id" true (contains out " two ");
  Alcotest.(check bool) "C box" true (contains out "│ C │")

let subgraph_quoted_and_plain_titles () =
  Alcotest.(check bool) "quoted" true
    (contains (plain "graph TD\n subgraph \"My Stuff\"\n A\n end\n S --> A") " My Stuff ");
  Alcotest.(check bool) "plain" true
    (contains (plain "graph TD\n subgraph batch jobs\n B\n end\n S --> B") " batch jobs ");
  let out3 = plain "graph TD\n subgraph \"a &lt;b&gt;\"\n C\n end\n S --> C" in
  Alcotest.(check bool) "entities" true (contains out3 "a <b>" && not (contains out3 "&lt;"))

let subgraph_empty_is_dropped () =
  let out = plain "graph TD\n subgraph ghost\n end\n A --> B" in
  Alcotest.(check bool) "no ghost" true (not (contains out "ghost"));
  Alcotest.(check bool) "arrow" true (contains out "▼")

let subgraph_bt_flips_frame_and_contents () =
  let out = plain "flowchart BT\n S --> one\n subgraph one [Up]\n A --> B\n end" in
  Alcotest.(check bool) "up" true (contains out " Up ");
  let ls = str_lines out in
  Alcotest.(check bool) "B above A" true (pos_of ls "│ B │" < pos_of ls "│ A │");
  Alcotest.(check bool) "frame above source" true (pos_of ls " Up " < pos_of ls "S");
  Alcotest.(check bool) "up head" true (contains out "▲")

let subgraph_depth_over_cap_falls_back () =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "graph TD\n";
  for i = 0 to 7 do Buffer.add_string buf (Printf.sprintf " subgraph g%d\n" i) done;
  Buffer.add_string buf " A --> B\n";
  for _ = 0 to 7 do Buffer.add_string buf " end\n" done;
  Alcotest.(check bool) "fallback" true (contains (plain (Buffer.contents buf)) "mermaid: graph")

let subgraph_groupless_path_unchanged () =
  Alcotest.(check int) "no groups" 0 (Vec.length (parse "graph TD\n A --> B").Ir.groups)

(* --- class diagrams --- *)

let class_renders_compartments () =
  let out =
    plain "classDiagram\n class Animal {\n +int age\n +isMammal() bool\n }\n Animal <|-- Duck"
  in
  Alcotest.(check bool) "name" true (contains out "Animal");
  Alcotest.(check bool) "attr" true (contains out "+int age");
  Alcotest.(check bool) "method" true (contains out "+isMammal() bool");
  Alcotest.(check bool) "rules" true (contains out "├" && contains out "┤");
  let ls = str_lines out in
  Alcotest.(check bool) "order" true
    (pos_of ls "Animal" < pos_of ls "+int age" && pos_of ls "+int age" < pos_of ls "+isMammal() bool")

let class_inheritance_triangle_at_parent () =
  let out = plain "classDiagram\n Animal <|-- Duck\n Animal <|-- Fish" in
  Alcotest.(check bool) "triangle" true (contains out "△");
  let ls = str_lines out in
  let animal = pos_of ls "Animal" and duck = pos_of ls "Duck" and tri = pos_of ls "△" in
  Alcotest.(check bool) "parent above child" true (animal < duck);
  Alcotest.(check bool) "triangle at parent" true (tri >= animal && tri < duck)

let class_realization_is_dotted_triangle () =
  let g = pc "classDiagram\n IShape <|.. Circle" in
  let e0 = nth (edges g) 0 in
  Alcotest.(check bool) "head_from Triangle" true (e_head_from e0 = Ir.Triangle);
  Alcotest.(check bool) "dotted" true (e0.Ir.line = Ir.Dotted);
  let out = plain "classDiagram\n IShape <|.. Circle" in
  Alcotest.(check bool) "dashed glyph" true (contains out "╎" || contains out "╌")

let class_composition_and_aggregation_diamonds () =
  let out = plain "classDiagram\n Car *-- Engine\n Pond o-- Duck" in
  Alcotest.(check bool) "filled" true (contains out "◆");
  Alcotest.(check bool) "open" true (contains out "◇")

let class_dependency_dotted_arrow () =
  let g = pc "classDiagram\n A ..> B" in
  let e0 = nth (edges g) 0 in
  Alcotest.(check bool) "head_to Arrow" true (e_head_to e0 = Ir.Arrow);
  Alcotest.(check bool) "dotted" true (e0.Ir.line = Ir.Dotted)

let class_colon_members_merge_with_block () =
  let out =
    plain "classDiagram\n class Duck {\n +swim()\n }\n Duck : +String beakColor\n S --> Duck"
  in
  Alcotest.(check bool) "swim" true (contains out "+swim()");
  Alcotest.(check bool) "beakColor" true (contains out "+String beakColor")

let class_annotation_renders_guillemets () =
  Alcotest.(check bool) "guillemets" true
    (contains (plain "classDiagram\n <<interface>> Shape\n Shape <|.. Circle") "«interface»")

let class_generics_display_as_angle_brackets () =
  let out = plain "classDiagram\n Shape~T~ : +area() T\n S --> Shape~T~" in
  Alcotest.(check bool) "angle brackets" true (contains out "Shape<T>");
  Alcotest.(check bool) "no tilde" true (not (contains out "~"))

let class_cardinalities_fold_into_label () =
  Alcotest.(check bool) "folded" true
    (contains (plain "classDiagram\n Student \"many\" --> \"1\" School : attends") "many attends 1")

let class_from_end_head_survives_fan_out_jog () =
  let out = plain "classDiagram\n Animal <|-- Duck\n Animal <|-- Fish\n Animal <|-- Cow" in
  Alcotest.(check int) "single from-end glyph" 1 (count_char out "△" + count_char out "▽")

let class_empty_class_is_plain_titled_box () =
  Alcotest.(check bool) "loner" true (contains (plain "classDiagram\n class Loner\n A --> Loner") "Loner")

let class_unknown_statement_falls_back () =
  Alcotest.(check bool) "fallback" true
    (contains (plain "classDiagram\n A --> B\n total garbage here") "mermaid: classDiagram")

let class_member_cap_ellipsis () =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "classDiagram\n class Big {\n";
  for i = 0 to 11 do Buffer.add_string buf (Printf.sprintf " +field%d\n" i) done;
  Buffer.add_string buf " }\n A --> Big";
  let out = plain (Buffer.contents buf) in
  Alcotest.(check bool) "field7" true (contains out "+field7");
  Alcotest.(check bool) "no field9" true (not (contains out "+field9"));
  Alcotest.(check bool) "ellipsis" true (contains out "…")

let class_direction_lr () =
  let out = plain "classDiagram\n direction LR\n A --> B" in
  Alcotest.(check bool) "A row has B" true (contains (find_line (str_lines out) "A") "B")

(* --- entity relationship --- *)

let er_renders_entities_and_relationship_labels () =
  let out =
    plain
      "erDiagram\n CUSTOMER ||--o{ ORDER : places\n CUSTOMER {\n string name PK \"full name\"\n int custNumber\n }"
  in
  Alcotest.(check bool) "customer" true (contains out "CUSTOMER");
  Alcotest.(check bool) "order" true (contains out "ORDER");
  Alcotest.(check bool) "attr" true (contains out "string name PK");
  Alcotest.(check bool) "comment dropped" true (not (contains out "full name"));
  Alcotest.(check bool) "cardinalities" true (contains out "1 places 0..*");
  Alcotest.(check bool) "rule" true (contains out "├")

let er_cardinality_map () =
  let cases =
    [ ("||--||", "1", "1"); ("|o--o|", "0..1", "0..1"); ("}o--o{", "0..*", "0..*")
    ; ("}|--|{", "1..*", "1..*"); ("||--o{", "1", "0..*") ]
  in
  List.iter
    (fun (op, l, r) ->
      match I.Parse_class.parse_er_op op with
      | Some (cl, cr, line) ->
        Alcotest.(check string) ("l " ^ op) l cl;
        Alcotest.(check string) ("r " ^ op) r cr;
        Alcotest.(check bool) ("solid " ^ op) true (line = Ir.Solid)
      | None -> Alcotest.fail op)
    cases;
  Alcotest.(check bool) "dotted" true
    (match I.Parse_class.parse_er_op "||..o{" with Some (_, _, l) -> l = Ir.Dotted | None -> false);
  Alcotest.(check bool) "thick none" true (I.Parse_class.parse_er_op "||==o{" = None);
  Alcotest.(check bool) "garbage none" true (I.Parse_class.parse_er_op "garbage" = None)

let er_non_identifying_renders_dotted () =
  let out = plain "erDiagram\n A ||..o{ B : uses" in
  Alcotest.(check bool) "dashed" true (contains out "╎" || contains out "╌")

let er_relationships_have_no_arrowheads () =
  let out = plain "erDiagram\n A ||--o{ B : has" in
  List.iter
    (fun h -> Alcotest.(check bool) ("no " ^ h) true (not (contains out h)))
    [ "▼"; "▲"; "◄"; "▶"; "△"; "◆"; "◇" ]

let er_entity_alias_label () =
  let out = plain "erDiagram\n p[Person] ||--o{ a[\"Bank Account\"] : owns" in
  Alcotest.(check bool) "person" true (contains out "Person");
  Alcotest.(check bool) "bank account" true (contains out "Bank Account")

let er_unquoted_label_and_bare_entity_decl () =
  let g = pe "erDiagram\n LONER\n A ||--|| B : linked" in
  Alcotest.(check int) "nodes" 3 (List.length (nodes g));
  let out = plain "erDiagram\n LONER\n A ||--|| B : linked" in
  Alcotest.(check bool) "loner" true (contains out "LONER");
  Alcotest.(check bool) "label" true (contains out "1 linked 1")

let er_attribute_cap_ellipsis () =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "erDiagram\n BIG {\n";
  for i = 0 to 11 do Buffer.add_string buf (Printf.sprintf " int f%d\n" i) done;
  Buffer.add_string buf " }\n BIG ||--|| OTHER : x";
  let out = plain (Buffer.contents buf) in
  Alcotest.(check bool) "f7" true (contains out "int f7");
  Alcotest.(check bool) "no f9" true (not (contains out "int f9"));
  Alcotest.(check bool) "ellipsis" true (contains out "…")

let er_unknown_statement_falls_back () =
  Alcotest.(check bool) "fallback" true
    (contains (plain "erDiagram\n A ||--|| B : ok\n utter nonsense statement") "mermaid: erDiagram")

(* --- state diagrams --- *)

let state_diagram_renders_states_and_transitions () =
  let out = plain "stateDiagram-v2\n [*] --> Idle\n Idle --> Running: start\n Running --> [*]" in
  Alcotest.(check bool) "idle" true (contains out "Idle");
  Alcotest.(check bool) "running" true (contains out "Running");
  Alcotest.(check bool) "start" true (contains out "start");
  Alcotest.(check bool) "arrow" true (contains out "▼");
  Alcotest.(check int) "two markers" 2 (count_char out "●");
  let ls = str_lines out in
  Alcotest.(check bool) "start above idle above end" true
    (pos_of ls "●" < pos_of ls "Idle" && pos_of ls "Idle" < rpos_of ls "●")

let state_v1_header_renders () =
  Alcotest.(check bool) "arrow" true (contains (plain "stateDiagram\n A --> B") "▼")

let state_boxes_are_rounded () =
  let out = plain "stateDiagram-v2\n A --> B" in
  Alcotest.(check bool) "round" true (contains out "╭");
  Alcotest.(check bool) "not square" true (not (contains out "┌"))

let state_alias_label_renders () =
  Alcotest.(check bool) "alias" true
    (contains (plain "stateDiagram-v2\n state \"Waiting for input\" as W\n W --> Done") "Waiting for input")

let state_choice_parses_as_diamond () =
  let g = parse_state "stateDiagram-v2\n state c <<choice>>\n A --> c\n c --> B: yes\n c --> D: no" in
  Alcotest.(check bool) "diamond" true (n_shape (Vec.get g.Ir.nodes (gidx g "c")) = Ir.Diamond);
  Alcotest.(check int) "edges" 3 (List.length (edges g))

let state_description_sets_label () =
  Alcotest.(check bool) "desc" true
    (contains (plain "stateDiagram-v2\n s2 : waits patiently\n A --> s2") "waits patiently")

let state_direction_lr () =
  let out = plain "stateDiagram-v2\n direction LR\n A --> B --> C" in
  let td = plain "stateDiagram-v2\n A --> B" in
  Alcotest.(check bool) "flat" true (nlines out <= nlines td + 2);
  Alcotest.(check bool) "A row has B" true (contains (find_line (str_lines out) "A") "B")

let state_composite_contents_render_flat () =
  let out = plain "stateDiagram-v2\n state Active {\n A --> B\n }\n Active --> Done" in
  Alcotest.(check bool) "active" true (contains out "Active");
  Alcotest.(check bool) "a and b" true (contains out "A" && contains out "B");
  Alcotest.(check bool) "done" true (contains out "Done")

let state_notes_are_skipped () =
  let out =
    plain "stateDiagram-v2\n A --> B\n note right of A: inline note\n note left of B\n block text\n end note"
  in
  Alcotest.(check bool) "arrow" true (contains out "▼");
  Alcotest.(check bool) "no note" true (not (contains out "note"));
  Alcotest.(check bool) "no block" true (not (contains out "block text"))

let state_back_transition_uses_lane () =
  let out = plain "stateDiagram-v2\n A --> B\n B --> C\n C --> B: retry" in
  Alcotest.(check bool) "lane arrow" true (contains out "◄");
  Alcotest.(check bool) "retry" true (contains out "retry")

let state_unknown_statement_falls_back () =
  Alcotest.(check bool) "fallback" true
    (contains (plain "stateDiagram-v2\n A --> B\n some garbage line") "mermaid: stateDiagram-v2")

let state_over_cap_falls_back () =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "stateDiagram-v2\n";
  for i = 0 to 599 do Buffer.add_string buf (Printf.sprintf " S%d --> S%d\n" i (i + 1)) done;
  Alcotest.(check bool) "fallback" true
    (contains (plain (Buffer.contents buf)) "mermaid: stateDiagram-v2")

let state_extra_dash_arrow_tolerated () =
  let g = parse_state "stateDiagram-v2\n A ---> B" in
  Alcotest.(check int) "edges" 1 (List.length (edges g));
  Alcotest.(check int) "nodes" 2 (List.length (nodes g))

let state_description_preserves_choice_shape () =
  let g = parse_state "stateDiagram-v2\n state c <<choice>>\n c : pick a path\n A --> c\n c --> B" in
  Alcotest.(check bool) "diamond" true (n_shape (Vec.get g.Ir.nodes (gidx g "c")) = Ir.Diamond);
  Alcotest.(check string) "label" "pick a path" (n_label (Vec.get g.Ir.nodes (gidx g "c")));
  let g2 = parse_state "stateDiagram-v2\n state c <<choice>>\n state \"pick\" as c\n A --> c" in
  Alcotest.(check bool) "diamond2" true (n_shape (Vec.get g2.Ir.nodes (gidx g2 "c")) = Ir.Diamond);
  Alcotest.(check string) "label2" "pick" (n_label (Vec.get g2.Ir.nodes (gidx g2 "c")))

let state_chained_transitions_parse_as_separate_edges () =
  let g = parse_state "stateDiagram-v2\n A --> B --> C" in
  Alcotest.(check int) "nodes" 3 (List.length (nodes g));
  Alcotest.(check int) "edges" 2 (List.length (edges g));
  Alcotest.(check bool) "has B" true (Hashtbl.mem g.Ir.index "B");
  Alcotest.(check bool) "has C" true (Hashtbl.mem g.Ir.index "C");
  Alcotest.(check bool) "no arrow in label" true
    (not (List.exists (fun n -> contains (n_label n) "-->") (nodes g)));
  let has f t = List.exists (fun e -> e_from e = gidx g f && e_to e = gidx g t) (edges g) in
  Alcotest.(check bool) "edges present" true (has "A" "B" && has "B" "C")

let state_chain_with_markers_and_label () =
  let g = parse_state "stateDiagram-v2\n [*] --> A --> B: done" in
  Alcotest.(check int) "edges" 2 (List.length (edges g));
  Alcotest.(check bool) "done label" true
    (List.exists (fun e -> e_label e = Some "done") (edges g));
  let out = plain "stateDiagram-v2\n [*] --> A --> B: done" in
  Alcotest.(check bool) "marker" true (contains out "●");
  Alcotest.(check bool) "done" true (contains out "done")

let state_dangling_chain_falls_back () =
  Alcotest.(check bool) "fallback" true
    (contains (plain "stateDiagram-v2\n A --> B -->") "mermaid: stateDiagram-v2")

(* --- sequence diagrams --- *)

let sequence_renders_actors_and_messages () =
  let out = plain "sequenceDiagram\n Alice->>Bob: Hello Bob\n Bob-->>Alice: Hi Alice" in
  Alcotest.(check bool) "alice" true (contains out "Alice");
  Alcotest.(check bool) "bob" true (contains out "Bob");
  Alcotest.(check bool) "message" true (contains out "Hello Bob");
  Alcotest.(check bool) "call arrow" true (contains out "▶");
  Alcotest.(check bool) "reply arrow" true (contains out "◄");
  Alcotest.(check bool) "dashed reply" true (contains out "╌");
  Alcotest.(check int) "actor boxes repeat" 2 (count_char out "│ Alice │")

let sequence_participant_as_label () =
  let out = plain "sequenceDiagram\n participant C as Client\n participant S as Server\n C->>S: GET /" in
  Alcotest.(check bool) "client" true (contains out "Client");
  Alcotest.(check bool) "server" true (contains out "Server")

let sequence_declared_order_wins () =
  let out = plain "sequenceDiagram\n participant B\n participant A\n A->>B: hi" in
  let line = nth (str_lines out) 1 in
  Alcotest.(check bool) "B left of A" true (index_of line "B" < index_of line "A")

let sequence_self_message_loops () =
  let out = plain "sequenceDiagram\n A->>A: think" in
  Alcotest.(check bool) "corners" true (contains out "╮" && contains out "╯");
  Alcotest.(check bool) "think" true (contains out "think")

let sequence_cross_head () =
  Alcotest.(check bool) "cross" true (contains (plain "sequenceDiagram\n A-x B: lost") "×")

let sequence_note_over_renders_box () =
  Alcotest.(check bool) "note" true
    (contains (plain "sequenceDiagram\n A->>B: hi\n Note over A,B: happy path") "happy path")

let sequence_autonumber_prefixes_messages () =
  let out = plain "sequenceDiagram\n autonumber\n A->>B: one\n B->>A: two" in
  Alcotest.(check bool) "1. one" true (contains out "1. one");
  Alcotest.(check bool) "2. two" true (contains out "2. two")

let sequence_loop_renders_divider_and_end () =
  let out = plain "sequenceDiagram\n A->>B: hi\n loop retry x3\n A->>B: again\n end" in
  Alcotest.(check bool) "loop label" true (contains out "loop retry x3");
  Alcotest.(check bool) "end" true (contains out " end ")

let sequence_rect_block_is_invisible () =
  let out = plain "sequenceDiagram\n rect rgb(0,0,0)\n A->>B: hi\n end" in
  Alcotest.(check bool) "no rect" true (not (contains out "rect"));
  Alcotest.(check bool) "no end" true (not (contains out " end "))

let sequence_box_end_does_not_close_enclosing_block () =
  let out =
    plain "sequenceDiagram\n loop l1\n box g\n participant A\n end\n A->>B: hi\n A->>B: bye\n end"
  in
  Alcotest.(check int) "one end" 1 (count_char out " end ");
  let ls = str_lines out in
  Alcotest.(check bool) "messages inside loop" true
    (pos_of ls "loop l1" < pos_of ls "hi" && pos_of ls "bye" < pos_of ls " end ");
  Alcotest.(check bool) "no box" true (not (contains out "box"))

let sequence_critical_option_renders_dividers () =
  let out =
    plain "sequenceDiagram\n critical connect\n A->>B: try\n option timeout\n A->>A: log\n end"
  in
  Alcotest.(check bool) "critical" true (contains out "critical connect");
  Alcotest.(check bool) "option" true (contains out "option timeout");
  Alcotest.(check bool) "end" true (contains out " end ")

let sequence_long_label_widens_gap () =
  Alcotest.(check bool) "long label" true
    (contains
       (plain "sequenceDiagram\n A->>B: a very long message label that needs room\n B-->>A: ok")
       "a very long message label that needs room")

let sequence_unparseable_arrow_falls_back () =
  Alcotest.(check bool) "fallback" true
    (contains (plain "sequenceDiagram\n ->>B: orphan") "mermaid: sequenceDiagram")

let sequence_unknown_statement_falls_back () =
  Alcotest.(check bool) "fallback" true
    (contains (plain "sequenceDiagram\n A->>B: hi\n garbage statement here") "mermaid: sequenceDiagram")

let sequence_over_wide_falls_back () =
  Alcotest.(check bool) "fallback" true
    (contains
       (plain_at 30 "sequenceDiagram\n A->>B: this label is far wider than the available pane width")
       "mermaid: sequenceDiagram")

let sequence_over_cap_falls_back () =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "sequenceDiagram\n";
  for i = 0 to 599 do Buffer.add_string buf (Printf.sprintf " A->>B: msg %d\n" i) done;
  Alcotest.(check bool) "fallback" true (contains (plain (Buffer.contents buf)) "mermaid: sequenceDiagram")

let sequence_activation_markers_are_stripped () =
  let out = plain "sequenceDiagram\n A->>+B: call\n B-->>-A: return" in
  Alcotest.(check bool) "call" true (contains out "call");
  Alcotest.(check bool) "return" true (contains out "return");
  Alcotest.(check bool) "no plus" true (not (contains out "+"))

let sequence_rows_are_rectangular_and_sentinel_free () =
  let out = plain "sequenceDiagram\n Alice->>Bob: hi\n Note over Alice: solo note" in
  Alcotest.(check bool) "no sentinel" true (not (String.contains out '\000'));
  Alcotest.(check bool) "solo note" true (contains out "solo note")

(* --- entity decoding across every direct-push sink --- *)

let entity_escaped_flowchart_label_decodes_in_box_art () =
  let src =
    "flowchart LR\n  YAML[\"models-config/&lt;model&gt;/&lt;env&gt;.yaml\\nenterprise_api_config:\"]\n  PY[\"model_config_map.py\\nlanguage_model_dict_to_proto()\"]\n  YAML --> PY"
  in
  let g = parse src in
  Alcotest.(check bool) "label decoded" true
    (contains (n_label (nth (nodes g) 0)) "models-config/<model>/<env>.yaml");
  let art = plain src in
  Alcotest.(check bool) "art decoded" true (contains art "<model>" && contains art "<env>");
  Alcotest.(check bool) "no entities" true (not (contains art "&lt;") && not (contains art "&gt;"))

let diagram_with_html_labels_renders_without_tag_artifacts () =
  let out =
    plain
      "flowchart TD\n  IDs[\"<b>3. Token IDs</b><br/>[ 464, 3797 ]<br/><i>indices</i>\"] --> Out[\"<b>done</b>\"]"
  in
  Alcotest.(check bool) "no open tag" true (not (contains out "<b>"));
  Alcotest.(check bool) "no close tag" true (not (contains out "</"));
  Alcotest.(check bool) "no br artifact" true (not (contains out "br/"));
  Alcotest.(check bool) "text present" true (contains out "Token IDs")

let direct_push_sinks_decode_entities () =
  let module PS = I.Parse_sequence in
  let g =
    match
      I.Parse_state.parse_state
        "stateDiagram-v2\n  state \"work &lt;job&gt;\" as J\n  Idle --> Run: \"on &lt;go&gt;\"\n  Run: \"d &lt;e&gt;\""
    with
    | Some g -> g
    | None -> Alcotest.fail "state"
  in
  let node s = List.exists (fun n -> contains (n_label n) s) (nodes g) in
  let edge s = List.exists (fun e -> match e_label e with Some l -> contains l s | None -> false) (edges g) in
  Alcotest.(check bool) "state decoded" true (node "work <job>" && node "d <e>" && edge "on <go>");
  Alcotest.(check bool) "state no entities" true (not (node "&lt;") && not (edge "&lt;"));
  let cg = pc "classDiagram\n  A --> B : \"uses &lt;X&gt;\"" in
  Alcotest.(check bool) "class decoded" true
    (List.exists
       (fun e ->
         match e_label e with Some l -> contains l "uses <X>" && not (contains l "&lt;") | None -> false)
       (edges cg));
  let seq =
    match
      PS.parse_sequence
        "sequenceDiagram\n  A->>B: \"call &lt;svc&gt;\"\n  Note over A,B: \"memo &lt;o&gt;\"\n  alt \"c &lt;x&gt;\"\n    A->>B: ok\n  end"
    with
    | Some s -> s
    | None -> Alcotest.fail "sequence"
  in
  let items = Vec.to_list seq.PS.items in
  let has p = List.exists p items in
  Alcotest.(check bool) "msg decoded" true
    (has (function PS.Message { text = Some t; _ } -> contains t "call <svc>" && not (contains t "&lt;") | _ -> false));
  Alcotest.(check bool) "note decoded" true
    (has (function PS.Note { text; _ } -> contains text "memo <o>" && not (contains text "&lt;") | _ -> false));
  Alcotest.(check bool) "divider decoded" true
    (has (function PS.Divider { text } -> contains text "c <x>" && not (contains text "&lt;") | _ -> false));
  let m = I.Parse_class.empty_info () in
  I.Parse_class.push_member m "+run &lt;R&gt;";
  Alcotest.(check (list string)) "member decoded" [ "+run <R>" ] m.I.Parse_class.attrs;
  let a = I.Parse_class.empty_info () in
  I.Parse_class.push_er_attribute a "string &lt;pk&gt;";
  Alcotest.(check (list string)) "er attr decoded" [ "string <pk>" ] a.I.Parse_class.attrs

let () =
  let tc = Alcotest.test_case in
  Alcotest.run "termaid-invariants"
    [ ( "parse-structure"
      , [ tc "nodes/edges/direction" `Quick parses_nodes_edges_and_direction
        ; tc "non-flowchart is None" `Quick non_flowchart_returns_none_from_parse
        ] )
    ; ( "labels"
      , [ tc "html stripped" `Quick html_tags_are_stripped_from_labels
        ; tc "br -> space" `Quick br_tag_becomes_a_space
        ; tc "md bold/italic/code" `Quick markdown_string_strips_bold_italic_and_code
        ; tc "md snake_case" `Quick
            markdown_string_preserves_snake_case_and_strips_inline_code
        ; tc "md edge label" `Quick markdown_string_edge_label_is_stripped
        ; tc "plain literal" `Quick plain_label_keeps_literal_text_and_underscores
        ; tc "code/span stripped" `Quick code_and_span_tags_are_stripped
        ; tc "bare angles kept" `Quick bare_angle_brackets_are_kept
        ; tc "generics not html" `Quick generic_types_are_not_stripped_as_html
        ; tc "entity decode" `Quick
            decode_html_entities_covers_named_numeric_and_double_escape
        ; tc "entity in box art" `Quick entity_escaped_flowchart_label_decodes_in_box_art
        ; tc "html no artifacts" `Quick diagram_with_html_labels_renders_without_tag_artifacts
        ; tc "entity sinks decode" `Quick direct_push_sinks_decode_entities
        ] )
    ; ( "quoting"
      , [ tc "inner brackets one node" `Quick quoted_label_with_inner_brackets_is_one_node
        ; tc "embedded quote closes at bracket" `Quick
            unquoted_label_with_embedded_quote_closes_at_bracket
        ; tc "inner parens one node" `Quick quoted_label_with_inner_parens_is_one_node
        ] )
    ; ( "ranks", [ tc "ignore back edges" `Quick ranks_ignore_back_edges ] )
    ; ( "rendering"
      , [ tc "boxes/labels/arrow" `Quick td_render_has_boxes_labels_and_arrow
        ; tc "edge label rendered" `Quick edge_label_is_rendered
        ; tc "LR shorter than TD" `Quick lr_is_shorter_than_td_for_a_chain
        ; tc "fallback box" `Quick unsupported_diagram_uses_fallback_box
        ; tc "blank is None" `Quick blank_source_returns_none
        ; tc "inline x/o label" `Quick inline_label_with_x_or_o_letters
        ; tc "wide glyph aligned" `Quick wide_glyph_box_stays_aligned
        ; tc "merge single arrowhead" `Quick merge_has_single_arrowhead
        ; tc "long label wraps" `Quick long_label_wraps_without_truncation
        ; tc "very long truncates" `Quick very_long_label_truncates_after_max_lines
        ; tc "undirected no arrow" `Quick undirected_piped_label_has_no_arrowhead
        ; tc "chain straight" `Quick chain_edges_are_straight
        ; tc "adversarial fallback" `Quick adversarial_chain_falls_back
        ] )
    ; ( "wrapping"
      , [ tc "boundary break" `Quick wrap_label_breaks_long_identifier_on_boundary
        ; tc "per-char fallback" `Quick wrap_label_token_without_break_char_falls_back_per_char
        ; tc "flowchart boundary break" `Quick
            flowchart_long_identifier_breaks_on_boundary_not_mid_segment
        ; tc "mixed boundary tail" `Quick wrap_label_mixed_boundary_then_no_boundary_tail
        ; tc "truncate at max lines" `Quick
            wrap_label_boundary_breaking_still_truncates_at_max_lines
        ] )
    ; ( "orientation"
      , [ tc "BT flips" `Quick bt_flips_orientation
        ; tc "RL flips" `Quick rl_flips_orientation
        ] )
    ; ( "fallback"
      , [ tc "single-stmt over cap" `Quick single_statement_chain_over_cap_falls_back
        ; tc "deep chain renders" `Quick deep_chain_within_caps_renders
        ; tc "styled/plain widths" `Quick fallback_styled_and_plain_widths_match
        ; tc "over-wide falls back" `Quick over_wide_diagram_falls_back
        ; tc "too-wide hint" `Quick too_wide_fallback_appends_hint_below_box
        ; tc "unsupported not too-wide" `Quick unsupported_diagram_fallback_not_flagged_too_wide
        ; tc "fitting no warning" `Quick fitting_diagram_has_no_width_warning
        ; tc "wraps long lines" `Quick fallback_wraps_long_lines_to_max_width
        ] )
    ; ( "ordering"
      , [ tc "removes crossing" `Quick order_ranks_removes_avoidable_crossing
        ; tc "keeps free order" `Quick order_ranks_keeps_crossing_free_order
        ; tc "renders untangled" `Quick crossing_edges_render_untangled
        ; tc "three-layer weave" `Quick three_layer_weave_untangles
        ; tc "crossing bus rows" `Quick unavoidable_crossing_gets_separate_bus_rows
        ; tc "fan-out single bus" `Quick fan_out_keeps_single_bus_row
        ; tc "shared-target lane" `Quick shared_target_back_edges_share_one_lane
        ; tc "distinct lanes" `Quick distinct_back_edges_get_separate_lanes
        ] )
    ; ( "edges"
      , [ tc "bidirectional heads" `Quick bidirectional_link_draws_both_arrowheads
        ; tc "reversed swaps" `Quick reversed_arrow_swaps_edge_direction
        ; tc "reversed+marker swaps" `Quick reversed_arrow_with_end_marker_swaps_direction
        ; tc "both markers" `Quick both_end_markers_parse
        ; tc "no phantom o/x" `Quick circle_and_cross_endings_create_no_phantom_nodes
        ; tc "left endings" `Quick left_endings_decorate_without_reversing
        ; tc "fan-out cross product" `Quick fan_out_creates_cross_product_edges
        ; tc "fan-out in chain" `Quick fan_out_in_chain
        ; tc "fan-out reversed" `Quick fan_out_with_reversed_arrow
        ; tc "dotted/thick distinct" `Quick dotted_and_thick_lines_render_distinctly
        ; tc "dotted label dashed" `Quick dotted_label_form_renders_dashed
        ; tc "thick jog corners" `Quick thick_jog_uses_thick_corners
        ; tc "mixed bus light" `Quick mixed_solid_and_dotted_bus_stays_light
        ; tc "borders stay light" `Quick box_borders_stay_light_next_to_styled_edges
        ; tc "skip-edge lane" `Quick skip_edge_routes_around_intermediate_boxes
        ; tc "inline o-word label" `Quick inline_o_word_label_still_parses_as_label
        ] )
    ; ( "comments"
      , [ tc "quoted survives" `Quick semicolon_and_comment_survive_inside_quoted_label
        ; tc "stripped outside quotes" `Quick comment_outside_quotes_is_stripped
        ] )
    ; ( "self-loop"
      , [ tc "renders below" `Quick self_loop_renders_below_box
        ; tc "label" `Quick self_loop_label_renders
        ; tc "coexists forward" `Quick self_loop_coexists_with_forward_edge
        ; tc "flips BT" `Quick self_loop_flips_with_bt
        ; tc "in LR" `Quick self_loop_in_lr
        ] )
    ; ( "subgraph"
      , [ tc "titled frame" `Quick subgraph_renders_titled_frame
        ; tc "edge between groups" `Quick subgraph_edge_between_groups
        ; tc "nested frames" `Quick subgraph_nested_frames
        ; tc "cross-member edge" `Quick subgraph_cross_member_edge_attaches_to_frame
        ; tc "id before decl" `Quick subgraph_id_referenced_before_declaration
        ; tc "quoted/plain titles" `Quick subgraph_quoted_and_plain_titles
        ; tc "empty dropped" `Quick subgraph_empty_is_dropped
        ; tc "BT flips frame" `Quick subgraph_bt_flips_frame_and_contents
        ; tc "depth over cap" `Quick subgraph_depth_over_cap_falls_back
        ; tc "groupless unchanged" `Quick subgraph_groupless_path_unchanged
        ] )
    ; ( "class"
      , [ tc "compartments" `Quick class_renders_compartments
        ; tc "inheritance triangle" `Quick class_inheritance_triangle_at_parent
        ; tc "realization dotted" `Quick class_realization_is_dotted_triangle
        ; tc "comp/agg diamonds" `Quick class_composition_and_aggregation_diamonds
        ; tc "dependency dotted" `Quick class_dependency_dotted_arrow
        ; tc "colon members merge" `Quick class_colon_members_merge_with_block
        ; tc "annotation guillemets" `Quick class_annotation_renders_guillemets
        ; tc "generics angle" `Quick class_generics_display_as_angle_brackets
        ; tc "cardinalities fold" `Quick class_cardinalities_fold_into_label
        ; tc "from-end head" `Quick class_from_end_head_survives_fan_out_jog
        ; tc "empty titled box" `Quick class_empty_class_is_plain_titled_box
        ; tc "unknown fallback" `Quick class_unknown_statement_falls_back
        ; tc "member cap" `Quick class_member_cap_ellipsis
        ; tc "direction LR" `Quick class_direction_lr
        ] )
    ; ( "er"
      , [ tc "entities/labels" `Quick er_renders_entities_and_relationship_labels
        ; tc "cardinality map" `Quick er_cardinality_map
        ; tc "non-identifying dotted" `Quick er_non_identifying_renders_dotted
        ; tc "no arrowheads" `Quick er_relationships_have_no_arrowheads
        ; tc "entity alias" `Quick er_entity_alias_label
        ; tc "unquoted/bare" `Quick er_unquoted_label_and_bare_entity_decl
        ; tc "attribute cap" `Quick er_attribute_cap_ellipsis
        ; tc "unknown fallback" `Quick er_unknown_statement_falls_back
        ] )
    ; ( "state"
      , [ tc "states/transitions" `Quick state_diagram_renders_states_and_transitions
        ; tc "v1 header" `Quick state_v1_header_renders
        ; tc "rounded boxes" `Quick state_boxes_are_rounded
        ; tc "alias label" `Quick state_alias_label_renders
        ; tc "choice diamond" `Quick state_choice_parses_as_diamond
        ; tc "description label" `Quick state_description_sets_label
        ; tc "direction LR" `Quick state_direction_lr
        ; tc "composite flat" `Quick state_composite_contents_render_flat
        ; tc "notes skipped" `Quick state_notes_are_skipped
        ; tc "back transition lane" `Quick state_back_transition_uses_lane
        ; tc "unknown fallback" `Quick state_unknown_statement_falls_back
        ; tc "over cap" `Quick state_over_cap_falls_back
        ; tc "extra dash" `Quick state_extra_dash_arrow_tolerated
        ; tc "choice shape preserved" `Quick state_description_preserves_choice_shape
        ; tc "chained edges" `Quick state_chained_transitions_parse_as_separate_edges
        ; tc "chain markers+label" `Quick state_chain_with_markers_and_label
        ; tc "dangling fallback" `Quick state_dangling_chain_falls_back
        ] )
    ; ( "sequence"
      , [ tc "actors/messages" `Quick sequence_renders_actors_and_messages
        ; tc "participant label" `Quick sequence_participant_as_label
        ; tc "declared order" `Quick sequence_declared_order_wins
        ; tc "self message" `Quick sequence_self_message_loops
        ; tc "cross head" `Quick sequence_cross_head
        ; tc "note over" `Quick sequence_note_over_renders_box
        ; tc "autonumber" `Quick sequence_autonumber_prefixes_messages
        ; tc "loop divider/end" `Quick sequence_loop_renders_divider_and_end
        ; tc "rect invisible" `Quick sequence_rect_block_is_invisible
        ; tc "box end silent" `Quick sequence_box_end_does_not_close_enclosing_block
        ; tc "critical/option" `Quick sequence_critical_option_renders_dividers
        ; tc "long label gap" `Quick sequence_long_label_widens_gap
        ; tc "unparseable fallback" `Quick sequence_unparseable_arrow_falls_back
        ; tc "unknown fallback" `Quick sequence_unknown_statement_falls_back
        ; tc "over-wide fallback" `Quick sequence_over_wide_falls_back
        ; tc "over cap fallback" `Quick sequence_over_cap_falls_back
        ; tc "activation stripped" `Quick sequence_activation_markers_are_stripped
        ; tc "rectangular/sentinel-free" `Quick sequence_rows_are_rectangular_and_sentinel_free
        ] )
    ]

let _ = e_line
let _ = n_shape
