(* Class-diagram and entity-relationship parsers. Both build an {!Ir.graph} plus
   a parallel [class_info] per node (annotation + attribute/method compartments)
   that {!Render.render_class} paints as a compartmented box. Faithful port of
   upstream [parse_class]/[parse_er] and helpers.

   As with the state parser, malformed input (or a cap hit) raises [Fail],
   caught at the top to yield [None]. *)

open Ir

exception Fail

type class_info =
  { mutable annotation : string option
  ; mutable attrs : string list
  ; mutable methods : string list
  }

let empty_info () = { annotation = None; attrs = []; methods = [] }
let non_empty (s : string) : string option = if s = "" then None else Some s

let node_index_exn g id label shape =
  match Ir.node_index g id label shape with Some i -> i | None -> raise Fail

let node_label_exn g id label =
  match Ir.node_label g id label with Some i -> i | None -> raise Fail

let sync_infos (graph : graph) (infos : class_info Vec.t) : unit =
  while Vec.length infos < Vec.length graph.nodes do
    Vec.push infos (empty_info ())
  done

(* Mermaid writes generics as [~T~]; display them as angle brackets. *)
let display_generics (s : string) : string =
  let out = Buffer.create (String.length s) and open_ = ref false in
  Array.iter
    (fun c ->
      if Ucore.is c '~' then begin
        Buffer.add_char out (if !open_ then '>' else '<');
        open_ := not !open_
      end
      else Uutf.Buffer.add_utf_8 out c)
    (Ucore.to_uchars s);
  Buffer.contents out

let append (l : string list) (x : string) : string list = l @ [ x ]

let push_member (info : class_info) (raw : string) : unit =
  match Ucore.strip_prefix_str raw "<<" with
  | Some ann -> (
    match Ucore.split_once_str ann ">>" with
    | Some (a, _) -> info.annotation <- Some (Ucore.trim a)
    | None -> ())
  | None ->
    let member = Text.decode_html_entities (display_generics (Ucore.trim raw)) in
    let is_method = String.contains member '(' in
    let len = List.length (if is_method then info.methods else info.attrs) in
    let add s =
      if is_method then info.methods <- append info.methods s
      else info.attrs <- append info.attrs s
    in
    if len < Const.max_members then add member
    else if len = Const.max_members then add "…"

(* --- class relations --- *)

let class_ops =
  [ ("<|--", Triangle, No_head, Solid)
  ; ("--|>", No_head, Triangle, Solid)
  ; ("<|..", Triangle, No_head, Dotted)
  ; ("..|>", No_head, Triangle, Dotted)
  ; ("*--", Diamond_fill, No_head, Solid)
  ; ("--*", No_head, Diamond_fill, Solid)
  ; ("o--", Diamond_open, No_head, Solid)
  ; ("--o", No_head, Diamond_open, Solid)
  ; ("<--", Arrow, No_head, Solid)
  ; ("-->", No_head, Arrow, Solid)
  ; ("<..", Arrow, No_head, Dotted)
  ; ("..>", No_head, Arrow, Dotted)
  ; ("--", No_head, No_head, Solid)
  ; ("..", No_head, No_head, Dotted)
  ]

let strip_cardinality_suffix (s : string) : string * string =
  let t = Ucore.trim_end s in
  let tl = String.length t in
  if tl > 0 && t.[tl - 1] = '"' then begin
    let rest = String.sub t 0 (tl - 1) in
    match String.rindex_opt rest '"' with
    | Some q ->
      (Ucore.trim_end (String.sub rest 0 q), String.sub rest (q + 1) (String.length rest - q - 1))
    | None -> (t, "")
  end
  else (t, "")

let strip_cardinality_prefix (s : string) : string * string =
  let t = Ucore.trim_start s in
  if String.length t > 0 && t.[0] = '"' then begin
    let rest = String.sub t 1 (String.length t - 1) in
    match String.index_opt rest '"' with
    | Some q ->
      (Ucore.trim_start (String.sub rest (q + 1) (String.length rest - q - 1)), String.sub rest 0 q)
    | None -> (t, "")
  end
  else (t, "")

let parse_class_relation (st : string)
  : (string * string * head * head * line_kind * string option) option =
  let chars = Ucore.to_uchars st in
  let n = Array.length chars in
  let starts_with_op pos op =
    let ol = String.length op in
    pos + ol <= n
    &&
    let ok = ref true in
    for k = 0 to ol - 1 do
      if Uchar.to_int chars.(pos + k) <> Char.code op.[k] then ok := false
    done;
    !ok
  in
  let found = ref None in
  (try
     for pos = 0 to n - 1 do
       List.iter
         (fun (op, hf, ht, line) ->
           if !found = None && starts_with_op pos op then begin
             let skip1 = op.[0] = 'o' && pos > 0 && Parse_graph.is_id_char chars.(pos - 1) in
             let skip2 =
               op.[String.length op - 1] = 'o'
               && (match Ucore.get chars (pos + String.length op) with
                   | Some c -> Parse_graph.is_id_char c
                   | None -> false)
             in
             if not (skip1 || skip2) then found := Some (pos, op, hf, ht, line)
           end)
         class_ops;
       if !found <> None then raise Exit
     done
   with Exit -> ());
  match !found with
  | None -> None
  | Some (pos, op, head_from, head_to, line) ->
    let lhs = Ucore.trim (Ucore.sub_to_string chars 0 pos) in
    let rhs = Ucore.trim (Ucore.sub_to_string chars (pos + String.length op) n) in
    let lhs, card_from = strip_cardinality_suffix lhs in
    let rhs, card_to = strip_cardinality_prefix rhs in
    let to_id, rel_label =
      match Ucore.split_once_char rhs ':' with
      | Some (t, l) -> (Ucore.trim t, non_empty (Text.decode_html_entities (Ucore.trim l)))
      | None -> (Ucore.trim rhs, None)
    in
    if lhs = "" || to_id = "" || Ucore.has_whitespace lhs || Ucore.has_whitespace to_id
    then None
    else begin
      let parts =
        List.filter (fun s -> s <> "")
          [ card_from; (match rel_label with Some s -> s | None -> ""); card_to ]
      in
      let label = non_empty (String.concat " " parts) in
      Some (lhs, to_id, head_from, head_to, line, label)
    end

let parse_class (src : string) : (graph * class_info Vec.t) option =
  let acc = ref [] in
  List.iter (fun raw -> Parse_graph.split_statements raw acc) (Parse_graph.lines src);
  match List.rev !acc with
  | [] -> None
  | header :: rest -> (
    match Ucore.first_whitespace_token header with
    | Some t when Ucore.starts_with (String.lowercase_ascii t) "classdiagram" ->
      let graph = Ir.create_graph Down in
      let infos = Vec.create () in
      let cur_class = ref None in
      (try
         List.iter
           (fun st ->
             match !cur_class with
             | Some ci ->
               if st = "}" then cur_class := None else push_member (Vec.get infos ci) st
             | None -> (
               let first =
                 match Ucore.first_whitespace_token st with
                 | Some x -> String.lowercase_ascii x
                 | None -> ""
               in
               match first with
               | "direction" ->
                 let second =
                   match Ucore.split_whitespace st with _ :: b :: _ -> b | _ -> ""
                 in
                 graph.dir
                 <- (match String.uppercase_ascii second with
                     | "LR" -> Right
                     | "RL" -> Left
                     | "BT" -> Up
                     | _ -> Down)
               | "note" | "callback" | "click" | "link" | "style" | "cssclass"
               | "classdef" | "namespace" | "}" -> ()
               | "class" ->
                 let r = Ucore.trim (String.sub st 5 (String.length st - 5)) in
                 let name, open_ =
                   let tl = String.length r in
                   if tl > 0 && r.[tl - 1] = '{' then (Ucore.trim (String.sub r 0 (tl - 1)), true)
                   else (r, false)
                 in
                 if name = "" || Ucore.has_whitespace name then raise Fail;
                 let idx = node_index_exn graph name None Rect in
                 sync_infos graph infos;
                 if open_ then cur_class := Some idx
               | _ ->
                 if Ucore.starts_with st "<<" then begin
                   let body = String.sub st 2 (String.length st - 2) in
                   match Ucore.split_once_str body ">>" with
                   | None -> raise Fail
                   | Some (ann, rst) ->
                     let name = Ucore.trim rst in
                     if name = "" || Ucore.has_whitespace name then raise Fail;
                     let idx = node_index_exn graph name None Rect in
                     sync_infos graph infos;
                     (Vec.get infos idx).annotation <- Some (Ucore.trim ann)
                 end
                 else (
                   match parse_class_relation st with
                   | Some (from, to_id, head_from, head_to, line, label) ->
                     let f = node_index_exn graph from None Rect in
                     sync_infos graph infos;
                     let t = node_index_exn graph to_id None Rect in
                     sync_infos graph infos;
                     if Vec.length graph.edges >= Const.max_edges then raise Fail;
                     Vec.push graph.edges
                       { from_ = f; to_ = t; label; head_to; head_from; line }
                   | None -> (
                     match Ucore.split_once_char st ':' with
                     | Some (id, member) ->
                       let id = Ucore.trim id and member = Ucore.trim member in
                       if id = "" || Ucore.has_whitespace id || member = "" then raise Fail;
                       let idx = node_index_exn graph id None Rect in
                       sync_infos graph infos;
                       push_member (Vec.get infos idx) member
                     | None -> raise Fail))))
           rest;
         if Vec.length graph.nodes = 0 then None
         else begin
           sync_infos graph infos;
           Some (graph, infos)
         end
       with Fail -> None)
    | _ -> None)

(* --- entity-relationship --- *)

let er_card (tok : string) : string option =
  match tok with
  | "|o" | "o|" -> Some "0..1"
  | "||" -> Some "1"
  | "}o" | "o{" -> Some "0..*"
  | "}|" | "|{" -> Some "1..*"
  | _ -> None

let parse_er_op (tok : string) : (string * string * line_kind) option =
  if not (String.for_all (fun c -> Char.code c < 128) tok) || String.length tok <> 6 then
    None
  else
    let line =
      match String.sub tok 2 2 with "--" -> Some Solid | ".." -> Some Dotted | _ -> None
    in
    match (line, er_card (String.sub tok 0 2), er_card (String.sub tok 4 2)) with
    | Some line, Some a, Some b -> Some (a, b, line)
    | _ -> None

let split_er_relationship (st : string) : (string * string option) option =
  let rel, label =
    match Ucore.split_once_char st ':' with
    | Some (r, l) -> (r, Some (Ucore.trim l))
    | None -> (st, None)
  in
  let has_op = List.exists (fun t -> parse_er_op t <> None) (Ucore.split_whitespace rel) in
  if has_op then Some (rel, label) else None

let er_entity (graph : graph) (infos : class_info Vec.t) (token : string) : int =
  let idx =
    match String.index_opt token '[' with
    | Some op ->
      let id = String.sub token 0 op in
      let label =
        Text.clean_label
          (Parse_graph.trim_end_matches
             (String.sub token (op + 1) (String.length token - op - 1))
             ']')
      in
      if id = "" || label = "" then raise Fail;
      node_label_exn graph id label
    | None -> node_index_exn graph token None Rect
  in
  sync_infos graph infos;
  idx

let push_er_attribute (info : class_info) (raw : string) : unit =
  let parts = ref [] in
  (try
     List.iter
       (fun tok -> if Ucore.starts_with tok "\"" then raise Exit else parts := tok :: !parts)
       (Ucore.split_whitespace raw)
   with Exit -> ());
  let parts = List.rev !parts in
  if parts <> [] then begin
    let line = Text.decode_html_entities (String.concat " " parts) in
    let len = List.length info.attrs in
    if len < Const.max_members then info.attrs <- append info.attrs line
    else if len = Const.max_members then info.attrs <- append info.attrs "…"
  end

let parse_er (src : string) : (graph * class_info Vec.t) option =
  let acc = ref [] in
  List.iter (fun raw -> Parse_graph.split_statements raw acc) (Parse_graph.lines src);
  match List.rev !acc with
  | [] -> None
  | header :: rest -> (
    match Ucore.first_whitespace_token header with
    | Some t when String.lowercase_ascii t = "erdiagram" ->
      let graph = Ir.create_graph Down in
      let infos = Vec.create () in
      let cur_entity = ref None in
      (try
         List.iter
           (fun st ->
             match !cur_entity with
             | Some ei ->
               if st = "}" then cur_entity := None
               else push_er_attribute (Vec.get infos ei) st
             | None -> (
               match split_er_relationship st with
               | Some (rel, label_part) -> (
                 match Ucore.split_whitespace rel with
                 | [ lhs; op; rhs ] ->
                   let card_l, card_r, line =
                     match parse_er_op op with Some x -> x | None -> raise Fail
                   in
                   let f = er_entity graph infos lhs in
                   let t = er_entity graph infos rhs in
                   if Vec.length graph.edges >= Const.max_edges then raise Fail;
                   let rel_label =
                     match label_part with Some l -> Text.clean_label l | None -> ""
                   in
                   let parts = List.filter (fun s -> s <> "") [ card_l; rel_label; card_r ] in
                   let label = non_empty (String.concat " " parts) in
                   Vec.push graph.edges
                     { from_ = f; to_ = t; label; head_to = No_head; head_from = No_head; line }
                 | _ -> raise Fail)
               | None ->
                 let decl, open_ =
                   let tl = String.length st in
                   if tl > 0 && st.[tl - 1] = '{' then (Ucore.trim (String.sub st 0 (tl - 1)), true)
                   else (st, false)
                 in
                 if decl = "" || List.length (Ucore.split_whitespace decl) <> 1 then raise Fail;
                 let idx = er_entity graph infos decl in
                 if open_ then cur_entity := Some idx))
           rest;
         if Vec.length graph.nodes = 0 then None
         else begin
           sync_infos graph infos;
           Some (graph, infos)
         end
       with Fail -> None)
    | _ -> None)
