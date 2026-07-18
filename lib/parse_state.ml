(* State-diagram parser (`stateDiagram` / `stateDiagram-v2`): produces an
   {!Ir.graph} laid out by the flowchart engine. Faithful port of upstream
   [parse_state] and helpers.

   The upstream helpers return [Option<()>] and bail the whole parse with [?];
   here a malformed statement (or a node/edge cap hit) raises [Fail], caught at
   the top to yield [None]. *)

open Ir

exception Fail

let node_index_exn g id label shape =
  match Ir.node_index g id label shape with Some i -> i | None -> raise Fail

let node_label_exn g id label =
  match Ir.node_label g id label with Some i -> i | None -> raise Fail

let non_empty (s : string) : string option = if s = "" then None else Some s

let state_endpoint (g : graph) (id : string) (is_source : bool) : int =
  if id = "[*]" then
    let key = if is_source then "[*]start" else "[*]end" in
    node_index_exn g key (Some "●") Round
  else node_index_exn g id None Round

let parse_state_decl (st : string) (graph : graph) : unit =
  let rest =
    Ucore.trim (Ucore.rtrim_char (Ucore.trim (String.sub st 5 (String.length st - 5))) '{')
  in
  if rest = "" then ()
  else if rest.[0] = '"' then begin
    let q = String.sub rest 1 (String.length rest - 1) in
    match Ucore.split_once_char q '"' with
    | None -> raise Fail
    | Some (label, after) ->
      let after_t = Ucore.trim after in
      let id =
        match Ucore.strip_prefix_str after_t "as" with
        | Some s -> Ucore.trim s
        | None -> label
      in
      ignore (node_label_exn graph id (Text.decode_html_entities label))
  end
  else begin
    let shape = ref Round and id = ref rest and stereotyped = ref false in
    (match Ucore.find_str rest "<<" with
     | Some pos ->
       let stereo =
         Ucore.trim (Ucore.rtrim_str (String.sub rest (pos + 2) (String.length rest - pos - 2)) ">>")
       in
       if stereo = "choice" then shape := Diamond;
       id := Ucore.trim (String.sub rest 0 pos);
       stereotyped := true
     | None -> ());
    if !id = "" || Ucore.has_whitespace !id then raise Fail;
    let label = if !stereotyped then Some !id else None in
    ignore (node_index_exn graph !id label !shape)
  end

let parse_transition (st : string) (graph : graph) : unit =
  let rest = ref st and prev = ref None and continue = ref true in
  while !continue do
    match Ucore.split_once_str !rest "-->" with
    | None -> continue := false
    | Some (lhs, rhs) ->
      let from_id = Ucore.trim (Ucore.rtrim_char (Ucore.trim_end lhs) '-') in
      let from =
        match !prev with
        | Some p -> if from_id <> "" then raise Fail else p
        | None -> if from_id = "" then raise Fail else state_endpoint graph from_id true
      in
      let to_part, tail =
        match Ucore.split_once_str rhs "-->" with
        | Some (t, _) -> (t, String.sub rhs (String.length t) (String.length rhs - String.length t))
        | None -> (rhs, "")
      in
      let to_part, label =
        match Ucore.split_once_char to_part ':' with
        | Some (t, l) -> (t, non_empty (Text.decode_html_entities (Ucore.trim l)))
        | None -> (to_part, None)
      in
      let to_id =
        Ucore.trim
          (Ucore.rtrim_char
             (Ucore.trim_end (Ucore.ltrim_char (Ucore.trim_start to_part) '>'))
             '-')
      in
      if to_id = "" then raise Fail;
      let to_ = state_endpoint graph to_id false in
      if Vec.length graph.edges >= Const.max_edges then begin
        graph.over_cap <- true;
        continue := false
      end
      else begin
        Vec.push graph.edges
          { from_ = from
          ; to_
          ; label
          ; head_to = Arrow
          ; head_from = No_head
          ; line = Solid
          };
        prev := Some to_;
        rest := tail
      end
  done

let parse_state_desc (st : string) (graph : graph) : unit =
  match Ucore.split_once_char st ':' with
  | Some (id, desc) ->
    let id = Ucore.trim id and desc = Ucore.trim desc in
    if id = "" || Ucore.has_whitespace id || desc = "" then raise Fail;
    ignore (node_label_exn graph id (Text.decode_html_entities desc))
  | None ->
    if not (Ucore.has_whitespace st) then ignore (node_index_exn graph st None Round)
    else raise Fail

let parse_state (src : string) : graph option =
  let acc = ref [] in
  List.iter (fun raw -> Parse_graph.split_statements raw acc) (Parse_graph.lines src);
  match List.rev !acc with
  | [] -> None
  | header :: rest -> (
    match Ucore.first_whitespace_token header with
    | None -> None
    | Some t ->
      if not (Ucore.starts_with (String.lowercase_ascii t) "statediagram") then None
      else begin
        let graph = Ir.create_graph Down in
        try
          let in_note = ref false in
          List.iter
            (fun st ->
              if !in_note then begin
                if String.lowercase_ascii st = "end note" then in_note := false
              end
              else begin
                let first =
                  match Ucore.first_whitespace_token st with
                  | Some x -> String.lowercase_ascii x
                  | None -> ""
                in
                (match first with
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
                 | "note" -> if not (String.contains st ':') then in_note := true
                 | "state" -> parse_state_decl st graph
                 | "classdef" | "class" | "hide" | "scale" | "}" | "--" -> ()
                 | _ ->
                   if Ucore.contains_str st "-->" then parse_transition st graph
                   else parse_state_desc st graph);
                if graph.over_cap then raise Fail
              end)
            rest;
          if Vec.length graph.nodes = 0 then None else Some graph
        with Fail -> None
      end)
