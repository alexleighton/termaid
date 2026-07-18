(* Sequence-diagram parser. Produces a {!sequence} (participants + ordered items)
   rendered by {!Seq_layout}. Faithful port of upstream [parse_sequence] and
   helpers. Malformed input raises [Fail], caught at the top to yield [None]. *)

exception Fail

type seq_head =
  | Arrow
  | Cross

type note_anchor =
  | Over of int * int
  | Left of int
  | Right of int

type seq_item =
  | Message of
      { from_ : int
      ; to_ : int
      ; text : string option
      ; dashed : bool
      ; head : seq_head
      }
  | Note of
      { anchor : note_anchor
      ; text : string
      }
  | Divider of { text : string }

type sequence =
  { labels : string Vec.t
  ; index : (string, int) Hashtbl.t
  ; items : seq_item Vec.t
  }

let non_empty (s : string) : string option = if s = "" then None else Some s

let seq_ops =
  [ ("-->>", true, Arrow)
  ; ("->>", false, Arrow)
  ; ("--x", true, Cross)
  ; ("-x", false, Cross)
  ; ("--)", true, Arrow)
  ; ("-)", false, Arrow)
  ; ("-->", true, Arrow)
  ; ("->", false, Arrow)
  ]

let participant (seq : sequence) (id : string) (label : string option) : int option =
  match Hashtbl.find_opt seq.index id with
  | Some i ->
    (match label with Some l -> Vec.set seq.labels i l | None -> ());
    Some i
  | None ->
    if Vec.length seq.labels >= Const.max_nodes then None
    else begin
      Hashtbl.replace seq.index id (Vec.length seq.labels);
      Vec.push seq.labels (match label with Some l -> l | None -> id);
      Some (Vec.length seq.labels - 1)
    end

let participant_exn seq id label =
  match participant seq id label with Some i -> i | None -> raise Fail

let parse_note_anchor (rest : string) (seq : sequence) : string * note_anchor =
  let lower = String.lowercase_ascii rest in
  let strip p =
    if Ucore.starts_with lower p then
      Some (String.sub rest (String.length p) (String.length rest - String.length p))
    else None
  in
  let ids_and_text, kind =
    match strip "over " with
    | Some r -> (r, 0)
    | None -> (
      match strip "left of " with
      | Some r -> (r, 1)
      | None -> ( match strip "right of " with Some r -> (r, 2) | None -> raise Fail))
  in
  match Ucore.split_once_char ids_and_text ':' with
  | None -> raise Fail
  | Some (ids, text) -> (
    let text = Text.decode_html_entities (Ucore.trim text) in
    let parts =
      List.filter (fun s -> s <> "") (List.map Ucore.trim (String.split_on_char ',' ids))
    in
    match parts with
    | [] -> raise Fail
    | first :: more ->
      let a = participant_exn seq first None in
      let anchor =
        match kind with
        | 0 ->
          let b = match more with id :: _ -> participant_exn seq id None | [] -> a in
          Over (min a b, max a b)
        | 1 -> Left a
        | _ -> Right a
      in
      (text, anchor))

let parse_seq_message (st : string) (seq : sequence)
  : int * int * string option * bool * seq_head =
  let chars = Ucore.to_uchars st in
  let n = Array.length chars in
  let matches_at ci op =
    let ol = String.length op in
    ci + ol <= n
    &&
    let ok = ref true in
    for k = 0 to ol - 1 do
      if Uchar.to_int chars.(ci + k) <> Char.code op.[k] then ok := false
    done;
    !ok
  in
  let found = ref None in
  (try
     for ci = 0 to n - 1 do
       List.iter
         (fun (op, dashed, head) ->
           if !found = None && matches_at ci op then found := Some (ci, op, dashed, head))
         seq_ops;
       if !found <> None then raise Exit
     done
   with Exit -> ());
  match !found with
  | None -> raise Fail
  | Some (pos, op, dashed, head) ->
    let from_id = Ucore.trim (Ucore.sub_to_string chars 0 pos) in
    if from_id = "" then raise Fail;
    let rest =
      let s = Ucore.trim_start (Ucore.sub_to_string chars (pos + String.length op) n) in
      let i = ref 0 and sl = String.length s in
      while !i < sl && (s.[!i] = '+' || s.[!i] = '-') do incr i done;
      String.sub s !i (sl - !i)
    in
    let to_id, text =
      match Ucore.split_once_char rest ':' with
      | Some (to_, t) -> (Ucore.trim to_, non_empty (Text.decode_html_entities (Ucore.trim t)))
      | None -> (Ucore.trim rest, None)
    in
    if to_id = "" then raise Fail;
    let from = participant_exn seq from_id None in
    let to_ = participant_exn seq to_id None in
    (from, to_, text, dashed, head)

let parse_sequence (src : string) : sequence option =
  let acc = ref [] in
  List.iter (fun raw -> Parse_graph.split_statements raw acc) (Parse_graph.lines src);
  match List.rev !acc with
  | [] -> None
  | header :: rest -> (
    match Ucore.first_whitespace_token header with
    | Some t when String.lowercase_ascii t = "sequencediagram" ->
      let seq =
        { labels = Vec.create (); index = Hashtbl.create 32; items = Vec.create () }
      in
      let autonumber = ref false and msg_count = ref 0 and blocks = ref [] in
      let rest_after st first =
        Ucore.trim (String.sub st (String.length first) (String.length st - String.length first))
      in
      (try
         List.iter
           (fun st ->
             let first =
               match Ucore.first_whitespace_token st with Some x -> x | None -> ""
             in
             let firstl = String.lowercase_ascii first in
             match firstl with
             | "participant" | "actor" ->
               let r = rest_after st first in
               if r = "" then raise Fail;
               let id, label =
                 match Ucore.split_once_str r " as " with
                 | Some (id, label) -> (Ucore.trim id, Some (Text.clean_label label))
                 | None -> (r, None)
               in
               ignore (participant_exn seq id label)
             | "autonumber" -> autonumber := true
             | "activate" | "deactivate" | "create" | "destroy" | "title" | "acctitle"
             | "accdescr" | "links" | "link" | "properties" -> ()
             | "note" ->
               let text_part, anchor = parse_note_anchor (rest_after st first) seq in
               if Vec.length seq.items >= Const.max_edges then raise Fail;
               Vec.push seq.items (Note { anchor; text = text_part })
             | "loop" | "alt" | "opt" | "par" | "critical" | "break" | "else" | "and"
             | "option" ->
               let skip =
                 if firstl = "else" || firstl = "and" || firstl = "option" then
                   match !blocks with true :: _ -> false | _ -> true
                 else begin
                   blocks := true :: !blocks;
                   false
                 end
               in
               if not skip then begin
                 if Vec.length seq.items >= Const.max_edges then raise Fail;
                 Vec.push seq.items (Divider { text = Text.decode_html_entities st })
               end
             | "rect" | "box" -> blocks := false :: !blocks
             | "end" ->
               let popped =
                 match !blocks with
                 | x :: t ->
                   blocks := t;
                   Some x
                 | [] -> None
               in
               if popped = Some true then begin
                 if Vec.length seq.items >= Const.max_edges then raise Fail;
                 Vec.push seq.items (Divider { text = "end" })
               end
             | _ ->
               let from, to_, text0, dashed, head = parse_seq_message st seq in
               let text =
                 if !autonumber then begin
                   incr msg_count;
                   Some
                     (match text0 with
                      | Some t -> string_of_int !msg_count ^ ". " ^ t
                      | None -> string_of_int !msg_count ^ ".")
                 end
                 else text0
               in
               if Vec.length seq.items >= Const.max_edges then raise Fail;
               Vec.push seq.items (Message { from_ = from; to_; text; dashed; head }))
           rest;
         if Vec.length seq.labels = 0 then None else Some seq
       with Fail -> None)
    | _ -> None)
