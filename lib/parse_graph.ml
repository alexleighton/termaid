(* Flowchart / graph parser: turns `graph`/`flowchart` source into an {!Ir.graph}.
   Faithful port of upstream `parse_graph` and its helpers. Works over a scalar
   array ({!Ucore.uarr}) indexed exactly as the Rust [Vec<char>]. *)

open Ir

(* --- line / statement splitting --- *)

(* Rust [str::lines]: split on '\n', strip a trailing '\r', and no final empty
   line from a terminating newline. *)
let lines (s : string) : string list =
  if s = "" then []
  else begin
    let parts = String.split_on_char '\n' s in
    let parts = match List.rev parts with "" :: r -> List.rev r | _ -> parts in
    List.map
      (fun l ->
        let n = String.length l in
        if n > 0 && l.[n - 1] = '\r' then String.sub l 0 (n - 1) else l)
      parts
  end

(* Split a raw line into trimmed, non-empty statements on ';', honoring quotes
   and stopping at a '%%' comment. Results are prepended to [acc] (reversed). *)
let split_statements (line : string) (acc : string list ref) : unit =
  let chars = Ucore.to_uchars line in
  let n = Array.length chars in
  let cur = Buffer.create 32 in
  let flush () =
    let t = Ucore.trim (Buffer.contents cur) in
    if t <> "" then acc := t :: !acc;
    Buffer.clear cur
  in
  let in_quotes = ref false and i = ref 0 and stop = ref false in
  while (not !stop) && !i < n do
    let c = chars.(!i) in
    if !in_quotes then begin
      if Ucore.is c '"' then in_quotes := false;
      Uutf.Buffer.add_utf_8 cur c;
      incr i
    end
    else if Ucore.is c '"' then begin
      in_quotes := true;
      Uutf.Buffer.add_utf_8 cur c;
      incr i
    end
    else if
      Ucore.is c '%'
      && (match Ucore.get chars (!i + 1) with Some d -> Ucore.is d '%' | None -> false)
    then stop := true
    else if Ucore.is c ';' then begin
      flush ();
      incr i
    end
    else begin
      Uutf.Buffer.add_utf_8 cur c;
      incr i
    end
  done;
  flush ()

(* --- small scalar helpers --- *)

let is_id_char (u : Uchar.t) : bool = Ucore.is_alphanumeric u || Ucore.is u '_'
let is_link_char (u : Uchar.t) : bool = Ucore.is_one_of u [ '-'; '.'; '='; '<'; '>' ]

let skip_spaces (chars : Ucore.uarr) (i : int) : int =
  let n = Array.length chars and i = ref i in
  while !i < n && (Ucore.is chars.(!i) ' ' || Ucore.is chars.(!i) '\t') do incr i done;
  !i

let line_kind (op : string) : line_kind =
  if String.contains op '=' then Thick
  else if String.contains op '.' then Dotted
  else Solid

let trailing_head (chars : Ucore.uarr) (i : int) : (head * int) option =
  let head =
    match Ucore.get chars i with
    | Some c when Ucore.is c 'o' -> Some Circle
    | Some c when Ucore.is c 'x' -> Some Cross
    | _ -> None
  in
  match head with
  | None -> None
  | Some h -> (
    match Ucore.get chars (i + 1) with
    | None -> Some (h, i + 1)
    | Some c when Ucore.is_one_of c [ ' '; '\t'; '|'; '&'; ';' ] -> Some (h, i + 1)
    | Some _ -> None)

let non_empty (s : string) : string option = if s = "" then None else Some s

let peek (chars : Ucore.uarr) (k : int) (c : char) : bool =
  match Ucore.get chars k with Some d -> Ucore.is d c | None -> false

let starts_with_at (chars : Ucore.uarr) (i : int) (sub : Ucore.uarr) : bool =
  let sl = Array.length sub in
  i + sl <= Array.length chars
  &&
  let ok = ref true in
  for k = 0 to sl - 1 do
    if not (Uchar.equal chars.(i + k) sub.(k)) then ok := false
  done;
  !ok

(* --- shape / node --- *)

let read_shape (chars : Ucore.uarr) (start : int) (closer : string) (shape : shape)
  : shape option * string option * int =
  let closer_arr = Ucore.to_uchars closer in
  let cl = Array.length closer_arr in
  let n = Array.length chars in
  let text = Buffer.create 16 in
  let quoted =
    let j = ref start in
    while
      match Ucore.get chars !j with
      | Some c -> Ucore.is c ' ' || Ucore.is c '\t'
      | None -> false
    do incr j done;
    match Ucore.get chars !j with Some c -> Ucore.is c '"' | None -> false
  in
  let in_quotes = ref false and i = ref start and result = ref None in
  while !result = None && !i < n do
    let c = chars.(!i) in
    if quoted && Ucore.is c '"' then begin
      in_quotes := not !in_quotes;
      Uutf.Buffer.add_utf_8 text c;
      incr i
    end
    else if (not !in_quotes) && starts_with_at chars !i closer_arr then
      result := Some (Some shape, Some (Text.clean_label (Buffer.contents text)), !i + cl)
    else begin
      Uutf.Buffer.add_utf_8 text c;
      incr i
    end
  done;
  match !result with
  | Some r -> r
  | None -> (Some shape, Some (Text.clean_label (Buffer.contents text)), n)

let parse_node (chars : Ucore.uarr) (start : int) (graph : graph) : (int * int) option =
  let n = Array.length chars in
  let i = ref (skip_spaces chars start) in
  let id_start = !i in
  while !i < n && is_id_char chars.(!i) do incr i done;
  if !i = id_start then None
  else begin
    let id = Ucore.sub_to_string chars id_start !i in
    let shape_opt, label, after =
      match Ucore.get chars !i with
      | Some c when Ucore.is c '[' ->
        if peek chars (!i + 1) '[' then read_shape chars (!i + 2) "]]" Rect
        else if peek chars (!i + 1) '(' then read_shape chars (!i + 2) ")]" Round
        else read_shape chars (!i + 1) "]" Rect
      | Some c when Ucore.is c '(' ->
        if peek chars (!i + 1) '(' then read_shape chars (!i + 2) "))" Round
        else if peek chars (!i + 1) '[' then read_shape chars (!i + 2) "])" Round
        else read_shape chars (!i + 1) ")" Round
      | Some c when Ucore.is c '{' ->
        if peek chars (!i + 1) '{' then read_shape chars (!i + 2) "}}" Diamond
        else read_shape chars (!i + 1) "}" Diamond
      | Some c when Ucore.is c '>' -> read_shape chars (!i + 1) "]" Rect
      | _ -> (None, None, !i)
    in
    let shape = match shape_opt with Some s -> s | None -> Rect in
    match Ir.node_index graph id label shape with
    | None -> None
    | Some idx -> Some (idx, after)
  end

let parse_node_group (chars : Ucore.uarr) (start : int) (graph : graph)
  : (int list * int) option =
  match parse_node chars start graph with
  | None -> None
  | Some (first, i0) ->
    let rec loop acc i =
      let j = skip_spaces chars i in
      match Ucore.get chars j with
      | Some c when Ucore.is c '&' -> (
        match parse_node chars (j + 1) graph with
        | None -> None
        | Some (next, k) -> loop (next :: acc) k)
      | _ -> Some (List.rev acc, i)
    in
    loop [ first ] i0

(* --- links --- *)

let parse_link (chars : Ucore.uarr) (start : int)
  : (head * head * line_kind * string option * int) option =
  let n = Array.length chars in
  let i = ref (skip_spaces chars start) in
  let left = ref No_head in
  (match Ucore.get chars !i with
   | Some c
     when (Ucore.is c 'o' || Ucore.is c 'x')
          && (match Ucore.get chars (!i + 1) with
              | Some d -> Ucore.is_one_of d [ '-'; '.'; '=' ]
              | None -> false) ->
     left := (if Ucore.is c 'o' then Circle else Cross);
     incr i
   | _ -> ());
  let op_start = !i in
  while !i < n && is_link_char chars.(!i) do incr i done;
  if !i = op_start then None
  else begin
    let op1 = Ucore.sub_to_string chars op_start !i in
    if !left = No_head && String.length op1 > 0 && op1.[0] = '<' then left := Arrow;
    let line = ref (line_kind op1) in
    let right = ref (if String.contains op1 '>' then Arrow else No_head) in
    if !right = No_head then (
      match trailing_head chars !i with
      | Some (h, ni) ->
        right := h;
        i := ni
      | None -> ());
    if peek chars !i '|' then begin
      incr i;
      let l_start = !i in
      while !i < n && not (Ucore.is chars.(!i) '|') do incr i done;
      let label = Text.clean_label (Ucore.sub_to_string chars l_start !i) in
      if peek chars !i '|' then incr i;
      Some (!left, !right, !line, non_empty label, !i)
    end
    else if !right = No_head then begin
      let text_start = skip_spaces chars !i in
      let j = ref text_start in
      while !j < n && not (is_link_char chars.(!j)) do incr j done;
      if !j < n && !j > text_start && Ucore.is_one_of chars.(!j) [ '-'; '.'; '='; '>' ]
      then begin
        let text = Ucore.sub_to_string chars text_start !j in
        let op2_start = !j in
        while !j < n && is_link_char chars.(!j) do incr j done;
        let op2 = Ucore.sub_to_string chars op2_start !j in
        let right2 =
          if String.contains op2 '>' then Arrow
          else
            match trailing_head chars !j with
            | Some (h, nj) ->
              j := nj;
              h
            | None -> No_head
        in
        if !line = Solid then line := line_kind op2;
        Some (!left, right2, !line, non_empty (Text.clean_label text), !j)
      end
      else Some (!left, !right, !line, None, !i)
    end
    else Some (!left, !right, !line, None, !i)
  end

(* --- statements --- *)

let parse_statement (st : string) (graph : graph) : unit =
  let chars = Ucore.to_uchars st in
  match parse_node_group chars 0 graph with
  | None -> ()
  | Some (prev0, ni) ->
    let prev = ref prev0 and i = ref ni and continue = ref true in
    while !continue do
      i := skip_spaces chars !i;
      if !i >= Array.length chars then continue := false
      else
        match parse_link chars !i with
        | None -> continue := false
        | Some (left, right, line, label, ni2) -> (
          match parse_node_group chars (skip_spaces chars ni2) graph with
          | None -> continue := false
          | Some (next, ni4) ->
            i := ni4;
            let over = ref false in
            List.iter
              (fun f ->
                List.iter
                  (fun t ->
                    if not !over then
                      if Vec.length graph.edges >= Const.max_edges then begin
                        graph.over_cap <- true;
                        over := true
                      end
                      else begin
                        let from_, to_, head_to, head_from =
                          if left = Arrow && right <> Arrow then (t, f, Arrow, right)
                          else (f, t, right, left)
                        in
                        Vec.push graph.edges { from_; to_; label; head_to; head_from; line }
                      end)
                  next)
              !prev;
            if !over then continue := false else prev := next)
    done

(* --- subgraph header / entry point --- *)

let trim_end_matches (s : string) (ch : char) : string =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ch do decr n done;
  String.sub s 0 !n

let parse_subgraph_decl (rest : string) : string * string =
  let quoted =
    if String.length rest > 0 && rest.[0] = '"' then begin
      let q = String.sub rest 1 (String.length rest - 1) in
      match String.index_opt q '"' with
      | Some k -> Some (String.sub q 0 k)
      | None -> None
    end
    else None
  in
  match quoted with
  | Some label -> (label, Text.decode_html_entities label)
  | None -> (
    match String.index_opt rest '[' with
    | Some op ->
      let id = Ucore.trim (String.sub rest 0 op) in
      let after = String.sub rest (op + 1) (String.length rest - op - 1) in
      let label = Text.clean_label (Ucore.trim (trim_end_matches after ']')) in
      if id <> "" && label <> "" then (id, label) else (rest, rest)
    | None -> (rest, rest))

let parse_graph (src : string) : graph option =
  let acc = ref [] in
  List.iter (fun raw -> split_statements raw acc) (lines src);
  match List.rev !acc with
  | [] -> None
  | header :: rest -> (
    match Ucore.first_whitespace_token header with
    | None -> None
    | Some ktok ->
      let kind = String.lowercase_ascii ktok in
      if kind <> "graph" && kind <> "flowchart" then None
      else begin
        let dir =
          let second =
            match Ucore.split_whitespace header with _ :: b :: _ -> b | _ -> "TB"
          in
          match String.uppercase_ascii second with
          | "LR" -> Right
          | "RL" -> Left
          | "BT" -> Up
          | _ -> Down
        in
        let graph = Ir.create_graph dir in
        let stack = ref [] and bail = ref false in
        let top () = match !stack with h :: _ -> Some h | [] -> None in
        let process st =
          if !bail then ()
          else
            let fw =
              match Ucore.first_whitespace_token st with
              | Some t -> String.lowercase_ascii t
              | None -> ""
            in
            match fw with
            | "subgraph" ->
              if
                Vec.length graph.groups >= Const.max_groups
                || List.length !stack >= Const.max_group_depth
              then bail := true
              else begin
                let id, label =
                  parse_subgraph_decl
                    (Ucore.trim (String.sub st 8 (String.length st - 8)))
                in
                Vec.push graph.groups { id; label; parent = top () };
                stack := (Vec.length graph.groups - 1) :: !stack;
                graph.cur_group <- top ()
              end
            | "end" ->
              stack := (match !stack with _ :: t -> t | [] -> []);
              graph.cur_group <- top ()
            | "classdef" | "class" | "style" | "linkstyle" | "click" | "direction" -> ()
            | _ ->
              parse_statement st graph;
              if graph.over_cap then bail := true
        in
        List.iter process rest;
        if !bail then None
        else if Vec.length graph.nodes = 0 then None
        else Some graph
      end)
