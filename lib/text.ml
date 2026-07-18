(* Label text processing: HTML entity decoding, markdown/HTML stripping, and the
   [clean_label] pipeline. Faithful port of the corresponding upstream helpers.

   The quote/markdown delimiters (quotes, backtick, asterisk, underscore,
   ampersand, angle brackets) are all ASCII, whose bytes never occur inside a
   multi-byte UTF-8 sequence, so the byte-level string helpers here are safe
   alongside the scalar-level {!Ucore} ones. *)

let entity_lookahead = 10

(* Strict base-[base] parse; rejects anything Rust's [from_str_radix]/[parse]
   would (empty, non-digits) without OCaml's underscore/prefix leniency. *)
let parse_radix (s : string) (base : int) : int option =
  if String.length s = 0 then None
  else begin
    let ok = ref true and acc = ref 0 in
    String.iter
      (fun ch ->
        let d =
          match ch with
          | '0' .. '9' -> Char.code ch - Char.code '0'
          | 'a' .. 'f' -> 10 + Char.code ch - Char.code 'a'
          | 'A' .. 'F' -> 10 + Char.code ch - Char.code 'A'
          | _ -> base (* force out-of-range -> reject *)
        in
        if d >= base then ok := false else acc := (!acc * base) + d)
      s;
    if !ok then Some !acc else None
  end

let decode_entity_body (body : string) : Uchar.t option =
  match body with
  | "lt" -> Some (Uchar.of_char '<')
  | "gt" -> Some (Uchar.of_char '>')
  | "amp" -> Some (Uchar.of_char '&')
  | "quot" -> Some (Uchar.of_char '"')
  | "apos" -> Some (Uchar.of_char '\'')
  | _ ->
    if String.length body = 0 || body.[0] <> '#' then None
    else begin
      let num = String.sub body 1 (String.length body - 1) in
      let code =
        if String.length num > 0 && (num.[0] = 'x' || num.[0] = 'X') then
          parse_radix (String.sub num 1 (String.length num - 1)) 16
        else parse_radix num 10
      in
      match code with
      | Some code when Uchar.is_valid code ->
        let u = Uchar.of_int code in
        (* NUL collides with the CONT sentinel; ESC/other controls would inject
           into scrollback. Reject all Cc, matching char::is_control. *)
        if Ucore.is_control u then None else Some u
      | _ -> None
    end

let decode_html_entities (s : string) : string =
  if not (String.contains s '&') then s
  else begin
    let chars = Ucore.to_uchars s in
    let n = Array.length chars in
    let buf = Buffer.create (String.length s) in
    let i = ref 0 in
    while !i < n do
      if not (Ucore.is chars.(!i) '&') then begin
        Uutf.Buffer.add_utf_8 buf chars.(!i);
        incr i
      end
      else begin
        (* Scan window including the terminating ';'. *)
        let hi = min (!i + 1 + entity_lookahead) n in
        let semi = ref None and j = ref (!i + 1) in
        while !semi = None && !j < hi do
          if Ucore.is chars.(!j) ';' then semi := Some !j;
          incr j
        done;
        let decoded =
          match !semi with
          | Some j -> (
            match decode_entity_body (Ucore.sub_to_string chars (!i + 1) j) with
            | Some c -> Some (c, j)
            | None -> None)
          | None -> None
        in
        match decoded with
        (* Resume past ';'; a single pass never re-scans emitted text, so
           "&amp;lt;" yields the literal "&lt;", not "<". *)
        | Some (c, j) ->
          Uutf.Buffer.add_utf_8 buf c;
          i := j + 1
        | None ->
          Buffer.add_char buf '&';
          incr i
      end
    done;
    Buffer.contents buf
  end

(* Byte-level [replace_all]/[remove_char]: safe for the ASCII needles used here. *)
let remove_char (s : string) (ch : char) : string =
  let b = Buffer.create (String.length s) in
  String.iter (fun c -> if c <> ch then Buffer.add_char b c) s;
  Buffer.contents b

let replace_all (s : string) (sub : string) (rep : string) : string =
  let sl = String.length sub in
  if sl = 0 then s
  else begin
    let b = Buffer.create (String.length s) and n = String.length s and i = ref 0 in
    while !i < n do
      if !i + sl <= n && String.sub s !i sl = sub then begin
        Buffer.add_string b rep;
        i := !i + sl
      end
      else begin
        Buffer.add_char b s.[!i];
        incr i
      end
    done;
    Buffer.contents b
  end

let strip_markdown (s : string) : string =
  let no_code = remove_char s '`' in
  let no_strong = replace_all (replace_all no_code "**" "") "__" "" in
  let chars = Ucore.to_uchars no_strong in
  let n = Array.length chars in
  let buf = Buffer.create (String.length no_strong) in
  for i = 0 to n - 1 do
    let c = chars.(i) in
    let lone = Ucore.is c '*' || Ucore.is c '_' in
    let surrounded =
      i > 0
      && Ucore.is_alphanumeric chars.(i - 1)
      && i + 1 < n
      && Ucore.is_alphanumeric chars.(i + 1)
    in
    if lone && not surrounded then () else Uutf.Buffer.add_utf_8 buf c
  done;
  Ucore.trim (Buffer.contents buf)

let html_format_tags =
  [ "b"; "strong"; "i"; "em"; "u"; "s"; "strike"; "del"; "ins"; "mark"; "small"
  ; "big"; "sub"; "sup"; "code"; "kbd"; "samp"; "var"; "tt"; "span"; "font"; "q"
  ; "abbr"; "cite"; "pre" ]

(* [html_tag_at chars start] parses a tag opening at ['<'] (index [start]),
   returning its bare name and the index just past ['>']. *)
let html_tag_at (chars : Ucore.uarr) (start : int) : (string * int) option =
  let n = Array.length chars in
  let i = ref (start + 1) in
  (match Ucore.get chars !i with Some c when Ucore.is c '/' -> incr i | _ -> ());
  let name_start = !i in
  while !i < n && Ucore.is_ascii_alphanumeric chars.(!i) do incr i done;
  if !i = name_start then None
  else begin
    let name = Ucore.sub_to_string chars name_start !i in
    let aborted = ref false in
    while (not !aborted) && !i < n && not (Ucore.is chars.(!i) '>') do
      if Ucore.is chars.(!i) '<' then aborted := true else incr i
    done;
    if !aborted then None
    else
      match Ucore.get chars !i with
      | Some c when Ucore.is c '>' -> Some (name, !i + 1)
      | _ -> None
  end

let strip_html_tags (s : string) : string =
  let chars = Ucore.to_uchars s in
  let n = Array.length chars in
  let buf = Buffer.create (String.length s) in
  let i = ref 0 in
  while !i < n do
    let handled = ref false in
    (if Ucore.is chars.(!i) '<' then
       match html_tag_at chars !i with
       | Some (name, e) ->
         let lower = String.lowercase_ascii name in
         if lower = "br" then begin
           Buffer.add_char buf ' ';
           i := e;
           handled := true
         end
         else if List.mem lower html_format_tags then begin
           i := e;
           handled := true
         end
       | None -> ());
    if not !handled then begin
      Uutf.Buffer.add_utf_8 buf chars.(!i);
      incr i
    end
  done;
  Buffer.contents buf

(* [strip_quote t q]: Rust [t.strip_prefix(q).and_then(strip_suffix(q))] — both
   delimiters must be present (and distinct positions), stripping one each end. *)
let strip_quote (t : string) (q : char) : string option =
  let n = String.length t in
  if n >= 2 && t.[0] = q && t.[n - 1] = q then Some (String.sub t 1 (n - 2))
  else None

let clean_label (raw : string) : string =
  let stripped = strip_html_tags (Ucore.trim raw) in
  let trimmed = Ucore.trim stripped in
  let unquoted =
    match strip_quote trimmed '"' with
    | Some inner -> inner
    | None -> ( match strip_quote trimmed '\'' with Some inner -> inner | None -> trimmed)
  in
  let unquoted = Ucore.trim unquoted in
  let text =
    match strip_quote unquoted '`' with
    | Some md -> strip_markdown (Ucore.trim md)
    | None -> unquoted
  in
  (* Decode after tag-stripping so <b> is removed as markup while &lt;b&gt;
     survives as literal <b>; one decode at the single return covers both. *)
  decode_html_entities text
