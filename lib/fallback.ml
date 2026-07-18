(* Framed raw-source fallback for unrecognized (or too-wide) diagrams. Faithful
   port of upstream [fallback] and its [chunk_line]/[wrap_words]/[first_word]. *)

let too_wide_hint =
  "This diagram is too wide to display here \xe2\x80\x94 open the image to view it in full."

let repeat (s : string) (n : int) : string =
  if n <= 0 then "" else String.concat "" (List.init n (fun _ -> s))

let first_word (src : string) : string =
  match Ucore.first_whitespace_token src with Some w -> w | None -> "diagram"

let chunk_line (line : string) (limit : int option) : string list =
  match limit with
  | None -> [ line ]
  | Some limit ->
    if Ucore.width_string line <= limit then [ line ]
    else begin
      let out = ref [] and cur = Buffer.create 32 and cur_w = ref 0 in
      Array.iter
        (fun c ->
          let cw = max 1 (Ucore.width_uchar c) in
          if !cur_w + cw > limit && Buffer.length cur > 0 then begin
            out := Buffer.contents cur :: !out;
            Buffer.clear cur;
            cur_w := 0
          end;
          Uutf.Buffer.add_utf_8 cur c;
          cur_w := !cur_w + cw)
        (Ucore.to_uchars line);
      if Buffer.length cur > 0 then out := Buffer.contents cur :: !out;
      List.rev !out
    end

let wrap_words (text : string) (limit : int option) : string list =
  match limit with
  | None -> [ text ]
  | Some limit ->
    let lines = ref [] and cur = Buffer.create 32 in
    List.iter
      (fun word ->
        if word <> "" then
          if Buffer.length cur = 0 then Buffer.add_string cur word
          else if
            Ucore.width_string (Buffer.contents cur) + 1 + Ucore.width_string word <= limit
          then begin
            Buffer.add_char cur ' ';
            Buffer.add_string cur word
          end
          else begin
            lines := Buffer.contents cur :: !lines;
            Buffer.clear cur;
            Buffer.add_string cur word
          end)
      (String.split_on_char ' ' text);
    if Buffer.length cur > 0 then lines := Buffer.contents cur :: !lines;
    List.concat_map (fun l -> chunk_line l (Some limit)) (List.rev !lines)

let fallback (src : string) (max_width : int option) (too_wide : bool) : Style.t =
  let header = first_word src in
  let title = " mermaid: " ^ header ^ " " in
  let limit = match max_width with Some m -> Some (max 8 (Util.ssub m 4)) | None -> None in
  let body =
    let rec skip = function "" :: t -> skip t | l -> l in
    Parse_graph.lines src |> List.map Ucore.trim_end |> skip
    |> List.concat_map (fun l -> chunk_line l limit)
  in
  let title_w = Ucore.width_string title in
  let content_w = List.fold_left (fun a l -> max a (Ucore.width_string l)) title_w body in
  let inner = content_w + 2 in
  let plain = ref [] and styled = ref [] in
  let top = "╭" ^ title ^ repeat "─" (Util.ssub inner title_w) ^ "╮" in
  styled :=
    [ { Style.text = "╭"; cls = Style.Border }
    ; { Style.text = title; cls = Style.Title }
    ; { Style.text = repeat "─" (Util.ssub inner title_w) ^ "╮"; cls = Style.Border }
    ]
    :: !styled;
  plain := top :: !plain;
  List.iter
    (fun line ->
      let pad = Util.ssub content_w (Ucore.width_string line) in
      styled :=
        [ { Style.text = "│ "; cls = Style.Border }
        ; { Style.text = line; cls = Style.Text }
        ; { Style.text = repeat " " pad ^ " │"; cls = Style.Border }
        ]
        :: !styled;
      plain := ("│ " ^ line ^ repeat " " pad ^ " │") :: !plain)
    body;
  let bottom = "╰" ^ repeat "─" inner ^ "╯" in
  styled := [ { Style.text = bottom; cls = Style.Border } ] :: !styled;
  plain := bottom :: !plain;
  if too_wide then
    List.iter
      (fun chunk ->
        styled := [ { Style.text = chunk; cls = Style.Border } ] :: !styled;
        plain := chunk :: !plain)
      (wrap_words too_wide_hint max_width);
  { Style.styled_lines = List.rev !styled; plain_lines = List.rev !plain }
