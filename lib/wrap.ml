(* Label wrapping and truncation. Faithful port of upstream [wrap_label] and
   [fit_label]. *)

let ellipsis = "\xe2\x80\xa6" (* … *)

(* Width used for wrapping: at least 1 per glyph (so a zero-width scalar still
   advances), matching upstream's [char_width(c).max(1)]. *)
let char_w (c : Uchar.t) : int = max 1 (Ucore.width_uchar c)

let sum_char_w (s : string) : int =
  Array.fold_left (fun a c -> a + char_w c) 0 (Ucore.to_uchars s)

let rfind_break (s : string) : int option =
  let rec go i =
    if i < 0 then None
    else if List.mem s.[i] Const.label_break_chars then Some i
    else go (i - 1)
  in
  go (String.length s - 1)

(* Longest prefix of [s] whose width does not exceed [target]. *)
let truncate_to_width (s : string) (target : int) : string =
  let out = Buffer.create (String.length s) in
  let sw = ref 0 in
  (try
     Array.iter
       (fun c ->
         let cw = char_w c in
         if !sw + cw > target then raise Exit;
         Uutf.Buffer.add_utf_8 out c;
         sw := !sw + cw)
       (Ucore.to_uchars s)
   with Exit -> ());
  Buffer.contents out

let wrap_label (label : string) (width : int) (max_lines : int) : string list =
  let width = max 1 width in
  let lines_rev = ref [] in
  let cur = Buffer.create 32 and cur_w = ref 0 in
  let push_cur () =
    lines_rev := Buffer.contents cur :: !lines_rev;
    Buffer.clear cur;
    cur_w := 0
  in
  List.iter
    (fun word ->
      let ww = Ucore.width_string word in
      if ww > width then begin
        if Buffer.length cur > 0 then push_cur ();
        let chunk = Buffer.create 32 and chunk_w = ref 0 in
        Array.iter
          (fun ch ->
            let cw = char_w ch in
            if !chunk_w + cw > width && Buffer.length chunk > 0 then begin
              let s = Buffer.contents chunk in
              let head, carry =
                match rfind_break s with
                | Some p ->
                  ( String.sub s 0 (p + 1)
                  , String.sub s (p + 1) (String.length s - p - 1) )
                | None -> (s, "")
              in
              lines_rev := head :: !lines_rev;
              Buffer.clear chunk;
              Buffer.add_string chunk carry;
              chunk_w := sum_char_w carry
            end;
            Uutf.Buffer.add_utf_8 chunk ch;
            chunk_w := !chunk_w + cw)
          (Ucore.to_uchars word);
        Buffer.clear cur;
        Buffer.add_buffer cur chunk;
        cur_w := !chunk_w
      end
      else if Buffer.length cur = 0 then begin
        Buffer.add_string cur word;
        cur_w := ww
      end
      else if !cur_w + 1 + ww <= width then begin
        Buffer.add_char cur ' ';
        Buffer.add_string cur word;
        cur_w := !cur_w + 1 + ww
      end
      else begin
        push_cur ();
        Buffer.add_string cur word;
        cur_w := ww
      end)
    (Ucore.split_whitespace label);
  if Buffer.length cur > 0 then lines_rev := Buffer.contents cur :: !lines_rev;
  let lines = List.rev !lines_rev in
  let lines = if lines = [] then [ "" ] else lines in
  if List.length lines > max_lines then begin
    let kept = Util.take max_lines lines in
    match List.rev kept with
    | last :: rest_rev ->
      let target = max 1 (Util.ssub width 1) in
      let s = truncate_to_width last target ^ ellipsis in
      List.rev (s :: rest_rev)
    | [] -> kept
  end
  else lines

let fit_label (label : string) (inner : int) : string =
  if Ucore.width_string label <= inner then label
  else begin
    let out = Buffer.create (String.length label) in
    let used = ref 0 in
    (try
       Array.iter
         (fun c ->
           let cw = Ucore.width_uchar c in
           if !used + cw + 1 > inner then raise Exit;
           Uutf.Buffer.add_utf_8 out c;
           used := !used + cw)
         (Ucore.to_uchars label)
     with Exit -> ());
    Buffer.contents out ^ ellipsis
  end
