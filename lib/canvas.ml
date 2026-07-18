(* Character grid with box-drawing junction accumulation. Faithful port of the
   upstream [Canvas]. Cells carry a glyph, a semantic {!Style.cls}, an
   accumulating junction bit-mask (U/D/L/R), a line-style mask, and an
   occupancy flag (occupied cells reject edge bits). [finalize_mask] resolves
   accumulated masks into box-drawing glyphs. *)

(* Junction direction bits. *)
let u = 1
let d = 2
let l = 4
let r = 8

(* Line-style bits (edge kind). *)
let sty_dot = 1
let sty_thick = 2
let sty_solid = 4

type t =
  { w : int
  ; h : int
  ; ch : Uchar.t array
  ; cls : Style.cls array
  ; mask : int array
  ; style : int array
  ; occupied : bool array
  ; mutable cur_style : int
  }

let space = Uchar.of_char ' '

let create (w : int) (h : int) : t =
  let n = w * h in
  { w
  ; h
  ; ch = Array.make n space
  ; cls = Array.make n Style.Empty
  ; mask = Array.make n 0
  ; style = Array.make n 0
  ; occupied = Array.make n false
  ; cur_style = sty_solid
  }

let idx (cv : t) (x : int) (y : int) : int = (y * cv.w) + x

let set (cv : t) (x : int) (y : int) (c : Uchar.t) (cls : Style.cls) : unit =
  if x < cv.w && y < cv.h then begin
    let i = idx cv x y in
    cv.ch.(i) <- c;
    cv.cls.(i) <- cls
  end

let add_bits (cv : t) (x : int) (y : int) (bits : int) : unit =
  if x < cv.w && y < cv.h then begin
    let i = idx cv x y in
    if not cv.occupied.(i) then begin
      cv.mask.(i) <- cv.mask.(i) lor bits;
      cv.style.(i) <- cv.style.(i) lor cv.cur_style;
      if cv.cls.(i) <> Style.Border then cv.cls.(i) <- Style.Edge
    end
  end

let blit (cv : t) (sub : t) (ox : int) (oy : int) : unit =
  for sy = 0 to sub.h - 1 do
    for sx = 0 to sub.w - 1 do
      let x = ox + sx and y = oy + sy in
      if x < cv.w && y < cv.h then begin
        let si = idx sub sx sy and di = idx cv x y in
        cv.ch.(di) <- sub.ch.(si);
        cv.cls.(di) <- sub.cls.(si);
        cv.style.(di) <- sub.style.(si);
        cv.occupied.(di) <- true
      end
    done
  done

let junction (cv : t) (x : int) (y : int) (bits : int) : unit =
  if x < cv.w && y < cv.h then begin
    let i = idx cv x y in
    cv.mask.(i) <- cv.mask.(i) lor bits;
    if cv.cls.(i) <> Style.Border then cv.cls.(i) <- Style.Edge
  end

let seg_v (cv : t) (x : int) (y0 : int) (y1 : int) : unit =
  let a = min y0 y1 and b = max y0 y1 in
  for y = a to b do
    let bits = (if y > a then u else 0) lor (if y < b then d else 0) in
    add_bits cv x y bits
  done

let seg_h (cv : t) (y : int) (x0 : int) (x1 : int) : unit =
  let a = min x0 x1 and b = max x0 x1 in
  for x = a to b do
    let bits = (if x > a then l else 0) lor (if x < b then r else 0) in
    add_bits cv x y bits
  done

(* --- glyph tables --- *)

let uc (s : string) : Uchar.t = (Ucore.to_uchars s).(0)

let mask_char (m : int) : Uchar.t =
  if m = 0 then space
  else if m = u || m = d || m = u lor d then uc "\xe2\x94\x82" (* │ *)
  else if m = l || m = r || m = l lor r then uc "\xe2\x94\x80" (* ─ *)
  else if m = d lor r then uc "\xe2\x94\x8c" (* ┌ *)
  else if m = d lor l then uc "\xe2\x94\x90" (* ┐ *)
  else if m = u lor r then uc "\xe2\x94\x94" (* └ *)
  else if m = u lor l then uc "\xe2\x94\x98" (* ┘ *)
  else if m = u lor d lor r then uc "\xe2\x94\x9c" (* ├ *)
  else if m = u lor d lor l then uc "\xe2\x94\xa4" (* ┤ *)
  else if m = d lor l lor r then uc "\xe2\x94\xac" (* ┬ *)
  else if m = u lor l lor r then uc "\xe2\x94\xb4" (* ┴ *)
  else uc "\xe2\x94\xbc" (* ┼ *)

let make_map (pairs : (string * string) list) : Uchar.t -> Uchar.t =
  let h = Hashtbl.create 32 in
  List.iter (fun (a, b) -> Hashtbl.replace h (uc a) (uc b)) pairs;
  fun c -> match Hashtbl.find_opt h c with Some d -> d | None -> c

let dotted_char = make_map [ ("─", "╌"); ("│", "╎") ]

let thick_char =
  make_map
    [ ("─", "━"); ("│", "┃"); ("┌", "┏"); ("┐", "┓"); ("└", "┗"); ("┘", "┛")
    ; ("├", "┣"); ("┤", "┫"); ("┬", "┳"); ("┴", "┻"); ("┼", "╋") ]

let flip_glyph_v =
  make_map
    [ ("┌", "└"); ("└", "┌"); ("┐", "┘"); ("┘", "┐"); ("┏", "┗"); ("┗", "┏")
    ; ("┓", "┛"); ("┛", "┓"); ("╭", "╰"); ("╰", "╭"); ("╮", "╯"); ("╯", "╮")
    ; ("┬", "┴"); ("┴", "┬"); ("┳", "┻"); ("┻", "┳"); ("▼", "▲"); ("▲", "▼")
    ; ("▽", "△"); ("△", "▽") ]

let flip_glyph_h =
  make_map
    [ ("┌", "┐"); ("┐", "┌"); ("└", "┘"); ("┘", "└"); ("┏", "┓"); ("┓", "┏")
    ; ("┗", "┛"); ("┛", "┗"); ("╭", "╮"); ("╮", "╭"); ("╰", "╯"); ("╯", "╰")
    ; ("├", "┤"); ("┤", "├"); ("┣", "┫"); ("┫", "┣"); ("▶", "◄"); ("◄", "▶")
    ; ("▷", "◁"); ("◁", "▷") ]

let finalize_mask (cv : t) : unit =
  for i = 0 to Array.length cv.ch - 1 do
    if cv.mask.(i) <> 0 && Uchar.equal cv.ch.(i) space then begin
      let c = mask_char cv.mask.(i) in
      cv.ch.(i)
      <- (if cv.style.(i) = sty_dot then dotted_char c
          else if cv.style.(i) = sty_thick then thick_char c
          else c)
    end
  done

let reverse_sub (a : 'a array) (start : int) (stop : int) : unit =
  let i = ref start and j = ref (stop - 1) in
  while !i < !j do
    let t = a.(!i) in
    a.(!i) <- a.(!j);
    a.(!j) <- t;
    incr i;
    decr j
  done

let swap (a : 'a array) (i : int) (j : int) : unit =
  let t = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- t

let flip_vertical (cv : t) : unit =
  for y = 0 to (cv.h / 2) - 1 do
    let y2 = cv.h - 1 - y in
    for x = 0 to cv.w - 1 do
      let i = idx cv x y and j = idx cv x y2 in
      swap cv.ch i j;
      swap cv.cls i j
    done
  done;
  for i = 0 to Array.length cv.ch - 1 do
    cv.ch.(i) <- flip_glyph_v cv.ch.(i)
  done

let flip_horizontal (cv : t) : unit =
  for y = 0 to cv.h - 1 do
    for x = 0 to (cv.w / 2) - 1 do
      let x2 = cv.w - 1 - x in
      let i = idx cv x y and j = idx cv x2 y in
      swap cv.ch i j;
      swap cv.cls i j
    done
  done;
  for i = 0 to Array.length cv.ch - 1 do
    cv.ch.(i) <- flip_glyph_h cv.ch.(i)
  done;
  for y = 0 to cv.h - 1 do
    let x = ref 0 in
    while !x < cv.w do
      let cls = cv.cls.(idx cv !x y) in
      if cls = Style.Text || cls = Style.Edge_label then begin
        let start = idx cv !x y in
        while !x < cv.w && cv.cls.(idx cv !x y) = cls do incr x done;
        reverse_sub cv.ch start (idx cv !x y)
      end
      else incr x
    done
  done

let cont = Const.cont

let to_lines (cv : t) : Style.line list * string list =
  let styled = ref [] and plain = ref [] in
  for y = 0 to cv.h - 1 do
    let last = ref cv.w in
    (try
       for x = cv.w - 1 downto 0 do
         let c = cv.ch.(idx cv x y) in
         if (not (Uchar.equal c space)) && not (Uchar.equal c cont) then begin
           last := x + 1;
           raise Exit
         end
       done
     with Exit -> ());
    let spans = ref [] in
    let plain_row = Buffer.create cv.w in
    let run = Buffer.create 16 in
    let run_cls = ref Style.Empty in
    for x = 0 to !last - 1 do
      let i = idx cv x y in
      let c = cv.ch.(i) in
      if not (Uchar.equal c cont) then begin
        let cls = cv.cls.(i) in
        Uutf.Buffer.add_utf_8 plain_row c;
        if cls <> !run_cls && Buffer.length run > 0 then begin
          spans := { Style.text = Buffer.contents run; cls = !run_cls } :: !spans;
          Buffer.clear run
        end;
        run_cls := cls;
        Uutf.Buffer.add_utf_8 run c
      end
    done;
    if Buffer.length run > 0 then
      spans := { Style.text = Buffer.contents run; cls = !run_cls } :: !spans;
    styled := List.rev !spans :: !styled;
    plain := Ucore.trim_end (Buffer.contents plain_row) :: !plain
  done;
  (List.rev !styled, List.rev !plain)

(* Draw [text] at (x, y), clearing any junction mask under it (used for titles
   and compartment text painted over borders). *)
let draw_seq_text (cv : t) (text : string) (x : int) (y : int) (cls : Style.cls) : unit =
  let chars = Ucore.to_uchars text in
  let cur = ref x in
  Array.iter
    (fun c ->
      let cw = max 1 (Ucore.width_uchar c) in
      for k = 0 to cw - 1 do
        if !cur + k < cv.w && y < cv.h then cv.mask.(idx cv (!cur + k) y) <- 0;
        set cv (!cur + k) y (if k = 0 then c else cont) cls
      done;
      cur := !cur + cw)
    chars
