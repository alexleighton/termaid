(* Sequence-diagram layout. Places participant boxes with lifelines and paints
   messages, notes, and dividers between them. Faithful port of upstream
   [layout_sequence] / [note_geometry]. Reuses {!Layout.draw_box} and the
   {!Canvas} primitives. *)

open Parse_sequence

let ceil_div (a : int) (b : int) : int = (a + b - 1) / b

let note_geometry (xs : int array) (anchor : note_anchor) (text_w : int) : int * int =
  match anchor with
  | Over (l, r) ->
    let center = (xs.(l) + xs.(r)) / 2 in
    let w = max (xs.(r) - xs.(l) + 5) (text_w + (2 * Const.pad) + 2) in
    (Util.ssub center (w / 2), w)
  | Left i ->
    let w = text_w + (2 * Const.pad) + 2 in
    (Util.ssub xs.(i) (2 + w - 1), w)
  | Right i -> (xs.(i) + 2, text_w + (2 * Const.pad) + 2)

let dash = Canvas.uc "╌"
let solid = Canvas.uc "─"

let layout_sequence (seq : sequence) (max_width : int option)
  : (Style.t, Layout.oversize) result =
  let n = Vec.length seq.labels in
  let labels = Array.init n (fun i -> Wrap.fit_label (Vec.get seq.labels i) Const.wrap_width) in
  let box_w = Array.map (fun l -> max 1 (Ucore.width_string l) + (2 * Const.pad) + 2) labels in
  let box_h = 3 in
  let item_text_w = function Some t -> Ucore.width_string t | None -> 0 in
  let items = Array.of_list (Vec.to_list seq.items) in
  let gaps =
    Array.init (max 0 (n - 1)) (fun i ->
      max Const.seq_gap (ceil_div box_w.(i) 2 + ceil_div box_w.(i + 1) 2 + 1))
  in
  (* Widen gaps so every message/note span fits, widest span applied last. *)
  let reqs = ref [] in
  Array.iter
    (fun item ->
      match item with
      | Message { from_; to_; text; _ } ->
        let tw = item_text_w text in
        if from_ <> to_ then
          reqs := (min from_ to_, max from_ to_, max (tw + 2) 4) :: !reqs
        else if from_ + 1 < n then reqs := (from_, from_ + 1, 5 + tw + 2) :: !reqs
      | Note { anchor; text } -> (
        let tw = Ucore.width_string text in
        match anchor with
        | Over (l, r) when l < r -> reqs := (l, r, Util.ssub tw 1) :: !reqs
        | Over (i, _) ->
          let half = ceil_div (tw + 4) 2 + 2 in
          if i > 0 then reqs := (i - 1, i, half) :: !reqs;
          if i + 1 < n then reqs := (i, i + 1, half) :: !reqs
        | Left i when i > 0 -> reqs := (i - 1, i, tw + 7) :: !reqs
        | Right i when i + 1 < n -> reqs := (i, i + 1, tw + 7) :: !reqs
        | _ -> ())
      | Divider _ -> ())
    items;
  let reqs =
    List.stable_sort
      (fun (l1, r1, _) (l2, r2, _) -> compare (r1 - l1) (r2 - l2))
      (List.rev !reqs)
  in
  List.iter
    (fun (l, r, need) ->
      let cur = ref 0 in
      for i = l to r - 1 do cur := !cur + gaps.(i) done;
      if !cur < need then gaps.(r - 1) <- gaps.(r - 1) + (need - !cur))
    reqs;
  let xs = Array.make n 0 in
  xs.(0) <- box_w.(0) / 2;
  for i = 1 to n - 1 do xs.(i) <- xs.(i - 1) + gaps.(i - 1) done;
  let canvas_w = ref (xs.(n - 1) + ceil_div box_w.(n - 1) 2 + 1) in
  Array.iter
    (fun item ->
      match item with
      | Message { from_; to_; text; _ } when from_ = to_ ->
        canvas_w := max !canvas_w (xs.(from_) + 5 + item_text_w text + 1)
      | Note { anchor; text } ->
        let x, w = note_geometry xs anchor (Ucore.width_string text) in
        canvas_w := max !canvas_w (x + w + 1)
      | Divider { text } -> canvas_w := max !canvas_w (Ucore.width_string text + 4)
      | _ -> ())
    items;
  let canvas_w = !canvas_w in
  let rows = Array.make (Array.length items) 0 in
  let y = ref (box_h + 1) in
  Array.iteri
    (fun i item ->
      rows.(i) <- !y;
      y
      := !y
         +
         match item with
         | Message { from_; to_; text; _ } ->
           if from_ = to_ then 4 else if text <> None then 3 else 2
         | Note _ -> 4
         | Divider _ -> 2)
    items;
  let bottom_top = !y in
  let canvas_h = bottom_top + box_h in
  let too_wide = match max_width with Some mw -> canvas_w > mw | None -> false in
  if too_wide then Error Layout.Width
  else if canvas_w * canvas_h > Const.max_canvas_cells then Error Layout.Cells
  else begin
    let cv = Canvas.create canvas_w canvas_h in
    for i = 0 to n - 1 do
      List.iter
        (fun by ->
          let p =
            { Layout.x = Util.ssub xs.(i) (box_w.(i) / 2)
            ; y = by
            ; w = box_w.(i)
            ; h = box_h
            ; cx = xs.(i)
            ; cy = by + 1
            ; rank = 0
            }
          in
          Layout.draw_box cv p [ labels.(i) ] Ir.Rect)
        [ 0; bottom_top ]
    done;
    Array.iteri
      (fun idx item ->
        match item with
        | Note { anchor; text } ->
          let x, w = note_geometry xs anchor (Ucore.width_string text) in
          let r = rows.(idx) in
          let p = { Layout.x; y = r; w; h = 3; cx = x + (w / 2); cy = r + 1; rank = 0 } in
          Layout.draw_box cv p [ text ] Ir.Rect
        | _ -> ())
      items;
    Array.iter
      (fun x ->
        Canvas.junction cv x (box_h - 1) Canvas.d;
        Canvas.seg_v cv x box_h (bottom_top - 1);
        Canvas.junction cv x bottom_top Canvas.u)
      xs;
    Array.iteri
      (fun idx item ->
        let r = rows.(idx) in
        match item with
        | Message { from_; to_; text; dashed; head } ->
          let line_ch = if dashed then dash else solid in
          if from_ = to_ then begin
            let x = xs.(from_) in
            Canvas.junction cv x r Canvas.r;
            Canvas.set cv (x + 1) r line_ch Style.Edge;
            Canvas.set cv (x + 2) r line_ch Style.Edge;
            Canvas.set cv (x + 3) r (Canvas.uc "╮") Style.Edge;
            Canvas.set cv (x + 3) (r + 1) (Canvas.uc "│") Style.Edge;
            Canvas.set cv (x + 1) (r + 2)
              (if head = Cross then Canvas.uc "×" else Canvas.uc "◄")
              Style.Edge;
            Canvas.set cv (x + 2) (r + 2) line_ch Style.Edge;
            Canvas.set cv (x + 3) (r + 2) (Canvas.uc "╯") Style.Edge;
            match text with
            | Some t -> Canvas.draw_seq_text cv t (x + 5) (r + 1) Style.Text
            | None -> ()
          end
          else begin
            let x0 = xs.(from_) and x1 = xs.(to_) in
            let rightward = x1 > x0 in
            let arrow_row = if text <> None then r + 1 else r in
            let lo = min x0 x1 and hi = max x0 x1 in
            Canvas.junction cv x0 arrow_row (if rightward then Canvas.r else Canvas.l);
            for x = lo + 1 to hi - 1 do
              Canvas.set cv x arrow_row line_ch Style.Edge
            done;
            let head_ch =
              match (head, rightward) with
              | Cross, _ -> Canvas.uc "×"
              | Arrow, true -> Canvas.uc "▶"
              | Arrow, false -> Canvas.uc "◄"
            in
            let head_x = if rightward then x1 - 1 else x1 + 1 in
            Canvas.set cv head_x arrow_row head_ch Style.Edge;
            match text with
            | Some t ->
              let span = hi - lo - 1 in
              let t = Wrap.fit_label t (max 1 span) in
              let tx = lo + 1 + (Util.ssub span (Ucore.width_string t) / 2) in
              Canvas.draw_seq_text cv t tx r Style.Text
            | None -> ()
          end
        | Divider { text } ->
          for x = 0 to canvas_w - 1 do
            Canvas.set cv x r (Canvas.uc "─") Style.Edge
          done;
          let t = Wrap.fit_label text (Util.ssub canvas_w 4) in
          Canvas.draw_seq_text cv (" " ^ t ^ " ") 2 r Style.Edge_label
        | Note _ -> ())
      items;
    Canvas.finalize_mask cv;
    let styled_lines, plain_lines = Canvas.to_lines cv in
    Ok { Style.styled_lines; plain_lines }
  end
