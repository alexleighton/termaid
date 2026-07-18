(* Flowchart layout: node sizing, placement (via {!Rank}), box drawing, and edge
   routing onto a {!Canvas}. Faithful port of the upstream layout functions. *)

type oversize =
  | Width
  | Cells

type placed =
  { x : int
  ; y : int
  ; w : int
  ; h : int
  ; cx : int
  ; cy : int
  ; rank : int
  }

let placed0 = { x = 0; y = 0; w = 0; h = 0; cx = 0; cy = 0; rank = 0 }

type node_sizes =
  { box_w : int array
  ; box_h : int array
  ; lay_w : int array
  ; lay_h : int array
  ; extra_h : int array
  ; self_label_w : int array
  }

type node_extra =
  | Plain
  | Frame of Canvas.t
  | Compartments of string list list

type route_plan =
  { canvas : int * int
  ; band_end : int array
  ; edge_bus : int array
  ; lane_base : int
  ; edge_lane : int array
  }

let uc = Canvas.uc
let arr_down = uc "▼"
let arr_up = uc "▲"
let arr_left = uc "◄"
let arr_right = uc "▶"

let head_glyph (head : Ir.head) (arrow : Uchar.t) : Uchar.t =
  match head with
  | Ir.Circle -> uc "o"
  | Ir.Cross -> uc "×"
  | Ir.Diamond_fill -> uc "◆"
  | Ir.Diamond_open -> uc "◇"
  | Ir.Triangle ->
    if Uchar.equal arrow arr_down then uc "▽"
    else if Uchar.equal arrow arr_up then uc "△"
    else if Uchar.equal arrow arr_left then uc "◁"
    else if Uchar.equal arrow arr_right then uc "▷"
    else arrow
  | _ -> arrow

let place_label (cv : Canvas.t) (label : string) (row : int) (start_x : int) : unit =
  if row < cv.Canvas.h then begin
    let text = Wrap.fit_label label Const.max_label in
    let x = ref start_x in
    (try
       Array.iter
         (fun c ->
           let cw = max 1 (Ucore.width_uchar c) in
           if !x + cw > cv.Canvas.w then raise Exit;
           let blocked = ref false in
           for k = 0 to cw - 1 do
             let i = Canvas.idx cv (!x + k) row in
             if
               (not (Uchar.equal cv.Canvas.ch.(i) Canvas.space))
               || cv.Canvas.mask.(i) <> 0 || cv.Canvas.occupied.(i)
             then blocked := true
           done;
           if !blocked then raise Exit;
           Canvas.set cv !x row c Style.Edge_label;
           for k = 1 to cw - 1 do
             Canvas.set cv (!x + k) row Const.cont Style.Edge_label
           done;
           x := !x + cw)
         (Ucore.to_uchars text)
     with Exit -> ())
  end

let draw_box (cv : Canvas.t) (p : placed) (lines : string list) (shape : Ir.shape) : unit =
  let x = p.x and y = p.y and w = p.w and h = p.h in
  let right = x + w - 1 and bottom = y + h - 1 in
  let tl, tr, bl, br =
    match shape with
    | Ir.Round | Ir.Diamond -> (uc "╭", uc "╮", uc "╰", uc "╯")
    | Ir.Rect -> (uc "┌", uc "┐", uc "└", uc "┘")
  in
  Canvas.set cv x y tl Style.Border;
  Canvas.set cv right y tr Style.Border;
  Canvas.set cv x bottom bl Style.Border;
  Canvas.set cv right bottom br Style.Border;
  for cx = x + 1 to right - 1 do
    Canvas.add_bits cv cx y (Canvas.l lor Canvas.r);
    Canvas.add_bits cv cx bottom (Canvas.l lor Canvas.r)
  done;
  for cy = y + 1 to bottom - 1 do
    Canvas.add_bits cv x cy (Canvas.u lor Canvas.d);
    Canvas.add_bits cv right cy (Canvas.u lor Canvas.d)
  done;
  for cy = y to bottom do
    for cx = x to right do
      cv.Canvas.occupied.(Canvas.idx cv cx cy) <- true
    done
  done;
  let inner = max 1 (Util.ssub w ((2 * Const.pad) + 2)) in
  List.iteri
    (fun li line ->
      let row = y + 1 + li in
      let text = Wrap.fit_label line inner in
      let tw = Ucore.width_string text in
      let text_x = x + 1 + Const.pad + (Util.ssub inner tw / 2) in
      let cur = ref text_x in
      Array.iter
        (fun c ->
          let cw = max 1 (Ucore.width_uchar c) in
          Canvas.set cv !cur row c Style.Text;
          for k = 1 to cw - 1 do
            Canvas.set cv (!cur + k) row Const.cont Style.Text
          done;
          cur := !cur + cw)
        (Ucore.to_uchars text))
    lines

let draw_frame (cv : Canvas.t) (p : placed) (title : string) (sub : Canvas.t) : unit =
  draw_box cv p [] Ir.Rect;
  let t = Wrap.fit_label title (Util.ssub p.w 4) in
  Canvas.draw_seq_text cv (" " ^ t ^ " ") (p.x + 1) p.y Style.Text;
  let ox = p.x + 1 + ((p.w - 2 - sub.Canvas.w) / 2) in
  let oy = p.y + 1 + ((p.h - 2 - sub.Canvas.h) / 2) in
  Canvas.blit cv sub ox oy

let draw_class_box (cv : Canvas.t) (p : placed) (sections : string list list) : unit =
  draw_box cv p [] Ir.Rect;
  let inner = max 1 (Util.ssub p.w ((2 * Const.pad) + 2)) in
  let row = ref (p.y + 1) and first = ref true in
  List.iteri
    (fun si section ->
      if section <> [] then begin
        if not !first then begin
          Canvas.set cv p.x !row (uc "├") Style.Border;
          for x = p.x + 1 to p.x + p.w - 2 do
            Canvas.set cv x !row (uc "─") Style.Border
          done;
          Canvas.set cv (p.x + p.w - 1) !row (uc "┤") Style.Border;
          incr row
        end;
        first := false;
        List.iter
          (fun line ->
            let text = Wrap.fit_label line inner in
            let tx =
              if si = 0 then
                p.x + 1 + Const.pad + (Util.ssub inner (Ucore.width_string text) / 2)
              else p.x + 1 + Const.pad
            in
            Canvas.draw_seq_text cv text tx !row Style.Text;
            incr row)
          section
      end)
    sections

(* --- edge routing --- *)

let route_forward (cv : Canvas.t) (from : placed) (to_ : placed) (edge : Ir.edge)
  (bus : int) : unit =
  let tx = to_.cx in
  let bx = if Util.adiff from.cx tx <= 1 then tx else from.cx in
  let by = from.y + from.h - 1 in
  let head_row = to_.y - 1 in
  Canvas.junction cv bx by Canvas.d;
  Canvas.seg_v cv bx by bus;
  if bx = tx then Canvas.seg_v cv bx bus head_row
  else begin
    Canvas.seg_h cv bus bx tx;
    Canvas.seg_v cv tx bus head_row
  end;
  if edge.head_to = Ir.No_head then Canvas.add_bits cv tx head_row Canvas.u
  else Canvas.set cv tx head_row (head_glyph edge.head_to arr_down) Style.Edge;
  if edge.head_from <> Ir.No_head then
    Canvas.set cv bx by (head_glyph edge.head_from arr_up) Style.Edge;
  match edge.label with Some label -> place_label cv label head_row (tx + 1) | None -> ()

let route_self (cv : Canvas.t) (p : placed) (edge : Ir.edge) : unit =
  let bottom = p.y + p.h - 1 in
  let exit_x = p.cx + 1 in
  let ret_x = p.x + p.w - 2 in
  if ret_x <= exit_x || bottom + 2 >= cv.Canvas.h then ()
  else begin
    let v, hh, bl, br =
      match edge.line with
      | Ir.Dotted -> (uc "╎", uc "╌", uc "╰", uc "╯")
      | Ir.Thick -> (uc "┃", uc "━", uc "┗", uc "┛")
      | Ir.Solid -> (uc "│", uc "─", uc "╰", uc "╯")
    in
    Canvas.junction cv exit_x bottom Canvas.d;
    Canvas.set cv exit_x (bottom + 1) v Style.Edge;
    Canvas.set cv exit_x (bottom + 2) bl Style.Edge;
    for x = exit_x + 1 to ret_x - 1 do
      Canvas.set cv x (bottom + 2) hh Style.Edge
    done;
    Canvas.set cv ret_x (bottom + 2) br Style.Edge;
    Canvas.set cv ret_x (bottom + 1) (head_glyph edge.head_to arr_up) Style.Edge;
    match edge.label with
    | Some label -> place_label cv label (bottom + 1) (p.x + p.w + 1)
    | None -> ()
  end

let route_back (cv : Canvas.t) (from : placed) (to_ : placed) (edge : Ir.edge)
  (lane_x : int) : unit =
  let sx = from.x + from.w - 1 in
  let sy = from.cy in
  let tx = to_.x + to_.w - 1 in
  let tyc = to_.cy in
  Canvas.junction cv sx sy Canvas.r;
  Canvas.seg_h cv sy sx lane_x;
  Canvas.seg_v cv lane_x sy tyc;
  Canvas.seg_h cv tyc (tx + 1) lane_x;
  if edge.head_to = Ir.No_head then Canvas.add_bits cv (tx + 1) tyc Canvas.r
  else Canvas.set cv (tx + 1) tyc (head_glyph edge.head_to arr_left) Style.Edge;
  if edge.head_from <> Ir.No_head then
    Canvas.set cv sx sy (head_glyph edge.head_from arr_left) Style.Edge;
  match edge.label with
  | Some label ->
    place_label cv label (Util.ssub tyc 1) (Util.ssub lane_x (Ucore.width_string label + 1))
  | None -> ()

let route_forward_lr (cv : Canvas.t) (from : placed) (to_ : placed) (edge : Ir.edge)
  (bus : int) : unit =
  let rx = from.x + from.w - 1 in
  let ry = from.cy in
  let ly = to_.cy in
  let head_col = to_.x - 1 in
  Canvas.junction cv rx ry Canvas.r;
  Canvas.seg_h cv ry rx bus;
  if ry = ly then Canvas.seg_h cv ry bus head_col
  else begin
    Canvas.seg_v cv bus ry ly;
    Canvas.seg_h cv ly bus head_col
  end;
  if edge.head_to = Ir.No_head then Canvas.add_bits cv head_col ly Canvas.r
  else Canvas.set cv head_col ly (head_glyph edge.head_to arr_right) Style.Edge;
  if edge.head_from <> Ir.No_head then
    Canvas.set cv rx ry (head_glyph edge.head_from arr_left) Style.Edge;
  match edge.label with Some label -> place_label cv label (Util.ssub ly 1) (bus + 1) | None -> ()

let route_back_lr (cv : Canvas.t) (from : placed) (to_ : placed) (edge : Ir.edge)
  (lane_y : int) : unit =
  let sx = from.cx in
  let sy = from.y + from.h - 1 in
  let tx = to_.cx in
  let ty = to_.y + to_.h - 1 in
  Canvas.junction cv sx sy Canvas.d;
  Canvas.seg_v cv sx sy lane_y;
  Canvas.seg_h cv lane_y sx tx;
  Canvas.seg_v cv tx lane_y (ty + 1);
  if edge.head_to = Ir.No_head then Canvas.add_bits cv tx (ty + 1) Canvas.d
  else Canvas.set cv tx (ty + 1) (head_glyph edge.head_to arr_up) Style.Edge;
  if edge.head_from <> Ir.No_head then
    Canvas.set cv sx sy (head_glyph edge.head_from arr_up) Style.Edge;
  match edge.label with
  | Some label -> place_label cv label (Util.ssub lane_y 1) ((sx + tx) / 2)
  | None -> ()

(* --- placement --- *)

let bus_spans_td (edges : Ir.edge array) (ranks : int array) (centers : int array)
  (r : int) (exact : bool) : (int * int * int * int * int) list =
  let out = ref [] in
  Array.iteri
    (fun i e ->
      let jogs =
        if exact then centers.(e.Ir.from_) <> centers.(e.to_)
        else Util.adiff centers.(e.from_) centers.(e.to_) > 1
      in
      if e.from_ <> e.to_ && ranks.(e.from_) = r && ranks.(e.to_) = r + 1 && jogs
      then begin
        let a = min centers.(e.from_) centers.(e.to_) in
        let b = max centers.(e.from_) centers.(e.to_) in
        out := (a, b, e.from_, e.to_, i) :: !out
      end)
    edges;
  List.rev !out

let lane_spans (edges : Ir.edge array) (ranks : int array) (placed : placed array)
  (vertical : bool) : (int * int * int * int * int) list =
  let out = ref [] in
  Array.iteri
    (fun i e ->
      if e.Ir.from_ <> e.to_ && ranks.(e.to_) <> ranks.(e.from_) + 1 then begin
        let pf = placed.(e.from_) and pt = placed.(e.to_) in
        let a, b =
          if vertical then (min pf.cy pt.cy, max pf.cy pt.cy)
          else (min pf.cx pt.cx, max pf.cx pt.cx)
        in
        out := (a, b, e.from_, e.to_, i) :: !out
      end)
    edges;
  List.rev !out

let place_td (ranks : int array) (max_rank : int) (by_rank : int array array)
  (sizes : node_sizes) (edges : Ir.edge array) (placed : placed array) : route_plan =
  let centers = Rank.assign_positions by_rank sizes.lay_w Const.gap_x edges ranks in
  let ne = Array.length edges in
  let edge_bus = Array.make ne 0 in
  let bus_tracks = Array.make (max_rank + 1) 0 in
  for r = 0 to max_rank - 1 do
    let spans = bus_spans_td edges ranks centers r false in
    if spans <> [] then begin
      let assigned, count = Rank.assign_tracks spans in
      List.iter (fun (idx, slot) -> edge_bus.(idx) <- slot) assigned;
      bus_tracks.(r) <- count
    end
  done;
  let rank_h =
    Array.map
      (fun row ->
        if Array.length row = 0 then 3
        else
          Array.fold_left (fun a i -> max a (sizes.box_h.(i) + sizes.extra_h.(i))) 0 row)
      by_rank
  in
  let rank_y = Array.make (max_rank + 1) 0 in
  for r = 1 to max_rank do
    let gap = max Const.gap_y (bus_tracks.(r - 1) + 1) in
    rank_y.(r) <- rank_y.(r - 1) + rank_h.(r - 1) + gap
  done;
  let canvas_h = rank_y.(max_rank) + rank_h.(max_rank) in
  let band_end = Array.init (max_rank + 1) (fun r -> rank_y.(r) + rank_h.(r)) in
  let diagram_w = ref 1 in
  Array.iteri
    (fun r row ->
      Array.iter
        (fun idx ->
          let w = sizes.box_w.(idx) and h = sizes.box_h.(idx) in
          let cx = centers.(idx) in
          let x = Util.ssub cx (w / 2) in
          let y = rank_y.(r) + ((rank_h.(r) - h - sizes.extra_h.(idx)) / 2) in
          placed.(idx) <- { x; y; w; h; cx; cy = y + (h / 2); rank = r };
          diagram_w := max !diagram_w (x + w);
          if sizes.extra_h.(idx) > 0 && sizes.self_label_w.(idx) > 0 then
            diagram_w := max !diagram_w (x + w + 2 + sizes.self_label_w.(idx)))
        row)
    by_rank;
  let content_w = ref !diagram_w in
  Array.iter
    (fun e ->
      if e.Ir.from_ <> e.to_ then
        match e.label with
        | Some label ->
          let lw = min (Ucore.width_string label) Const.max_label in
          if ranks.(e.to_) = ranks.(e.from_) + 1 then
            content_w := max !content_w (placed.(e.to_).cx + 2 + lw)
          else content_w := max !content_w (!diagram_w + lw + 1)
        | None -> ())
    edges;
  let edge_lane = Array.make ne 0 in
  let lanes = lane_spans edges ranks placed true in
  let canvas_w, lane_base =
    if lanes = [] then (!content_w, 0)
    else begin
      let assigned, count = Rank.assign_tracks lanes in
      List.iter (fun (idx, slot) -> edge_lane.(idx) <- slot) assigned;
      (!content_w + 1 + count, !content_w + 1)
    end
  in
  { canvas = (canvas_w, canvas_h); band_end; edge_bus; lane_base; edge_lane }

let place_lr (ranks : int array) (max_rank : int) (by_rank : int array array)
  (sizes : node_sizes) (edges : Ir.edge array) (placed : placed array) : route_plan =
  let col_w =
    Array.map (fun row -> Array.fold_left (fun a i -> max a sizes.box_w.(i)) 0 row) by_rank
  in
  let max_label =
    let m = ref 0 in
    Array.iter
      (fun e ->
        if e.Ir.from_ = e.to_ || ranks.(e.to_) = ranks.(e.from_) + 1 then
          match e.label with
          | Some l -> m := max !m (min (Ucore.width_string l) Const.max_label)
          | None -> ())
      edges;
    !m
  in
  let base_gap = max (Const.gap_x + 1) (max_label + 3) in
  let centers = Rank.assign_positions by_rank sizes.lay_h 1 edges ranks in
  let ne = Array.length edges in
  let edge_bus = Array.make ne 0 in
  let bus_tracks = Array.make (max_rank + 1) 0 in
  for r = 0 to max_rank - 1 do
    let spans = bus_spans_td edges ranks centers r true in
    if spans <> [] then begin
      let assigned, count = Rank.assign_tracks spans in
      List.iter (fun (idx, slot) -> edge_bus.(idx) <- slot) assigned;
      bus_tracks.(r) <- count
    end
  done;
  let rank_x = Array.make (max_rank + 1) 0 in
  for r = 1 to max_rank do
    let gap = max base_gap (bus_tracks.(r - 1) + 1) in
    rank_x.(r) <- rank_x.(r - 1) + col_w.(r - 1) + gap
  done;
  let tail =
    let m = ref 0 in
    Array.iter
      (fun i ->
        if sizes.extra_h.(i) > 0 && sizes.self_label_w.(i) > 0 then
          m := max !m (2 + sizes.self_label_w.(i)))
      by_rank.(max_rank);
    !m
  in
  let canvas_w = rank_x.(max_rank) + col_w.(max_rank) + tail in
  let band_end = Array.init (max_rank + 1) (fun r -> rank_x.(r) + col_w.(r)) in
  let diagram_h = ref 1 in
  Array.iteri
    (fun r row ->
      let x = rank_x.(r) in
      Array.iter
        (fun idx ->
          let w = sizes.box_w.(idx) and h = sizes.box_h.(idx) in
          let cy = centers.(idx) in
          let y = Util.ssub cy ((h + sizes.extra_h.(idx)) / 2) in
          placed.(idx) <- { x; y; w; h; cx = x + (w / 2); cy = y + (h / 2); rank = r };
          diagram_h := max !diagram_h (y + h + sizes.extra_h.(idx)))
        row)
    by_rank;
  let edge_lane = Array.make ne 0 in
  let lanes = lane_spans edges ranks placed false in
  let canvas_h, lane_base =
    if lanes = [] then (!diagram_h, 0)
    else begin
      let assigned, count = Rank.assign_tracks lanes in
      List.iter (fun (idx, slot) -> edge_lane.(idx) <- slot) assigned;
      (!diagram_h + 1 + count, !diagram_h + 1)
    end
  in
  { canvas = (canvas_w, canvas_h); band_end; edge_bus; lane_base; edge_lane }

(* --- canvas orchestration --- *)

let layout_canvas (graph : Ir.graph) (extras : node_extra array) (max_width : int option)
  : (Canvas.t, oversize) result =
  let n = Vec.length graph.nodes in
  if n = 0 then Error Cells
  else begin
    let edges = Array.of_list (Vec.to_list graph.edges) in
    let node i = Vec.get graph.nodes i in
    let ranks = Rank.compute_ranks edges n in
    let max_rank = Array.fold_left max 0 ranks in
    let rows = Array.make (max_rank + 1) [] in
    for idx = n - 1 downto 0 do
      rows.(ranks.(idx)) <- idx :: rows.(ranks.(idx))
    done;
    let by_rank = Array.map Array.of_list rows in
    Rank.order_ranks by_rank edges ranks;
    let wrapped =
      Array.init n (fun i -> Wrap.wrap_label (node i).label Const.wrap_width Const.max_lines)
    in
    let box_w =
      Array.init n (fun i ->
        match extras.(i) with
        | Frame sub ->
          let title_w = Ucore.width_string (Wrap.fit_label (node i).label Const.wrap_width) in
          max (sub.Canvas.w + 2) (title_w + 4)
        | Compartments sections ->
          let m =
            List.fold_left
              (fun a sec -> List.fold_left (fun a l -> max a (Ucore.width_string l)) a sec)
              0 sections
          in
          max 1 m + (2 * Const.pad) + 2
        | Plain ->
          let m = List.fold_left (fun a l -> max a (Ucore.width_string l)) 0 wrapped.(i) in
          max 1 m + (2 * Const.pad) + 2)
    in
    let box_h =
      Array.init n (fun i ->
        match extras.(i) with
        | Frame sub -> sub.Canvas.h + 2
        | Compartments sections ->
          let filled = List.length (List.filter (fun s -> s <> []) sections) in
          List.fold_left (fun a s -> a + List.length s) 0 sections
          + Util.ssub filled 1 + 2
        | Plain -> List.length wrapped.(i) + 2)
    in
    let extra_h = Array.make n 0 and self_label_w = Array.make n 0 in
    Array.iter
      (fun e ->
        if e.Ir.from_ = e.to_ then begin
          extra_h.(e.from_) <- 2;
          match e.label with
          | Some l ->
            self_label_w.(e.from_)
            <- max self_label_w.(e.from_) (min (Ucore.width_string l) Const.max_label)
          | None -> ()
        end)
      edges;
    for i = 0 to n - 1 do
      if extra_h.(i) > 0 then box_w.(i) <- max box_w.(i) 7
    done;
    let lay_w =
      Array.init n (fun i ->
        box_w.(i) + if self_label_w.(i) > 0 then 2 * (self_label_w.(i) + 3) else 0)
    in
    let lay_h = Array.init n (fun i -> box_h.(i) + extra_h.(i)) in
    let sizes = { box_w; box_h; lay_w; lay_h; extra_h; self_label_w } in
    let placed = Array.make n placed0 in
    let vertical = match graph.dir with Ir.Down | Ir.Up -> true | _ -> false in
    let plan =
      if vertical then place_td ranks max_rank by_rank sizes edges placed
      else place_lr ranks max_rank by_rank sizes edges placed
    in
    let canvas_w, canvas_h = plan.canvas in
    let too_wide = match max_width with Some mw -> canvas_w > mw | None -> false in
    if too_wide then Error Width
    else if canvas_w * canvas_h > Const.max_canvas_cells then Error Cells
    else begin
      let cv = Canvas.create canvas_w canvas_h in
      for idx = 0 to n - 1 do
        match extras.(idx) with
        | Frame sub -> draw_frame cv placed.(idx) (node idx).label sub
        | Compartments sections -> draw_class_box cv placed.(idx) sections
        | Plain -> draw_box cv placed.(idx) wrapped.(idx) (node idx).shape
      done;
      Array.iteri
        (fun i edge ->
          cv.Canvas.cur_style
          <- (match edge.Ir.line with
              | Ir.Solid -> Canvas.sty_solid
              | Ir.Dotted -> Canvas.sty_dot
              | Ir.Thick -> Canvas.sty_thick);
          if edge.from_ = edge.to_ then route_self cv placed.(edge.from_) edge
          else begin
            let from = placed.(edge.from_) and to_ = placed.(edge.to_) in
            let adjacent = to_.rank = from.rank + 1 in
            let bus = plan.band_end.(from.rank) + plan.edge_bus.(i) in
            let lane = plan.lane_base + plan.edge_lane.(i) in
            match (vertical, adjacent) with
            | true, true -> route_forward cv from to_ edge bus
            | true, false -> route_back cv from to_ edge lane
            | false, true -> route_forward_lr cv from to_ edge bus
            | false, false -> route_back_lr cv from to_ edge lane
          end)
        edges;
      Canvas.finalize_mask cv;
      Ok cv
    end
  end

let finish (graph : Ir.graph) (cv : Canvas.t) : Style.t =
  (match graph.dir with
   | Ir.Up -> Canvas.flip_vertical cv
   | Ir.Left -> Canvas.flip_horizontal cv
   | _ -> ());
  let styled_lines, plain_lines = Canvas.to_lines cv in
  { Style.styled_lines; plain_lines }

let layout_flowchart (graph : Ir.graph) (max_width : int option)
  : (Style.t, oversize) result =
  let extras = Array.make (Vec.length graph.nodes) Plain in
  match layout_canvas graph extras max_width with
  | Error o -> Error o
  | Ok cv -> Ok (finish graph cv)

(* --- grouped (subgraph) flowcharts --- *)

type item =
  | Node of int
  | Group of int

exception Oversize_exn of oversize

let render_grouped (graph : Ir.graph) (max_width : int option)
  : (Style.t, oversize) result =
  let ng = Vec.length graph.groups in
  let proxy : (int, int) Hashtbl.t = Hashtbl.create 16 in
  Vec.iteri
    (fun gi g ->
      match Hashtbl.find_opt graph.index g.Ir.id with
      | Some ni -> Hashtbl.replace proxy ni gi
      | None -> ())
    graph.groups;
  let group_chain (g0 : int option) : int list =
    let rec go acc = function
      | Some gi -> go (gi :: acc) (Vec.get graph.groups gi).Ir.parent
      | None -> acc
    in
    go [] g0
  in
  let endpoint (nn : int) : item * int list =
    match Hashtbl.find_opt proxy nn with
    | Some gi -> (Group gi, group_chain (Vec.get graph.groups gi).Ir.parent)
    | None -> (Node nn, group_chain (Vec.get graph.node_group nn))
  in
  let scope_edges : (int option, (item * item * int) list ref) Hashtbl.t =
    Hashtbl.create 16
  in
  let push_scope_edge scope v =
    match Hashtbl.find_opt scope_edges scope with
    | Some r -> r := v :: !r
    | None -> Hashtbl.replace scope_edges scope (ref [ v ])
  in
  let referenced = Array.make ng false in
  Vec.iteri
    (fun ei e ->
      let item_f, chain_f = endpoint e.Ir.from_ in
      let item_t, chain_t = endpoint e.Ir.to_ in
      let rec common a b =
        match (a, b) with x :: xs, y :: ys when x = y -> 1 + common xs ys | _ -> 0
      in
      let k = common chain_f chain_t in
      let scope = if k = 0 then None else Some (List.nth chain_f (k - 1)) in
      let f = if List.length chain_f > k then Group (List.nth chain_f k) else item_f in
      let t = if List.length chain_t > k then Group (List.nth chain_t k) else item_t in
      (match f with Group gi -> referenced.(gi) <- true | _ -> ());
      (match t with Group gi -> referenced.(gi) <- true | _ -> ());
      push_scope_edge scope (f, t, ei))
    graph.edges;
  let direct_nodes : (int option, int list ref) Hashtbl.t = Hashtbl.create 16 in
  Vec.iteri
    (fun ni g ->
      if not (Hashtbl.mem proxy ni) then
        match Hashtbl.find_opt direct_nodes g with
        | Some r -> r := ni :: !r
        | None -> Hashtbl.replace direct_nodes g (ref [ ni ]))
    graph.node_group;
  let keep = Array.make ng false in
  for gi = ng - 1 downto 0 do
    let has_nodes =
      match Hashtbl.find_opt direct_nodes (Some gi) with Some r -> !r <> [] | None -> false
    in
    let has_children =
      let found = ref false in
      for c = 0 to ng - 1 do
        if (Vec.get graph.groups c).Ir.parent = Some gi && keep.(c) then found := true
      done;
      !found
    in
    keep.(gi) <- has_nodes || has_children || referenced.(gi)
  done;
  let rec build_scope (scope : int option) (mw : int option) : Canvas.t =
    let node_items =
      match Hashtbl.find_opt direct_nodes scope with
      | Some r -> List.map (fun n -> Node n) (List.rev !r)
      | None -> []
    in
    let child_groups =
      List.filter
        (fun gi -> (Vec.get graph.groups gi).Ir.parent = scope && keep.(gi))
        (List.init ng (fun i -> i))
    in
    let items = node_items @ List.map (fun gi -> Group gi) child_groups in
    if items = [] then Canvas.create 1 1
    else begin
      let index_of : (item, int) Hashtbl.t = Hashtbl.create 16 in
      let synth_nodes = Vec.create () in
      let extras_rev = ref [] in
      List.iteri
        (fun i item ->
          Hashtbl.replace index_of item i;
          match item with
          | Node ni ->
            let src = Vec.get graph.nodes ni in
            Vec.push synth_nodes { Ir.label = src.label; shape = src.shape };
            extras_rev := Plain :: !extras_rev
          | Group gi ->
            let sub = build_scope (Some gi) None in
            Vec.push synth_nodes { Ir.label = (Vec.get graph.groups gi).label; shape = Ir.Rect };
            extras_rev := Frame sub :: !extras_rev)
        items;
      let synth_edges = Vec.create () in
      (match Hashtbl.find_opt scope_edges scope with
       | Some r ->
         List.iter
           (fun (f, t, ei) ->
             match (Hashtbl.find_opt index_of f, Hashtbl.find_opt index_of t) with
             | Some fi, Some ti ->
               let e = Vec.get graph.edges ei in
               Vec.push synth_edges
                 { Ir.from_ = fi
                 ; to_ = ti
                 ; label = e.label
                 ; head_to = e.head_to
                 ; head_from = e.head_from
                 ; line = e.line
                 }
             | _ -> ())
           (List.rev !r)
       | None -> ());
      let synth =
        { Ir.nodes = synth_nodes
        ; edges = synth_edges
        ; index = Hashtbl.create 1
        ; groups = Vec.create ()
        ; node_group = Vec.create ()
        ; cur_group = None
        ; over_cap = false
        ; dir = graph.dir
        }
      in
      let extras = Array.of_list (List.rev !extras_rev) in
      match layout_canvas synth extras mw with
      | Ok cv -> cv
      | Error o -> raise (Oversize_exn o)
    end
  in
  match (try Ok (build_scope None max_width) with Oversize_exn o -> Error o) with
  | Error o -> Error o
  | Ok cv -> Ok (finish graph cv)
