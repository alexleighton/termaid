(* Layout and safety-cap constants, values taken verbatim from upstream. *)

let max_label = 28
let pad = 1
let gap_x = 3
let gap_y = 2
let wrap_width = 24
let max_lines = 4
let label_break_chars = [ '_'; '-'; '.'; '/' ]

(* Sentinel for a wide-glyph continuation cell (upstream uses NUL). *)
let cont : Uchar.t = Uchar.of_int 0

let max_nodes = 128
let max_edges = 512
let max_groups = 24
let max_group_depth = 6
let max_canvas_cells = 1 lsl 21
let max_members = 8
let seq_gap = 5
