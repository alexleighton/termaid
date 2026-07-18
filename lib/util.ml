(* Small integer helpers mirroring Rust's saturating/abs-diff usize semantics.
   Used where the upstream relies on [saturating_sub] / [abs_diff] (OCaml ints
   would otherwise go negative). *)

let ssub (a : int) (b : int) : int = if a > b then a - b else 0
let adiff (a : int) (b : int) : int = abs (a - b)

let rec take (n : int) (l : 'a list) : 'a list =
  match (n, l) with 0, _ | _, [] -> [] | n, x :: xs -> x :: take (n - 1) xs
