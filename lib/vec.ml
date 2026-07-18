(* Minimal growable vector with index access and in-place element mutation,
   standing in for Rust's [Vec] (OCaml < 5.2 has no [Dynarray]). Elements are
   boxed in [option] cells so we need no dummy value; [get] returns the element
   physically, so mutating a mutable-record element in place works as in Rust. *)

type 'a t =
  { mutable data : 'a option array
  ; mutable len : int
  }

let create () : 'a t = { data = Array.make 8 None; len = 0 }
let length (v : 'a t) : int = v.len

let get (v : 'a t) (i : int) : 'a =
  match v.data.(i) with Some x -> x | None -> invalid_arg "Vec.get"

let set (v : 'a t) (i : int) (x : 'a) : unit = v.data.(i) <- Some x

let push (v : 'a t) (x : 'a) : unit =
  if v.len >= Array.length v.data then begin
    let nd = Array.make (2 * Array.length v.data) None in
    Array.blit v.data 0 nd 0 v.len;
    v.data <- nd
  end;
  v.data.(v.len) <- Some x;
  v.len <- v.len + 1

let to_list (v : 'a t) : 'a list = List.init v.len (get v)
let iteri (f : int -> 'a -> unit) (v : 'a t) : unit =
  for i = 0 to v.len - 1 do f i (get v i) done
