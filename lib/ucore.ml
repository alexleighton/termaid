(* Unicode substrate.

   The upstream Rust operates on [Vec<char>] (Unicode scalar values), indexing by
   scalar position and measuring terminal width per scalar. OCaml strings are
   bytes, so we decode to a [Uchar.t array] up front and index that, mirroring the
   Rust exactly. This module also provides the scalar classifications and the
   Unicode-aware string trims the parser relies on. *)

type uarr = Uchar.t array

(* Decode UTF-8 to scalar values; malformed bytes become U+FFFD, matching how a
   valid Rust [&str] would already be well-formed. *)
let to_uchars (s : string) : uarr =
  let acc = ref [] in
  Uutf.String.fold_utf_8
    (fun () _ dec ->
      match dec with
      | `Uchar u -> acc := u :: !acc
      | `Malformed _ -> acc := Uutf.u_rep :: !acc)
    () s;
  Array.of_list (List.rev !acc)

(* Encode the half-open slice [\[i, j)] of a scalar array back to UTF-8. *)
let sub_to_string (a : uarr) (i : int) (j : int) : string =
  let buf = Buffer.create ((j - i) * 2) in
  for k = i to j - 1 do
    Uutf.Buffer.add_utf_8 buf a.(k)
  done;
  Buffer.contents buf

let to_string (a : uarr) : string = sub_to_string a 0 (Array.length a)

(* [get a i] is [a.(i)] or [None] out of range — mirrors Rust [slice::get]. *)
let get (a : uarr) (i : int) : Uchar.t option =
  if i >= 0 && i < Array.length a then Some a.(i) else None

(* Scalar equals a specific ASCII char. *)
let is (u : Uchar.t) (c : char) : bool = Uchar.to_int u = Char.code c

let is_one_of (u : Uchar.t) (cs : char list) : bool = List.exists (is u) cs

(* Terminal cell width: Rust uses [UnicodeWidthChar::width(c).unwrap_or(0)], i.e.
   control chars count as 0. [tty_width_hint] returns -1 for controls, so clamp. *)
let width_uchar (u : Uchar.t) : int = max 0 (Uucp.Break.tty_width_hint u)

let width_string (s : string) : int =
  let w = ref 0 in
  Uutf.String.fold_utf_8
    (fun () _ dec ->
      match dec with `Uchar u -> w := !w + width_uchar u | `Malformed _ -> ())
    () s;
  !w

(* Rust [char::is_alphanumeric] = alphabetic OR numeric (Nd/Nl/No). *)
let is_numeric (u : Uchar.t) : bool =
  match Uucp.Gc.general_category u with `Nd | `Nl | `No -> true | _ -> false

let is_alphanumeric (u : Uchar.t) : bool =
  Uucp.Alpha.is_alphabetic u || is_numeric u

let is_ascii_alphanumeric (u : Uchar.t) : bool =
  let n = Uchar.to_int u in
  (n >= Char.code '0' && n <= Char.code '9')
  || (n >= Char.code 'a' && n <= Char.code 'z')
  || (n >= Char.code 'A' && n <= Char.code 'Z')

(* Rust [char::is_control] = general category Cc. *)
let is_control (u : Uchar.t) : bool =
  match Uucp.Gc.general_category u with `Cc -> true | _ -> false

let is_white_space (u : Uchar.t) : bool = Uucp.White.is_white_space u

(* Unicode-aware trims (Rust [str::trim] / [trim_end] use White_Space). *)
let trim (s : string) : string =
  let a = to_uchars s in
  let n = Array.length a in
  let i = ref 0 and j = ref n in
  while !i < !j && is_white_space a.(!i) do incr i done;
  while !j > !i && is_white_space a.(!j - 1) do decr j done;
  sub_to_string a !i !j

let trim_end (s : string) : string =
  let a = to_uchars s in
  let j = ref (Array.length a) in
  while !j > 0 && is_white_space a.(!j - 1) do decr j done;
  sub_to_string a 0 !j

(* Rust [str::split_whitespace]: split on runs of Unicode whitespace, no empties. *)
let split_whitespace (s : string) : string list =
  let a = to_uchars s in
  let n = Array.length a in
  let out = ref [] and i = ref 0 in
  while !i < n do
    while !i < n && is_white_space a.(!i) do incr i done;
    let start = !i in
    while !i < n && not (is_white_space a.(!i)) do incr i done;
    if !i > start then out := sub_to_string a start !i :: !out
  done;
  List.rev !out

let first_whitespace_token (s : string) : string option =
  match split_whitespace s with [] -> None | t :: _ -> Some t

let has_whitespace (s : string) : bool = Array.exists is_white_space (to_uchars s)

let trim_start (s : string) : string =
  let a = to_uchars s in
  let n = Array.length a and i = ref 0 in
  while !i < n && is_white_space a.(!i) do incr i done;
  sub_to_string a !i n

(* Byte-level helpers over ASCII delimiters (safe: these bytes never occur mid
   UTF-8 sequence). Mirror Rust's [trim_start_matches]/[trim_end_matches]. *)
let ltrim_char (s : string) (c : char) : string =
  let n = String.length s and i = ref 0 in
  while !i < n && s.[!i] = c do incr i done;
  String.sub s !i (n - !i)

let rtrim_char (s : string) (c : char) : string =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = c do decr n done;
  String.sub s 0 !n

let rtrim_str (s : string) (sub : string) : string =
  let sl = String.length sub in
  if sl = 0 then s
  else begin
    let e = ref (String.length s) in
    while !e >= sl && String.sub s (!e - sl) sl = sub do e := !e - sl done;
    String.sub s 0 !e
  end

let starts_with (s : string) (prefix : string) : bool =
  let pl = String.length prefix in
  String.length s >= pl && String.sub s 0 pl = prefix

let strip_prefix_str (s : string) (prefix : string) : string option =
  if starts_with s prefix then
    Some (String.sub s (String.length prefix) (String.length s - String.length prefix))
  else None

(* First byte index of [sub] in [s] at or after [start] (naive). *)
let find_str ?(start = 0) (s : string) (sub : string) : int option =
  let sl = String.length sub and n = String.length s in
  if sl = 0 then Some start
  else begin
    let i = ref start and res = ref None in
    while !res = None && !i + sl <= n do
      if String.sub s !i sl = sub then res := Some !i else incr i
    done;
    !res
  end

let contains_str (s : string) (sub : string) : bool = find_str s sub <> None

let split_once_char (s : string) (c : char) : (string * string) option =
  match String.index_opt s c with
  | Some i -> Some (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  | None -> None

let split_once_str (s : string) (sub : string) : (string * string) option =
  match find_str s sub with
  | Some i ->
    let after = i + String.length sub in
    Some (String.sub s 0 i, String.sub s after (String.length s - after))
  | None -> None
