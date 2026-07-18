(* Minimal CLI: read a Mermaid diagram from stdin (or a file argument) and print
   the rendered box art. Optional [--width N] caps the output width. *)

let read_all ic =
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_channel buf ic 4096
     done
   with End_of_file -> ());
  Buffer.contents buf

let () =
  let width = ref None in
  let file = ref None in
  let rec parse = function
    | [] -> ()
    | "--width" :: n :: rest -> width := Some (int_of_string n); parse rest
    | f :: rest -> file := Some f; parse rest
  in
  parse (List.tl (Array.to_list Sys.argv));
  let src =
    match !file with
    | Some f -> let ic = open_in f in let s = read_all ic in close_in ic; s
    | None -> read_all stdin
  in
  match Termaid.render ?max_width:!width src with
  | None -> ()
  | Some art -> List.iter print_endline art.Termaid.plain_lines
