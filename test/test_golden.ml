(* Golden tests — exact box art compared against fixtures captured from the Rust
   oracle. For each [golden/NAME.mmd] there is a [golden/NAME.expected] holding
   the upstream renderer's output (regenerate with scripts/gen_golden.sh). An
   optional [golden/NAME.width] overrides the default render width of 120. *)

let golden_dir = "golden"

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* Match the oracle's stdout: one plain line per row, each newline-terminated;
   empty (or [None]) render -> empty output. *)
let render_to_string ~max_width src =
  match Termaid.render ~max_width src with
  | None | Some { Termaid.plain_lines = []; _ } -> ""
  | Some { Termaid.plain_lines = ls; _ } -> String.concat "\n" ls ^ "\n"

let width_for base =
  let wf = base ^ ".width" in
  if Sys.file_exists wf then int_of_string (String.trim (read_file wf)) else 120

let make_case base =
  Alcotest.test_case base `Quick (fun () ->
    let src = read_file (Filename.concat golden_dir (base ^ ".mmd")) in
    let expected = read_file (Filename.concat golden_dir (base ^ ".expected")) in
    let width = width_for (Filename.concat golden_dir base) in
    let actual = render_to_string ~max_width:width src in
    Alcotest.(check string) base expected actual)

let () =
  let bases =
    Sys.readdir golden_dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".mmd")
    |> List.map Filename.remove_extension
    |> List.sort compare
  in
  Alcotest.run "termaid-golden" [ ("golden", List.map make_case bases) ]
