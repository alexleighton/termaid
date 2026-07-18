(* Renders a few sample diagrams to stdout. Doubles as a smoke test:
     dune exec examples/demo.exe *)

let samples =
  [ ( "flowchart"
    , "flowchart TD\n\
      \  Src[Mermaid text] --> R{render}\n\
      \  R -->|recognized| P[parse]\n\
      \  R -->|no match| F[fallback]\n\
      \  P --> L[layout]\n\
      \  L --> Cv[canvas]\n\
      \  Cv --> Art[box art]\n\
      \  F --> Art" )
  ; ("sequence", "sequenceDiagram\n  Alice->>Bob: Hello\n  Bob-->>Alice: Hi")
  ; ("class", "classDiagram\n  class Animal {\n    +int age\n    +speak() void\n  }\n  Animal <|-- Dog")
  ]

let () =
  List.iter
    (fun (name, src) ->
      Printf.printf "== %s ==\n" name;
      (match Termaid.render ~max_width:80 src with
       | Some art -> List.iter print_endline art.Termaid.plain_lines
       | None -> print_endline "(empty)");
      print_newline ())
    samples
