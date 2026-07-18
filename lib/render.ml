(* Top-level dispatcher: pick a parser, lay it out, and fall back to a framed
   raw box when nothing matches or the result is too wide. Faithful port of the
   upstream [render].

   Only the flowchart parser is wired in so far; state/class/ER/sequence slot in
   here as those parsers land. *)

(* Class/ER diagrams: paint each node as a compartmented box (annotation +
   attributes + methods). Faithful port of upstream [render_class]. *)
let render_class (graph : Ir.graph) (infos : Parse_class.class_info Vec.t)
  (max_width : int option) : (Style.t, Layout.oversize) result =
  let n = Vec.length graph.nodes in
  let extras =
    Array.init n (fun i ->
      let node = Vec.get graph.nodes i in
      let info = Vec.get infos i in
      let title =
        (match info.Parse_class.annotation with Some a -> [ "«" ^ a ^ "»" ] | None -> [])
        @ [ Parse_class.display_generics node.Ir.label ]
      in
      Layout.Compartments [ title; info.attrs; info.methods ])
  in
  match Layout.layout_canvas graph extras max_width with
  | Error o -> Error o
  | Ok cv -> Ok (Layout.finish graph cv)

let render ?max_width (src : string) : Style.t option =
  if Ucore.trim src = "" then None
  else begin
    let ( ||> ) opt f = match opt with Some _ -> opt | None -> f () in
    let outcome =
      (match Parse_graph.parse_graph src with
       | Some g ->
         Some
           (if Vec.length g.Ir.groups = 0 then Layout.layout_flowchart g max_width
            else Layout.render_grouped g max_width)
       | None -> None)
      ||> (fun () ->
            match Parse_state.parse_state src with
            | Some g -> Some (Layout.layout_flowchart g max_width)
            | None -> None)
      ||> (fun () ->
            match Parse_class.parse_class src with
            | Some (g, infos) -> Some (render_class g infos max_width)
            | None -> None)
      ||> (fun () ->
            match Parse_class.parse_er src with
            | Some (g, infos) -> Some (render_class g infos max_width)
            | None -> None)
      ||> fun () ->
      match Parse_sequence.parse_sequence src with
      | Some seq -> Some (Seq_layout.layout_sequence seq max_width)
      | None -> None
    in
    match outcome with
    | Some (Ok art) -> Some art
    | Some (Error Layout.Width) -> Some (Fallback.fallback src max_width true)
    | Some (Error Layout.Cells) | None -> Some (Fallback.fallback src max_width false)
  end
