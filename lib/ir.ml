(* Intermediate representation shared by the diagram parsers and the layout
   engine: a directed [graph] of shaped [node]s and [edge]s, with optional
   subgraph [group]s. Mirrors the upstream structs. *)

type shape =
  | Rect
  | Round
  | Diamond

type head =
  | No_head
  | Arrow
  | Circle
  | Cross
  | Triangle
  | Diamond_fill
  | Diamond_open

type line_kind =
  | Solid
  | Dotted
  | Thick

type dir =
  | Down
  | Up
  | Right
  | Left

type node =
  { mutable label : string
  ; mutable shape : shape
  }

type edge =
  { from_ : int
  ; to_ : int
  ; label : string option
  ; head_to : head
  ; head_from : head
  ; line : line_kind
  }

type group =
  { id : string
  ; label : string
  ; parent : int option
  }

type graph =
  { nodes : node Vec.t
  ; edges : edge Vec.t
  ; index : (string, int) Hashtbl.t
  ; groups : group Vec.t
  ; node_group : int option Vec.t
  ; mutable cur_group : int option
  ; mutable over_cap : bool
  ; mutable dir : dir
  }

let create_graph (dir : dir) : graph =
  { nodes = Vec.create ()
  ; edges = Vec.create ()
  ; index = Hashtbl.create 32
  ; groups = Vec.create ()
  ; node_group = Vec.create ()
  ; cur_group = None
  ; over_cap = false
  ; dir
  }

(* Look up [id], updating its label/shape when a label is supplied; otherwise
   append a new node (respecting [max_nodes], setting [over_cap] when hit). *)
let node_index (g : graph) (id : string) (label : string option) (shape : shape)
  : int option =
  match Hashtbl.find_opt g.index id with
  | Some i ->
    (match label with
     | Some l ->
       let n = Vec.get g.nodes i in
       n.label <- l;
       n.shape <- shape
     | None -> ());
    Some i
  | None ->
    if Vec.length g.nodes >= Const.max_nodes then begin
      g.over_cap <- true;
      None
    end
    else begin
      let label = match label with Some l -> l | None -> id in
      Hashtbl.replace g.index id (Vec.length g.nodes);
      Vec.push g.nodes { label; shape };
      Vec.push g.node_group g.cur_group;
      Some (Vec.length g.nodes - 1)
    end

let node_label (g : graph) (id : string) (label : string) : int option =
  match Hashtbl.find_opt g.index id with
  | Some i ->
    (Vec.get g.nodes i).label <- label;
    Some i
  | None -> node_index g id (Some label) Round
