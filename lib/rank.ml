(* Layered-layout graph algorithms (Sugiyama): rank assignment via a DFS-built
   DAG longest path, barycenter crossing reduction, coordinate relaxation, and
   greedy track packing for non-adjacent edge routing. Faithful port.

   Adjacency lists are built by prepending then reversing so child/parent order
   matches the Rust edge-iteration (push) order — DFS visit order depends on it. *)

let build_adj (n : int) (edges : Ir.edge array) (keep : Ir.edge -> bool)
  (dst : Ir.edge -> int) (src : Ir.edge -> int) : int list array =
  let a = Array.make n [] in
  Array.iter (fun e -> if keep e then a.(dst e) <- src e :: a.(dst e)) edges;
  Array.map List.rev a

(* --- rank assignment --- *)

let dfs_dag (start : int) (children : int list array) (color : int array)
  (dag : int list array) (order : int list ref) : unit =
  let stack = ref [ (start, ref 0) ] in
  color.(start) <- 1;
  let continue = ref true in
  while !continue do
    match !stack with
    | [] -> continue := false
    | (u, fi) :: _ ->
      let ch = children.(u) in
      if !fi < List.length ch then begin
        let v = List.nth ch !fi in
        incr fi;
        if color.(v) = 1 then ()
        else begin
          dag.(u) <- v :: dag.(u);
          if color.(v) = 0 then begin
            color.(v) <- 1;
            stack := (v, ref 0) :: !stack
          end
        end
      end
      else begin
        color.(u) <- 2;
        order := u :: !order;
        stack := (match !stack with _ :: t -> t | [] -> [])
      end
  done

let compute_ranks (edges : Ir.edge array) (n : int) : int array =
  let children =
    build_adj n edges (fun e -> e.Ir.from_ <> e.to_) (fun e -> e.from_) (fun e -> e.to_)
  in
  let indeg = Array.make n 0 in
  Array.iter
    (fun e -> if e.Ir.from_ <> e.to_ then indeg.(e.to_) <- indeg.(e.to_) + 1)
    edges;
  let color = Array.make n 0 in
  let dag = Array.make n [] in
  let order = ref [] in
  (* [order] is prepended, so it is already the reverse of the Rust post-order. *)
  let roots = List.filter (fun i -> indeg.(i) = 0) (List.init n (fun i -> i)) in
  List.iter
    (fun start -> if color.(start) = 0 then dfs_dag start children color dag order)
    (roots @ List.init n (fun i -> i));
  let rank = Array.make n 0 in
  List.iter
    (fun u -> List.iter (fun v -> rank.(v) <- max rank.(v) (rank.(u) + 1)) dag.(u))
    !order;
  rank

(* --- crossing reduction --- *)

let count_crossings (edges : Ir.edge array) (ranks : int array) (pos : int array) : int =
  let adj = ref [] in
  Array.iter
    (fun e ->
      if e.Ir.from_ <> e.to_ && ranks.(e.to_) = ranks.(e.from_) + 1 then
        adj := (ranks.(e.from_), pos.(e.from_), pos.(e.to_)) :: !adj)
    edges;
  let adj = Array.of_list (List.rev !adj) in
  let m = Array.length adj and c = ref 0 in
  for i = 0 to m - 1 do
    let r, af, at = adj.(i) in
    for j = i + 1 to m - 1 do
      let r2, bf, bt = adj.(j) in
      if r = r2 && ((af < bf && at > bt) || (af > bf && at < bt)) then incr c
    done
  done;
  !c

let sort_by_barycenter (row : int array) (neigh : int list array) (pos : int array) : unit =
  let keyed =
    Array.mapi
      (fun i v ->
        let key =
          match neigh.(v) with
          | [] -> float_of_int pos.(v)
          | ns ->
            List.fold_left (fun a u -> a +. float_of_int pos.(u)) 0.0 ns
            /. float_of_int (List.length ns)
        in
        (key, i, v))
      row
  in
  (* Stable sort by key: original index [i] breaks ties. *)
  Array.sort
    (fun (k1, i1, _) (k2, i2, _) ->
      let c = Float.compare k1 k2 in
      if c <> 0 then c else compare i1 i2)
    keyed;
  Array.iteri (fun i (_, _, v) -> row.(i) <- v) keyed

let order_ranks (by_rank : int array array) (edges : Ir.edge array) (ranks : int array)
  : unit =
  let n = Array.length ranks in
  if Array.length by_rank < 2 || n < 3 then ()
  else begin
    let up e = e.Ir.from_ <> e.to_ && ranks.(e.to_) > ranks.(e.from_) in
    let parents = build_adj n edges up (fun e -> e.to_) (fun e -> e.from_) in
    let children = build_adj n edges up (fun e -> e.from_) (fun e -> e.to_) in
    let pos = Array.make n 0 in
    let set_pos () =
      Array.iter (fun row -> Array.iteri (fun i v -> pos.(v) <- i) row) by_rank
    in
    set_pos ();
    let best = ref (Array.map Array.copy by_rank) in
    let best_crossings = ref (count_crossings edges ranks pos) in
    if !best_crossings <> 0 then begin
      (try
         for it = 0 to 7 do
           if it mod 2 = 0 then
             for ri = 1 to Array.length by_rank - 1 do
               sort_by_barycenter by_rank.(ri) parents pos;
               Array.iteri (fun i v -> pos.(v) <- i) by_rank.(ri)
             done
           else
             for ri = Array.length by_rank - 2 downto 0 do
               sort_by_barycenter by_rank.(ri) children pos;
               Array.iteri (fun i v -> pos.(v) <- i) by_rank.(ri)
             done;
           let crossings = count_crossings edges ranks pos in
           if crossings < !best_crossings then begin
             best_crossings := crossings;
             best := Array.map Array.copy by_rank
           end;
           if !best_crossings = 0 then raise Exit
         done
       with Exit -> ());
      Array.iteri (fun ri row -> Array.blit row 0 by_rank.(ri) 0 (Array.length row)) !best
    end
  end

(* --- coordinate assignment --- *)

let relax_rank (nodes : int array) (neigh : int list array) (pos : float array)
  (size : int array) (sep : int) : unit =
  let n = Array.length nodes in
  if n <> 0 then begin
    let desired =
      Array.map
        (fun v ->
          match neigh.(v) with
          | [] -> pos.(v)
          | ns ->
            List.fold_left (fun a u -> a +. pos.(u)) 0.0 ns
            /. float_of_int (List.length ns))
        nodes
    in
    let half i = float_of_int size.(nodes.(i)) /. 2.0 in
    let left = Array.make n 0.0 and right = Array.make n 0.0 in
    for i = 0 to n - 1 do
      left.(i)
      <- (if i = 0 then desired.(i)
          else
            Float.max desired.(i)
              (left.(i - 1) +. half (i - 1) +. float_of_int sep +. half i))
    done;
    for i = n - 1 downto 0 do
      right.(i)
      <- (if i = n - 1 then desired.(i)
          else
            Float.min desired.(i)
              (right.(i + 1) -. half (i + 1) -. float_of_int sep -. half i))
    done;
    for i = 0 to n - 1 do
      pos.(nodes.(i)) <- (left.(i) +. right.(i)) /. 2.0
    done;
    for i = 1 to n - 1 do
      let min_p = pos.(nodes.(i - 1)) +. half (i - 1) +. float_of_int sep +. half i in
      if pos.(nodes.(i)) < min_p then pos.(nodes.(i)) <- min_p
    done
  end

let assign_positions (by_rank : int array array) (size : int array) (sep : int)
  (edges : Ir.edge array) (ranks : int array) : int array =
  let n = Array.length size in
  let up e = e.Ir.from_ <> e.to_ && ranks.(e.to_) > ranks.(e.from_) in
  let parents = build_adj n edges up (fun e -> e.to_) (fun e -> e.from_) in
  let children = build_adj n edges up (fun e -> e.from_) (fun e -> e.to_) in
  let pos = Array.make n 0.0 in
  Array.iter
    (fun row ->
      let x = ref 0.0 in
      Array.iter
        (fun v ->
          let half = float_of_int size.(v) /. 2.0 in
          x := !x +. half;
          pos.(v) <- !x;
          x := !x +. half +. float_of_int sep)
        row)
    by_rank;
  for it = 0 to 9 do
    if it mod 2 = 0 then
      Array.iter (fun row -> relax_rank row parents pos size sep) by_rank
    else
      for ri = Array.length by_rank - 1 downto 0 do
        relax_rank by_rank.(ri) children pos size sep
      done
  done;
  let min_left = ref infinity in
  for v = 0 to n - 1 do
    let ml = pos.(v) -. (float_of_int size.(v) /. 2.0) in
    if ml < !min_left then min_left := ml
  done;
  let min_left = if Float.is_finite !min_left then !min_left else 0.0 in
  Array.init n (fun v -> int_of_float (Float.max 0.0 (Float.round (pos.(v) -. min_left))))

(* --- track packing --- *)

(* [assign_tracks spans] packs half-open spans [(s, e, from, to, idx)] into the
   fewest tracks so that same-track spans are disjoint (>= 2 apart) or share an
   endpoint. Returns per-edge [(idx, slot)] plus the track count. *)
let assign_tracks (spans : (int * int * int * int * int) list) : (int * int) list * int =
  let sorted = List.sort compare spans in
  let tracks = ref [] (* reversed list of track-content refs *) in
  let ntracks = ref 0 in
  let out = ref [] in
  List.iter
    (fun (s, e, f, t, idx) ->
      let ordered = List.rev !tracks in
      let compatible members =
        List.for_all
          (fun (s2, e2, f2, t2) -> e2 + 2 <= s || e + 2 <= s2 || f2 = f || t2 = t)
          members
      in
      let rec find i = function
        | [] -> None
        | tr :: rest -> if compatible !tr then Some (i, tr) else find (i + 1) rest
      in
      match find 0 ordered with
      | Some (i, tr) ->
        tr := !tr @ [ (s, e, f, t) ];
        out := (idx, i) :: !out
      | None ->
        tracks := ref [ (s, e, f, t) ] :: !tracks;
        out := (idx, !ntracks) :: !out;
        incr ntracks)
    sorted;
  (List.rev !out, !ntracks)
