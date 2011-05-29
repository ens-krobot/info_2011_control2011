let rec map_filter f = function
  | [] -> []
  | t::q ->
    match f t with
      | None -> map_filter f q
      | Some v -> v::(map_filter f q)

let map_option f = function
  | None -> None
  | Some v -> Some (f v)

let iter_option f = function
  | None -> ()
  | Some v -> f v

module Vect = struct

  type point = { px : float; py : float }
  type vect = { vx : float; vy : float }
  type circle = { c : point; r : float }
  type segment = { p1 : point; p2 : point }
  type line = { p : point; v : vect }
      
  let epsilon = 0.0000000001
    
  let vect p1 p2 = { vx = p2.px -. p1.px; vy = p2.py -. p1.py }
    
  let ( +! ) p v = { px = v.vx +. p.px; py = v.vy +. p.py }
  let ( -! ) p v = { px = p.px -. v.vx; py = p.py -. v.vy }
    
  let ( +| ) v1 v2 = { vx = v1.vx +. v2.vx; vy = v1.vy +. v2.vy }
  let ( -| ) v1 v2 = { vx = v1.vx -. v2.vx; vy = v1.vy -. v2.vy }
  let ( *@ ) n v = { vx = n *. v.vx; vy = n *. v.vy }
  let norm2 { vx; vy } = vx*.vx +. vy*.vy
  let norm v = sqrt (norm2 v)
  let unitaire v = ( 1. /. (norm v) ) *@ v
  let turn_trigo { vx; vy } = { vx = -. vy; vy = vx }
  let turn_antitrigo { vx; vy } = { vx = vy; vy = -. vx }
  let line s = { p = s.p1; v = vect s.p1 s.p2 }
  let scal v1 v2 = v1.vx *. v2.vx +. v1.vy *. v2.vy
  let colineaire v1 v2 = abs_float (v1.vx *. v2.vy -. v1.vy *. v2.vx) < epsilon
    
  let distance s p =
    let middle =
      ( scal
	  (vect s.p1 s.p2)
	  (vect s.p1 p) >= 0. )
      && ( scal
	    (vect s.p2 s.p1)
	    (vect s.p2 p) >= 0. )
    in
    if middle
    then
      let l = line s in
      let nv = unitaire l.v in
      abs_float (scal (turn_trigo nv) (vect s.p1 p))
    else
      min (norm (vect s.p1 p)) (norm (vect s.p2 p))

end

module Tangentes = struct
  open Vect

  type 'a opt =
    | One of 'a
    | Two of ('a * 'a)

  let map_opt f = function
    | One v -> One (f v)
    | Two (v1,v2) -> Two (f v1,f v2)

  let iter_opt f = function
    | One v -> f v
    | Two (v1,v2) -> f v1; f v2

  let p_iso c1 c2 =
    if c1.r <> c2.r
    then
      let cm,cM = if c1.r < c2.r then c1,c2 else c2,c1 in
      let ba = vect cM.c cm.c in
      let p1 = cm.c +! (( cm.r /. ( cM.r -. cm.r ) ) *@ ba) in
      let p2 = cM.c +! (( cM.r /. ( cM.r +. cm.r ) ) *@ ba) in
      Two (p1,p2)
    else
      let ba = vect c1.c c2.c in
      One (c1.c +! ( 0.5 *@ ba ))

  let tangentes_point_circle p c =
    let pc = vect p c.c in
    let pc2 = norm2 pc in
    let r2 = c.r *. c.r in
    let y = sqrt ((r2 *. pc2) /. (pc2 -. r2)) in
    let u = turn_trigo (unitaire pc) in
    let v = y *@ u in
    let s1,s2 = { p1 = p; p2 = c.c +! v }, { p1 = p; p2 = c.c -! v } in
    let l1 = line s1
    and l2 = line s2 in
    let v1 = c.r *@ (unitaire (turn_trigo l1.v)) in
    let v2 = c.r *@ (unitaire (turn_antitrigo l2.v)) in
    { p1 = p; p2 = c.c +! v1 },
    { p1 = p; p2 = c.c +! v2 }

  let tangentes_same_size c1 c2 =
    let ab = vect c1.c c2.c in
    let v = c1.r *@ (turn_trigo (unitaire ab)) in
    { p1 = c1.c +! v; p2 = c2.c +! v },
    { p1 = c1.c -! v; p2 = c2.c -! v }

  let tangentes c1 c2 =
    let tan p c1 c2 =
      let s1,s2 = tangentes_point_circle p c1 in
      let s3,s4 = tangentes_point_circle p c2 in
      { p1 = s1.p2; p2 = s3.p2 },
      { p1 = s2.p2; p2 = s4.p2 }
    in
    let ((s1,s2),(s3,s4)) =
      match (p_iso c1 c2) with
	| One p -> tan p c1 c2, tangentes_same_size c1 c2
	| Two (p1,p2) -> tan p1 c1 c2, tan p2 c1 c2
    in
    [s1;s2;s3;s4]

end

include Krobot_solve
include Vect
include Tangentes

type board = { src : point;
	       dst : point;
	       obstacles : circle list; }

type tangente =
    { orig : point;
      contact : point;
      dir : vect;
      distance : float; (* distance du point d'origine *)
      circle : circle;
      prec : tangente list; }

type result =
    { path : segment list;
      length : float }

let distance' s nv p =
  let middle =
    ( scal
	(vect s.p1 s.p2)
	(vect s.p1 p) >= 0. )
    && ( scal
	  (vect s.p2 s.p1)
	  (vect s.p2 p) >= 0. )
  in
  if middle
  then
    abs_float (scal (turn_trigo nv) (vect s.p1 p))
  else
    sqrt ((min (norm2 (vect s.p1 p)) (norm2 (vect s.p2 p))))

let check_segment b s =
  let l = line s in
  let nv = unitaire l.v in
  let rec check = function
    | [] -> true
    | t::q ->
      if distance' s nv t.c < t.r -. epsilon
      then false
      else check q
  in
  check b.obstacles

let check_segment_1circle b c1 s =
  let rec check = function
    | [] -> true
    | t::q ->
      if t = c1
      then check q
      else
	if distance s t.c < t.r
	then false
	else check q
  in
  check b.obstacles

let check_segment_circle b (c1,c2) s =
  let rec check = function
    | [] -> true
    | t::q ->
      if t = c1 || t = c2
      then check q
      else
	if distance s t.c < t.r
	then false
	else check q
  in
  check b.obstacles

let start_list b =
  let aux c {p1;p2} =
    { orig = p1;
      contact = p2;
      dir = vect p1 p2;
      distance = 0.;
      circle = c;
      prec = []; }
  in
  if check_segment b {p1 = b.src; p2 = b.dst}
  then
    (Some { length = norm (vect b.src b.dst);
	    path = [{p1 = b.dst; p2 = b.src}] },[])
  else
    (None,
     List.flatten
       (List.map 
	  (fun c ->
	    let (s1,s2) = tangentes_point_circle b.src c in
	    List.map (aux c)
	      (List.filter (check_segment_1circle b c) [s1;s2])
	  ) b.obstacles))

let intersect l1 l2 =
  if colineaire l1.v l2.v
  then None
  else let v = [| l2.p.px -. l1.p.px; l2.p.py -. l1.p.py |] in
       let m = [| [| l1.v.vx ; l2.v.vx |]; [| l1.v.vy ; l2.v.vy |] |] in
       let k1,k2 =
	 match solve m v with
	   | [| k1 ; k2 |] -> k1,k2
	   | _ -> assert false in
       Some (l1.p +! k1 *@ l1.v)

let after {p1;p2} p =
  scal (vect p1 p2) (vect p2 p) >= 0.

let make_tangente b t c ({p1;p2} as s) =
  if (scal (line s).v (vect t.contact t.circle.c) >= 0.)
    && (check_segment_circle b (t.circle,c) s)
  then
    let l1 = { p = t.orig; v = t.dir } in
    let l2 = line s in
    match intersect l1 l2 with
      | None -> None
      | Some inter ->
	if (after {p1 = t.orig;p2 = t.contact} inter)
	  && (check_segment_circle b (t.circle,c) {p1 = inter; p2 = t.contact})
	then
	  Some { distance = norm (vect t.orig inter) +. t.distance;
		 orig = inter;
		 contact = p2;
		 circle = c;
		 dir = l2.v;
		 prec = t::t.prec}
	else None
  else None

let make_tangente' b t c =
  map_filter (make_tangente b t c) (tangentes t.circle c)

let make_tangentes b t =
  let aux1 t c = map_filter (make_tangente b t c) (tangentes t.circle c) in
  List.flatten (List.map (aux1 t) b.obstacles)

let make_result b inter t =
  let points = b.dst::inter::(List.map (fun t -> t.orig) (t::t.prec)) in
  let rec couples = function
    | [] | [_] -> []
    | p1::p2::l -> {p1;p2}::(couples (p2::l)) in
  { length = norm (vect t.orig inter)
             +. norm (vect inter b.dst)
             +. t.distance;
    path = couples points}

let reach_tangente' b t =
  let (s1,s2) = tangentes_point_circle b.dst t.circle in
  let tan_line = { p = t.orig; v = t.dir } in
  let check_seg s =
    match intersect tan_line (line s) with
      | None -> None
      | Some inter ->
	if (check_segment_1circle b t.circle { p1 = t.contact; p2 = inter })
	  && (check_segment_1circle b t.circle { p1 = b.dst; p2 = inter })
	then Some (make_result b inter t)
	else None
  in
  map_filter check_seg [s1;s2]

let reach b tl =
  let l = List.flatten (List.map (reach_tangente' b) tl) in
  match l with
    | [] -> None
    | t::q ->
      Some
	(List.fold_left
	   (fun t1 t2 -> if t1.length < t2.length then t1 else t2) t q)

let filter_tangents b min_reach tl =
  List.filter (fun t -> t.distance +.
    norm (vect t.orig t.contact) +.
    norm (vect t.contact b.dst) < min_reach.length) tl

let tangentes_step b tl =
  let aux1 t c = map_filter (make_tangente b t c) (tangentes t.circle c) in
  let aux2 t = List.map (aux1 t) b.obstacles in
  List.flatten (List.flatten (List.map aux2 tl))

let min_result r1 r2 = match r1,r2 with
  | None, _ -> r2
  | _, None -> r1
  | Some v1, Some v2 when v1.length < v2.length -> r1
  | _ -> r2

let step b (r,tl) =
  let tl = tangentes_step b tl in
  let r = min_result (reach b tl) r in
  let tl = match r with
    | None -> tl
    | Some r -> filter_tangents b r tl in
  (r,tl)

let stepn b n =
  let rec stepn n (r,tl) =
    if n = 0 then (r,tl)
    else stepn (n-1) (step b (r,tl))
  in
  let (r,tl) = start_list b in
  let r = min_result (reach b tl) r in
  let tl = match r with
    | None -> tl
    | Some r -> filter_tangents b r tl in
  stepn n (r,tl)

let v_of_vertice { Krobot_geom.x; y } =
  { px = x; py = y }

let vertice_of_seg { p1 = {px;py} } =
  { Krobot_geom.x = px; y = py }

let find_path ~src ~dst objects =
  let b = { src = v_of_vertice src;
	    dst = v_of_vertice dst;
	    obstacles = List.map (fun (v,r) -> { c = v_of_vertice v; r }) objects; } in
  map_option (fun res -> List.rev_map vertice_of_seg res.path) (fst (stepn b 10))
