open Icp_minimisation

(**** table ****)

let linear a0 an n =
  let dx = (an -. a0) /. (float (n-1)) in
  let p n = a0 +. (float n) *. dx in
  Array.init n p

let line (x0,y0) (xn,yn) n =
  linear x0 xn n, linear y0 yn n

let shuffle l =
  let rec split (ac1,ac2) = function
    | [] -> ac1,ac2
    | [t] -> t::ac1,ac2
    | t1::t2::q -> split (t1::ac1,t2::ac2) q in
  let rec merge acc l1 l2 = match l1,l2 with
    | [],l | l, [] -> l@acc
    | t1::q1, t2::q2 -> merge (t1::t2::acc) q1 q2 in
  let l1,l2 = split ([],[]) l in
  merge [] l1 (List.rev l2)

let table ~width ~length n =
  let x1,y1 = line (0.,0.) (length,0.) n in
  let x2,y2 = line (length,0.) (length,width) n in
  let x3,y3 = line (length,width) (0.,width) n in
  let x4,y4 = line (0.,width) (0.,0.) n in
  let x = Array.concat [x1;x2;x3;x4] in
  let y = Array.concat [y1;y2;y3;y4] in
  let f a = Array.of_list (shuffle (shuffle (Array.to_list a))) in
  { dx = f x; dy = f y }



(**** dump loading ****)

let load_float ic =
  Scanf.bscanf ic " %f " (fun f -> f)

let load_float_cpl ic =
  Scanf.bscanf ic " %f %f " (fun f1 f2 -> f1, f2)

let load_line ic =
  let line = Pervasives.input_line ic in
  let ic = Scanf.Scanning.from_string line in
  let ts = load_float ic in
  let rec aux () =
    let c = try Some (load_float_cpl ic) with _ -> None in
    match c with
    | Some c -> c :: aux ()
    | None -> []
  in
  ts, aux ()

let load_file' f =
  let ic = open_in f in
  let rec aux () =
    let l = try Some (load_line ic) with _ -> None in
    match l with
    | Some l -> l::aux ()
    | None -> []
  in
  aux ()

let filter_dist min max (x,y) =
  let d = sqrt (x*.x +. y*.y) in
  d >= min && d <= max

let load_file ?(min_dist=0.15) ?(max_dist=6.) f =
  let l = load_file' f in
  let l' =
    List.map (fun (ts,v) ->
      let v = List.filter (filter_dist min_dist max_dist) v in
      let x,y = List.split v in
      ts, { dx = Array.of_list x; dy = Array.of_list y }) l in
  Array.of_list l'


(**** filtering ****)

let far_enougth_filter kd a min_dist data =
  let dist = distance_transform (fun i -> i) kd a data in
  let dist = Array.mapi (fun i d -> i,d) dist in
  let distl = Array.to_list dist in
  let min_dist2 = min_dist *. min_dist in
  let far_enougth (i,d) = d >= min_dist2 in
  let ai = Array.of_list (List.filter far_enougth distl) in
  { dx = Array.map (fun (i,_) -> data.dx.(i)) ai;
    dy = Array.map (fun (i,_) -> data.dy.(i)) ai }
