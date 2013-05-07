
(************************************************
   2d Point cloud registration: based on
   'Robust Registration of 2D and 3D Point Sets'
   by Andrew W. Fitzgibbon
 ************************************************)

open Kd_tree
open Lacaml.D

type a = { ath : float; ax : float; ay : float }
type data = { dx : float array; dy : float array }

let transform' dth dx dy ax ay =
  let co = cos dth in
  let si = sin dth in
  let len = Array.length ax in
  let x' = Array.init len (fun i -> ax.(i) *. co -. ay.(i) *. si +. dx) in
  let y' = Array.init len (fun i -> ax.(i) *. si +. ay.(i) *. co +. dy) in
  x', y'

let transform { ath; ax; ay } { dx; dy } =
  let dx, dy = transform' ath ax ay dx dy in
  { dx; dy }

let make_kd_tree { dx; dy } =
  let a = Array.init (Array.length dx) (fun i -> i, { x = dx.(i); y = dy.(i) }) in
  Kd_tree.make a

type kernel = float -> float

let closest_points kd_tree { dx; dy } =
  Array.init (Array.length dx)
    (fun i ->
      let pos, _, sq_dist = Kd_tree.nearest_neighbor
          { Kd_tree.x = dx.(i); Kd_tree.y = dy.(i) } kd_tree in
      pos, sq_dist)

let distance_transform (kernel:float -> float) kd_tree a data =
  let { dx = datax'; dy = datay' } = transform a data in
  Array.init (Array.length datax')
    (fun i ->
      let _, _, sq_dist = nearest_neighbor { x = datax'.(i); y = datay'.(i) } kd_tree in
      kernel sq_dist)

let epsilon = 1e-15

let jacobian ?(kernel=fun i -> i) kd_tree a data =
  let v_base = distance_transform kernel kd_tree a data in
  let a_dth = { a with ath = a.ath +. epsilon } in
  let a_dx = { a with ax = a.ax +. epsilon } in
  let a_dy = { a with ay = a.ay +. epsilon } in
  let diff a =
    let v_diff = distance_transform kernel kd_tree a data in
    Array.mapi (fun i v -> (v -. v_base.(i)) /. epsilon) v_diff
  in
  let v_dth = diff a_dth in
  let v_dx = diff a_dx in
  let v_dy = diff a_dy in
  v_base, [|v_dth; v_dx; v_dy|]


open Lacaml.D

let mat_sum m1 m2 =
  assert(Mat.dim1 m1 = Mat.dim1 m2);
  assert(Mat.dim2 m1 = Mat.dim2 m2);
  let v1 = Mat.to_col_vecs m1 in
  let v2 = Mat.to_col_vecs m2 in
  Mat.of_col_vecs
    (Array.init (Array.length v1) (fun i -> Vec.add v1.(i) v2.(i)))

let mat_sub m1 m2 =
  assert(Mat.dim1 m1 = Mat.dim1 m2);
  assert(Mat.dim2 m1 = Mat.dim2 m2);
  let v1 = Mat.to_col_vecs m1 in
  let v2 = Mat.to_col_vecs m2 in
  Mat.of_col_vecs
    (Array.init (Array.length v1) (fun i -> Vec.add v1.(i) v2.(i)))

let lid n l =
  let m = Mat.identity n in
  Mat.scal l m;
  m

(* gradiant descent step *)

let step ?kernel ?(lambda=1.) kd data a =
  let e',j' = jacobian ?kernel kd a data in
  let e = Vec.of_array e' in
  let jt = Mat.of_array j' in
  let j = Mat.transpose jt in
  let size = Lacaml.D.Mat.dim2 j in

  let t1' = gemm jt j in
  let t1'' = lid size lambda in
  let t1 = mat_sum t1' t1'' in
  getri t1;
  let t2 = gemm t1 jt in
  let update = gemv t2 e in
  let a = Vec.of_array [|a.ath; a.ax; a.ay|] in
  let a = Vec.sub a update in
  let a = Vec.to_array a in
  { ath = a.(0); ax = a.(1); ay = a.(2) }

(* simpler evolution step, equivalent to [step] with [lambda = 0.] *)

let simple_step ?kernel kd data a =
  let e',j' = jacobian ?kernel kd a data in
  let e = Vec.of_array e' in
  let jt = Mat.of_array j' in
  let j = Mat.transpose jt in

  let t1 = gemm jt j in
  getri t1;
  let t2 = gemm t1 jt in
  let update = gemv t2 e in
  let a = Vec.of_array [|a.ath; a.ax; a.ay|] in
  let a = Vec.sub a update in
  let a = Vec.to_array a in
  { ath = a.(0); ax = a.(1); ay = a.(2) }

(***** default kernel ****)

let sigma = 0.05
let sigma2 = sigma*.sigma
let default_kernel x =
  if x < sigma2
  then x
  else 2. *. sigma *. (sqrt x) -. sigma2



(***** data filtering *****)

(* filtering multiple point associated to the same value: if more than
   one point closest to a point [p], keep only the closest one *)

let closest_match ai =
  let n = Array.fold_left (fun m (n,_) -> max m n) (-1) ai in
  let matcher = Array.init (n+1) (fun _ -> None) in
  Array.iteri (fun index (matched,(d:float)) ->
    match matcher.(matched) with
    | None -> matcher.(matched) <- Some (index,d)
    | Some (_,d1) ->
      if d < d1 then matcher.(matched) <- Some (index,d)) ai;
  matcher

let get_closest matcher ((ax',ay'):float array * float array) =
  let r = ref [] in
  Array.iter (function
    | None -> ()
    | Some (i,_) ->
      r := (ax'.(i), ay'.(i)) :: !r) matcher;
  let x,y = List.split !r in
  Array.of_list x, Array.of_list y

let closest_filter kd a d2 =
  let d' = transform a d2 in
  let cp = closest_points kd d' in
  let cm = closest_match cp in
  let dx, dy = get_closest cm (d2.dx,d2.dy) in
  { dx; dy }

(* filter point farther than 3 time the median distance *)

let far_filter kd a d2 =
  let d' = transform a d2 in
  let cp = closest_points kd d' in
  let cp = Array.mapi (fun i (_,d) -> i,d) cp in
  Array.sort (fun (_,d1) (_,d2) -> compare d1 d2) cp;
  let (_,median) = cp.(Array.length cp/2) in
  let cl = Array.to_list cp in
  let too_far (i,d) = d <= median *. 9. in
  let ai = Array.of_list (List.filter too_far cl) in
  { dx = Array.map (fun (i,_) -> d2.dx.(i)) ai;
    dy = Array.map (fun (i,_) -> d2.dy.(i)) ai }


(*************************)

let rec register_simple ?kernel kd data a n =
  if n = 0 then a
  else
    let data' = closest_filter kd a data in
    let data' = far_filter kd a data' in
    let a = simple_step ?kernel kd data' a in
    register_simple ?kernel kd data a (n-1)

let rec register ?kernel ?lambda kd data a n =
  if n = 0 then a
  else
    let data' = closest_filter kd a data in
    let data' = far_filter kd a data' in
    let a = step ?kernel ?lambda kd data' a in
    register ?kernel ?lambda kd data a (n-1)
