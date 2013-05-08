
type direction =
  | Vert
  | Hori

type vertice = { x : float; y : float }

type 'a t =
  | Split of direction * 'a * vertice * 'a t * 'a t
  | Leaf of 'a * vertice
  | Empty

let splitn n l =
  let rec aux n l acc =
    if n = 0
    then List.rev acc, l
    else match l with
      | [] -> assert false
      | t::q -> aux (n-1) q (t::acc) in
  aux n l []

let rec make_horiz a =
  match Array.length a with
  | 0 -> Empty
  | 1 ->
    let va, vert = a.(0) in
    Leaf (va, vert)
  | len ->
    Array.sort (fun (_,{ x = x1 }) (_,{ x = x2 }) -> compare x1 x2 ) a;
    let median = (len / 2) in
    let med_val, median_elt = a.(median) in
    let left = Array.sub a 0 median in
    let right = Array.sub a (median + 1) (len - median - 1) in
    let l1 = make_vert left in
    let l2 = make_vert right in
    Split (Hori, med_val, median_elt, l1, l2)

and make_vert a =
  match Array.length a with
  | 0 -> Empty
  | 1 ->
    let va, vert = a.(0) in
    Leaf (va, vert)
  | len ->
    Array.sort (fun (_,{ y = y1 }) (_,{ y = y2 }) -> compare y1 y2 ) a;
    let median = (len / 2) in
    let med_val, median_elt = a.(median) in
    let left = Array.sub a 0 median in
    let right = Array.sub a (median + 1) (len - median - 1) in
    let l1 = make_vert left in
    let l2 = make_vert right in
    Split (Vert, med_val, median_elt, l1, l2)

let make a = make_horiz a

let sq_dist v1 v2 =
  let dx = v1.x -. v2.x in
  let dy = v1.y -. v2.y in
  dx *. dx +. dy *. dy

let sq x = x *. x

let rec nearest_neighbor' v ((cur_val,cur,cur_dist) as curr) t = match t with
  | Empty -> curr
  | Leaf (va,p) ->
    let dist = sq_dist v p in
    if dist < cur_dist
    then va, p, dist
    else curr
  | Split (dir, med_val, median_elt, left, right) ->
    let split_distance = match dir with
      | Hori -> sq (v.x -. median_elt.x)
      | Vert -> sq (v.y -. median_elt.y) in
    let (_, _, cur_dist) as curr =
      let dist = sq_dist v median_elt in
      if dist < cur_dist
      then med_val, median_elt, dist
      else curr in
    let cmp = match dir with
      | Hori -> compare v.x median_elt.x
      | Vert -> compare v.y median_elt.y in
    if cmp < 0
    then
      let (_, _, cur_dist) as curr = nearest_neighbor' v curr left in
      if split_distance >= cur_dist
      then curr
      else nearest_neighbor' v curr right
    else
      let (_, _, cur_dist) as curr = nearest_neighbor' v curr right in
      if split_distance >= cur_dist
      then curr
      else nearest_neighbor' v curr left

let nearest_neighbor v = function
  | Empty -> failwith "nearest_neighbor: empty"
  | Leaf (va, p) -> va, p, sq_dist p v
  | Split (_, med_val, median_elt, _, _) as t ->
    let curr = (med_val, median_elt, sq_dist median_elt v) in
    nearest_neighbor' v curr t

let rec closer' dist_sq v t = match t with
  | Empty -> false
  | Leaf (va,p) -> sq_dist v p <= dist_sq
  | Split (dir, med_val, median_elt, left, right) ->
    let split_distance = match dir with
      | Hori -> sq (v.x -. median_elt.x)
      | Vert -> sq (v.y -. median_elt.y) in
    if split_distance <= dist_sq
    then begin
      if sq_dist v median_elt <= dist_sq
      then true
      else closer' dist_sq v left || closer' dist_sq v right
    end
    else
      let cmp = match dir with
        | Hori -> compare v.x median_elt.x
        | Vert -> compare v.y median_elt.y in
      if cmp > 0
      then closer' dist_sq v right
      else closer' dist_sq v left

let closer dist v t = closer' (sq dist) v t

let add_closer dist_sq v p va acc =
  if sq_dist v p <= dist_sq
  then va::acc
  else acc

let rec closer_points' dist_sq v t acc = match t with
  | Empty -> acc
  | Leaf (va,p) ->
    add_closer dist_sq v p va acc
  | Split (dir, med_val, median_elt, left, right) ->
    let split_distance = match dir with
      | Hori -> sq (v.x -. median_elt.x)
      | Vert -> sq (v.y -. median_elt.y) in
    if split_distance <= dist_sq
    then begin
      let acc = add_closer dist_sq v median_elt med_val acc in
      let acc = closer_points' dist_sq v left acc in
      closer_points' dist_sq v right acc
    end
    else
      let cmp = match dir with
        | Hori -> compare v.x median_elt.x
        | Vert -> compare v.y median_elt.y in
      if cmp > 0
      then closer_points' dist_sq v right acc
      else closer_points' dist_sq v left acc

let closer_points dist v t = closer_points' (sq dist) v t []

let rec depth = function
  | Split (_, _, _, t1,t2 ) ->
    1 + max (depth t1) (depth t2)
  | Leaf _ -> 1
  | Empty -> 0

(* let a = [|{x = 1.; y = 2.}; {x = 3.; y = 4.}; {x = 2.; y = 2.}; {x = 3.; y = 1.}; {x = 4.; y = 2.}|] *)
(* let a' = Array.mapi (fun i v -> i,v) a *)
(* let t = make a' *)
(* let (i,r,d) = nearest_neighbor { x = 3.; y = 2.5 } t *)
