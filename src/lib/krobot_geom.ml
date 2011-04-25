(*
 * krobot_geom.ml
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

let sqr x = x *. x

let pi = 4. *. atan 1.

let math_mod_float a b =
  let b2 = b /. 2. in
  let modf = mod_float a b in
  if modf > b2 then
    modf -. b
  else if modf < -. b2 then
    modf +. b
  else
    modf

(* +-----------------------------------------------------------------+
   | Vectors                                                         |
   +-----------------------------------------------------------------+ *)

type vector = { vx : float; vy : float }

let null = { vx = 0.; vy = 0. }

let add a b = {
  vx = a.vx +. b.vx;
  vy = a.vy +. b.vy;
}

let sub a b = {
  vx = a.vx -. b.vx;
  vy = a.vy -. b.vy;
}

let minus v = {
  vx = -. v.vx;
  vy = -. v.vy;
}

let mul v s = {
  vx = v.vx *. s;
  vy = v.vy *. s;
}

let div v s = {
  vx = v.vx /. s;
  vy = v.vy /. s;
}

let prod a b =
  a.vx *. b.vx +. a.vy *. b.vy

let ( +| ) = add
let ( -| ) = sub
let ( ~| ) = minus
let ( *| ) = mul
let ( /| ) = div

let norm v = sqrt (sqr v.vx +. sqr v.vy)

(* +-----------------------------------------------------------------+
   | Vertices                                                        |
   +-----------------------------------------------------------------+ *)

type vertice = { x : float; y : float }

let origin = { x = 0.; y = 0. }

let translate a v = {
  x = a.x +. v.vx;
  y = a.y +. v.vy;
}

let vector a b = {
  vx = b.x -. a.x;
  vy = b.y -. a.y;
}

let distance a b =
  sqrt (sqr (a.x -. b.x) +. sqr (a.y -. b.y))

let tangents a b c =
  let ba = vector b a /| distance b a and bc = vector b c /| distance b c in
  let v1 = ba -| bc and v2 = bc -| ba in
  (v1 /| norm v1, v2 /| norm v2)

(* +-----------------------------------------------------------------+
   | Cubic bezier curves                                             |
   +-----------------------------------------------------------------+ *)

module Bezier = struct

  type curve = {
    p : vector;
    a : vector;
    b : vector;
    c : vector;
  }

  let of_vertices p q r s =
    let p = vector origin p
    and q = vector origin q
    and r = vector origin r
    and s = vector origin s in
    let c = (q -| p) *| 3. in
    let b = (r -| q) *| 3. -| c in
    let a = s -| p -| c -| b in
    { p; a; b; c }

  let make ~p ~s ~vp ~vs ~a ~error_max =
    let sp = norm vp and ss = norm vs in
    (* Compute Rp and Rs. *)
    let r_p = sqr sp /. a and r_s = sqr ss /. a in
    (* Normalize speed vectors. *)
    let vp = vp /| sp and vs = vs /| ss in
    (* Compute g0, g1, g2, h0, h1 and h2. *)
    let g0 = s.x -. p.x and h0 = s.y -. p.y in
    let g1 = 2. *. (vs.vy *. vp.vx -. vs.vx *. vp.vy) in
    let g2 = g1 in
    let h1 = 2. *. (h0 *. vp.vx -. g0 *. vp.vy) in
    let h2 = 2. *. (h0 *. vs.vx -. g0 *. vs.vy) in
    (* The loop for finding d1 and d2. *)
    let rec loop d1 d2 =
      let rho_p = 3. *. sqr d1 /. (h1 +. d2 *. g1)
      and rho_s = 3. *. sqr d2 /. (h2 +. d1 *. g2) in
      let err_1 = r_p -. rho_p and err_2 = r_s -. rho_s in
      let error = max (abs_float err_1) (abs_float err_2) in
      if error < error_max then
        let q = translate p (vp *| d1)
        and r = translate s (vs *| d2) in
        of_vertices p q r s
      else
        loop (d1 +. err_1 /. r_p) (d2 +. err_2 /. r_s)
    in
    loop 1. 1.

  let vertice b t =
    if t < 0. || t > 1. then invalid_arg "Krobot_geom.Bezier.vertice";
    let t1 = t in
    let t2 = t1 *. t in
    let t3 = t2 *. t in
    translate origin ((b.a *| t3) +| (b.b *| t2) +| (b.c *| t1) +| b.p)

  let fold_vertices f initial vertices acc =
    let add_vertices q r v1 v2 acc = f q (translate q v1) (translate r v2) r acc in

    (* Compute cubic bezier curves. *)
    let rec loop acc = function
      |  p :: (q :: r :: s :: _ as rest) ->
           (* Computes tangents with a length that is half of the
              minimum length of the adjacent segments. *)
           let _, v1 = tangents p q r and v2, _ = tangents q r s in
           let v1 = v1 *| (min (distance p q) (distance q r) /. 2.)
           and v2 = v2 *| (min (distance q r) (distance r s) /. 2.) in
           loop (add_vertices q r v1 v2 acc) rest
      | [p; q; r] ->
          let _, v1 = tangents p q r and v2 = vector r q /| distance q r in
          let v1 = v1 *| (min (distance p q) (distance q r) /. 2.)
          and v2 = v2 *| (distance q r /. 2.) in
          add_vertices q r v1 v2 acc
      | _ ->
          acc
    in
    match vertices with
      | q :: r :: s :: _ ->
          let initial = if prod initial (vector q r) < 0. then minus initial else initial in
          let v1 = initial
          and v2, _ = tangents q r s in
          let v1 = v1 *| (distance q r /. 2.)
          and v2 = v2 *| (distance q r /. 2.) in
          loop (add_vertices q r v1 v2 acc) vertices
      | [q; r] ->
          let initial = if prod initial (vector q r) < 0. then minus initial else initial in
          let v1 = initial
          and v2 = vector r q /| distance q r  in
          let v1 = v1 *| (distance q r /. 2.)
          and v2 = v2 *| (distance q r /. 2.) in
          add_vertices q r v1 v2 acc
      | [_] | [] ->
          acc

  let fold_curves f initial vertices acc =
    fold_vertices (fun p q r s acc -> f (of_vertices p q r s) acc) initial vertices acc
end
