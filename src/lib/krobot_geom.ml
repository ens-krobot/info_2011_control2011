(*
 * krobot_geom.ml
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

let sqr x = x *. x

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

let tangent a b c =
  let a = vector origin a
  and b = vector origin b
  and c = vector origin c in
  let v = (b -| a) +| (b -| c) in
  v /| norm v

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
    let b = (r -| q) -| c in
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
end
