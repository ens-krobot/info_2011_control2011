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
let angle v = atan2 v.vy v.vx

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

let vector_of_polar ~norm ~angle = {
  vx = norm *. cos angle;
  vy = norm *. sin angle;
}

let distance a b =
  sqrt (sqr (a.x -. b.x) +. sqr (a.y -. b.y))

let tangents a b c =
  let ba = vector b a /| distance b a and bc = vector b c /| distance b c in
  let v1 = ba -| bc and v2 = bc -| ba in
  (v1 /| norm v1, v2 /| norm v2)

let rot_mat theta =
  [| [| cos theta; -. (sin theta); 0.; |];
     [| sin theta; cos theta; 0. |];
     [| 0.; 0.; 1. |]; |]

let mult m v =
  Array.init (Array.length m)
    (fun k -> Array.fold_left (+.) 0.
      (Array.mapi (fun i n -> v.(i) *. n) m.(k)))

let normalize v = mul v (1. /. norm v)

type obj = { pos : vertice; size : float }

(* +-----------------------------------------------------------------+
   | Cubic bezier curves                                             |
   +-----------------------------------------------------------------+ *)

module Bezier = struct

  type curve = {
    src : vertice;
    dst : vertice;
    p : vector;
    a : vector;
    b : vector;
    c : vector;
  }

  let vert_of_vect p = { x = p.vx; y = p.vy }
  let vect_of_vert p = { vx = p.x; vy = p.y }

  let pqrs' c =
    let p = c.p in
    let s = c.a +| c.b +| c.c +| c.p in
    let q = c.c/|3. +| c.p in
    let r = c.p +| c.c *| (2./.3.) +| c.b /| 3. in
    p, q, r, s

  let pqrs c =
    let p, q, r, s = pqrs' c in
    vert_of_vect p, vert_of_vect q, vert_of_vect r, vert_of_vect s

  let of_vertices p q r s =
    let src = p and dst = s in
    let p = vector origin p
    and q = vector origin q
    and r = vector origin r
    and s = vector origin s in
    let c = (q -| p) *| 3. in
    let b = (r -| q) *| 3. -| c in
    let a = s -| p -| c -| b in
    { src; dst; p; a; b; c }

  let mul_d1 c coef =
    let p,q,r,s = pqrs' c in
    let v1 = q -| p in
    let v1' = v1 *| coef in
    let q' = p +| v1' in
    of_vertices (vert_of_vect p) (vert_of_vect q') (vert_of_vect r) (vert_of_vect s)

  let mul_d2 c coef =
    let p,q,r,s = pqrs' c in
    let v2 = r -| s in
    let v2' = v2 *| coef in
    let r' = s +| v2' in
    of_vertices (vert_of_vect p) (vert_of_vect q) (vert_of_vect r') (vert_of_vect s)

  let src c = c.src
  let dst c = c.dst

  let string_of_curve c =
    Printf.sprintf "<bezier { x = %f; y = %f } -> { x = %f; y = %f }>" c.src.x c.src.y c.dst.x c.dst.y

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

  let curve_vertices b n =
    let rec aux i =
      if i < 0
      then []
      else
        let t = ((float i) /. (float n)) in
        (t, vertice b t)::(aux (i-1))
    in
    aux n

  let dt b t =
    let t1 = t in
    let t2 = t1 *. t in
    (b.a *| (3. *. t2)) +| (b.b *| (2. *. t1)) +| b.c

  let ddt b t =
    (b.a *| (6. *. t)) +| (b.b *| 2.)

  let cr b t =
    let db = dt b t in
    let ddb = ddt b t in
    ((db.vx*.db.vx +. db.vy*.db.vy) ** 1.5) /. (db.vx*.ddb.vy -. db.vy*.ddb.vx)

  type robot_info =
    { r_width : float; (* distance between wheels: m *)
      r_max_wheel_speed : float; (* m / s *)
      r_max_a : float; } (* m / s^2 *)

  exception Exit_for_vel

  let velocity_profile b v_max omega_max at_max ar_max v_ini v_end du =
    let n_pts = int_of_float ( 1. /. du +. 1.) in
    let us = Array.init n_pts (fun i -> (float_of_int i) *. du) in
    let v_tab = Array.map (fun _ -> v_max) us in
    let mins = ref [(0,v_ini); ((n_pts-1),v_end)] in
    let cr_pp = ref (abs_float (cr b 0.)) in
    let cr_p = ref (abs_float (cr b du)) in
    Array.iteri (fun i u -> if (i > 1) then begin
      let cr = abs_float (cr b u) in
      if (cr >= !cr_p && !cr_p < !cr_pp) then begin
        mins := (i, sqrt (ar_max*.cr)) :: !mins;
      end;
      cr_pp := !cr_p;
      cr_p := cr;
      end
    ) us;
    mins := List.rev !mins;
    List.iter (fun (m, vm) ->
      if (vm < v_tab.(m)) then begin
        v_tab.(m) <- vm;
        (try
           for i = m-1 downto 0 do
             let db = dt b (us.(i+1)) in
             let dsu = sqrt (db.vx*.db.vx +. db.vy*.db.vy) in
             let dt = (-.v_tab.(i+1)+.sqrt(v_tab.(i+1)*.v_tab.(i+1)+.2.*.at_max*.du*.dsu))/.at_max in
             let nv = v_tab.(i+1)+.at_max*.dt in
             if (nv < v_tab.(i)) then
               v_tab.(i) <- nv
             else
               raise Exit_for_vel
           done
         with Exit_for_vel -> ());
        (try
           for i = m+1 to (n_pts-1) do
             let db = dt b (us.(i-1)) in
             let dsu = sqrt (db.vx*.db.vx +. db.vy*.db.vy) in
             let dt = (-.v_tab.(i-1)+.sqrt(v_tab.(i-1)*.v_tab.(i-1)+.2.*.at_max*.du*.dsu))/.at_max in
             let nv = v_tab.(i-1)+.at_max*.dt in
             if (nv < v_tab.(i)) then
               v_tab.(i) <- nv
             else
               raise Exit_for_vel
           done
         with Exit_for_vel -> ());
      end
    ) !mins;
    v_tab

  let wheel_speed_rapport r b t =
    let s' = norm (dt b t) in
    let { vx = x'; vy = y' } = dt b t in
    let { vx = x'';vy = y''} = ddt b t in
    let theta' = ( y'' *. x' -. x'' *. y' ) /. ( x' *. x' +. y' *. y' ) in
    let rapport = ( r.r_width *. theta' *. 0.5 +. s' )
      /. ( -. r.r_width *. theta' *. 0.5 +. s' ) in
    rapport

  let max_wheel_speed r b t =
    let rapport = wheel_speed_rapport r b t in
    let rapport' = abs_float rapport in
    let rapport = ( min rapport' (1. /. rapport') )
      *. (if rapport >= 0. then 1. else -.1.) in
    ( 1. +. rapport ) *. 0.5 *. r.r_max_wheel_speed

(*
  let trajectory n r b =
    let du = 1. /. (float n) in
    let rec aux i =
      if i > n
      then []
      else
        let u = du *. (float i) in
        let p = point ~u b in
        (u,p)::(aux (i+1))
    in
    aux 0
*)

  let integrate n f ui uf =
    let du = ( uf -. ui ) /. (float n) in
    let acc = ref 0. in
    for i = 0 to (n-1) do
      acc := !acc +. (f ~du ~u:( ui +. ( (float i) *. du ) ) ) *. du ;
    done;
    !acc

  let time n b r =
    integrate n (fun ~du ~u ->
      let len = abs_float (norm (dt b u)) in
      len /. (max_wheel_speed r b u) ) 0. 1.

  let length n b =
    integrate n (fun ~du ~u ->
      abs_float (norm (dt b u)) ) 0. 1.

  let fold_vertices ?last f initial vertices acc =
    let add_vertices sign q r v1 v2 acc = f sign q (translate q v1) (translate r v2) r acc in

    (* Compute cubic bezier curves. *)
    let rec loop sign acc = function
      |  p :: (q :: r :: s :: _ as rest) ->
           (* Computes tangents with a length that is half of the
              minimum length of the adjacent segments. *)
           let _, v1 = tangents p q r and v2, _ = tangents q r s in
           let v1 = v1 *| (min (distance p q) (distance q r) /. 2.)
           and v2 = v2 *| (min (distance q r) (distance r s) /. 2.) in
           loop sign (add_vertices sign q r v1 v2 acc) rest
      | [p; q; r] ->
          let _, v1 = tangents p q r and v2 = vector r q /| distance q r in
          let v1 = v1 *| (min (distance p q) (distance q r) /. 2.)
          and v2 = v2 *| (distance q r /. 2.) in
          let v2 = match last with
            | None -> v2
            | Some v -> v *| (distance q r /. 2.) in
          add_vertices sign q r v1 v2 acc
      | _ ->
          acc
    in
    match vertices with
      | q :: r :: s :: _ ->
          let initial, sign = if prod initial (vector q r) < 0. then (minus initial, -1.) else (initial, 1.) in
          let v1 = initial
          and v2, _ = tangents q r s in
          let v1 = v1 *| (distance q r /. 2.)
          and v2 = v2 *| (distance q r /. 2.) in
          loop 1. (add_vertices sign q r v1 v2 acc) vertices
      | [q; r] ->
          let initial, sign = if prod initial (vector q r) < 0. then (minus initial, -1.) else (initial, 1.) in
          let v1 = initial
          and v2 = vector r q /| distance q r  in
          let v1 = v1 *| (distance q r /. 2.)
          and v2 = v2 *| (distance q r /. 2.) in
          let v2 = match last with
            | None -> v2
            | Some v -> v *| (distance q r /. 2.) in
          add_vertices sign q r v1 v2 acc
      | [_] | [] ->
          acc

  let fold_curves ?last f initial vertices acc =
    fold_vertices ?last (fun sign p q r s acc -> f sign (of_vertices p q r s) acc) initial vertices acc

end

