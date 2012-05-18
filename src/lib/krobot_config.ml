(*
 * krobot_config.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

let sqr x = x *. x

let world_height = 2.
let world_width = 3.
let robot_size = 0.30
let robot_width = 0.32
let wheels_diameter = 0.098
let wheels_distance = 0.224
let wheels_position = robot_size *. 0.2
let rotary_beacon_index_pos = 0.
let object_radius = 0.1
let border_safety_distance =
  sqrt (sqr robot_size /. 2.) +. 0.05
  (* find a correct one *)
let object_safety_distance = object_radius +. robot_size /. 2.
let beacon_safety_distance = 0.4

let coin_radius = 0.06

open Krobot_geom

let pi = 4. *. atan 1.

let red_initial_position =
  { x = 0.10;
    y = world_height -. 0.25; },
  0.

let blue_initial_position =
  { x = world_width -. 0.10;
    y = world_height -. 0.25; },
  pi


let symetrical p =
  { p with pos = { p.pos with x = world_width -. p.pos.x } }


let rec (-->) i j =
  if i > j
  then []
  else i::((i+1) --> j)

let line_obs p1 p2 =
  let v = vector p1 p2 in
  let d = distance p1 p2 in
  let n = int_of_float (d /. 0.1) in
  let l = 0 --> (n-1) in
  List.map (fun i ->
    let c = float i /. float n in
    { pos = { x = c *. v.vx +. p1.x;
              y = c *. v.vy +. p1.y;};
      size = 0.05 } ) l

let left_obstacles =
  [
    (* the trees *)
    { pos =
        { x = 0.64 +. 0.477;
          y = 1. };
      size = sqrt (2. *. 0.125 *. 0.125) +. 0.12; };

    (* entry position *)
    { pos =
        { x = 0.;
          y = 1.5 };
      size = 0.05; };
    { pos =
        { x = 0.1;
          y = 1.5 };
      size = 0.05; };
    { pos =
        { x = 0.2;
          y = 1.5 };
      size = 0.05; };
    { pos =
        { x = 0.3;
          y = 1.5 };
      size = 0.05; };
    { pos =
        { x = 0.4;
          y = 1.5 };
      size = 0.05; };

    (* coins at the bottom *)

    { pos =
        { x = 1.5;
          y = 0.3; };
      size = 0.15 };

  ] @ (line_obs { x = 0.325; y = 0. } { x = 0.325 +. (0.075 /. 2.); y = 0.75 } )

let fixed_obstacles =
  [
    { pos =
        { x = 1.5;
          y = 1. };
      size = 0.1; };
  ] @ (List.map symetrical left_obstacles) @ left_obstacles


let initial_coins =
  List.map (fun (x, y) -> {x;y})
  [ 2., 1.5;
    1., 1.5;
    (* 0.45, 0.3; *)
    (* 2.55, 0.3; *) ]

