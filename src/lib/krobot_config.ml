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
let robot_size = 0.26
let wheels_diameter = 0.098
let wheels_distance = 0.224
let wheels_position = robot_size *. 0.2
let rotary_beacon_index_pos = (4. *. atan 1.) /. 2. (* left side *)
let object_radius = 0.1
let border_safety_distance = sqrt (sqr robot_size /. 2.) +. 0.05
let object_safety_distance = object_radius +. robot_size /. 2.
let beacon_safety_distance = 0.7

let coin_radius = 0.12

open Krobot_geom

let pi = 4. *. atan 1.

let red_initial_position =
  { x = 0.25;
    y = world_height -. 0.25; },
  0.

let blue_initial_position =
  { x = world_width -. 0.25;
    y = world_height -. 0.25; },
  pi


let fixed_obstacles =
  [
    (* the trees *)
    { pos =
        { x = 0.64 +. 0.477;
          y = 1. };
      size = sqrt (2. *. 0.125 *. 0.125); };
    { pos =
        { x = world_width -. (0.64 +. 0.477);
          y = 1. };
      size = sqrt (2. *. 0.125 *. 0.125); };

    { pos =
        { x = 1.5;
          y = 1. };
      size = 0.1; };

    (* entry position *)
    { pos =
        { x = 0.;
          y = 1.5 };
      size = 0.1; };
    { pos =
        { x = 0.2;
          y = 1.5 };
      size = 0.1; };
    { pos =
        { x = 0.4;
          y = 1.5 };
      size = 0.1; };

    { pos =
        { x = world_width;
          y = 1.5 };
      size = 0.1; };
    { pos =
        { x = world_width -. 0.2;
          y = 1.5 };
      size = 0.1; };
    { pos =
        { x = world_width -. 0.4;
          y = 1.5 };
      size = 0.1; };


  ]
