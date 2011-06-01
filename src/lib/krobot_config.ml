(*
 * krobot_config.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

let sqr x = x *. x

let world_height = 2.1
let world_width = 3.
let robot_size = 0.26
let wheels_diameter = 0.098
let wheels_distance = 0.150
let wheels_position = robot_size /. 2.
let rotary_beacon_index_pos = (4. *. atan 1.) /. 2. (* left side *)
let object_radius = 0.1
let border_safety_distance = sqrt (sqr robot_size /. 2.) +. 0.05
let object_safety_distance = object_radius +. robot_size /. 2.
let beacon_safety_distance = 0.7
