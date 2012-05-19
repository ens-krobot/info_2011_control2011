(*
 * krobot_collision.mli
 * --------------------
 * Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Collision tests. *)

open Krobot_geom

val point_in_world : vertice -> bool
(** [point_in_world v] returns [true] iff [v] is in the world. *)

val point_in_world_safe : vertice -> bool
(** [point_in_world v] returns [true] iff [v] is in the world and not
    within the safety margin. *)

val robot_in_world : vertice -> float -> bool
(** [robot_in_world position orientation] returns [true] iff the robot
    (given by the position of the center of its wheels and its
    orientation) is in the world. It takes into account
    {!Krobot_config.safety_margin}. *)

val collision_robot_circle : vertice -> float -> vertice -> float -> bool
(** [collision_robot_circle pos angle center radius] returns [true]
    iff the circle and the robot intersect. *)

val move_possible : obj list -> vertice -> float -> float -> bool
(** [move_possible objects pos angle dist] returns [true] iff there is
    no collisions when moving by [dist] (can be negative). *)

val last_possible : obj list -> ('a * vertice * float) list -> 'a option
(** [prefix_possible objects configs] returns the last possible
    configuration such that all previous ones are possible if any. *)

val possible : obj list -> vertice -> float -> bool
(** [possible objects pos angle] returns [true] iff the given position
    is possible. *)
