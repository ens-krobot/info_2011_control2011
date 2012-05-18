(*
 * krobot_config.mli
 * -----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Parameters *)

val world_width : float
  (** The width of the board. *)

val world_height : float
  (** THe height of the board. *)

val robot_size : float
  (** The size of the robot, which is a square.
      heuu plus maintenant... *)

val robot_width : float

val wheels_diameter : float
  (** The diameter of the wheels. *)

val wheels_distance : float
  (** The distance between the two wheels. *)

val wheels_position : float
  (** The distance between the axe of the wheels and the back of the
      robot. *)

val rotary_beacon_index_pos : float
  (** The angle of the rotary beacon index angle with respect to the
      robot's front *)

val object_radius : float
  (** Radius of objects. *)

val border_safety_distance : float
  (** Minimum distance between borders and the robot. *)

val object_safety_distance : float
  (** Minimum distance from the center of objects to the center robot. *)

val beacon_safety_distance : float
  (** Minimum distance between the center of the robot and the beacon. *)

val coin_radius : float
  (** Radius of coins. *)

val red_initial_position : Krobot_geom.vertice * float
  (** position and angle of the robot as red *)

val blue_initial_position : Krobot_geom.vertice * float
  (** position and angle of the robot as blue *)


val fixed_obstacles : Krobot_geom.obj list

val initial_coins : Krobot_geom.vertice list
