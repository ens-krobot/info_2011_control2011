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
  (** The height of the board. *)

val red_fixed_beacons : Krobot_geom.vertice list
  (** Positions of the fixed beacons for the red team *)

val blue_fixed_beacons : Krobot_geom.vertice list
  (** Positions of the fixed beacons for the blue team *)

val robot_length : float
val robot_width : float

val safety_margin : float
(** The distance between the robot and the border/an object must
    always be greater than this vlaue. *)

val wheels_diameter : float
(** The diameter of the wheels. *)

val wheels_distance : float
  (** The distance between the two wheels. *)

val wheels_position : float
  (** The distance between the axe of the wheels and the back of the
      robot. *)

val robot_radius : float
  (** distance between the point between the wheel and the farthest point *)

val rotary_beacon_index_pos : float
  (** The angle of the rotary beacon index angle with respect to the
      robot's front *)

val beacon_radius : float
  (** Radius of the ennemy. *)

val fire_radius : float
  (** Bounding box radius of fires. *)

val red_initial_position : Krobot_geom.vertice * float
  (** position and angle of the robot as red *)

val blue_initial_position : Krobot_geom.vertice * float
  (** position and angle of the robot as blue *)


val fixed_obstacles : Krobot_geom.obj list

val test_obstacles : Krobot_geom.obj list

val initial_fires : (Krobot_geom.vertice * float) list

val initial_torches : Krobot_geom.vertice list

val rear_sharp_lower_threshold : int
val rear_sharp_upper_threshold : int

val urg_position : Krobot_geom.vertice

val urg_angle_limits : float * float
  (** angles above which urg points are discarded *)

val urg_min_distance : float
  (** distance below which urg points are discarded *)

val urg_angles : float array
(* angles (in radiant) for each index of urg messages *)
