(*
 * krobot_action.mli
 * -----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Robot's actions *)

open Krobot_geom

type curve =
  | Curve_bezier of (bool * Bezier.curve)
  | Curve_rotation of direction * float (* final orientation *)
  | No_curve

type timeout =
  | Timeout_before of float
  | Timeout_started of float
  | Timeout_none

type lift_status =
  { moving_left : bool option;
    moving_right : bool option;
    homed_left : bool option;
    homed_right : bool option }

type node_kind =
  | Simple
  | Retry of int * t
  | Loop of t
  | Next

(** Type of actions. *)
and t =
  | Node of node_kind * t list

      (** A sequence of action to execute in order. *)
  | Stop
      (** Stop all actions. *)
  | Think
      (** This is the highest-level action. It instruct the VM to
          think about a new strategy. *)
  | Goto of vertice * vector option
      (** Go to the given point.
          if the bool parameter is true, the path is inverted according
          to the robot team *)
  | Simple_goto of vertice * vector option
      (** Same as Goto but can fail when obstacles block the trajectory *)

  | Random_move of ( vertice * vertice )

  | Set_limits of float * float * float * float
      (** limit the speed *)

  (* TODO: en faire un node pour pouvoir revenir a des limites normales en sortant
     d'une serie d'actions *)

  | Follow_path of vertice list * vector option * bool
      (** Follow the given path. It does not check for obstacles.
          the last boolean tells to correct the bezier curves
          to avoid obstacles.
          Use only if the lines between vertices does not colide obstacles *)
  | Bezier of float * vertice * vertice * vertice * vertice * float
      (** Follow the bezier curve determined by the given four
          vertices. The first float is the sign, the last one is the
          end velocity. *)
  | Move_straight of float
  | Set_curve of curve
      (** Set the curve currently being followed. *)

  | Turn of float * float * float
      (** angle, speed, acceleration *)

  | Wait_for_jack of bool
      (** Wait for the jack to be in the given state. *)
  | Wait_for_bezier_moving of bool * float option
      (** Wait for the robot to move or not with a limit date. *)
  | Wait_for_motors_moving of bool * float option
      (** Wait for the robot to move or not with a limit date. *)
  | Reset_odometry of [ `Red | `Blue | `Auto ]
      (** Set the odometry to the initial position according to the
          team selector or the given color. *)
  | Wait_for_odometry of [ `Eq | `Gt | `Ge | `Lt | `Le ] * int
      (** Wait for the curve parameter of the odometry to reach the
          given state. *)
  | Wait_for_orientation of float * float
      (** Wait_for_orientation(start,stop) *)
  | Wait_for_lift_status of lift_status * timeout

  | Try_something of vertice
      (** Try to do something that would bring us clother to the given
          vertice. *)

  | Fail
      (** Abort current strategy. *)

  | Wait_for_odometry_reset of [ `Red | `Blue | `Auto ]
  (** Wait for the odometry to say it is in red or blue initial position. *)

  | Wait_for of float
      (** Wait for the given number of seconds. *)
  | Wait_until of float
      (** Wait until the given date. *)

  | Start_timer of float * t list
  | Stop_timer
  | Start_match

  | Can of Krobot_can.frame
  | Set_led of [ `Red | `Yellow | `Green ] * bool
  | Set_orientation of float
      (** turn to given orrientation, fail if possible collision *)
  | Set_odometry of float option * float option * float option
      (** [Set_odometry (x,y,orientation)] set the provided odometry informations *)
  | Calibrate of vertice * float * float * float option * float option * float option
      (** [Calibrate ( approach_position, approach_orientation, distance,
                       supposed_x, supposed_y, supposed_orientation )]
          Go to approach_position with orientation approach_orientation,
          move of distance,
          at the end reset the odometry to given supposed coordinates

          i.e.: calibrate using a border
      *)

  | Elevator_homing

  | End (** send strategy finished *)

val to_string : t -> string
  (** [to_string action] returns the string representation of the
      given string. *)

val list_to_string : t list -> string
