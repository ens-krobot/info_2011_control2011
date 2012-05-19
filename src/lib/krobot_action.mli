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

(** Type of actions. *)
type t =
  | Node of t option * t list
      (** A sequence of action to execute in order. *)
  | Stop
      (** Stop all actions. *)
  | Think
      (** This is the highest-level action. It instruct the VM to
          think about a new strategy. *)
  | Goto of bool * vertice * vector option
      (** Go to the given point.
          if the bool parameter is true, the path is inverted according
          to the robot team *)

  | Set_limits of float * float * float
      (** limit the speed *)

  (* TODO: en faire un node pour pouvoir revenir a des limites normales en sortant
     d'une serie d'actions *)

  | Follow_path of bool * vertice list * vector option * bool
      (** Follow the given path. It does not check for obstacles.
          if the bool parameter is true, the path is inverted according
          to the robot team.
          the last boolean tells to correct the bezier curves
          to avoid obstacles.
          Use only if the lines between vertices does not colide obstacles *)
  | Bezier of float * vertice * vertice * vertice * vertice * float
      (** Follow the bezier curve determined by the given four
          vertices. The first float is the sign, the last one is the
          end velocity. *)
  | Set_curve of Bezier.curve option
      (** Set the curve currently being followed. *)
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

  | Try_something of vertice
      (** Try to do something that would bring us clother to the given
          vertice. *)

  | Fail
      (** Abort current strategy. *)

  | Wait_for_odometry_reset of [ `Red | `Blue | `Auto ]
  (** Wait for the odometry to say it is in red or blue initial position. *)

  | Load of [ `Front | `Back ]
      (** Load a pawn. *)
  | Lift_down of [ `Front | `Back ]
      (** Move the front or back lift down. *)
  | Lift_up of [ `Front | `Back ]
      (** Move the front or back lift up. *)
  | Open_grip_low of [ `Front | `Back ]
      (** Open the front or back low grip. *)
  | Close_grip_low of [ `Front | `Back ]
      (** Close the front or back low grip. *)
  | Open_grip_high of [ `Front | `Back ]
      (** Open the front or back low grip. *)
  | Close_grip_high of [ `Front | `Back ]
      (** Close the front or back low grip. *)
  | Wait_for of float
      (** Wait for the given number of seconds. *)
  | Wait_until of float
      (** Wait until the given date. *)
  | Wait_for_grip_open_low of [ `Front | `Back ]
      (** Wait for the given low grip to be opened. *)
  | Wait_for_grip_close_low of [ `Front | `Back ]
      (** Wait for the given low grip to be opened. *)

  | Start_timer
  | Can of Krobot_can.frame
  | Set_led of [ `Red | `Yellow | `Green ] * bool
  | Set_orientation of float
      (** turn to given orrientation, fail if possible collision *)
  | Set_odometry of float option * float option * float option
      (** [Set_odometry (x,y,orientation)] set the provided odometry informations *)
  | Calibrate of vertice * float * float * float option * float option * float option
      (** [Calibrate ( approach_position, approach_orientation, distance,
                       supposed_x, supposed_y, supposed_orientation )
          Go to approach_position with orientation approach_orientation,
          move of distance,
          at the end reset the odometry to given supposed coordinates

          i.e.: calibrate using a border
      *)
  | End (** send strategy finished *)

val to_string : t -> string
  (** [to_string action] returns the string representation of the
      given string. *)
