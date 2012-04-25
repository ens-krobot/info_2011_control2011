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
  | Node of t list
      (** A sequence of action to execute in order. *)
  | Stop
      (** Stop all actions. *)
  | Think
      (** This is the highest-level action. It instruct the VM to
          think about a new strategy. *)
  | Goto of vertice
      (** Go to the given point. *)
  | Follow_path of vertice list
      (** Follow the given path. It does not check for obstacles. *)
  | Bezier of float * vertice * vertice * vertice * vertice * float
      (** Follow the bezier curve determined by the given four
          vertices. The first float is the sign, the last one is the
          end velocity. *)
  | Set_curve of Bezier.curve option
      (** Set the curve currently being followed. *)
  | Wait_for_jack of bool
      (** Wait for the jack to be in the given state. *)
  | Wait_for_moving of bool
      (** Wait for the robot to move or not. *)
  | Reset_odometry of [ `Red | `Blue | `Auto ]
      (** Set the odometry to the initial position according to the
          team selector or the given color. *)
  | Wait_for_odometry of [ `Eq | `Gt | `Ge | `Lt | `Le ] * int
      (** Wait for the curve parameter of the odometry to reach the
          given state. *)
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

val to_string : t -> string
  (** [to_string action] returns the string representation of the
      given string. *)