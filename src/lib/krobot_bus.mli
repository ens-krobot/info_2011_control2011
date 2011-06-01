(*
 * krobot_bus.mli
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** The krobot bus. *)

open Krobot_geom

type t
  (** Type of a krobot bus connected to the local HUB. *)

val get : unit -> t Lwt.t
  (** [get ()] returns the krobot bus. It exits the program on
      error. *)

type frame_source = Elec | Info
    (** The source of CAN frames. *)

(** Type of message exchanged over the bus. *)
type message =
  | CAN of frame_source * Krobot_can.frame
      (** [CAN(source, frmae)] a CAN frame. *)
  | Log of string
      (** A log message. *)
  | Send
      (** Ask for sending parameters. *)
  | Kill of string
      (** Kill the given service. *)

  (** Trajectory messages. *)

  | Trajectory_vertices of vertice list * (vertice * vertice * vertice * vertice) list
      (** The list of vertices for the planned trajectory, along with
          the bezier curves. *)
  | Trajectory_set_vertices of vertice list
      (** Sets the trajectory. *)
  | Trajectory_add_vertice of vertice
      (** Add a vertice to the trajectory. *)
  | Trajectory_simplify of float
      (** Simplify the trajectory with the given tolerance. *)
  | Trajectory_go of float * float * float * float
      (** [Trajectory_go(rotation_speed, rotation_acceleration,
          moving_speed, moving_acceleration)]. *)
  | Trajectory_goto of vertice
      (** [Trajectory_goto dest] go to the given position. *)
  | Trajectory_stop
      (** Stop the current trajectory. *)
  | Trajectory_moving of bool
      (** Is the robot following a trajectory ? *)
  | Trajectory_find_path
      (** Find a path avoiding objects. *)

  (** Objects *)

  | Objects of vertice list
      (** The list of objects on the board. *)

  (** Sharps *)

  | Sharps of float array
      (** Distances measured by the sharps. *)

val string_of_message : message -> string
  (** Returns a string representation of the given message. *)

val send : t -> (float * message) -> unit Lwt.t
  (** [send bus (timestamp, message)] sends a message over the krobot
      bus. *)

val recv : t -> (float * message) React.event
  (** [recv bus] returns the event which receive messages from the
      krobot bus. *)
