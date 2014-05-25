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

val logger : t -> Lwt_log.logger
  (** [logger bus] creates a logger which sends log over [bus]. *)

type frame_source = Elec | Info
    (** The source of CAN frames. *)

type collision =
  | Col_bezier of Krobot_geom.Bezier.curve * (float * (Krobot_geom.vertice * float) option) list
  | Col_rotation of (Krobot_geom.vertice * float) list

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

  | Trajectory_path of Bezier.curve list
      (** The planned trajectory. *)
  | Trajectory_set_vertices of vertice list
      (** Sets the trajectory. *)
  | Trajectory_add_vertice of vertice
      (** Add a vertice to the trajectory. *)
  | Trajectory_simplify of float
      (** Simplify the trajectory with the given tolerance. *)
  | Trajectory_go
      (** Follow currently registered trajectory. *)
  | Trajectory_find_path
      (** Find a path avoiding objects. *)

  (** Objects *)

  | Objects of (vertice*float) list
      (** The list of objects on the board. *)

  (** Sharps *)

  | Sharps of float array
      (** Distances measured by the sharps. *)

  (** Strategy *)

  | Strategy_append of Krobot_action.t list
      (** Append the given list of action to the current strategy. *)
  | Strategy_stop
      (** Stop and empty the current strategy. *)
  | Strategy_set of Krobot_action.t list
      (** Stop and replace the current stategy by the given one. *)
  | Strategy_path of (Bezier.curve list) option
      (** Message emitted when the robot starts or stops a
          trajectory. *)
  | Strategy_finished

  (** Fake beacons *)

  | Set_fake_beacons of vertice option * vertice option
      (** Sets the positions of the fake  beacons. *)

  (** Collisions *)

  | Collisions of collision
  (** A curve and a list of collision. *)

  (** Vision *)
  | Coins of vertice list

  (** distances mesured by the URG (in millimeters) *)
  | Urg of vertice array
  | Urg_lines of (vertice*vertice) array

  | Beacon_raw of (int * int * int * int * int * int
      * int * int * int * int * int)

  | Match_start

  | Vision_find_target of int * string (* request id, selected camera *)
  | Vision_find_target_response of int * string * ((int * int) list)

  | Run_ax12_sequence of (string * Krobot_ax12_format.action list)
      (** log * actions *)
  | Run_ax12_framed_sequence of (string * Krobot_ax12_format.keyframe_dict * (int * int) list)
      (** log * keyframe dictionary * actions *)
  | Finished_ax12_sequence of string

val string_of_message : message -> string
  (** Returns a string representation of the given message. *)

val send : t -> (float * message) -> unit Lwt.t
  (** [send bus (timestamp, message)] sends a message over the krobot
      bus. *)

val recv : t -> (float * message) React.event
  (** [recv bus] returns the event which receive messages from the
      krobot bus. *)
