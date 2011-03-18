(*
 * krobot_message.mli
 * ------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Krobot messages. *)

type direction = Forward | Backward
    (** Type of directions. *)

(** Type of messages. *)
type t =
  | Encoder_position_direction_3_4 of int * direction * int * direction
      (** The position and direction of encoders 3 and 4. *)
  | Encoder_position_speed_3 of float * float
      (** The position and speed of encoder 3. *)
  | Encoder_position_speed_4 of float * float
      (** The position and speed of encoder 4. *)
  | Unknown of Krobot_can.frame
      (** An unknown can frame. *)

val to_string : t -> string
  (** Returns the string representation of the given message. *)

val encode : t -> Krobot_can.frame
  (** Encode the given message into a CAN frame. *)

val decode : Krobot_can.frame -> t
  (** Decode the given CAN frame into a message. *)

val send : Krobot_bus.t -> (float * t) -> unit Lwt.t
  (** [send bus (timestamp, message)] sends the given message over
      D-Bus. *)

val recv : Krobot_bus.t -> (float * t) React.event
  (** [recv bus] is the event which receive messages. *)
