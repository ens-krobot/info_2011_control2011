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

(** Type of state of an encoder. *)
type encoder_state = {
  es_position : int;
  (** The position of the encoder. *)
  es_direction : direction;
  (** The direction of the encoder. *)
}

(** Type of messages. *)
type t =
  | Encoder_state_1_2 of encoder_state * encoder_state
      (** State of the encoder 1 and 2. *)
  | Encoder_state_3_4 of encoder_state * encoder_state
      (** State of the encoder 3 and 4. *)
  | Unknown of Krobot_can.frame
      (** An unknown can frame. *)

val to_string : t -> string
  (** Returns the string representation of the given message. *)

val encode : t -> Krobot_can.frame
  (** Encode the given message into a CAN frame. *)

val decode : Krobot_can.frame -> t
  (** Decode the given CAN frame into a message. *)

val send : Krobot_bus.t -> t -> unit Lwt.t
  (** [send bus message] sends the given message over D-Bus. *)

val recv : Krobot_bus.t -> t React.event
  (** [recv bus] is the event which receive messages. *)
