(*
 * krobot_can_bus.mli
 * ------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Bus CAN *)

type t
  (** Type of a CAN bus. *)

val open_can : string -> t Lwt.t
  (** [open_can iface] opens the specified bus CAN. For example
      [open_can "can0"]. *)

val close : t -> unit Lwt.t
  (** Closes the given bus CAN. *)

exception Closed
  (** Exception raised when trying to use a closed bus. *)

val send : t -> Krobot_can.frame -> unit Lwt.t
  (** [send bus frame] sends [frame] over [bus]. *)

val recv : t -> Krobot_can.frame Lwt.t
  (** [recv bus] waits and reads one frame from the given bus. *)
