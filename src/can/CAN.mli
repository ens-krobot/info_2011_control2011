(*
 * CAN.mli
 * -------
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

exception Invalid_frame of string
  (** Exception raised when trying to send an invalid frame. The
      argument is an error message. *)

(** Type of frame. *)
type frame_type =
  | Type_data
      (** The frame contains data. *)
  | Type_error
      (** The frame is an error frame. *)

type frame_format =
  | Format_11bits
      (** The identifier is on 11 bits. *)
  | Format_29bits
      (** The identifier is on 29 bits. *)

(** Type of CAN frames. *)
type frame = {
  frame_identifier : int;
  (** The CAN identifier. *)
  frame_type : frame_type;
  (** The type of the frame. *)
  frame_remote : bool;
  (** [true] iff this is aremote transmission request. *)
  frame_format : frame_format;
  (** The format of the frame. *)
  frame_data : string;
  (** The data of the frame. It is a array of [0..8] bytes. *)
}

val send : t -> frame -> unit Lwt.t
  (** [send bus frame] sends [frame] over [bus]. *)

val recv : t -> frame Lwt.t
  (** [recv bus] waits and reads one frame from the given bus. *)
