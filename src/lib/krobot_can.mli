(*
 * krobot_can.mli
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** CAN frames *)

(** {6 CAN frames} *)

exception Invalid_frame of string
  (** Exception raised when trying to make an invalid frame. The
      argument is an error message. Reasons why may not be valid are:

      - the identifier is too large for the frame format,
      - there is more that 8 bytes of data.
  *)

(** Type of frame kinds. *)
type kind =
  | Data
      (** The frame contains data. *)
  | Error
      (** The frame is an error frame. *)

(** Frame formats. *)
type format =
  | F11bits
      (** The identifier is on 11 bits. *)
  | F29bits
      (** The identifier is on 29 bits. *)

(** Type of CAN frames. *)
type frame = private {
  identifier : int;
  (** The CAN identifier. *)
  kind : kind;
  (** The type of the frame. *)
  remote : bool;
  (** [true] iff this is aremote transmission request. *)
  format : format;
  (** The format of the frame. *)
  data : string;
  (** The data of the frame. It is a array of [0..8] bytes. *)
}

val identifier : frame -> int
val kind : frame -> kind
val remote : frame -> bool
val format : frame -> format
val data : frame -> string

val frame :
  identifier : int ->
  kind : kind ->
  remote : bool ->
  format : format ->
  data : string -> frame
  (** Create a frame. It raises {!Invalid_frame} if the frame is not
      valid. *)

val string_of_frame : frame -> string
  (** Returns the string representation of a frame. *)

(** {6 Reading/writing numbers} *)

val get_sint8 : string -> int -> int
val get_uint8 : string -> int -> int

val get_sint16 : string -> int -> int
val get_uint16 : string -> int -> int

val get_sint32 : string -> int -> int
val get_uint32 : string -> int -> int

val get_float32 : string -> int -> float

val put_sint8 : string -> int -> int -> unit
val put_uint8 : string -> int -> int -> unit

val put_sint16 : string -> int -> int -> unit
val put_uint16 : string -> int -> int -> unit

val put_sint32 : string -> int -> int -> unit
val put_uint32 : string -> int -> int -> unit

val put_float32 : string -> int -> float -> unit

(** {6 D-Bus frame conversion} *)

val value_of_frame : float * frame -> OBus_value.V.single
  (** Converts a can frame into a D-Bus structure. *)

val frame_of_value : OBus_value.V.single -> float * frame
  (** Converts a D-Bus structure into a can frame. *)

(** {6 Sending/receiving frames} *)

val send : Krobot_bus.t -> (float * frame) -> unit Lwt.t
  (** [send bus (timestamp, frame)] sends the given frame over
      D-Bus. *)

val recv : Krobot_bus.t -> (float * frame) React.event
  (** [recv bus] returns is the event which occurs whan a CAN frame is
      received. *)
