(*
 * krobot_can.mli
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** {6 Reading/writing integers} *)

val get_int8 : string -> int -> int
val get_uint8 : string -> int -> int

val get_int16 : string -> int -> int
val get_uint16 : string -> int -> int

val get_int32 : string -> int -> int
val get_uint32 : string -> int -> int

val put_int8 : string -> int -> int -> unit
val put_uint8 : string -> int -> int -> unit

val put_int16 : string -> int -> int -> unit
val put_uint16 : string -> int -> int -> unit

val put_int32 : string -> int -> int -> unit
val put_uint32 : string -> int -> int -> unit
