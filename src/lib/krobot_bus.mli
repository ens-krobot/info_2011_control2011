(*
 * krobot_bus.mli
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** The krobot bus. *)

type t = private OBus_bus.t
    (** Type of the krobot bus. *)

external of_bus : OBus_bus.t -> t = "%identity"
external to_bus : t -> OBus_bus.t = "%identity"

val get : unit -> t Lwt.t
  (** [get ()] returns the krobot bus. *)

(**/**)

val address : string ref
