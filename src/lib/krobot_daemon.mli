(*
 * krobot_daemon.mli
 * -----------------
 * Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

val daemonize : Krobot_bus.t -> unit
  (** [daemonize bus] daemonizes the running program. *)
