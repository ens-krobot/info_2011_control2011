(*
 * krobot_init.mli
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Initialization *)

val arg : Arg.key -> Arg.spec -> Arg.doc -> unit
  (** [arg key spec doc] registers the given command line argument. *)

val init_program : string -> Krobot_bus.t Lwt.t
  (** [init_program name] initialises a program. A program always run
      in foreground. *)

val init_service : string -> Krobot_bus.t Lwt.t
  (** [init_service name] initialises a service. A service may run in
      foreground or in background according to command line arguments.
      This module ensure that only one instance of the service is
      running at a time. It also export a simple interface over
      D-Bus. *)
