(*
 * krobot_service.mli
 * ------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

include OBus_proxy.Private

val make : Krobot_bus.t -> string -> t
  (** [make bus name pid] creates a service proxy from its name. *)

val all : Krobot_bus.t -> t
  (** Proxy that represent all running services. You can use it to
      receive logs from all services. *)

val name : t -> string
  (** Returns the name of the given service. *)

val list : Krobot_bus.t -> t list Lwt.t
  (** List all available services on the bus. *)

val monitor : Krobot_bus.t -> t list React.signal Lwt.t
  (** Returns the signal holding the sorted list of services on the
      bus. *)

val kill : t -> unit Lwt.t
  (** [kill service] kills the given service. *)

val log : t -> string OBus_signal.t
  (** [log service] is the D-Bus signal which occurs when [service]
      emit a log message. *)
