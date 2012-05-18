(*
 * krobot_daemon.ml
 * ----------------
 * Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

let daemonize bus =
  Lwt_daemon.daemonize ~syslog:false ();
  Lwt_log.default := Krobot_bus.logger bus
