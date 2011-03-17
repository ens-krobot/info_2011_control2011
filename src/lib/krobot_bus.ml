(*
 * krobot_bus.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

let section = Lwt_log.Section.make "bus"

type t = OBus_bus.t

external of_bus : OBus_bus.t -> t = "%identity"
external to_bus : t -> OBus_bus.t = "%identity"

let bus = lazy(
  let addresses =
    match try Some(Sys.getenv "DBUS_KROBOT_BUS_ADDRESS") with Not_found -> None with
      | Some addr ->
          OBus_address.of_string addr
      | None ->
          [OBus_address.make "unix" [("abstract", "krobot")]]
  in
  try_lwt
    OBus_bus.of_addresses addresses
  with exn ->
    lwt () = Lwt_log.error_f ~section ~exn "failed to connect to the krobot bus at address '%s'" (OBus_address.to_string addresses) in
    raise_lwt exn
)

let get () = Lazy.force bus
