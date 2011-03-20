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

let default_address = [OBus_address.make "unix" [("abstract", "krobot")]]

let address =
  match try Some(Sys.getenv "DBUS_KROBOT_BUS_ADDRESS") with Not_found -> None with
    | Some str -> begin
        try
          OBus_address.of_string str
        with OBus_address.Parse_failure(str, pos, err) ->
          ignore (Lwt_log.error_f "The environment variable DBUS_KROBOT_BUS_ADDRESS contains an invalid D-Bus address: \"%s\", at position %d: %s" str pos err);
          default_address
      end
    | None ->
        default_address

let bus = lazy(
  try_lwt
    lwt () = Lwt_log.info "openning the D-Bus krobot bus" in
    OBus_bus.of_addresses address
  with exn ->
    lwt () = Lwt_log.error_f ~section ~exn "failed to connect to the D-Bus krobot bus at address \"%s\"" (OBus_address.to_string address) in
    exit 1
)

let get () = Lazy.force bus

(* We put that here because all programs use this module. *)
let () =
  Printexc.register_printer
    (function
       | Unix.Unix_error(error, func, "") ->
           Some(Printf.sprintf "%s: %s" func (Unix.error_message error))
       | Unix.Unix_error(error, func, arg) ->
           Some(Printf.sprintf "%s(%S): %s" func arg (Unix.error_message error))
       | _ ->
           None)
