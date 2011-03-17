(*
 * krobot_service.ml
 * -----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

include OBus_proxy.Private

open Lwt
open Lwt_react
open Krobot_interface_service.Fr_krobot_Service

let all bus =
  OBus_proxy.make
    (OBus_peer.anonymous (Krobot_bus.to_bus bus))
    ["fr"; "krobot"; "Service"]

let make bus name =
  if name = "" then
    all bus
  else
    OBus_proxy.make
      (OBus_peer.make
         (Krobot_bus.to_bus bus)
         (Printf.sprintf "fr.krobot.Service.%s" name))
      ["fr"; "krobot"; "Service"]

let name service =
  let s = OBus_proxy.name service in
  let i = String.rindex s '.' in
  String.sub s (i + 1) (String.length s - (i + 1))

let list bus =
  lwt names = OBus_bus.list_names (Krobot_bus.to_bus bus) in
  return
    (List.map
       (fun name ->
          OBus_proxy.make
            (OBus_peer.make (Krobot_bus.to_bus bus) name)
            ["fr"; "krobot"; "Service"])
       (List.sort
          String.compare
          (List.filter
             (fun name ->
                String.sub name 0 (String.rindex name '.') = "fr.krobot.Service")
             names)))

let monitor bus =
  lwt e = OBus_signal.connect (OBus_bus.name_owner_changed (Krobot_bus.to_bus bus)) in
  lwt l = list bus in
  return (S.hold l (E.map_s (fun _ -> list bus) e))

let kill service =
  OBus_method.call_no_reply m_kill service ()

let log service =
  OBus_signal.make s_log service
