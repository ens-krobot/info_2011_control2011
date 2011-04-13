(*
 * krobot_service.ml
 * -----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

include OBus_proxy.Private

let section = Lwt_log.Section.make "service"

(* +-----------------------------------------------------------------+
   | Service management                                              |
   +-----------------------------------------------------------------+ *)

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

let kill' bus name =
  try_lwt
    lwt owner = OBus_bus.get_name_owner bus name in
    let proxy = OBus_proxy.make (OBus_peer.make bus owner) ["fr"; "krobot"; "Service"] in
    OBus_method.call_no_reply m_kill proxy ()
  with OBus_bus.Name_has_no_owner _ ->
    return ()

let kill service =
  kill' (OBus_proxy.connection service) (OBus_proxy.name service)

let log service =
  OBus_signal.make s_log service

(* +-----------------------------------------------------------------+
   | Creating services                                               |
   +-----------------------------------------------------------------+ *)

open Krobot_interface_service.Fr_krobot_Service

let init bus ?(fork=true) ?(kill=false) name =
  let dbus_name = Printf.sprintf "fr.krobot.Service.%s" name in

  (* Kill other instances. *)
  lwt () =
    if kill then
      lwt () = kill' (Krobot_bus.to_bus bus) dbus_name in
      exit 0
    else
      return ()
  in

  (* Exit the program when we lost the name. *)
  lwt () =
    OBus_signal.connect (OBus_bus.name_lost (Krobot_bus.to_bus bus))
    >|= E.map (fun lost_name -> if dbus_name = lost_name then exit 0)
    >|= E.keep
  in

  (* Daemonize or not. *)
  lwt () =
    if fork then begin
      lwt () = Lwt_log.notice_f ~section "starting %s in daemon mode" name in
      Lwt_daemon.daemonize ();
      return ()
    end else
      Lwt_log.notice_f ~section "starting %s in foreground mode" name
  in

  (* Create the service object and export it. *)
  let obj = OBus_object.make ~interfaces:[make { m_kill = (fun obj () -> exit 0) }] ["fr"; "krobot"; "Service"] in
  OBus_object.attach obj ();
  OBus_object.export (Krobot_bus.to_bus bus) obj;

  (* Send logs over D-Bus. *)
  let dbus_logger =
    Lwt_log.make
      (fun section level lines ->
         let buf = Buffer.create 42 in
         let lines =
           List.map
             (fun line ->
                Buffer.clear buf;
                Lwt_log.render ~buffer:buf ~template:"$(name)[$(section)]: $(message)" ~section ~level ~message:line;
                Buffer.contents buf)
             lines
         in
         OBus_signal.emit s_log obj (String.concat "\n" lines))
      return
  in

  Lwt_log.default := Lwt_log.broadcast [!Lwt_log.default; dbus_logger];

  (* Register the program on the bus. *)
  lwt () =
    OBus_bus.request_name (Krobot_bus.to_bus bus) ~allow_replacement:true ~replace_existing:true dbus_name >>= function
      | `Primary_owner ->
          return ()
      | _ ->
          lwt () = Lwt_log.error_f ~section "cannot obtain the name %S on D-Bus" dbus_name in
          exit 1
  in

  return ()
