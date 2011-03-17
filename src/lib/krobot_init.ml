(*
 * krobot_init.ml
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Lwt_react

let section = Lwt_log.Section.make "init"

(* +-----------------------------------------------------------------+
   | Misc                                                            |
   +-----------------------------------------------------------------+ *)

let () =
  Printexc.register_printer
    (function
       | Unix.Unix_error(error, func, "") ->
           Some(Printf.sprintf "%s: %s" func (Unix.error_message error))
       | Unix.Unix_error(error, func, arg) ->
           Some(Printf.sprintf "%s(%S): %s" func arg (Unix.error_message error))
       | _ ->
           None)

let program_name = Filename.basename Sys.executable_name
let usage = Printf.sprintf "%s <options>" program_name

(* +-----------------------------------------------------------------+
   | Command line arguments                                          |
   +-----------------------------------------------------------------+ *)

let arguments = ref []
let arg key spec doc = arguments := (key, spec, doc) :: !arguments

let () = arg "-dbus-address" (Arg.Set_string Krobot_bus.address) "<address> The address to connect to the krobot bus"

(* +-----------------------------------------------------------------+
   | Programs                                                        |
   +-----------------------------------------------------------------+ *)

let init_program name =
  Arg.parse (Arg.align !arguments) (fun _ -> raise (Arg.Bad "unknown option")) usage;
  lwt () = Lwt_log.info ~section "openning the robot bus" in
  try_lwt
    Krobot_bus.get ()
  with exn ->
    exit 1

(* +-----------------------------------------------------------------+
   | Services                                                        |
   +-----------------------------------------------------------------+ *)

open Krobot_interface_service.Fr_krobot_Service

let init_service name =
  let no_fork = ref false and kill = ref false in
  arg "-no-fork" (Arg.Set no_fork) " Run in foreground";
  arg "-kill" (Arg.Set kill) " Kill any running instance and exit";
  Arg.parse (Arg.align !arguments) (fun _ -> raise (Arg.Bad "unknown option")) usage;

  lwt () = Lwt_log.info ~section "openning the robot bus" in
  lwt bus = try_lwt Krobot_bus.get () with exn -> exit 1 in

  let dbus_name = Printf.sprintf "fr.krobot.Service.%s" name in

  (* Kill other instances. *)
  lwt () =
    if !kill then begin
      try_lwt
        lwt owner = OBus_bus.get_name_owner (Krobot_bus.to_bus bus) dbus_name in
        lwt () = Lwt_log.info_f ~section "killing the running %s service" name in
        Krobot_service.kill (Krobot_service.of_proxy (OBus_proxy.make (OBus_peer.make (Krobot_bus.to_bus bus) owner) ["fr"; "krobot"; "Service"]))
      with OBus_bus.Name_has_no_owner _ ->
        return ()
      finally
        exit 0
    end else
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
    if !no_fork then
      Lwt_log.notice_f ~section "starting %s in foreground mode" name
    else begin
      lwt () = Lwt_log.notice_f ~section "starting %s in daemon mode" name in
      Lwt_daemon.daemonize ();
      return ()
    end
  in

  (* Create the service object and export it. *)
  let obj = OBus_object.make ~interfaces:[make { m_kill = (fun obj () -> exit 0) }] ["fr"; "krobot"; "Service"] in
  OBus_object.attach obj ();
  OBus_object.export (Krobot_bus.to_bus bus) obj;

  (* Send logs over D-Bus. *)
  let dbus_logger =
    Lwt_log.make
      (fun section level lines ->
         if level > Lwt_log.Info then
           let buf = Buffer.create 42 in
           let lines =
             List.map
               (fun line ->
                  Buffer.clear buf;
                  Lwt_log.render ~buffer:buf ~template:"$(level)@$(name)[$(section)]: $(message)" ~section ~level ~message:line;
                  Buffer.contents buf)
               lines
           in
           OBus_signal.emit s_log obj (String.concat "\n" lines)
         else
           return ())
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

  return bus
