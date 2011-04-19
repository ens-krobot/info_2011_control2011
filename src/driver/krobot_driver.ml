(*
 * krobot_driver.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* The interface between the CAN and D-Bus *)

open Lwt
open Lwt_react
open Krobot_bus

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true
let kill = ref false
let once = ref false

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
  "-kill", Arg.Set kill, " Kill any running driver and exit";
  "-once", Arg.Set once, " Do not reopen the device on errors";
]

let usage = "\
Usage: krobot-driver [options] [device]
<device> defaults to 'slcan0'
options are:"

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

lwt () =
  let args = ref [] in
  Arg.parse options (fun arg -> args := arg :: !args) usage;

  let device =
    match !args with
      | [] -> "slcan0"
      | [dev] -> dev
      | _ -> Arg.usage options usage; exit 2
  in

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  (* Fork if not prevented. *)
  if !fork then Lwt_daemon.daemonize ();

  (* Kill any running planner. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "driver") in

  (* The CAN bus, when it is available. *)
  let can_opt = ref None in

  (* Handle messages. *)
  E.keep
    (E.map_s
       (fun (ts, msg) ->
          match msg with
            | Kill "driver" ->
                exit 0

            | CAN frame -> begin
                match !can_opt with
                  | Some can ->
                      Krobot_can_bus.send can (ts, frame)
                  | None ->
                      return ()
              end

            | _ ->
                return ())
       (Krobot_bus.recv bus));

  while_lwt true do
    try_lwt
      (* Open the CAN bus. *)
      lwt can = Krobot_can_bus.open_can device in
      can_opt := Some can;

      try_lwt
        while_lwt true do
          lwt (ts, frame) = Krobot_can_bus.recv can in
          Krobot_bus.send bus (ts, CAN frame)
        done
      with exn ->
        (* Make sure no more messages are sent on the CAN bus. *)
        can_opt := None;
        lwt () = Krobot_can_bus.close can in
        raise_lwt exn
    with exn ->
      ignore (Lwt_log.error ~exn "failure");
      if !once then
        exit 0
      else
        (* Wait a bit before retrying. *)
        Lwt_unix.sleep 0.5
  done
