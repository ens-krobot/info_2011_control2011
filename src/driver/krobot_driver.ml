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

  lwt bus = Krobot_bus.get () in
  lwt () = Krobot_service.init bus ~kill:!kill ~fork:!fork "Driver" in

  while_lwt true do
    try_lwt
      (* Open the CAN bus. *)
      lwt can = Krobot_can_bus.open_can device in

      let active, set_active = S.create true in

      (* D-Bus --> CAN *)
      let ev = E.map_s (Krobot_can_bus.send can) (E.when_ active (Krobot_can.recv bus)) in

      try_lwt
        (* CAN --> D-Bus *)
        while_lwt true do
          Krobot_can_bus.recv can >>= Krobot_can.send bus
        done
      with exn ->
        lwt () = Krobot_can_bus.close can in
        (* Make sure no more messages are sent on the CAN bus. *)
        set_active false;
        (* This is just here to keep a reference to [ev]. *)
        E.stop ev;
        raise_lwt exn
    with exn ->
      lwt () = Lwt_log.error ~exn "failure" in
      if !once then
        exit 0
      else
        (* Wait a bit before retrying. *)
        Lwt_unix.sleep 0.5
  done
