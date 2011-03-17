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

let device = ref "slcan0"

let () = Krobot_init.arg "-device" (Arg.Set_string device) "<device> The device to use"

lwt () =
  lwt bus = Krobot_init.init_service "Driver" in

  while_lwt true do
    try_lwt
      (* Open the CAN bus. *)
      lwt can = Krobot_can_bus.open_can !device in

      let active, set_active = S.create true in

      (* D-Bus --> CAN *)
      let ev = E.map_s (Krobot_can_bus.send can) (E.when_ active (Krobot_can.recv bus)) in

      try_lwt
        (* CAN --> D-Bus *)
        while_lwt true do
          Krobot_can_bus.recv can >>= Krobot_can.send bus
        done
      with exn ->
        (* Make sure no more messages are sent on the CAN bus. *)
        set_active false;
        (* This is just here to keep a reference to [ev]. *)
        E.stop ev;
        raise_lwt exn
    with exn ->
      lwt () = Lwt_log.error ~exn "failure" in
      (* Wait a bit before retrying. *)
      Lwt_unix.sleep 0.5
  done
