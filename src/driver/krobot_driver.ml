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

  (* Open the CAN bus. *)
  lwt can = Krobot_can_bus.open_can !device in

  (* D-Bus --> CAN *)
  E.keep (E.map_s (Krobot_can_bus.send can) (Krobot_can.recv bus));

  (* CAN --> D-Bus *)
  while_lwt true do
    Krobot_can_bus.recv can >>= Krobot_can.send bus
  done
