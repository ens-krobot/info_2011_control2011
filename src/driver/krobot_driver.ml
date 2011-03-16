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

lwt () =
  if Array.length Sys.argv <> 2 then begin
    print_endline "usage: krobot-driver <interface>";
    exit 2;
  end;

  (* Open the CAN bus. *)
  lwt can = Krobot_can_bus.open_can Sys.argv.(1) in

  (* Open the D-Bus connection. *)
  lwt bus = OBus_bus.of_addresses [OBus_address.make "unix" [("abstract", "krobot")]] in

  (* D-Bus --> CAN *)
  lwt () =
    OBus_signal.connect (OBus_signal.with_context (Krobot_can.frames bus))
    >|= E.map_s (fun (ctx, frame) ->
                   if OBus_peer.name (OBus_context.sender ctx) = OBus_connection.name bus then
                     return ()
                   else
                     Krobot_can_bus.send can frame)
    >|= E.keep
  in

  (* CAN --> D-Bus *)
  while_lwt true do
    Krobot_can_bus.recv can >>= Krobot_can.send bus
  done
