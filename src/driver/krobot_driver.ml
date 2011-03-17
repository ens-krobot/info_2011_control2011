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
  lwt bus = Krobot_bus.get () in

  (* D-Bus --> CAN *)
  E.keep (E.map_s (Krobot_can_bus.send can) (Krobot_can.recv bus));

  (* CAN --> D-Bus *)
  while_lwt true do
    Krobot_can_bus.recv can >>= Krobot_can.send bus
  done
