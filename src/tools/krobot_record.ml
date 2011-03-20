(*
 * krobot_record.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Record CAN frames. *)

open Lwt
open Lwt_react

lwt () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: krobot-record <file>";
    exit 2
  end;

  lwt bus = Krobot_bus.get () in
  lwt oc = Lwt_io.open_file ~mode:Lwt_io.output Sys.argv.(1) in

  (* The proxy for the driver. *)
  let driver = OBus_proxy.make (OBus_peer.make (Krobot_bus.to_bus bus) "fr.krobot.Driver") ["fr"; "krobot"; "CAN"] in

  (* Receive frames comming from the driver. *)
  lwt ev = OBus_signal.connect (OBus_signal.make Krobot_interface_can.Fr_krobot_CAN.s_message driver) in

  (* Write all frames to the output file. *)
  E.keep (E.map_s (fun v -> Lwt_io.write_value oc (Krobot_can.frame_of_tuple v)) ev);

  fst (wait ())
