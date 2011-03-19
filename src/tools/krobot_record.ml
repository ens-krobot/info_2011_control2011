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
  let file = ref "krobot.record" in
  Krobot_init.arg "-output" (Arg.Set_string file) "<file> output file";
  lwt bus = Krobot_init.init_program "Record" in

  lwt oc = Lwt_io.open_file ~mode:Lwt_io.output !file in

  (* The proxy for the driver. *)
  let driver = OBus_proxy.make (OBus_peer.make (Krobot_bus.to_bus bus) "fr.krobot.Driver") ["fr"; "krobot"; "CAN"] in

  (* Receive frames comming from the driver. *)
  lwt ev = OBus_signal.connect (OBus_signal.make Krobot_interface_can.Fr_krobot_CAN.s_message driver) in

  (* Write all frames to the output file. *)
  E.keep (E.map_s (fun v -> Lwt_io.write_value oc (Krobot_can.frame_of_tuple v)) ev);

  fst (wait ())
