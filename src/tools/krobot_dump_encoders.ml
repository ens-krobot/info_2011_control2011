(*
 * krobot_dump_encoders.ml
 * -----------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Dump values of encoders into files. *)

open Lwt
open Lwt_react
open Krobot_message

lwt () =
  lwt bus = Krobot_bus.get () in

  lwt oc1 = Lwt_io.open_file ~mode:Lwt_io.output "krobot1.dump"
  and oc2 = Lwt_io.open_file ~mode:Lwt_io.output "krobot2.dump" in

  E.keep
    (E.map_s
       (fun (timestamp, msg) ->
          match msg with
            | Encoder_position_speed_3(pos, speed) ->
                Lwt_io.fprintlf oc1 "%f %f %f" timestamp pos speed
            | Encoder_position_direction_3_4(pos3, dir3, pos4, dir4) ->
                Lwt_io.fprintlf oc2 "%f %d %d" timestamp pos3 (match dir3 with Forward -> 0 | Backward -> 1)
            | _ ->
                return ())
       (Krobot_message.recv bus));

  lwt () = Lwt_io.printl "press Enter to exit." in
  lwt _ = Lwt_io.read_line Lwt_io.stdin in
  return ()
