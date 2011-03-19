(*
 * krobot_replay.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Record CAN frames. *)

open Lwt
open Lwt_react

let rec loop bus ic delta prev_timestamp =
  lwt timestamp, frame = Lwt_io.read_value ic in
  lwt () = Lwt_unix.sleep (timestamp -. prev_timestamp) in
  lwt () = Krobot_can.send bus (timestamp +. delta, frame) in
  loop bus ic delta timestamp

lwt () =
  let file = ref "krobot.record" in
  Krobot_init.arg "-input" (Arg.Set_string file) "<file> input file";
  lwt bus = Krobot_init.init_program "Record" in

  lwt ic = Lwt_io.open_file ~mode:Lwt_io.input !file in

  try_lwt
    (* Read the first frame. *)
    lwt timestamp, frame = Lwt_io.read_value ic in
    (* Compute the difference of time to add to each timestamp. *)
    let delta = Unix.gettimeofday () -. timestamp in
    lwt () = Krobot_can.send bus (timestamp +. delta, frame) in
    loop bus ic delta timestamp
  with End_of_file ->
    Lwt_io.close ic
