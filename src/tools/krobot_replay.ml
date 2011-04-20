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
open Krobot_bus

let rec loop bus ic prev_timestamp =
  lwt timestamp, frame = Lwt_io.read_value ic in
  lwt () = Lwt_unix.sleep (timestamp -. prev_timestamp) in
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), CAN(Elec, frame)) in
  loop bus ic timestamp

lwt () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: krobot-replay <file>";
    exit 2
  end;

  lwt bus = Krobot_bus.get () in
  lwt ic = Lwt_io.open_file ~mode:Lwt_io.input Sys.argv.(1) in

  try_lwt
    lwt timestamp, frame = Lwt_io.read_value ic in
    lwt () = Krobot_bus.send bus (Unix.gettimeofday (), CAN(Elec, frame)) in
    loop bus ic timestamp
  with End_of_file ->
    Lwt_io.close ic
