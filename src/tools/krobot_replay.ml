(*
 * krobot_replay.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Send recorded CAN frames to the bus. *)

open Lwt
open Lwt_react
open Krobot_bus

let rec loop bus prev_timestamp =
  lwt timestamp, message = Lwt_io.read_value Lwt_io.stdin in
  lwt () = Lwt_unix.sleep (timestamp -. prev_timestamp) in
  match message with
    | CAN (Elec, _) ->
      lwt () = Krobot_bus.send bus (Unix.gettimeofday (), message) in
      loop bus timestamp
    | _ ->
      loop bus timestamp

lwt () =
  lwt bus = Krobot_bus.get () in

  try_lwt
    lwt timestamp, message = Lwt_io.read_value Lwt_io.stdin in
    match message with
      | CAN (Elec, _) ->
        lwt () = Krobot_bus.send bus (Unix.gettimeofday (), message) in
        loop bus timestamp
      | _ ->
        loop bus timestamp
  with End_of_file ->
    return ()
