(*
 * dump_can.ml
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Dump all frames received from the CAN. *)

open Lwt
open CAN

let rec loop bus =
  lwt frame = CAN.recv bus in
  let data = String.create 8 in
  String.blit frame.frame_data 0 data 0 (String.length frame.frame_data);
  lwt () =
    Lwt_io.printf "can frame received:
  id = %d;
  type = %s;
  remote = %B;
  format = %d bits;
  data = %02x %02x %02x %02x %02x %02x %02x %02x;
"
      frame.frame_identifier
      (match frame.frame_type with
         | Type_data -> "data"
         | Type_error -> "error")
      frame.frame_remote
      (match frame.frame_format with
         | Format_11bits -> 11
         | Format_29bits -> 29)
      (Char.code data.[0])
      (Char.code data.[1])
      (Char.code data.[2])
      (Char.code data.[3])
      (Char.code data.[4])
      (Char.code data.[5])
      (Char.code data.[6])
      (Char.code data.[7])
  in
  loop bus

lwt () =
  if Array.length Sys.argv <> 2 then begin
    print_endline "usage: dump-can <interface>";
    exit 2;
  end;
  try_lwt
    CAN.open_can Sys.argv.(1) >>= loop
  with Unix.Unix_error(error, func, arg) ->
    Lwt_log.error_f "'%s' failed with: %s" func (Unix.error_message error)
