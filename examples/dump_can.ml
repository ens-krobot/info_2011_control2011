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
open Krobot_can

let rec loop bus =
  lwt frame = Krobot_can_bus.recv bus in
  let buf = Buffer.create 24 in
  String.iter (fun ch -> Printf.bprintf buf " %02x" (Char.code ch)) frame.data;
  lwt () =
    Lwt_io.printf "can frame received:
  id = %d;
  kind = %s;
  remote = %B;
  format = %d bits;
  data[%d] =%s;
"
      frame.identifier
      (match frame.kind with
         | Data -> "data"
         | Error -> "error")
      frame.remote
      (match frame.format with
         | F11bits -> 11
         | F29bits -> 29)
      (String.length frame.data)
      (Buffer.contents buf)
  in
  loop bus

lwt () =
  if Array.length Sys.argv <> 2 then begin
    print_endline "usage: dump-can <interface>";
    exit 2;
  end;
  try_lwt
    Krobot_can_bus.open_can Sys.argv.(1) >>= loop
  with Unix.Unix_error(error, func, arg) ->
    Lwt_log.error_f "'%s' failed with: %s" func (Unix.error_message error)
