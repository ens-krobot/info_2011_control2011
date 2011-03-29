(*
 * send_can.ml
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Krobot_can


lwt () =
  if Array.length Sys.argv <> 2 then begin
    print_endline "usage: dump-can <interface>";
    exit 2;
  end;
  try_lwt
    lwt can = Krobot_can_bus.open_can Sys.argv.(1) in
    Krobot_can_bus.send can
      (Unix.gettimeofday (),
       frame
         ~identifier:103
         ~kind:Data
         ~remote:true
         ~format:F29bits
         ~data:"")
  with Unix.Unix_error(error, func, arg) ->
    Lwt_log.error_f "'%s' failed with: %s" func (Unix.error_message error)

