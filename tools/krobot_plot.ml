(*
 * krobot_plot.ml
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Krobot_can

type data = {
  coder_pos : int;
  coder_dir : int;
}

let decode_frame frame =
  let data = Krobot_can.data frame in
  ({ coder_pos = get_uint16 data 0;
     coder_dir = get_uint8 data 4 },
   { coder_pos = get_uint16 data 2;
     coder_dir = get_uint8 data 5 })

let rec loop bus oc =
  let time = Unix.gettimeofday () in
  lwt coder1, coder2 = Krobot_can_bus.recv bus >|= decode_frame in
  lwt coder3, coder4 = Krobot_can_bus.recv bus >|= decode_frame in
  lwt () =
    Lwt_io.fprintlf oc
      "%f %d %d %d %d %d %d %d %d"
      time
      coder1.coder_pos coder1.coder_dir
      coder2.coder_pos coder2.coder_dir
      coder3.coder_pos coder3.coder_dir
      coder4.coder_pos coder4.coder_dir
  in
  lwt () =
    Lwt_io.printlf "time: %f
  coder1: pos = %d, dir = %d
  coder2: pos = %d, dir = %d
  coder3: pos = %d, dir = %d
  coder4: pos = %d, dir = %d
"
      time
      coder1.coder_pos coder1.coder_dir
      coder2.coder_pos coder2.coder_dir
      coder3.coder_pos coder3.coder_dir
      coder4.coder_pos coder4.coder_dir
  in
  loop bus oc

lwt () =
  if Array.length Sys.argv <> 2 then begin
    print_endline "usage: krobot-plot <interface>";
    exit 2;
  end;
  try_lwt
    lwt bus = Krobot_can_bus.open_can Sys.argv.(1) and oc = Lwt_io.open_file ~mode:Lwt_io.output "data" in
    loop bus oc
  with Unix.Unix_error(error, func, arg) ->
    Lwt_log.error_f "'%s' failed with: %s" func (Unix.error_message error)
