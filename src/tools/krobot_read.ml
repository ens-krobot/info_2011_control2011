(*
 * krobot_read.ml
 * --------------
 * Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Read a record file. *)

open Lwt
open Lwt_react

let print_date time =
  let tm = Unix.localtime time in
  let month_string =
    match tm.Unix.tm_mon with
      | 0 -> "Jan"
      | 1 -> "Feb"
      | 2 -> "Mar"
      | 3 -> "Apr"
      | 4 -> "May"
      | 5 -> "Jun"
      | 6 -> "Jul"
      | 7 -> "Aug"
      | 8 -> "Sep"
      | 9 -> "Oct"
      | 10 -> "Nov"
      | 11 -> "Dec"
      | _ -> Printf.ksprintf failwith "Lwt_log.ascdate: invalid month, %d" tm.Unix.tm_mon
  in
  Printf.printf
    "%s %2d %02d:%02d:%02d.%s: "
    month_string
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec
    (String.sub (Printf.sprintf "%.4f" (fst (modf time))) 2 4)

let () =
  try
    while true do
      let timestamp, message = input_value stdin in
      print_date timestamp;
      (match message with
        | Krobot_bus.CAN (Krobot_bus.Elec, frame) ->
          print_string "can-elec: ";
          print_string (Krobot_message.to_string (Krobot_message.decode frame))
        | Krobot_bus.CAN (Krobot_bus.Info, frame) ->
          print_string "can-info: ";
          print_string (Krobot_message.to_string (Krobot_message.decode frame))
        | _ ->
          print_string "computer: ";
          print_string (Krobot_bus.string_of_message message));
      print_char '\n'
    done
  with End_of_file ->
    ()
