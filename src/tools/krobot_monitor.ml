(*
 * krobot_monitor.ml
 * -----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Monitor all messages passing on the krobot bus. *)

open Lwt
open Lwt_react
open Krobot_bus

let date_string time =
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
  Printf.sprintf
    "%s %2d %02d:%02d:%02d.%s"
    month_string
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec
    (String.sub (Printf.sprintf "%.4f" (fst (modf time))) 2 4)

lwt () =
  lwt bus = Krobot_bus.get () in

  E.keep
    (E.map_s
       (fun (timestamp, message) ->
          match message with
            | CAN(_, frame) ->
                Lwt_io.printlf "%s: %s -> %s"
                  (date_string timestamp)
                  (Krobot_bus.string_of_message message)
                  (Krobot_message.to_string (Krobot_message.decode frame))
            | _ ->
                Lwt_io.printlf "%s: %s"
                  (date_string timestamp)
                  (Krobot_bus.string_of_message message))
       (Krobot_bus.recv bus));

  fst (wait ())

