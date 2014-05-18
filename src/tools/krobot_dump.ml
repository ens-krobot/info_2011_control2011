(*
 * krobot_dump.ml
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Print CAN frames that passes on D-Bus. *)

open Lwt
open Lwt_react
open Krobot_bus

let raw = ref false
let decoded = ref true
let log = ref false
let nocan = ref false

let options = Arg.align [
  "-raw", Arg.Set raw, " prints raw CAN frames";
  "-no-decoded", Arg.Clear decoded, " do not prints decoded frames";
  "-log", Arg.Set log, " prints logs";
  "-nocan", Arg.Set nocan, " don't prints can messages";
]

let usage = "\
Usage: krobot-dump [options]
options are:"

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
  Arg.parse options ignore usage;

  lwt bus = Krobot_bus.get () in

  E.keep
    (E.map_s
       (fun (timestamp, message) ->
          match message with
            | CAN(source, frame) ->
              if not !nocan
              then
                let msg = Krobot_message.decode frame in
                lwt () = Lwt_io.printf "%s| %s" (match source with Elec -> "elec" | Info -> "info") (date_string timestamp)in
                lwt () =
                  if !decoded then
                    Lwt_io.printf ": %s" (Krobot_message.to_string msg)
                  else
                    return ()
                in
                lwt () =
                  if !raw then
                    Lwt_io.printf ": %s" (Krobot_can.string_of_frame frame)
                  else
                    return ()
                in
                Lwt_io.printl ""
              else Lwt.return_unit
            | Log line ->
              if !log
              then Lwt_io.printf "log| %s\n" line
              else Lwt.return_unit
            | _ -> return ())
       (Krobot_bus.recv bus));

  fst (wait ())
