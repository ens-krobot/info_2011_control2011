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

let raw = ref false
let decoded = ref true

let options = Arg.align [
  "-raw", Arg.Set raw, " prints raw CAN frames";
  "-no-decoded", Arg.Clear decoded, " do not prints decoded frames";
]

let usage = "\
Usage: krobot-dump [options]
options are:"

lwt () =
  Arg.parse options ignore usage;

  lwt bus = Krobot_bus.get () in

  E.keep
    (E.map_s
       (fun (timestamp, frame) ->
          let msg = Krobot_message.decode frame in
          lwt () = Lwt_io.printf "%f" timestamp in
          lwt () =
            if !decoded then
              Lwt_io.printf ", %s" (Krobot_message.to_string msg)
            else
              return ()
          in
          lwt () =
            if !raw then
              Lwt_io.printf ", %s" (Krobot_can.string_of_frame frame)
            else
              return ()
          in
          Lwt_io.printl "")
       (Krobot_can.recv bus));

  fst (wait ())
