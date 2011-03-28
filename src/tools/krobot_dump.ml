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

lwt () =
  lwt bus = Krobot_bus.get () in

  E.keep
    (E.map_s
       (fun (timestamp, msg) ->
          Lwt_io.printlf "%f, %s" timestamp (Krobot_message.to_string msg))
       (Krobot_message.recv bus));

  fst (wait ())