(*
 * krobot_record.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Record CAN frames. *)

open Lwt
open Lwt_react
open Krobot_bus

lwt () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: krobot-record <file>";
    exit 2
  end;

  lwt bus = Krobot_bus.get () in
  lwt oc = Lwt_io.open_file ~mode:Lwt_io.output Sys.argv.(1) in

  (* Write all frames comming from the electronic to the output
     file. *)
  E.keep
    (E.map_s
       (fun (timestamp, message) ->
          match message with
            | CAN(Elec, frame) ->
                Lwt_io.write_value oc (timestamp, frame)
            | _ ->
                return ())
       (Krobot_bus.recv bus));

  fst (wait ())
