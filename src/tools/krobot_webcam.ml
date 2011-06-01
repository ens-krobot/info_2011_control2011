(*
 * krobot_webcam.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Read webcam output and send results. *)

open Lwt
open Lwt_react
open Krobot_bus
open Krobot_geom

let section = Lwt_log.Section.make "krobot(webcam)"

(* +-----------------------------------------------------------------+
   | Parsing                                                         |
   +-----------------------------------------------------------------+ *)

let rec parse bus objects ic =
  (* Read one line from the object finder. *)
  lwt line = Lwt_io.read_line ic in
  if line = "=====" then begin
    (* If it is a new frame, send current objects. *)
    ignore (Krobot_bus.send bus (Unix.gettimeofday (), Objects objects));
    parse bus [] ic
  end else begin
    (* Otherwise read one and add it to the current list of objects. *)
    let cx, cy = Scanf.sscanf line "%f %f" (fun cx cy-> (cx, cy)) in
    parse bus ({ x = cx; y = cy } :: objects) ic
  end

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message bus (timestamp, message) =
  match message with
    | Kill "webcam" ->
        exit 0

    | _ ->
        ()

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
]

let usage = "\
Usage: krobot-webcam [options]
options are:"

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

lwt () =
  Arg.parse options ignore usage;

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  (* Fork if not prevented. *)
  if !fork then Lwt_daemon.daemonize ();

  (* Kill any running webcam handler. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "webcam") in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message bus) (Krobot_bus.recv bus));

  (* for printf to print floats with points and not colons *)
  Unix.putenv "LANG" "C";

  (* Launch the objects finder. *)
  let process = Lwt_process.open_process_in ("krobot-find-objects", [|"krobot-find-objects"; Sys.argv.(1); Sys.argv.(2); Sys.argv.(3); Sys.argv.(4)|]) in

  (* Read the first separator. *)
  lwt _ = Lwt_io.read_line process#stdout in

  (* Parse forever. *)
  parse bus [] process#stdout
