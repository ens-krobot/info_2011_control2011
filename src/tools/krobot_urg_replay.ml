(*
 * krobot_urg.ml
 * ----------------
 * Copyright : (c) 2013, Pierre Chambart
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Service providing raw urg data. *)

open Lwt
open Lwt_react
open Lwt_preemptive
open Krobot_bus
open Krobot_message
open Krobot_geom

let section = Lwt_log.Section.make "krobot(urg_replay)"

(* +-----------------------------------------------------------------+
   | read/send loop                                                  |
   +-----------------------------------------------------------------+ *)

let to_vertices { Icp_minimisation.dx; dy } =
  Array.init (Array.length dx) (fun i -> { x = dx.(i); y = dy.(i) })

let loop bus data =
  let rec aux i =
    if i >= Array.length data
    then aux 0
    else
      let _, d = data.(i) in
      let time = Unix.gettimeofday () in
      let msg = Urg (to_vertices d) in
      lwt () = Krobot_bus.send bus (time, msg) in
      lwt () = Lwt_unix.sleep 0.2 in
      aux (i+1) in
  aux 0

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message (timestamp, message) =
  match message with
    | Kill "urg" -> exit 0
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
Usage: krobot-urg-replay [options] file
options are:"

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let run_sender data bus =
  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  (* Handle krobot message. *)
  E.keep (E.map handle_message (Krobot_bus.recv bus));

  (* Kill any running urg_handler. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "urg") in

  (* Loop forever. *)
  loop bus data

let file = ref None

lwt () =
  Arg.parse options (fun s -> file := Some s) usage;

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  let data = match !file with
    | None -> exit 0
    | Some f -> Icp_utils.load_raw_file f in

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  run_sender data bus
