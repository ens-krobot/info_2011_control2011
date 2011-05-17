(*
 * krobot_objects.ml
 * -----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Service handling the position of the objects on the board. *)

open Lwt
open Lwt_react
open Krobot_bus
open Krobot_geom

let section = Lwt_log.Section.make "krobot(objects)"

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type objects = {
  bus : Krobot_bus.t;
  (* The message bus used to communicate with the robot. *)

  mutable objects : vertice list;
  (* The list of objects on the board. *)
}

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message objects (timestamp, message) =
  match message with
    | Kill "objects" ->
        exit 0

    | Send ->
        ignore (
          let ts = Unix.gettimeofday () in
          Krobot_bus.send objects.bus (ts, Objects objects.objects)
        )

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
Usage: krobot-planner [options]
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

  (* Kill any running planner. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "objects") in

  (* Create a new planner. *)
  let objects = {
    bus;
    objects = [];
  } in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message objects) (Krobot_bus.recv bus));

  (* Wait forever. *)
  fst (wait ())
