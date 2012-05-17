(*
 * krobot_fake_beacons.ml
 * ----------------------
 * Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Lwt_react
open Krobot_bus
open Krobot_geom
open Krobot_config
open Krobot_message

let section = Lwt_log.Section.make "krobot(fake-beacons)"

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type beacons = {
  bus : Krobot_bus.t;
  mutable position : vertice;
  mutable orientation : float;
  mutable beacons : vertice option * vertice option;
}

(* +-----------------------------------------------------------------+
   | Primitives                                                      |
   +-----------------------------------------------------------------+ *)

let send_beacons beacons =
  let make_angle_distance = function
    | Some v ->
      let v = vector beacons.position v in
      (atan2 v.vy v.vx -. rotary_beacon_index_pos -. beacons.orientation, norm v)
    | None ->
      (0., 0.)
  in
  let b1, b2 = beacons.beacons in
  let a1, d1 = make_angle_distance b1
  and a2, d2 = make_angle_distance b2 in
  ignore
    (Krobot_bus.send
       beacons.bus
       (Unix.gettimeofday (),
        CAN (Info,
             encode
               (Beacon_position
                  (a1, a2, d1, d2)))))

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message beacons (timestamp, message) =
  match message with
    | CAN(_, frame) -> begin
      match decode frame with
        | Odometry(x, y, theta) ->
          beacons.position <- { x; y };
          beacons.orientation <- math_mod_float theta (2. *. pi)
        | _ ->
          ()
    end

    | Kill "fake-beacons" ->
      exit 0

    | Set_fake_beacons (b1, b2) ->
      beacons.beacons <- (b1, b2);
      send_beacons beacons

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
Usage: krobot-fake-beacons [options]
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

  (* Kill any running fake-beacons. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "fake-beacons") in

  (* Create a new fake-beacons. *)
  let beacons = {
    bus;
    position = { x = 0.2;
                 y = 1.9 +. Krobot_config.robot_size /. 2. -. Krobot_config.wheels_position };
    orientation = -0.5 *. pi;
    beacons = (None, None);
  } in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message beacons) (Krobot_bus.recv bus));

  (* Ask for initial parameters. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Send) in

  (* Periodiacally send fake beaconspositions. *)
  ignore (Lwt_engine.on_timer 0.05 true (fun _ -> send_beacons beacons));

  (* Wait forever. *)
  fst (wait ())
