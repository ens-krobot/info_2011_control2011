(*
 * krobot_io_simulator.ml
 * ----------------
 * Copyright : (c) 2013, Xavier Lagorce <Xavier.Lagorce@crans.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Service simulating all inputs/outputs from the robot *)

open Lwt
open Lwt_react
open Lwt_preemptive
open Krobot_bus
open Krobot_message
open Krobot_geom

let section = Lwt_log.Section.make "krobot(io_simulator)"

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

(* State of the robot. *)
type state = {
  s_x : float;
  s_y : float;
  theta : float;
}

let robot = ref { s_x = 0.; s_y = 0.; theta = 0.}

(* +-----------------------------------------------------------------+
   | read/send loop                                                  |
   +-----------------------------------------------------------------+ *)

let dist_x x y theta obstacle =
  let y_int = y +. (obstacle -. x) *. (tan theta) in
  if (y_int > 0. && y_int < Krobot_config.world_height) then
    let d = (obstacle -. x) /. (cos theta) in
    if d >= 0. then
      Some d
    else
      None
  else
    None

let dist_y x y theta obstacle =
  let x_int = x +. (obstacle -. y) /. (tan theta) in
  if (x_int > 0. && x_int < Krobot_config.world_width) then
    let d = (obstacle -. y) /. (sin theta) in
    if d >= 0. then
      Some d
    else
      None
  else
    None

let min_border_distance x y theta =
  let l = match dist_x x y theta 0. with Some v -> [v] | None -> [] in
  let l = match dist_x x y theta Krobot_config.world_width with Some v -> v::l | None -> l in
  let l = match dist_y x y theta 0. with Some v -> v::l | None -> l in
  let l = match dist_y x y theta Krobot_config.world_height with Some v -> v::l | None -> l in
  match l with
    | [] -> assert false
    | l -> List.fold_left min max_float l
    | _ -> assert false

let gen_data robot =
  let dim = Array.length Krobot_config.urg_angles in
  let l = ref [] in
  for i = 0 to dim - 1 do
    let angle = Krobot_config.urg_angles.(i) in
    let dist = min_border_distance robot.s_x robot.s_y (robot.theta +. angle) in
    let x = dist *. cos angle in
    let y = dist *. sin angle  in
    l := {x;y} :: !l
  done;
  Array.of_list !l

let loop bus =
  let rec aux () =
    let time = Unix.gettimeofday () in
    let msg = Urg (gen_data !robot) in
    lwt () = Krobot_bus.send bus (time, msg) in
    lwt () = Lwt_unix.sleep 0.01 in
    aux () in
  aux ()

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message (timestamp, message) =
  match message with
    | Kill "io_simulator" ->
      exit 0
    | CAN(_, frame) -> begin
      match decode frame with
        | Odometry(x, y, theta) ->
          robot := { s_x = x; s_y = y; theta }
        | _ ->
          ()
    end
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
Usage: krobot-io-simulator [options]
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
  if !fork then Krobot_daemon.daemonize bus;

  (* Handle krobot message. *)
  E.keep (E.map (handle_message) (Krobot_bus.recv bus));

  (* Kill any running io_simulator. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "io_simulator") in

  (* Wait a bit to let the other handler release the connection *)
  lwt () = Lwt_unix.sleep 0.4 in

  (* Loop forever. *)
  Lwt_unix.run (loop bus)
