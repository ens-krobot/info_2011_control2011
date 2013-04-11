(*
 * krobot_motor_controller_simulator.ml
 * -----------------------
 * Copyright : (c) 2013, Xavier Lagorce <Xavier.Lagorce@crans.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Simulate the robot. *)

open Lwt
open Lwt_react
open Krobot_config
open Krobot_bus
open Krobot_message
open Krobot_geom

let section = Lwt_log.Section.make "krobot(mc_simulator)"

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

(* State of the robot. *)
type state = {
  x : float;
  y : float;
  theta : float;
}

type internal_state = {
  theta_l : float;
  theta_r : float;
}

type command =
  | Idle
      (* The robot is doint nothing. *)
  | Speed of float * float
      (* [Speed(left_velocity, right_velocity)] *)
  | Turn of float * float
      (* [Turn(t_acc, velocity)] *)
  | Move of float * float
      (* [Move(t_acc, velocity)] *)

(* Type of simulators. *)
type simulator = {
  mutable state : state;
  (* The state of the robot. *)
  mutable internal_state : internal_state;
  (* The state of the wheels. *)
  mutable velocity_l : float;
  (* Velocity of the left motor (read from the CAN). *)
  mutable velocity_r : float;
  (* Velocity of the right motor (read from the CAN). *)
  mutable time : float;
  (* The current time. *)
}

(* +-----------------------------------------------------------------+
   | Utility functions                                               |
   +-----------------------------------------------------------------+ *)

let print sim =
  Lwt_log.debug_f "
time = %f
state:
  x = %f
  y = %f
  theta = %f
internal_state:
  theta_l = %f
  theta_r = %f
velocities:
  left = %f
  right = %f
"
    sim.time
    sim.state.x
    sim.state.y
    sim.state.theta
    sim.internal_state.theta_l
    sim.internal_state.theta_r
    sim.velocity_l
    sim.velocity_r

(* +-----------------------------------------------------------------+
   | Main loop                                                       |
   +-----------------------------------------------------------------+ *)

let sim = ref None

let loop bus sim =
  let rec aux () =
    let time = Unix.gettimeofday () in
    let delta = time -. sim.time in
    sim.time <- time;

    (* Sends the state of the robot. *)
    lwt () = Krobot_message.send bus (sim.time, Odometry(sim.state.x, sim.state.y, sim.state.theta)) in
    lwt () = print sim in

    let u1 = (sim.velocity_l +. sim.velocity_r) *. wheels_diameter /. 4.
    and u2 = (sim.velocity_l -. sim.velocity_r) *. wheels_diameter /. wheels_distance /. 2. in
    let dx = u1 *. (cos sim.state.theta)
    and dy = u1 *. (sin sim.state.theta)
    and dtheta = u2 in
    sim.state <- {
      x = sim.state.x +. dx *. delta;
      y = sim.state.y +. dy *. delta;
      theta = math_mod_float (sim.state.theta +. dtheta *. delta) (2. *. pi);
    };
    sim.internal_state <- {
      theta_l = sim.internal_state.theta_l +. delta *. (u1 *. 4. +. u2 *. wheels_distance) /. (2. *. wheels_diameter);
      theta_r = sim.internal_state.theta_r +. delta *. (u1 *. 4. -. u2 *. wheels_distance) /. (2. *. wheels_diameter);
    };
    lwt () = Lwt_unix.sleep 0.01 in
    aux () in
  aux ()

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message (timestamp, message) =
  match message with
    | Kill "mc_simulator" ->
      exit 0
    | CAN(_, frame) -> begin
      match !sim with
        | Some sim -> begin
          match decode frame with
            | Encoder_position_speed_3(pos, speed) ->
              sim.velocity_r <- speed;
              Lwt.return ()
            | Encoder_position_speed_4(pos, speed) ->
              sim.velocity_l <- speed;
              Lwt.return ()
            | Set_odometry(x, y, theta) ->
              sim.state <- { x; y; theta };
              Lwt.return ()
            | _ ->
              Lwt.return () end
        | None ->
          Lwt.return ()
      end
    | _ -> Lwt.return ()

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true
let hil = ref false

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
  "-hil", Arg.Set hil, " Run in hardware in the loop mode";
]

let usage = "\
Usage: krobot-motor-controller-simulator [options]
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
  E.keep (E.map handle_message (Krobot_bus.recv bus));

  (* Kill any running mc_simulator. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "mc_simulator") in

  (* Wait a bit to let the other handler release the connection *)
  lwt () = Lwt_unix.sleep 0.4 in

  (* Set the motor_controller card in HIL mode if necessary *)
  lwt () = if !hil then
      Krobot_message.send bus (Unix.gettimeofday (), Set_controller_mode true)
    else
      Krobot_message.send bus (Unix.gettimeofday (), Set_controller_mode false) in

  (* Initial state of the simulator *)
  let local_sim = {
    state = { x = 0.; y = 0.; theta = 0. };
    internal_state = { theta_l = 0.; theta_r = 0. };
    velocity_l = 0.;
    velocity_r = 0.;
    time = Unix.gettimeofday ();
  } in
  sim := Some local_sim;

  (* Loop forever. *)
  loop bus local_sim
