(*
 * krobot_hil_simulator.ml
 * -----------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Simulate the robot. *)

open Lwt
open Lwt_react
open Krobot_config
open Krobot_message
open Krobot_geom

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
   | Entry point                                                     |
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

lwt () =
  lwt bus = Krobot_bus.get () in

  let sim = {
    state = { x = 0.; y = 0.; theta = 0. };
    internal_state = { theta_l = 0.; theta_r = 0. };
    velocity_l = 0.;
    velocity_r = 0.;
    time = Unix.gettimeofday ();
  } in

  (* Handle commands. *)
  E.keep
    (E.map_s
       (fun (ts, msg) ->
          match msg with
            | Encoder_position_speed_3(pos, speed) ->
                sim.velocity_r <- speed;
                return ()
            | Encoder_position_speed_4(pos, speed) ->
                sim.velocity_l <- speed;
                return ()
            | Set_odometry(x, y, theta) ->
                sim.state <- { x; y; theta };
                return ()
            | _ ->
                return ())
       (Krobot_message.recv bus));

  lwt () = Krobot_message.send bus (Unix.gettimeofday (), Set_controller_mode true) in

  while_lwt true do
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

    Lwt_unix.sleep 0.01
  done
