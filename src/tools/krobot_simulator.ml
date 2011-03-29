(*
 * krobot_simulator.ml
 * -------------------
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
  | Speed of float * float * float * float
      (* [Speed(start_time, end_time, left_velocity, right_velocity)] *)
  | Turn of float * float * float * float
      (* [Turn(start_time, t_acc, end_time, velocity)] *)
  | Move of float * float * float * float
      (* [Move(start_time, t_acc, end_time, velocity)] *)

(* Type of simulators. *)
type simulator = {
  mutable state : state;
  mutable internal_state : internal_state;
  mutable command : command;
  mutable time : float;
}

(* +-----------------------------------------------------------------+
   | Simulation                                                      |
   +-----------------------------------------------------------------+ *)

let sim_step = 0.01

let velocities_of_command cmd time =
  match cmd with
    | Speed (start, tend, l_v, r_v) ->
        if time < start || time > tend then
          (0., 0.)
        else
          ((l_v +. r_v) *. wheels_diameter /. 4., (l_v -. r_v) *. wheels_diameter /. wheels_distance)
    | Turn (start, t_acc, tend, vel) ->
        if time < start || time > tend then
          (0., 0.)
        else if time < (start +. t_acc) then
          (0., vel *. (time -. start) /. t_acc)
        else if time < (tend -. t_acc) then
          (0., vel)
        else
          (0., vel *. (tend -. time) /. t_acc)
    | Move (start, t_acc, tend, vel) ->
        if time < start || time > tend then
          (0., 0.)
        else if time < (start +. t_acc) then
          (vel *. (time -. start) /. t_acc, 0.)
        else if time < (tend -. t_acc) then
          (vel, 0.)
        else
          (vel *. (tend -. time) /. t_acc, 0.)

let move sim distance velocity acceleration =
  let t_acc = velocity /. acceleration in
  let t_end = (velocity *. velocity +. distance *. acceleration) /. (velocity *. acceleration) in
  sim.command <-
    if t_end > 2. *. t_acc
    then
      Move (sim.time, t_acc, sim.time +. t_end, velocity)
    else begin
      let t_acc = sqrt (distance /. acceleration) in
      let t_end = 2. *. t_acc in
      let velocity = acceleration *. t_acc in
      Move (sim.time, t_acc, sim.time +. t_end, velocity)
    end

let turn sim angle velocity acceleration =
  let t_acc = velocity /. acceleration in
  let t_end = (velocity *. velocity +. angle *. acceleration) /. (velocity *. acceleration) in
  sim.command <-
    if t_end > 2. *. t_acc
    then
      Turn (sim.time, t_acc, sim.time +. t_end, velocity)
    else begin
      let t_acc = sqrt (angle /. acceleration) in
      let t_end = 2. *. t_acc in
      let velocity = acceleration *. t_acc in
      Turn (sim.time, t_acc, sim.time +. t_end, velocity)
    end

let set_velocities sim left_velocity right_velocity duration =
  sim.command <- Speed (sim.time, sim.time +. duration, left_velocity, right_velocity)

let get_velocities sim =
  let (u1, u2) = velocities_of_command sim.command sim.time in
  let l_v = (4. *. u1 +. wheels_distance *. u2) /. (2. *. wheels_diameter)
  and r_v = (4. *. u1 -. wheels_distance *. u2) /. (2. *. wheels_diameter) in
  (l_v, r_v)

let get_state sim =
  sim.state

let get_encoders sim =
  let (theta_l, theta_r) = (sim.internal_state.theta_l, sim.internal_state.theta_r) in
  (theta_l *. wheels_diameter /. 2., theta_r *. wheels_diameter /. 2.)

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
command = %s
"
    sim.time
    sim.state.x
    sim.state.y
    sim.state.theta
    sim.internal_state.theta_l
    sim.internal_state.theta_r
    (match sim.command with
       | Speed(s, e, l, r) ->
           Printf.sprintf "Speed(%f, %f, %f, %f)" s e l r
       | Move(s, x, e, a) ->
           Printf.sprintf "Move(%f, %f, %f, %f)" s x e a
       | Turn(s, x, e, a) ->
           Printf.sprintf "Turn(%f, %f, %f, %f)" s x e a)

lwt () =
  lwt bus = Krobot_bus.get () in

  let sim = {
    state = { x = 0.; y = 0.; theta = 0. };
    internal_state = { theta_l = 0.; theta_r = 0. };
    command = Speed(0., 0., 0., 0.);
    time = Unix.gettimeofday ();
  } in

  (* Handle commands. *)
  E.keep
    (E.map_s
       (fun (ts, msg) ->
          match msg with
            | Motor_move(dist, speed, acc) ->
                lwt () = Lwt_log.info_f "received: move(%f, %f, %f)" dist speed acc in
                move sim dist speed acc;
                return ()
            | Motor_turn(angle, speed, acc) ->
                lwt () = Lwt_log.info_f "received: turn(%f, %f, %f)" angle speed acc in
                turn sim angle speed acc;
                return ()
            | Req_motor_status ->
                let s, e =
                  match sim.command with
                    | Speed(s, e, _, _) -> (s, e)
                    | Move(s, _, e, _) -> (s, e)
                    | Turn(s, _, e, _) -> (s, e)
                in
                let t = Unix.gettimeofday () in
                Krobot_message.send bus (t, Motor_status(s <= t && t < e))
            | _ ->
                return ())
       (Krobot_message.recv bus));

  while_lwt true do
    sim.time <- Unix.gettimeofday ();

    (* Sends the state of the robot. *)
    lwt () = Krobot_message.send bus (sim.time, Odometry(sim.state.x, sim.state.y, sim.state.theta)) in

    lwt () = print sim in

    let (u1, u2) = velocities_of_command sim.command sim.time in
    let dx = u1 *. (cos sim.state.theta)
    and dy = u1 *. (sin sim.state.theta)
    and dtheta = u2 in
    sim.state <- {
      x = sim.state.x +. dx *. sim_step;
      y = sim.state.y +. dy *. sim_step;
      theta = sim.state.theta +. dtheta *. sim_step;
    };
    sim.internal_state <- {
      theta_l = sim.internal_state.theta_l +. sim_step *. (u1 *. 4. +. u2 *. wheels_distance) /. (2. *. wheels_diameter);
      theta_r = sim.internal_state.theta_r +. sim_step *. (u1 *. 4. -. u2 *. wheels_distance) /. (2. *. wheels_diameter);
    };

    Lwt_unix.sleep sim_step
  done
