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
  mutable command : command;
  (* The current command. *)
  mutable command_start : float;
  (* The start time of the current command. *)
  mutable command_end : float;
  (* The end time of the current command. *)
  mutable time : float;
  (* The current time. *)
}

(* +-----------------------------------------------------------------+
   | Simulation                                                      |
   +-----------------------------------------------------------------+ *)

let sim_step = 0.01

let velocities sim =
  match sim.command with
    | Idle ->
        (0., 0.)
    | Speed(l_v, r_v) ->
        ((l_v +. r_v) *. wheels_diameter /. 4., (l_v -. r_v) *. wheels_diameter /. wheels_distance)
    | Turn(t_acc, vel) ->
        if sim.time < (sim.command_start +. t_acc) then
          (0., vel *. (sim.time -. sim.command_start) /. t_acc)
        else if sim.time < (sim.command_end -. t_acc) then
          (0., vel)
        else
          (0., vel *. (sim.command_end -. sim.time) /. t_acc)
    | Move(t_acc, vel) ->
        if sim.time < (sim.command_start +. t_acc) then
          (vel *. (sim.time -. sim.command_start) /. t_acc, 0.)
        else if sim.time < (sim.command_end -. t_acc) then
          (vel, 0.)
        else
          (vel *. (sim.command_end -. sim.time) /. t_acc, 0.)

let move sim distance velocity acceleration =
  if velocity <> 0. && acceleration <> 0. then begin
    let t_acc = velocity /. acceleration in
    let t_end = (velocity *. velocity +. distance *. acceleration) /. (velocity *. acceleration) in
    if t_end > 2. *. t_acc then begin
      if t_acc <> 0. then begin
        sim.command <- Move(t_acc, velocity);
        sim.command_start <- sim.time;
        sim.command_end <- sim.time +. t_end
      end
    end else begin
      if t_acc <> 0. then begin
        let t_acc = sqrt (distance /. acceleration) in
        let t_end = 2. *. t_acc in
        let velocity = acceleration *. t_acc in
        sim.command <- Move(t_acc, velocity);
        sim.command_start <- sim.time;
        sim.command_end <- sim.time +. t_end
      end
    end
  end

let turn sim angle velocity acceleration =
  if velocity <> 0. && acceleration <> 0. then begin
    let t_acc = velocity /. acceleration in
    let t_end = (velocity *. velocity +. angle *. acceleration) /. (velocity *. acceleration) in
    if t_end > 2. *. t_acc then begin
      if t_acc <> 0. then begin
        sim.command <- Turn(t_acc, velocity);
        sim.command_start <- sim.time;
        sim.command_end <- sim.time +. t_end
      end
    end else begin
      let t_acc = sqrt (angle /. acceleration) in
      let t_end = 2. *. t_acc in
      let velocity = acceleration *. t_acc in
      if t_acc <> 0. then begin
        sim.command <- Turn(t_acc, velocity);
        sim.command_start <- sim.time;
        sim.command_end <- sim.time +. t_end
      end
    end
  end

let set_velocities sim left_velocity right_velocity duration =
  sim.command <- Speed(left_velocity, right_velocity);
  sim.command_start <- sim.time;
  sim.command_end <- sim.time +. duration

let get_velocities sim =
  let (u1, u2) = velocities sim in
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
command:
  start = %f
  end = %f
  kind = %s
"
    sim.time
    sim.state.x
    sim.state.y
    sim.state.theta
    sim.internal_state.theta_l
    sim.internal_state.theta_r
    sim.command_start
    sim.command_end
    (match sim.command with
       | Idle ->
           "Idle"
       | Speed(l, r) ->
           Printf.sprintf "Speed(%f, %f)" l r
       | Move(t, a) ->
           Printf.sprintf "Move(%f, %f)" t a
       | Turn(t, a) ->
           Printf.sprintf "Turn(%f, %f)" t a)

lwt () =
  lwt bus = Krobot_bus.get () in

  let sim = {
    state = { x = 0.; y = 0.; theta = 0. };
    internal_state = { theta_l = 0.; theta_r = 0. };
    command = Idle;
    command_start = 0.;
    command_end = 0.;
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
                Krobot_message.send bus (Unix.gettimeofday (), Motor_status(sim.command <> Idle))
            | _ ->
                return ())
       (Krobot_message.recv bus));

  while_lwt true do
    sim.time <- Unix.gettimeofday ();

    (* Put the robot into idle if the last command is terminated. *)
    if sim.time > sim.command_end then sim.command <- Idle;

    (* Sends the state of the robot. *)
    lwt () = Krobot_message.send bus (sim.time, Odometry(sim.state.x, sim.state.y, sim.state.theta)) in

    lwt () = print sim in

    let (u1, u2) = velocities sim in
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
