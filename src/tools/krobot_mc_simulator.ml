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
let time_step = 0.001

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
Usage: krobot-mc-simulator [options]
options are:"

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
      (* The robot is doing nothing. *)
  | Speed of float * float
      (* [Speed(left_velocity, right_velocity)] *)
  | Turn of float * float
      (* [Turn(t_acc, velocity)] *)
  | Move of float * float
      (* [Move(t_acc, velocity)] *)
  | Bezier_cmd of
      ( float * float * float * float * float * float ) *
      ( float * float * float )
      (** [Motor_bezier(x_end, y_end, d1, d2, theta_end, v_end)]
          [Motor_bezier_limits(v_max, a_tan_max, a_rad_max)] *)

(* Type of simulators. *)
type simulator = {
  mutable state : state;
  (* The state of the robot. *)
  mutable state_indep : state;
  (* The state of the robot for second set of encoders. *)
  mutable internal_state : internal_state;
  (* The state of the wheels. *)
  mutable velocity_l : float;
  (* Velocity of the left motor. *)
  mutable velocity_r : float;
  (* Velocity of the right motor. *)
  mutable ghost : state;
  (* The state of the ghost. *)
  mutable bezier_u : float;
  (* position on the Bezier's curve*)
  mutable bezier_curve : Bezier.curve option;
  (* Bezier's curve currently being followed if existing *)
  mutable time : float;
  (* The current time. *)
  mutable command : command;
  (* The current command. *)
  mutable command_start : float;
  (* The start time of the current command. *)
  mutable command_end : float;
  (* The end time of the current command. *)
  mutable bezier_limits : float * float * float;
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
   | Trajectory generation                                           |
   +-----------------------------------------------------------------+ *)

let velocities sim =
  (* Put the robot into idle if the last command is terminated. *)
  (match sim.command with
    | Bezier_cmd _ -> ()
    | _ -> if sim.time > sim.command_end then sim.command <- Idle);
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
    | Bezier_cmd(_,_) ->
      sim.command <- Idle;
      sim.bezier_curve <- None;
      (0., 0.)

(*  val of_vertices : vertice -> vertice -> vertice -> vertice -> curve
    (** [of_vertices p q r s] creates a bezier curve from the given
        four control points. [p] and [s] are the first and end point
        of the curve. *)*)

let bezier sim (x_end, y_end, d1, d2, theta_end, v_end) =
  sim.bezier_u <- 0.;
  let p,theta_start = match sim.bezier_curve with
    | None ->
      {Krobot_geom.x = sim.state.x; Krobot_geom.y = sim.state.y},
      sim.state.theta
    | Some curve ->
      let p,_,r,s = Bezier.pqrs curve in
      s,
      (angle (vector r s))
  in
  let s = {Krobot_geom.x = x_end; Krobot_geom.y = y_end} in
  let q = translate p (vector_of_polar d1 theta_start) in
  let r = translate s (vector_of_polar (-.d2) theta_end) in
  sim.bezier_curve <- Some (Bezier.of_vertices p q r s)

let move sim distance velocity acceleration =
  if distance <> 0. && velocity > 0. && acceleration > 0. then begin
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
        let t_acc = sqrt (abs_float (distance) /. acceleration) in
        let t_end = 2. *. t_acc in
        let sign = if distance >= 0. then 1. else -1. in
        let velocity = sign *. acceleration *. t_acc in
        sim.command <- Move(t_acc, velocity);
        sim.command_start <- sim.time;
        sim.command_end <- sim.time +. t_end
      end
    end
  end

let turn sim angle velocity acceleration =
  if angle <> 0. && velocity > 0. && acceleration > 0. then begin
    let t_acc = velocity /. acceleration in
    let t_end = (velocity *. velocity +. angle *. acceleration) /. (velocity *. acceleration) in
    if t_end > 2. *. t_acc then begin
      if t_acc <> 0. then begin
        sim.command <- Turn(t_acc, velocity);
        sim.command_start <- sim.time;
        sim.command_end <- sim.time +. t_end
      end
    end else begin
      let t_acc = sqrt (abs_float (angle) /. acceleration) in
      let t_end = 2. *. t_acc in
      let sign = if angle >= 0. then 1. else -1. in
      let velocity = sign *. acceleration *. t_acc in
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
   | Main loops                                                      |
   +-----------------------------------------------------------------+ *)

let sim = ref None

let loop bus sim =
  let rec aux () =
    let time = Unix.gettimeofday () in
    let delta = time -. sim.time in
    sim.time <- time;

    lwt () = print sim in

    let u1, u2 = if !hil then
      (sim.velocity_l +. sim.velocity_r) *. wheels_diameter /. 4. ,
      (sim.velocity_l -. sim.velocity_r) *. wheels_diameter /. wheels_distance /. 2.
    else
      velocities sim
    in
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
    lwt () = Lwt_unix.sleep time_step in
    aux () in
  aux ()

let send_CAN_messages sim bus =
  let rec aux () =
    (* Sends the state of the robot. *)
    lwt () = Krobot_message.send bus (sim.time, Odometry(sim.state.x, sim.state.y, sim.state.theta)) in
    lwt () = Krobot_message.send bus (sim.time, Odometry_indep(sim.state.x, sim.state.y, sim.state.theta)) in
    (* Wait before next batch of packets (emulate the electronic board behavior) *)
    lwt () = Lwt_unix.sleep 0.005 in
    (* Sends the state of the ghost. *)
    lwt () = Krobot_message.send bus (sim.time,
                                      Odometry_ghost(sim.ghost.x,
                                                     sim.ghost.y,
                                                     sim.ghost.theta,
                                                     int_of_float (255. *. sim.bezier_u),
                                                     match sim.command with Bezier_cmd _ -> true | _ -> false)) in
    (* Sends the state of the motors. *)
    lwt () = match sim.command with
               | Turn(a, b) ->
                 Krobot_message.send bus (Unix.gettimeofday (), Motor_status(false, true, false, false))
               | Move(a, b) ->
                 Krobot_message.send bus (Unix.gettimeofday (), Motor_status(true, false, false, false))
               | Bezier_cmd _ ->
                 (* Motor State to be verifyed on the real Motor Controller card *)
                 Krobot_message.send bus (Unix.gettimeofday (), Motor_status(true, true, false, false))
               | _ ->
                 Krobot_message.send bus (Unix.gettimeofday (), Motor_status(false, false, false, false)) in
    lwt () = Lwt_unix.sleep 0.005 in
    aux () in
  aux ()

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message bus (timestamp, message) =
  match message with
    | Kill "mc_simulator" ->
      exit 0
    | CAN(_, frame) -> begin
      match !sim with
        | Some sim -> begin
          (* Generic messages *)
          (match decode frame with
            | Set_odometry(x, y, theta) ->
              sim.state <- { x; y; theta }
            | _ ->
              ());
          (* Messages related to HIL mode *)
          (if !hil then begin
            match decode frame with
              | Encoder_position_speed_3(pos, speed) ->
                sim.velocity_r <- speed
              | Encoder_position_speed_4(pos, speed) ->
                sim.velocity_l <- speed
              | _ ->
                () end
           (* Message related to full software simulation mode *)
           else begin
             match decode frame with
              | Motor_move(dist, speed, acc) ->
                Lwt_unix.run (Lwt_log.info_f "received: move(%f, %f, %f)" dist speed acc);
                move sim dist speed acc
              | Motor_turn(angle, speed, acc) ->
                Lwt_unix.run (Lwt_log.info_f "received: turn(%f, %f, %f)" angle speed acc);
                turn sim angle speed acc
              | Motor_stop(lin_acc, rot_acc) ->
                sim.command <- Idle
              | Set_odometry(x, y, theta) ->
                sim.state_indep <- { x; y; theta }
              | Set_odometry_indep(x, y, theta) ->
                sim.state <- { x; y; theta }
              | Motor_bezier_limits(v_max, a_tan_max, a_rad_max) ->
                sim.bezier_limits <- (v_max, a_tan_max, a_rad_max)
              | Motor_bezier(x_end, y_end, d1, d2, theta_end, v_end) ->
                bezier sim (x_end, y_end, d1, d2, theta_end, v_end)
              | _ ->
                () end);
          Lwt.return () end
        | None ->
          Lwt.return ()
      end
    | _ -> Lwt.return ()

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
  E.keep (E.map (handle_message bus) (Krobot_bus.recv bus));

  (* Kill any running mc_simulator. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "mc_simulator") in

  (* Wait a bit to let the other handler release the connection *)
  lwt () = Lwt_unix.sleep 0.4 in

  (* Set the motor_controller card in HIL mode if necessary *)
  if !hil then
    ignore (Krobot_message.send bus (Unix.gettimeofday (), Set_controller_mode true)) ;

  (* Initial state of the simulator *)
  let local_sim = {
    state = { x = 0.; y = 0.; theta = 0. };
    state_indep = { x = 0.; y = 0.; theta = 0. };
    internal_state = { theta_l = 0.; theta_r = 0. };
    velocity_l = 0.;
    velocity_r = 0.;
    ghost = { x = 0.; y = 0.; theta = 0. };
    bezier_u = 0.;
    bezier_curve = None;
    command = Idle;
    command_start = 0.;
    command_end = 0.;
    time = Unix.gettimeofday ();
    bezier_limits = 1., 1., 1.;
  } in
  sim := Some local_sim;

  ignore(send_CAN_messages local_sim bus);

  (* Loop forever. *)
  Lwt_unix.run (loop bus local_sim)
