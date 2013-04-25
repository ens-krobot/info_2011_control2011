(*
 * krobot_simulator.ml
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

let section = Lwt_log.Section.make "krobot(simulator)"
let time_step = 0.001

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true
let hil = ref false
let sensors_emu = ref true
let robot_sim = ref true
let go_normal = ref false
let go_simu = ref false
let go_hil = ref false

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
  "-no-sensor", Arg.Clear sensors_emu, " Don't emulate sensor inputs";
  "-no-simulation", Arg.Clear robot_sim, " Don't simulate the robot";
  "-hil", Arg.Set hil, " Run in hardware in the loop mode";
  "-go-normal", Arg.Set go_normal, " Put the card in normal mode and exit";
  "-go-simulation", Arg.Set go_simu, " Put the cards in simulation mode and exit";
  "-go-normal", Arg.Set go_normal, " Put the cards in normal mode and exit";
  "-go-hil", Arg.Set go_hil, " Put the cards in hardware in the loop mode and exit";
]

let usage = "\
Usage: krobot-simulator [options]
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
  | Bezier_cmd of (float * float * float * float) * bool
      (** [Motor_bezier_limits(v_max, omega_max, a_tan_max, a_rad_max)] *)

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
  mutable bezier_next : (Bezier.curve * bool) option;
  (* Next Bezier's curve to follow *)
  mutable time : float;
  (* The current time. *)
  mutable command : command;
  (* The current command. *)
  mutable command_start : float;
  (* The start time of the current command. *)
  mutable command_end : float;
  (* The end time of the current command. *)
  mutable bezier_limits : float * float * float * float;
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

let velocities sim dt =
  (* Put the robot into idle if the last command is terminated. *)
  (match sim.command with
    | Bezier_cmd (_,cur_dir) ->
      if sim.bezier_u >= 1. then begin
        match sim.bezier_next with
        | None ->
          sim.command <- Idle
        | Some (curve,dir) ->
          sim.command <- Bezier_cmd (sim.bezier_limits, cur_dir);
          sim.bezier_curve <- Some curve;
          sim.bezier_next <- None;
          sim.bezier_u <- 0.
      end
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
    | Bezier_cmd (limits, dir) ->
      (match sim.bezier_curve with
         | None ->
           sim.command <- Idle;
           (0., 0.)
         | Some curve ->
           let (v_max,omega_max,_,a_r_max) = limits in
           let s' = norm (Bezier.dt curve sim.bezier_u) in
           let { vx = x'; vy = y' } = Bezier.dt curve sim.bezier_u in
           let { vx = x'';vy = y''} = Bezier.ddt curve sim.bezier_u in
           let theta' = ( y'' *. x' -. x'' *. y' ) /. ( x' *. x' +. y' *. y' ) in
           let cr = (x'*.x'+.y'*.y') ** 1.5 /. (x'*.y''-.y'*.x'') in
           let vel = min v_max (sqrt (a_r_max *. (abs_float cr))) in
           let omega = theta' *. vel /. s' in
           let vel, omega =
             if omega > omega_max then
               omega_max *. s' /. theta', omega_max
             else if omega < -. omega_max then
               -. omega_max *. s' /. theta', -. omega_max
             else
               vel, omega
           in
           sim.bezier_u <- sim.bezier_u +. vel /. s' *. dt;
           if dir then
             (vel, theta' *. vel /. s')
           else
             (-.vel, theta' *. vel /. s'))

let bezier sim (x_end, y_end, d1, d2, theta_end, v_end) =
  let p,theta_start = match sim.bezier_curve with
    | None ->
      {Krobot_geom.x = sim.state.x; Krobot_geom.y = sim.state.y},
      sim.state.theta
    | Some curve ->
      let _,_,r,s = Bezier.pqrs curve in
      s,
      (angle (vector r s))
  in
  let s = {Krobot_geom.x = x_end; Krobot_geom.y = y_end} in
  let q = translate p (vector_of_polar d1 theta_start) in
  let r = translate s (vector_of_polar (-.d2) theta_end) in
  let dir = d1 >= 0. in
  match sim.bezier_curve with
    | None ->
      sim.command <- Bezier_cmd (sim.bezier_limits,dir);
      sim.bezier_u <- 0.;
      sim.bezier_curve <- Some (Bezier.of_vertices p q r s);
    | Some _ ->
      sim.bezier_next <- Some ((Bezier.of_vertices p q r s),dir)

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

let get_velocities sim dt =
  let (u1, u2) = velocities sim dt in
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
      velocities sim delta
    in
    let dx = u1 *. (cos sim.state.theta)
    and dy = u1 *. (sin sim.state.theta)
    and dtheta = u2 in
    let theta = sim.state.theta +. dtheta *. delta in
    let theta =
      if theta > pi then
        theta -. 2. *. pi
      else if theta < -.pi then
        theta +. 2. *. pi
      else
        theta
    in
    sim.state <- {
      x = sim.state.x +. dx *. delta;
      y = sim.state.y +. dy *. delta;
      theta = theta;
    };
    sim.internal_state <- {
      theta_l = sim.internal_state.theta_l +. delta *. (u1 *. 4. +. u2 *. wheels_distance) /. (2. *. wheels_diameter);
      theta_r = sim.internal_state.theta_r +. delta *. (u1 *. 4. -. u2 *. wheels_distance) /. (2. *. wheels_diameter);
    };
    (match sim.command with
       | Bezier_cmd _ -> sim.ghost <- sim.state
       | _ -> ());
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
    | Kill "simulator" ->
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
                sim.command <- Idle;
                sim.bezier_curve <- None;
                sim.bezier_next <- None;
              | Set_odometry(x, y, theta) ->
                sim.state_indep <- { x; y; theta }
              | Set_odometry_indep(x, y, theta) ->
                sim.state <- { x; y; theta }
              | Motor_bezier_limits(v_max, omega_max, a_tan_max, a_rad_max) ->
                sim.bezier_limits <- (v_max, omega_max, a_tan_max, a_rad_max)
              | Motor_bezier(x_end, y_end, d1, d2, theta_end, v_end) ->
                Lwt_unix.run (Lwt_log.info_f "received: bezier(%f, %f, %f, %f, %f, %f)" x_end y_end d1 d2 theta_end v_end);
                bezier sim (x_end, y_end, d1, d2, theta_end, v_end)
              | _ ->
                () end);
          Lwt.return () end
        | None ->
          Lwt.return ()
      end
    | _ -> Lwt.return ()

(* +-----------------------------------------------------------------+
   | Hokuyo emulation                                                |
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
    | [] -> None
    | l -> Some (List.fold_left min max_float l)

let distance_to_obj x y theta obj =
  let x2, y2 = x +. cos theta, y +. sin theta in
  let x3, y3, r = obj.pos.Krobot_geom.x, obj.pos.Krobot_geom.y, obj.size in
  let a = (x2 -. x) ** 2. +. (y2 -. y) ** 2. in
  let b = 2. *. ( (x2-.x)*.(x-.x3)+.(y2-.y)*.(y-.y3) ) in
  let c = x3**2. +. y3**2. +. x**2. +. y**2. -. 2. *. (x*.x3+.y*.y3) -. r**2. in
  let delta = b**2. -. 4. *.a*.c in
  if abs_float delta < 0.01 then
    Some (-.b/.(2.*.a))
  else if delta > 0. then
    let d1 = (-.b -. sqrt (delta)) /. (2.*.a) in
    let d2 = (-.b +. sqrt (delta)) /. (2.*.a) in
    if d1 < 0. && d2 < 0. then
      None
    else if d1 < 0. && d2 > 0. then
      Some d2
    else if d1 > 0. && d2 < 0. then
      Some d1
    else
      Some (min d1 d2)
  else
    None

let closest_obstacle x y theta objs =
  let border = min_border_distance x y theta in
  let dist_to_objs = List.map (distance_to_obj x y theta) objs in
  List.fold_left
    (fun min_dist dist -> match dist with
       | Some dist ->
         (match min_dist with
            | Some min_dist -> Some (min min_dist dist)
            | None -> Some dist)
       | None -> min_dist)
    border
    dist_to_objs

(*let gen_data robot =
  let dim = Array.length Krobot_config.urg_angles in
  let {Krobot_geom.x=urg_rel_x;
       Krobot_geom.y=urg_rel_y } = Krobot_config.urg_position in
  let urg_pos = [| urg_rel_x; urg_rel_y; 1. |] in
  let rot = rot_mat robot.theta in
  let urg_pos = mult rot urg_pos in
  let pos = Krobot_geom.translate
    { Krobot_geom.x=robot.x; Krobot_geom.y=robot.y }
    { Krobot_geom.vx = urg_pos.(0); Krobot_geom.vy = urg_pos.(1) } in
  let {Krobot_geom.x=cen_x;
       Krobot_geom.y=cen_y } = pos in
  let l = ref [] in
  for i = 0 to dim - 1 do
    let angle = Krobot_config.urg_angles.(i) in
    match min_border_distance cen_x cen_y (robot.theta +. angle) with
      | Some dist ->
        let x = dist *. cos angle +. urg_pos.(0) in
        let y = dist *. sin angle +. urg_pos.(1) in
        l := {Krobot_geom.x;Krobot_geom.y} :: !l
      | None ->
        ()
  done;
  Array.of_list !l*)

let gen_data robot =
  let dim = Array.length Krobot_config.urg_angles in
  let l = ref [] in
  for i = 0 to dim - 1 do
    let angle = Krobot_config.urg_angles.(i) in
    match closest_obstacle robot.x robot.y (robot.theta +. angle) Krobot_config.fixed_obstacles with
      | Some dist ->
        let x = dist *. cos angle in
        let y = dist *. sin angle  in
        l := {Krobot_geom.x=x;Krobot_geom.y=y} :: !l
      | None ->
        ()
  done;
  Array.of_list !l

let loop_urg sim bus =
  let rec aux () =
    let time = Unix.gettimeofday () in
    let msg = Urg (gen_data sim.state) in
    lwt () = Krobot_bus.send bus (time, msg) in
    lwt () = Lwt_unix.sleep 0.01 in
    aux () in
  aux ()

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

lwt () =
  Arg.parse options ignore usage;

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  if (!go_hil or !go_simu or !go_normal) then begin
    if !go_normal then
      ignore(Krobot_message.send bus (Unix.gettimeofday (), Set_simulation_mode Sim_no))
    else if !go_hil then
      ignore(Krobot_message.send bus (Unix.gettimeofday (), Set_simulation_mode Sim_HIL))
    else if !go_simu then
      ignore(Krobot_message.send bus (Unix.gettimeofday (), Set_simulation_mode Sim_normal))
    else
    exit 0
  end;

  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  (* Handle krobot message. *)
  E.keep (E.map (handle_message bus) (Krobot_bus.recv bus));

  (* Kill any running simulator. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "simulator") in

  (* Wait a bit to let the other handler release the connection *)
  lwt () = Lwt_unix.sleep 0.4 in

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
    bezier_next = None;
    command = Idle;
    command_start = 0.;
    command_end = 0.;
    time = Unix.gettimeofday ();
    bezier_limits = 0.5, 3.14, 1., 1.;
  } in
  sim := Some local_sim;

  (* Set the correct simulation mode *)
  if !robot_sim || !sensors_emu then
    (if !hil then
        ignore(Krobot_message.send bus (Unix.gettimeofday (), Set_simulation_mode Sim_HIL))
     else
        ignore(Krobot_message.send bus (Unix.gettimeofday (), Set_simulation_mode Sim_normal))
    );
  (* Launch robot simulation *)
  if !robot_sim then begin
    ignore(send_CAN_messages local_sim bus);
    ignore(loop bus local_sim)
  end;
  (* Launch sensor simulation *)
  if !sensors_emu then begin
  ignore(loop_urg local_sim bus)
  end;

  (* Run forever *)
  let t, _ = Lwt.wait () in
  t
