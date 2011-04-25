(*
 * krobot_planner.ml
 * -----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* The trajectory planner. *)

open Lwt
open Lwt_react
open Krobot_bus
open Krobot_geom
open Krobot_config
open Krobot_message

let section = Lwt_log.Section.make "krobot(planner)"

let pi = 4. *. atan 1.

let math_mod_float a b =
  let b2 = b /. 2. in
  let modf = mod_float a b in
  if modf > b2 then
    modf -. b
  else if modf < -. b2 then
    modf +. b
  else
    modf;;

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type planner = {
  bus : Krobot_bus.t;
  (* The message bus used to communicate with the robot. *)

  mutable vertices : vertice list;
  (* The list of vertices for the trajectory. *)

  mutable curves : (float * vertice * vertice * vertice * vertice) list;
  (* The parameters of bezier curves. *)

  mutable moving : bool;
  (* Is the robot moving ? *)

  mutable motors_moving : bool;
  (* Are the motor currently active ? *)

  mutable curve_status : int;
  (* The status of the trajectory controller on the curve (between 0
     and 255). *)

  mutable mover : unit Lwt.t;
  (* The thread moving the robot. *)

  mutable position : vertice;
  (* The position of the robot on the board. *)

  mutable orientation : float;
  (* The orientation of the robot. *)

  mutable event : unit event;
  (* Event kept in the planner. *)
}

(* +-----------------------------------------------------------------+
   | Primitives                                                      |
   +-----------------------------------------------------------------+ *)

let set_vertices planner ?curves vertices =
  planner.vertices <- vertices;
  (* If the robot is not moving, add its current position to the
     trajectory. *)
  let vertices =
    match planner.moving with
      | true -> vertices
      | false -> planner.position :: vertices
  in
  (* Compute bezier curves if not provided. *)
  let curves =
    match curves with
      | Some l ->
          l
      | None ->
          let v = { vx = cos planner.orientation; vy = sin planner.orientation } in
          List.rev (Bezier.fold_vertices (fun sign p q r s acc -> (sign, p, q, r, s) :: acc) v vertices [])
  in
  planner.curves <- curves;
  ignore (Krobot_bus.send planner.bus (Unix.gettimeofday (), Trajectory_vertices(vertices,
                                                                                 List.map (fun (sign, p, q, r, s) -> (p, q, r, s)) curves)))

let set_moving planner moving =
  planner.moving <- moving;
  ignore (Krobot_bus.send planner.bus (Unix.gettimeofday (), Trajectory_moving moving))

let simplify planner tolerance =
  match planner.vertices with
    | [] ->
        ()
    | points ->
        let points = Array.of_list points in
        let rec loop = function
          | i1 :: i2 :: rest ->
              let { x = x1; y = y1 } = points.(i1) and { x = x2; y = y2 } = points.(i2) in
              let a = y2 -. y1 and b = x1 -. x2 and c = x2 *. y1 -. x1 *. y2 in
              let r = sqrt (a *. a +. b *. b) in
              if r <> 0. then begin
                (* Search the furthest point from the line passing by (x1,
                   y1) and (x2, y2) *)
                let max_dist = ref 0. and at_max = ref i1 in
                for i = i1 + 1 to i2 - 1 do
                  let { x; y } = points.(i) in
                  let d = abs_float (a *. x +. b *. y +. c) /. r in
                  if d > !max_dist then begin
                    max_dist := d;
                    at_max := i
                  end
                done;
                if !max_dist > tolerance then
                  (* The furthest point is out of tolerance, we split the
                     current region with it. *)
                  loop (i1 :: !at_max :: i2 :: rest)
                else
                  (* The point is acceptable, we pass the next region. *)
                  i1:: loop (i2 :: rest)
              end else
                (* The two point are the same so we drop one. *)
                loop (i2 :: rest)
          | rest ->
              rest
        in
        let result = loop [0; Array.length points - 1] in
        set_vertices planner (List.map (fun i -> points.(i)) result)

let wait_done planner =
  lwt () = Lwt_log.info "waiting for the robot to stop moving" in
  lwt () = Lwt_unix.sleep 0.3 in
  lwt () =
    while_lwt planner.motors_moving do
      Lwt_unix.sleep 0.1
    done
  in
  Lwt_log.info "trajectory done"

let wait_middle planner =
  lwt () = Lwt_log.info "waiting for the robot to be in the middle of the trajectory" in
  lwt () = Lwt_unix.sleep 0.3 in
  lwt () =
    while_lwt planner.curve_status < 128 do
      Lwt_unix.sleep 0.001
    done
  in
  Lwt_log.info "robot in the middle of the trajectory"

let wait_start planner =
  lwt () = Lwt_log.info "waiting for the robot to start the new trajectory" in
  lwt () =
    while_lwt planner.curve_status >= 128 do
      Lwt_unix.sleep 0.001
    done
  in
  Lwt_log.info "robot started the new trajectory"

let go planner rotation_speed rotation_acceleration moving_speed moving_acceleration =
  if planner.moving then
    return ()
  else begin
    (*    let rec loop () =
          match S.value planner.vertices with
          | { x; y } :: rest ->
          let sqr x = x *. x in
          let radius = sqrt (sqr (max wheels_position (robot_size -. wheels_position)) +. sqr (robot_size /. 2.)) in
          if x >= radius && x <= world_width -. radius && y >= radius && y <= world_height -. radius then begin
    (* Turn the robot. *)
          let alpha = math_mod_float (atan2 (y -. planner.position.y) (x -. planner.position.x) -. planner.orientation) (2. *. pi) in
          lwt () = Lwt_log.info_f "turning by %f radians" alpha in
          lwt () = Krobot_message.send planner.bus (Unix.gettimeofday (),
          Motor_turn(alpha,
          rotation_speed,
          rotation_acceleration)) in
          lwt () = wait_done planner in

    (* Move the robot. *)
          let dist = sqrt (sqr (x -. planner.position.x) +. sqr (y -. planner.position.y)) in
          lwt () = Lwt_log.info_f "moving by %f meters" dist in
          lwt () = Krobot_message.send planner.bus (Unix.gettimeofday (),
          Motor_move(dist,
          moving_speed,
          moving_acceleration)) in
          lwt () = wait_done planner in

    (* Remove the point. *)
          (match S.value planner.vertices with
          | _ :: l -> planner.set_vertices l
          | [] -> ());
          planner.set_origin (planner.position,
          { vx = cos planner.orientation;
          vy = sin planner.orientation });

          loop ()
          end else
          Lwt_log.warning_f "can not move to (%f, %f)" x y
          | [] ->
          return ()
          in*)
    set_moving planner true;
    planner.mover <- (
      try_lwt
        (* Send a bezier curve to the robot. *)
        let send_curve (sign, p, q, r, s) v_end =
          (* Compute parameters. *)
          let d1 = sign *. distance p q and d2 = distance r s in
          let v = vector r s in
          let theta_end = atan2 v.vy v.vx in

          ignore (
            Lwt_log.info_f
              "sending bezier curve, x = %f, y = %f, d1 = %f, d2 = %f, theta_end = %f, v_end = %f"
              s.x s.y d1 d2 theta_end v_end
          );

          (* Send the curve. *)
          lwt () = Krobot_message.send planner.bus (Unix.gettimeofday (), Motor_bezier(s.x, s.y, d1, d2, theta_end, v_end)) in

          return (s, theta_end)
        in

        (* Remove the first vertice of the trajecotry. *)
        let drop_vertice (s, theta) =
          set_vertices planner ~curves:(List.tl planner.curves) (List.tl planner.vertices)
        in

        let rec loop x = function
          | [] ->
              lwt () = wait_done planner in
              set_vertices planner [];
              return ()

          | [points] ->
              lwt () = wait_middle planner in
              lwt _ = send_curve points 0.01 in
              lwt () = wait_start planner in
              drop_vertice x;
              lwt () = wait_done planner in
              set_vertices planner [];
              return ()

          | points :: rest ->
              lwt () = wait_middle planner in
              lwt y = send_curve points 0.5 in
              lwt () = wait_start planner in
              drop_vertice x;
              loop y rest
        in

        (* Add the origin of the trajectory to keep displaying it. *)
        planner.vertices <- planner.position :: planner.vertices;
        match planner.curves with
          | [] ->
              set_vertices planner [];
              return ()

          | [points] ->
              lwt _ = send_curve points 0.01 in
              lwt () = wait_done planner in
              set_vertices planner [];
              return ()

          | points :: rest ->
              lwt x = send_curve points 0.5 in
              loop x rest

      with exn ->
        Lwt_log.error_f ~section ~exn "failed to move"

      finally
        set_moving planner false;
        return ()
    );
    return ()
  end

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message planner (timestamp, message) =
  match message with
    | CAN(_, frame) -> begin
        match decode frame with
          | Odometry(x, y, theta) ->
              planner.position <- { x; y };
              planner.orientation <- math_mod_float theta (2. *. pi)

          | Odometry_ghost(x, y, theta, u, following) ->
              planner.curve_status <- u;
              planner.motors_moving <- following

          | _ ->
              ()
      end

    | Kill "planner" ->
        exit 0

    | Send ->
        ignore (
          let ts = Unix.gettimeofday () in
          let vertices = if planner.moving then planner.vertices else planner.position :: planner.vertices in
          join [
            Krobot_bus.send planner.bus (ts, Trajectory_vertices(vertices, List.map (fun (sign, p, q, r, s) -> (p, q, r, s)) planner.curves));
            Krobot_bus.send planner.bus (ts, Trajectory_moving planner.moving);
          ]
        )

    | Trajectory_set_vertices l ->
        if not planner.moving then
          set_vertices planner l

    | Trajectory_add_vertice vertice ->
        if not planner.moving then
          set_vertices planner (planner.vertices @ [vertice])

    | Trajectory_simplify tolerance ->
        if not planner.moving then
          simplify planner tolerance

    | Trajectory_go(rotation_speed, rotation_acceleration, moving_speed, moving_acceleration) ->
        if not planner.moving then
          ignore (go planner rotation_speed rotation_acceleration moving_speed moving_acceleration)

    | Trajectory_stop ->
        cancel planner.mover;
        set_moving planner false;
        set_vertices planner [];
        ignore (Krobot_message.send planner.bus (Unix.gettimeofday (), Motor_stop(1.0,0.0)))

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
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "planner") in

  (* Create a new planner. *)
  let planner = {
    bus;
    vertices = [];
    curves = [];
    moving = false;
    motors_moving = false;
    curve_status = 0;
    mover = return ();
    position = { x = 0.; y = 0. };
    orientation = 0.;
    event = E.never;
  } in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message planner) (Krobot_bus.recv bus));

  (* Wait forever. *)
  fst (wait ())
