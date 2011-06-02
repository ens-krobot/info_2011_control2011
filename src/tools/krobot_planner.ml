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

  mutable objects : vertice list;
  (* The list of objects on the board. *)

  mutable beacon : vertice option;
  (* Position of the beacon. *)

  mutable event : unit event;
  (* Event kept in the planner. *)
}

(* +-----------------------------------------------------------------+
   | Motion planning                                                 |
   +-----------------------------------------------------------------+ *)

let find_path planner src dst =
  (* Remove objects that are near the destination. *)
  let objects = List.filter (fun obj -> distance dst obj >= object_safety_distance) planner.objects in
  (* Remove objects that are near the curent position. *)
  let objects = List.filter (fun obj -> distance src obj >= object_safety_distance) objects in
  let l = List.map (fun v -> (v, object_safety_distance +. 0.01)) objects in
  let l =
    match planner.beacon with
      | Some v ->
          (v, beacon_safety_distance +. 0.01) :: l
      | None ->
          l
  in
  Krobot_pathfinding.find_path ~src ~dst
    ({ x = border_safety_distance;
       y = border_safety_distance},
     { x = world_width -. border_safety_distance;
       y = world_height -. border_safety_distance})
    l

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

(* Send a bezier curve to the robot. *)
let send_curve planner sign p q r s v_end =
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
  Krobot_message.send planner.bus (Unix.gettimeofday (), Motor_bezier(s.x, s.y, d1, d2, theta_end, v_end))

(* Remove the first vertice of the trajecotry. *)
let drop_vertice planner =
  match planner.vertices with
    | [] ->
        ()
    | _ :: vertices ->
        set_vertices planner ~curves:(List.tl planner.curves) vertices

(* Follow the given path, updating [current_curve] each time we follow
   a new curve. *)
let follow_path planner current_curve path =
  let rec loop = function
    | [] ->
        lwt () = wait_done planner in
        set_vertices planner [];
        return ()

    | [(sign, p, q, r, s)] ->
        lwt () = wait_middle planner in
        lwt () = send_curve planner sign p q r s 0.01 in
        lwt () = wait_start planner in
        current_curve := Bezier.of_vertices p q r s;
        drop_vertice planner;
        lwt () = wait_done planner in
        set_vertices planner [];
        return ()

    | (sign, p, q, r, s) :: rest ->
        lwt () = wait_middle planner in
        lwt () = send_curve planner sign p q r s 0.5 in
        lwt () = wait_start planner in
        current_curve := Bezier.of_vertices p q r s;
        drop_vertice planner;
        loop rest
  in

  (* Add the origin of the trajectory to keep displaying it. *)
  planner.vertices <- planner.position :: path;
  match planner.curves with
    | [] ->
        set_vertices planner [];
        return ()

    | [(sign, p, q, r, s)] ->
        current_curve := Bezier.of_vertices p q r s;
        lwt () = send_curve planner sign p q r s 0.01 in
        lwt () = wait_done planner in
        set_vertices planner [];
        return ()

    | (sign, p, q, r, s) :: rest ->
        current_curve := Bezier.of_vertices p q r s;
        lwt () = send_curve planner sign p q r s 0.5 in
        loop rest

let go planner rotation_speed rotation_acceleration moving_speed moving_acceleration =
  if planner.moving then
    return ()
  else begin
    set_moving planner true;
    planner.mover <- (
      try_lwt
        follow_path planner (ref (Bezier.of_vertices origin origin origin origin)) planner.vertices

      with exn ->
        Lwt_log.error_f ~section ~exn "failed to move"

      finally
        set_moving planner false;
        return ()
    );
    return ()
  end

let abort planner =
  Krobot_message.send planner.bus (Unix.gettimeofday (), Motor_stop(1.0, 0.0))

let check planner curve =
  let rec loop i =
    if i = 256 then
      true
    else
      let v = Bezier.vertice curve (float i /. 255.) in
      if (List.for_all (fun obj -> distance v obj >= object_safety_distance) planner.objects
          && (match planner.beacon with
                | Some v' -> distance v v' >= beacon_safety_distance
                | None -> true)) then
        loop (i + 1)
      else
        false
  in
  loop planner.curve_status

(* Go to the given destination. *)
let goto planner dst =
  if planner.moving then
    return ()
  else begin
    set_moving planner true;
    planner.mover <- (
      try_lwt
        let rec loop () =
          match find_path planner planner.position dst with
            | None ->
                ignore (Lwt_log.info ~section "cannot find a path to the destination");
                return ()
            | Some path ->
                set_vertices planner (planner.position :: path);
                let dummy = Bezier.of_vertices origin origin origin origin in
                let current_curve = ref dummy in
                match_lwt
                  pick [
                    (lwt () = follow_path planner current_curve path in return true);
                    (lwt () =
                       while_lwt !current_curve = dummy do
                         Lwt_unix.sleep 0.01
                       done
                     in
                     lwt () =
                       while_lwt check planner !current_curve do
                         Lwt_unix.sleep 0.01
                       done
                     in
                     return false);
                  ]
                with
                  | true ->
                      return ()
                  | false ->
                      ignore (Lwt_log.info ~section "aborting current trajectory");
                      lwt () = abort planner in
                      lwt () = Lwt_unix.sleep 1.0 in
                      loop ()
        in
        loop ()
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

          | Beacon_position(angle, distance, period) ->
              if distance <> 0. then
                let angle = math_mod_float (planner.orientation +. Krobot_config.rotary_beacon_index_pos +. angle) (2. *. pi) in
                planner.beacon <- Some{
                  x = planner.position.x +. distance *. cos angle;
                  y = planner.position.y +. distance *. sin angle;
                }
              else
                planner.beacon <- None

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

    | Trajectory_goto v ->
        if not planner.moving then
          ignore (goto planner v)

    | Trajectory_stop ->
        cancel planner.mover;
        set_moving planner false;
        set_vertices planner [];
        ignore (Krobot_message.send planner.bus (Unix.gettimeofday (), Motor_stop(1.0, 0.0)))

    | Trajectory_find_path ->
        if not planner.moving then begin
          match planner.vertices with
            | v :: _ ->
                set_vertices planner (match find_path planner planner.position v with Some p -> p | None -> [])
            | _ ->
                ()
        end

    | Objects l ->
        planner.objects <- l

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
    objects = [];
    beacon = None;
    event = E.never;
  } in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message planner) (Krobot_bus.recv bus));

  (* Ask for initial parameters. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Send) in

  (* Wait forever. *)
  fst (wait ())
