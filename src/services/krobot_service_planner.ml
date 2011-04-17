(*
 * krobot_service_planner.ml
 * -------------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* The trajectory planner. *)

open Lwt
open Lwt_react
open Krobot_geom
open Krobot_config
open Krobot_message
open Krobot_interface_planner.Fr_krobot_Planner

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

  obus : planner OBus_object.t;
  (* The D-Bus object attached to this planner. *)

  origin : (vertice * vector) signal;
  (* If the robot is moving, this is the origin of the current
     trajectory with the initial direction vector, otherwise it is the
     current position with the current direction vector. *)

  set_origin : (vertice * vector) -> unit;
  (* Set the origin of the trajectory. *)

  vertices : vertice list signal;
  (* The list of vertices for the trajectory. *)

  set_vertices : vertice list -> unit;
  (* Set the list of vertices. *)

  moving : bool signal;
  (* Is the robot moving ? *)

  set_moving : bool -> unit;
  (* Set the moving status. *)

  mutable motors_moving : bool;
  (* Are the motor currently active ? *)

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

let add_vertice planner vertice =
  planner.set_vertices (S.value planner.vertices @ [vertice])

let simplify planner tolerance =
  match S.value planner.vertices with
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
        planner.set_vertices (List.map (fun i -> points.(i)) result)

let wait_done planner =
  lwt () = Lwt_log.info "waiting for the robot to stop moving" in
  lwt () = Lwt_unix.sleep 0.3 in
  lwt () =
    while_lwt planner.motors_moving do
      Lwt_unix.sleep 0.2
    done
  in
  Lwt_log.info "trajectory done"

let go planner rotation_speed rotation_acceleration moving_speed moving_acceleration =
  if S.value planner.moving then
    return ()
  else begin
    let rec loop () =
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
    in
    planner.set_moving true;
    planner.mover <- (
      try_lwt
        loop ()
      finally
        planner.set_moving false;
        return ()
    );
    return ()
  end

let stop planner =
  cancel planner.mover;
  planner.set_moving false;
  planner.set_vertices [];
  Krobot_message.send planner.bus (Unix.gettimeofday (), Motor_stop)

(* +-----------------------------------------------------------------+
   | Object creation                                                 |
   +-----------------------------------------------------------------+ *)

let create bus =
  let obus_object =
    OBus_object.make
      ~interfaces:[
        Krobot_interface_planner.Fr_krobot_Planner.make {
          p_origin =
            (fun obj ->
               S.map (fun ({ x; y }, { vx; vy }) -> ((x, y), (vx, vy))) (OBus_object.get obj).origin);
          p_vertices =
            ((fun obj ->
                S.map (List.map (fun { x; y } -> (x, y))) (OBus_object.get obj).vertices),
             (fun obj vertices ->
                (OBus_object.get obj).set_vertices (List.map (fun (x, y) -> { x; y }) vertices);
                return ()));
          p_moving =
            (fun obj ->
               (OBus_object.get obj).moving);
          m_simplify =
            (fun obj tolerance ->
               simplify (OBus_object.get obj) tolerance;
               return ());
          m_add_vertice =
            (fun obj (x, y) ->
               add_vertice (OBus_object.get obj) { x; y };
               return ());
          m_go =
            (fun obj (a, b, c, d) ->
               go (OBus_object.get obj) a b c d);
          m_stop =
            (fun obj () ->
               stop (OBus_object.get obj));
        };
      ]
      ["fr"; "krobot"; "Planner"]
  in
  let origin, set_origin = S.create ({ x = 0.; y = 0. }, { vx = 1.; vy = 0. }) in
  let vertices, set_vertices = S.create [] in
  let moving, set_moving = S.create false in
  let planner = {
    bus;
    obus = obus_object;
    origin;
    set_origin;
    vertices;
    set_vertices;
    moving;
    set_moving;
    motors_moving = false;
    mover = return ();
    position = { x = 0.; y = 0. };
    orientation = 0.;
    event = E.never;
  } in
  OBus_object.attach obus_object planner;

  planner.event <- (
      E.map
        (fun (ts, frame) ->
           match frame with
             | Odometry(x, y, theta) ->
                 planner.position <- { x; y };
                 planner.orientation <- math_mod_float theta (2. *. pi);
                 if not (S.value planner.moving) then
                   planner.set_origin (planner.position,
                                       { vx = cos planner.orientation;
                                         vy = sin planner.orientation })
             | Motor_status(m1, m2, m3, m4) ->
                 planner.motors_moving <- m1 || m2
             | _ ->
                 ())
        (Krobot_message.recv bus)
  );

  planner

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true
let kill = ref false

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
  "-kill", Arg.Set kill, " Kill any running planner and exit";
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
  Lwt_log.Section.set_level Lwt_log.Section.main Lwt_log.Info;

  lwt bus = Krobot_bus.get () in

  (* Create a new planner. *)
  let planner = create bus in

  (* Export it on the krobot bus. *)
  OBus_object.export (Krobot_bus.to_bus bus) planner.obus;

  (* Start the service. *)
  lwt () = Krobot_service.init bus ~kill:!kill ~fork:!fork "Planner" in

  (* Wait forever. *)
  fst (wait ())
