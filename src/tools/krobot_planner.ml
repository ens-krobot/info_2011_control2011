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
  (* The list of vertices of the current trajectory. *)

  mutable curves : Bezier.curve list;
  (* The list of bezier curves of the current trajectory. *)

  mutable following_path : bool;
  (* Is the VM following a path ? *)

  mutable position : vertice;
  (* The position of the robot on the board. *)

  mutable orientation : float;
  (* The orientation of the robot. *)

  mutable beacon : vertice option * vertice option;
  (* Position of the beacon. *)
}

(* +-----------------------------------------------------------------+
   | Motion planning                                                 |
   +-----------------------------------------------------------------+ *)

let find_path planner src dst =
  Krobot_path.find ~src ~dst ~beacon:planner.beacon

(* +-----------------------------------------------------------------+
   | Primitives                                                      |
   +-----------------------------------------------------------------+ *)

let set_vertices planner vertices =
  let v = { vx = cos planner.orientation; vy = sin planner.orientation } in
  let curves = List.rev (Bezier.fold_curves (fun _ curve acc -> curve :: acc) v (planner.position :: vertices) []) in
  planner.vertices <- vertices;
  planner.curves <- curves;
  ignore (Krobot_bus.send planner.bus (Unix.gettimeofday (), Trajectory_path curves))

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

          | Beacon_position(angle1, angle2, distance1, distance2) ->
              let compute_beacon angle distance =
                if distance <> 0. then begin
                  let angle = math_mod_float (planner.orientation +. rotary_beacon_index_pos +. angle) (2. *. pi) in
                  Some{
                    x = planner.position.x +. distance *. cos angle;
                    y = planner.position.y +. distance *. sin angle;
                  }
                end else
                  None
              in
              planner.beacon <- (compute_beacon angle1 distance1,
                                 compute_beacon angle2 distance2)
          | _ ->
              ()
      end

    | Kill "planner" ->
        exit 0

    | Send ->
        ignore (Krobot_bus.send planner.bus (Unix.gettimeofday (), Trajectory_path planner.curves))

    | Trajectory_set_vertices l ->
        set_vertices planner l

    | Trajectory_add_vertice vertice ->
        set_vertices planner (planner.vertices @ [vertice])

    | Trajectory_simplify tolerance ->
        simplify planner tolerance

    | Trajectory_go ->
        let path = planner.vertices in
        ignore (Krobot_bus.send planner.bus (Unix.gettimeofday (), Strategy_set [Krobot_action.Follow_path (false,path,None,false)]));
        set_vertices planner []

    | Trajectory_find_path -> begin
        match planner.vertices with
          | v :: _ ->
              set_vertices planner (match find_path planner planner.position v with Some p -> p | None -> [])
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
  if !fork then Krobot_daemon.daemonize bus;

  (* Kill any running planner. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "planner") in

  (* Create a new planner. *)
  let planner = {
    bus;
    vertices = [];
    curves = [];
    following_path = false;
    position = { x = 0.; y = 0. };
    orientation = 0.;
    beacon = None, None;
  } in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message planner) (Krobot_bus.recv bus));

  (* Ask for initial parameters. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Send) in

  (* Wait forever. *)
  fst (wait ())
