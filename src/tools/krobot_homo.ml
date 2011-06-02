(*
 * krobot_homo.ml
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Lwt_react
open Krobot_bus
open Krobot_message
open Krobot_geom

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type robot = {
  bus : Krobot_bus.t;
  (* The message bus used to communicate with the robot. *)

  mutable moving : bool;
  (* Is the robot moving ? *)

  mutable position : vertice;
  (* The position of the robot on the board. *)

  mutable orientation : float;
  (* The orientation of the robot. *)

  mutable objects : vertice list;
  (* The list of objects on the board. *)

  mutable beacon : vertice option;
  (* Position of the beacon. *)
}

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message robot (timestamp, message) =
  match message with
    | CAN(_, frame) -> begin
        match decode frame with
          | Odometry(x, y, theta) ->
              robot.position <- { x; y };
              robot.orientation <- math_mod_float theta (2. *. pi)

          | Beacon_position(angle, distance, period) ->
              if distance <> 0. then
                let angle = math_mod_float (robot.orientation +. Krobot_config.rotary_beacon_index_pos +. angle) (2. *. pi) in
                robot.beacon <- Some{
                  x = robot.position.x +. distance *. cos angle;
                  y = robot.position.y +. distance *. sin angle;
                }
              else
                robot.beacon <- None

          | _ ->
              ()
      end

    | Trajectory_moving moving ->
        robot.moving <- moving

    | Objects l ->
        robot.objects <- l

    | _ ->
        ()

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

lwt () =
  lwt bus = Krobot_bus.get () in

  let robot = {
    bus;
    moving = false;
    position = origin;
    orientation = 0.;
    objects = [];
    beacon = None;
  } in

  E.keep (E.map (handle_message robot) (Krobot_bus.recv bus));

  lwt () = Lwt_unix.sleep 1.0 in

  let dst = { x = 1.5; y = 1.05 } in
  match Krobot_path.goto_object ~src:robot.position ~dst ~objects:robot.objects ~beacon:robot.beacon with
    | None ->
        Lwt_io.eprintl "no solution"
    | Some v ->
        lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Trajectory_goto v) in
        lwt () = Lwt_unix.sleep 1.0 in
        while_lwt robot.moving do
          Lwt_unix.sleep 0.01
        done
