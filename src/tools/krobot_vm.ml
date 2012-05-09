(*
 * krobot_vm.ml
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* The krobot virtual machine *)

open Lwt
open Lwt_react
open Krobot_config
open Krobot_geom
open Krobot_bus
open Krobot_message
open Krobot_action

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

(* State of an AX12. *)
type ax12 = {
  mutable ax12_position : int;
  mutable ax12_speed : int;
  mutable ax12_torque : int;
}

(* Type of robots. *)
type robot = {
  bus : Krobot_bus.t;
  (* The bus used to communicate with the robot. *)

  mutable strategy : Krobot_action.t list;
  (* The current strategy. *)

  mutable change_strategy : Krobot_action.t list option;
  (* This is used the change the current strategy. *)

  mutable append_strategy : Krobot_action.t list option;
  (* This is used to append actions to the current strategy. *)

  mutable path : Bezier.curve list option;
  (* The path currently followed by the robot. *)

  mutable position : vertice;
  (* The position of the robot on the table. *)

  mutable ghost_position : vertice;
  (* The position of the ghost on the table. *)

  mutable orientation : float;
  (* The orientation of the robot. *)

  mutable objects : vertice list;
  (* Position of objects on the table. *)

  mutable moving : bool;
  (* Is the robot moving ? *)

  mutable curve : Bezier.curve option;
  (* The bezier curve currently being followed by the robot. *)

  mutable curve_parameter : int;
  (* The parameter of the bezier curve currently followed by the
     robot. *)

  mutable jack : bool;
  (* Status of the jack. *)

  mutable beacon : vertice option;
  (* The detected position of the beacon, if any. *)

  mutable date_seen_beacon : float;
  (* Date at which the beacon has been seen. *)

  mutable team : [ `Red | `Blue ];
  (* The state of the team selector. *)

  ax12_front_low_left : ax12;
  ax12_front_low_right : ax12;
  ax12_front_high_left : ax12;
  ax12_front_high_right : ax12;
  ax12_back_low_left : ax12;
  ax12_back_low_right : ax12;
  ax12_back_high_left : ax12;
  ax12_back_high_right : ax12;
  (* State of AX12s. *)
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

          | Odometry_ghost(x, y, theta, u, following) ->
              robot.ghost_position <- { x; y };
              robot.curve_parameter <- u;
              robot.moving <- following

          | Beacon_position(angle, distance, period) ->
              if distance <> 0. then begin
                robot.date_seen_beacon <- Unix.gettimeofday ();
                let angle = math_mod_float (robot.orientation +. rotary_beacon_index_pos +. angle) (2. *. pi) in
                robot.beacon <- Some{
                  x = robot.position.x +. distance *. cos angle;
                  y = robot.position.y +. distance *. sin angle;
                }
              end else
                robot.beacon <- None

          | Switch1_status(b, _, _, _, _, _, _, _) ->
              robot.jack <- not b

          | Ax12_State(id, position, speed, torque) -> begin
              let set ax12 =
                ax12.ax12_position <- position;
                ax12.ax12_speed <- speed;
                ax12.ax12_torque <- torque
              in
              match id with
                | 1 -> set robot.ax12_front_low_left
                | 2 -> set robot.ax12_front_low_right
                | 3 -> set robot.ax12_front_high_left
                | 4 -> set robot.ax12_front_high_right
                | _ -> ()
            end

          | _ ->
              ()
      end

    | Kill "vm" ->
        exit 0

    | Send ->
        ignore (
          let timestamp = Unix.gettimeofday () in
          Krobot_bus.send robot.bus (timestamp, Strategy_path robot.path)
        )

    | Objects l ->
        robot.objects <- l

    | Strategy_append l -> begin
        match robot.append_strategy with
          | Some l' ->
              robot.append_strategy <- Some(l' @ l)
          | None ->
              robot.append_strategy <- Some l
      end

    | Strategy_stop ->
        robot.change_strategy <- Some [Stop]

    | Strategy_set l ->
        robot.append_strategy <- None;
        robot.change_strategy <- Some(Stop :: l)

    | _ ->
        ()

(* +-----------------------------------------------------------------+
   | Helpers                                                         |
   +-----------------------------------------------------------------+ *)

let set_path robot path =
  robot.path <- path;
  ignore (Krobot_bus.send robot.bus (Unix.gettimeofday (), Strategy_path path))

let reset robot =
  robot.curve <- None;
  set_path robot None

(* +-----------------------------------------------------------------+
   | Execution                                                       |
   +-----------------------------------------------------------------+ *)

(* Remove the leftest node from a tree of actions. *)
let rec remove_leftest_node = function
  | Node(Node _ :: _ as l) :: rest ->
      Node(remove_leftest_node l) :: rest
  | Node _ :: rest ->
      rest
  | l ->
      l

(* The effect triggered by the execution of the first action of a tree
   of action. *)
type effect =
  | Wait
      (* Wait a bit. *)
  | Send of Krobot_message.t
      (* Send a message. *)

let string_of_test = function
  | `Eq -> "Eq"
  | `Gt -> "Gt"
  | `Ge -> "Ge"
  | `Lt -> "Lt"
  | `Le -> "Le"

(* [exec robot actions] searches for the first action to execute in a
   tree of actions and returns a new tree of actions and an effect. *)
let rec exec robot actions =
  match actions with
    | [] ->
        ([], Wait)
    | Node [] :: rest ->
        exec robot rest
    | Node actions :: rest ->
        let actions, effect = exec robot actions in
        (Node actions :: rest, effect)
    | Wait_for_jack state :: rest ->
        if robot.jack = state then
          exec robot rest
        else
          (actions, Wait)
    | Wait_for_moving state :: rest ->
        if robot.moving = state then
          (ignore (Lwt_log.info_f "Wait_for_moving %b done" state);
           exec robot rest)
        else
          (actions, Wait)
    | Wait_for_odometry(test, value) :: rest ->
        if (match test with
              | `Eq -> robot.curve_parameter = value
              | `Gt -> robot.curve_parameter > value
              | `Ge -> robot.curve_parameter >= value
              | `Lt -> robot.curve_parameter < value
              | `Le -> robot.curve_parameter <= value) then
          (ignore (Lwt_log.info_f "Wait_for_odometry %i %s %i done"
                     robot.curve_parameter (string_of_test test) value);
           exec robot rest)
        else
          (actions, Wait)
    | Wait_for t :: rest ->
        exec robot (Wait_until (Unix.gettimeofday () +. t) :: rest)
    | Wait_until t :: rest ->
        if Unix.gettimeofday () >= t then
          exec robot rest
        else
          (actions, Wait)
    | Set_curve None :: rest ->
        reset robot;
        exec robot rest
    | Set_curve(Some curve) :: rest ->
        robot.curve <- Some curve;
        exec robot rest
    | Goto v :: rest -> begin
        (* Try to find a path to the destination. *)
        match Krobot_path.find ~src:robot.position ~dst:v ~objects:robot.objects ~beacon:robot.beacon with
          | Some vertices ->
              exec robot (Follow_path vertices :: rest)
          | None ->
              (* If not found, skip the command. *)
              exec robot rest
      end
    | Follow_path vertices :: rest -> begin
        ignore (Lwt_log.info_f "Follow_path %i vertices" (List.length vertices));
        (* Compute bezier curves. *)
        let vector = { vx = cos robot.orientation; vy = sin robot.orientation } in
        let curves = List.rev (Bezier.fold_vertices (fun sign p q r s acc -> (sign, p, q, r, s) :: acc) vector (robot.position :: vertices) []) in
        (* Set the path. *)
        set_path robot (Some (List.map (fun (sign, p, q, r, s) -> Bezier.of_vertices p q r s) curves));
        (* Compute orders. *)
        let rec loop = function
          | [] ->
              [Wait_for_moving false]
          | [(sign, p, q, r, s)] ->
              ignore (Lwt_log.info_f "add last %f %f" p.x p.y);
              [
                (* Wait for the odometry to reach the middle of the
                   current trajectory. *)
                Wait_for_odometry(`Ge, 128);
                (* Send the next bezier curve. *)
                Bezier(sign, p, q, r, s, 0.01);
                (* Wait for the odometry to start the new curve. *)
                Wait_for_odometry(`Lt, 128);
                (* Set the new curve. *)
                Set_curve(Some(Bezier.of_vertices p q r s));
                (* Wait for the end of the new curve. *)
                Wait_for_moving false;
                (* Remove the current curve. *)
                Set_curve None;
              ]
          | (sign, p, q, r, s) :: rest ->
              ignore (Lwt_log.info_f "add middle %f %f" p.x p.y);
              Wait_for_odometry(`Ge, 128)
              :: Bezier(sign, p, q, r, s, 0.5)
              :: Wait_for_odometry(`Lt, 128)
              :: Set_curve(Some(Bezier.of_vertices p q r s))
              :: loop rest
        in
        match curves with
          | [] ->
              exec robot rest
          | [(sign, p, q, r, s)] ->
              exec robot (Node [
                            Set_curve(Some(Bezier.of_vertices p q r s));
                            Bezier(sign, p, q, r, s, 0.01);
                            Wait_for_odometry(`Le, 128);
                            Wait_for_odometry(`Ge, 128);
                            Wait_for_moving false;
                            Set_curve None;
                          ] :: rest)
          | (sign, p, q, r, s) :: curves ->
              exec robot (Node(Set_curve(Some(Bezier.of_vertices p q r s))
                               :: Bezier(sign, p, q, r, s, 0.5)
                               :: Wait_for_odometry(`Le, 128)
                               :: loop curves) :: rest)
      end
    | Bezier(sign, p, q, r, s, v_end) :: rest ->
        ignore (Lwt_log.info "Bezier");
        (* Compute parameters. *)
        let d1 = sign *. distance p q and d2 = distance r s in
        let v = vector r s in
        let theta_end = atan2 v.vy v.vx in
        (rest, Send(Motor_bezier(s.x, s.y, d1, d2, theta_end, v_end)))
    | Stop :: rest ->
        reset robot;
        (rest, Send(Motor_stop(1.0, 0.0)))
    | Reset_odometry which :: rest ->
        (rest,
         Send
           (match which, robot.team with
              | `Red, _ | `Auto, `Red ->
                  Set_odometry(0.215 -. robot_size /. 2. +. wheels_position, 1.885, 0.)
              | `Blue, _ | `Auto, `Blue ->
                  Set_odometry(2.77, 1.915, pi)))
    | Load face :: rest ->
        exec robot (Node [
                      Lift_down face;
                      Open_grip_low face;
                      Wait_for_grip_open_low face;
                      Wait_for 0.5;
                      (* Move_to_pawn *)
                      Close_grip_low face;
                      Lift_up face;
                      Wait_for_grip_close_low face;
                      Wait_for 0.5;
                    ] :: rest)
    | Lift_down `Front :: rest ->
        (rest, Send(Elevator(0., -1.)))
    | Lift_up `Front :: rest ->
        (rest, Send(Elevator(1., -1.)))
    | Think :: rest ->
        exec robot rest
    | _ :: rest ->
        exec robot rest

(* +-----------------------------------------------------------------+
   | Main loop                                                       |
   +-----------------------------------------------------------------+ *)

let run robot =
  while_lwt true do
    let timestamp = Unix.gettimeofday () in

    (* Check if a program asked for the strategy to change. *)
    begin
      match robot.change_strategy with
        | Some l ->
            robot.strategy <- l;
            robot.change_strategy <- None
        | None ->
            ()
    end;

    (* Check if a program added for adding actions to the current strategy. *)
    begin
      match robot.append_strategy with
        | Some l ->
            robot.strategy <- robot.strategy @ l;
            robot.append_strategy <- None
        | None ->
            ()
    end;

    (* Check obstacles. *)
    if robot.moving then begin
      match robot.curve with
        | None ->
            ()
        | Some curve ->
            try
              (* Check that the robot is not too far from the ghost.
                 if it is then stop brutaly:
                 TODO do something interesting after the stop: retry what we were doing *)
              if distance robot.ghost_position robot.position > 0.1 then begin
                ignore (Lwt_log.info_f "Robot too far from the ghost");
                robot.strategy <- [Stop];
                reset robot;
                raise Exit
              end;
              (* Check that there is no colision between the current
                 position and the end of the current curve. *)
              for i = robot.curve_parameter to 255 do
                let v = Bezier.vertice curve (float i /. 255.) in
                if (List.exists (fun obj -> distance v obj < object_safety_distance) robot.objects
                    || robot.date_seen_beacon +. 5. > timestamp) then begin
                  robot.strategy <- Stop :: remove_leftest_node robot.strategy;
                  reset robot;
                  raise Exit
                end
              done
            with Exit ->
              ()
    end;

    let actions, effect = exec robot robot.strategy in
    robot.strategy <- actions;
    match effect with
      | Wait ->
          Lwt_unix.sleep 0.01
      | Send msg ->
          Krobot_message.send robot.bus (timestamp, msg)
  done

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
]

let usage = "\
Usage: krobot-vm [options]
options are:"

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

lwt () =
  Arg.parse options ignore usage;

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  Lwt_log.default :=
    Lwt_log.channel ~template:"$(name): $(section): $(message) $(date) $(milliseconds)"
    ~close_mode:`Keep ~channel:Lwt_io.stderr ();

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  (* Fork if not prevented. *)
  if !fork then Lwt_daemon.daemonize ();

  (* Kill any running vm. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "vm") in

  (* Create a new robot. *)
  let robot = {
    bus;
    strategy = [];
    change_strategy = None;
    append_strategy = None;
    position = { x = 0.; y = 0. };
    ghost_position = { x = 0.; y = 0. };
    orientation = 0.;
    objects = [];
    moving = false;
    path = None;
    curve = None;
    curve_parameter = 0;
    jack = false;
    beacon = None;
    date_seen_beacon = 0.;
    team = `Red;
    ax12_front_low_left = { ax12_position = 0; ax12_speed = 0; ax12_torque = 0 };
    ax12_front_low_right = { ax12_position = 0; ax12_speed = 0; ax12_torque = 0 };
    ax12_front_high_left = { ax12_position = 0; ax12_speed = 0; ax12_torque = 0 };
    ax12_front_high_right = { ax12_position = 0; ax12_speed = 0; ax12_torque = 0 };
    ax12_back_low_left = { ax12_position = 0; ax12_speed = 0; ax12_torque = 0 };
    ax12_back_low_right = { ax12_position = 0; ax12_speed = 0; ax12_torque = 0 };
    ax12_back_high_left = { ax12_position = 0; ax12_speed = 0; ax12_torque = 0 };
    ax12_back_high_right = { ax12_position = 0; ax12_speed = 0; ax12_torque = 0 };
  } in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message robot) (Krobot_bus.recv bus));

  (* Ask for parameters. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Send) in

  (* Run forever. *)
  run robot
