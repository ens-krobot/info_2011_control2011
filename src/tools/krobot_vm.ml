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

  mutable coins : vertice list;
  (* Position of coins on the table *)

  mutable moving : bool;
  (* Is the robot moving ? *)

  mutable curve : Bezier.curve option;
  (* The bezier curve currently being followed by the robot. *)

  mutable curve_parameter : int;
  (* The parameter of the bezier curve currently followed by the
     robot. *)

  mutable jack : bool;
  (* Status of the jack. *)

  mutable beacon : vertice option * vertice option;
  (* The detected position of the beacon, if any. *)

  mutable date_seen_beacon : float;
  (* Date at which the beacon has been seen. *)

  mutable team : [ `Red | `Blue ];
  (* The state of the team selector. *)

  mutable emergency_stop : bool;
  (* The state of the emergency button. *)

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

let rec blink bus f =
  lwt () = Krobot_message.send bus (Unix.gettimeofday (),
                                    Switch_request(5,f)) in
  lwt () = Lwt_unix.sleep 0.5 in
  blink bus (not f)

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

          | Beacon_position(angle1, angle2, distance1, distance2) ->
              let compute_beacon angle distance =
                if distance <> 0. then begin
                  robot.date_seen_beacon <- Unix.gettimeofday ();
                  let angle = math_mod_float (robot.orientation +. rotary_beacon_index_pos +. angle) (2. *. pi) in
                  Some{
                    x = robot.position.x +. distance *. cos angle;
                    y = robot.position.y +. distance *. sin angle;
                  }
                end else
                  None
              in
              robot.beacon <- (compute_beacon angle1 distance1,
                               compute_beacon angle2 distance2)

          | Switch1_status(jack, team, emergency, _, _, _, _, _) ->
              robot.jack <- not jack;
              robot.team <- if team then `Red else `Blue;
              robot.emergency_stop <- emergency

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

    | Coins l ->
        robot.coins <-
          List.map
            (fun v ->
              let v = [|v.x;v.y;1.|] in
              let v = mult (rot_mat robot.orientation) v in
              Krobot_geom.translate robot.position { vx = v.(0); vy = v.(1) }) l

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
  | Node(t, (Node _ :: _ as l)) :: rest ->
      Node(t, remove_leftest_node l) :: rest
  | Node _ :: rest ->
      rest
  | l ->
      l

let rec cancel_node = function
  | (Node(Some t, l)) :: rest ->
    (match cancel_node l with
      | None -> Some (t::rest)
      | Some rep -> Some (Node(Some t,rep)::rest))
  | (Node (None, l)) :: rest ->
      cancel_node l
  | _ -> None

(* The effect triggered by the execution of the first action of a tree
   of action. *)
type effect =
  | Wait
      (* Wait a bit. *)
  | Send of Krobot_message.t list
      (* Send messages. *)

let string_of_test = function
  | `Eq -> "Eq"
  | `Gt -> "Gt"
  | `Ge -> "Ge"
  | `Lt -> "Lt"
  | `Le -> "Le"

let revert_vertice v = { v with x = Krobot_config.world_width -. v.x }
let revert_vector_opt v =
  match v with
    | None -> None
    | Some v -> Some { v with vx = -. v.vx }

(* [exec robot actions] searches for the first action to execute in a
   tree of actions and returns a new tree of actions and an effect. *)
let rec exec robot actions =
  match actions with
    | [] ->
        ([], Wait)
    | Node (_,[]) :: rest ->
        ignore (Lwt_log.info_f "Exit node");
        exec robot rest
    | Node (t,actions) :: rest ->
      let actions, effect = exec robot actions in
      (Node (t,actions) :: rest, effect)
    | Wait_for_jack state :: rest ->
        if robot.jack = state then
          (ignore (Lwt_log.info_f "Wait_for_jack finished");
           exec robot rest)
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
    | Wait_for_odometry_reset which :: rest ->
      let init_pos, init_theta = match which, robot.team with
        | `Red, _ | `Auto, `Red ->
          Krobot_config.red_initial_position
        | `Blue, _ | `Auto, `Blue ->
          Krobot_config.blue_initial_position
      in
      if distance robot.position init_pos < 0.01 &&
        ( abs_float (robot.orientation -. init_theta) < 0.01 ||
          abs_float (abs_float (robot.orientation -. init_theta) -. (2. *. pi)) < 0.01 )
      then exec robot rest
      else (actions, Wait)
    | Wait_for t :: rest ->
        ignore (Lwt_log.info_f "Wait_for %f" t);
        exec robot (Wait_until (Unix.gettimeofday () +. t) :: rest)
    | Wait_until t :: rest ->
        if Unix.gettimeofday () >= t then
          (ignore (Lwt_log.info_f "Wait finish");
           exec robot rest)
        else
          (actions, Wait)
    | Set_curve None :: rest ->
        reset robot;
        exec robot rest
    | Set_curve(Some curve) :: rest ->
        robot.curve <- Some curve;
        exec robot rest
    | Goto (revert,v,last_vector) :: rest -> begin
      ignore (Lwt_log.info_f "Goto");
        let v,last_vector = if revert && robot.team = `Blue
          then revert_vertice v, revert_vector_opt last_vector
          else v, last_vector
        in
        (* Try to find a path to the destination. *)
        match Krobot_path.find ~src:robot.position ~dst:v ~objects:robot.objects ~beacon:robot.beacon with
          | Some vertices ->

              exec robot
                (Node
                   (Some
                      (Node (None, [Wait_for 5.;
                                    Goto (revert,v,last_vector)])),
                    [Follow_path (false,vertices,last_vector)]) :: rest)


(*
            let rec aux = function
              | [] -> []
              | [v] ->
                [Node
                   (Some
                      (Node (None, [Follow_path (false,[v],last_vector)])),
                    [Follow_path (false,[v],last_vector)])]
              | v::q ->
                (Node
                   (Some
                      (Node (None, [Follow_path (false,[v],last_vector)])),
                    [Follow_path (false,[v],last_vector)]))::
                  (aux q) in

              exec robot ((Node (None,(aux vertices))) :: rest)
*)

          | None ->
              (* cancel is probably a better idea ? *)
              (* If not found, skip the command. *)
              exec robot rest
      end
    | Set_limits(vmax,atan_max,arad_max) :: rest ->
        ignore (Lwt_log.info_f "Set_limit");
        (rest, Send[Motor_bezier_limits(vmax,atan_max,arad_max)])
    | Set_led(led,value) :: rest ->
        ignore (Lwt_log.info_f "Set_led");
        let led = match led with
          | `Red -> 7
          | `Green -> 6
          | `Yellow -> 5 in
        (rest, Send[Switch_request(led,value)])
    | Follow_path (revert,vertices,last_vector) :: rest -> begin
        let vertices,vector = if revert && robot.team = `Blue
          then List.map revert_vertice vertices, revert_vector_opt last_vector
          else vertices, last_vector
        in
        ignore (Lwt_log.info_f "Follow_path %i vertices" (List.length vertices));
        (* Compute bezier curves. *)
        let vector = { vx = cos robot.orientation; vy = sin robot.orientation } in
        let curves = List.rev (Bezier.fold_vertices ?last:last_vector (fun sign p q r s acc -> (sign, p, q, r, s) :: acc) vector (robot.position :: vertices) []) in
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
              exec robot (Node (None,[
                            Set_curve(Some(Bezier.of_vertices p q r s));
                            Bezier(sign, p, q, r, s, 0.01);
                            Wait_for_odometry(`Le, 128);
                            Wait_for_odometry(`Ge, 128);
                            Wait_for_moving false;
                            Set_curve None;
                          ]) :: rest)
          | (sign, p, q, r, s) :: curves ->
              exec robot (Node(None,Set_curve(Some(Bezier.of_vertices p q r s))
                               :: Bezier(sign, p, q, r, s, 0.5)
                               :: Wait_for_odometry(`Le, 128)
                               :: loop curves) :: rest)
      end
    | Bezier(sign, p, q, r, s, v_end) :: rest ->
        ignore (Lwt_log.info "Bezier");
        (* Compute parameters. *)
        let d1 = sign *. distance p q and d2 = distance r s in
        if abs_float d1 <= 0.01 || abs_float d2 = 0.01
        then
          (* in that case: there is an error somewhere else:
             search and destroy it ! *)
          (ignore (Lwt_log.error_f "Error: Bezier with d1 = %f, d2 = %f" d1 d2);
           ([], Wait))
        else
          let v = vector r s in
          let theta_end = atan2 v.vy v.vx in
          (rest, Send[Switch_request(5,false);
                      Motor_bezier(s.x, s.y, d1, d2, theta_end, v_end)])
    | Stop :: rest ->
        ignore (Lwt_log.info_f "Stop");
        reset robot;
        (rest, Send[Switch_request(5,true); Motor_stop(1.0, 0.0)])
    | Reset_odometry which :: rest ->
        (rest,
         Send
           (match which, robot.team with
              | `Red, _ | `Auto, `Red ->
                let { Krobot_geom.x; y }, angle = Krobot_config.red_initial_position in
                [Set_odometry( x, y, 0. );
                 Set_odometry_indep( x, y, 0. ); ]
              | `Blue, _ | `Auto, `Blue ->
                let { Krobot_geom.x; y }, angle = Krobot_config.blue_initial_position in
                [Set_odometry( x, y, pi);
                 Set_odometry( x, y, pi )]))
    | Load face :: rest ->
        exec robot (Node (None,[
                      Lift_down face;
                      Open_grip_low face;
                      Wait_for_grip_open_low face;
                      Wait_for 0.5;
                      (* Move_to_pawn *)
                      Close_grip_low face;
                      Lift_up face;
                      Wait_for_grip_close_low face;
                      Wait_for 0.5;
                    ]) :: rest)
    | Lift_down `Front :: rest ->
        (rest, Send[Elevator(0., -1.)])
    | Lift_up `Front :: rest ->
        (rest, Send[Elevator(1., -1.)])
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

    (* Check if a program asked for adding actions to the current strategy. *)
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
                raise Exit
              end;
              (* Check that there is no colision between the current
                 position and the end of the current curve. *)
              for i = robot.curve_parameter to 255 do
                let v = Bezier.vertice curve (float i /. 255.) in
                let b1,b2 = robot.beacon in
                let c1 = match b1 with
                  | None -> false
                  | Some b1 -> distance b1 v < beacon_safety_distance in
                let c2 = match b2 with
                  | None -> false
                  | Some b2 -> distance b2 v < beacon_safety_distance in
                if (List.exists (fun obj -> distance v obj < object_safety_distance)
                      robot.objects
                    || c1 || c2) then begin
                  ignore (Lwt_log.info_f "Obstacle in the trajectory");
                  raise Exit
                end
              done
            with Exit ->
              let replacer = match cancel_node robot.strategy with
                | None -> [Stop]
                | Some r -> r in
              robot.strategy <- replacer;
              reset robot
    end;

    let actions, effect = exec robot robot.strategy in
    robot.strategy <- actions;
    match effect with
      | Wait ->
          Lwt_unix.sleep 0.01
      | Send msgs ->
        Lwt_list.iter_s
          (fun m -> Krobot_message.send robot.bus (timestamp, m))
          msgs
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
    coins = [];
    moving = false;
    path = None;
    curve = None;
    curve_parameter = 0;
    jack = false;
    beacon = None, None;
    date_seen_beacon = 0.;
    team = `Red;
    emergency_stop = false;
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

  ignore (
    lwt () = Lwt_unix.sleep 2. in
    Krobot_message.send bus (Unix.gettimeofday (),Motor_command (2,1000)));

  ignore(blink bus false);

  (* Run forever. *)
  run robot
