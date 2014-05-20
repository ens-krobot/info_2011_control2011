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

let time_zero = Unix.gettimeofday ()
let current_time () = Unix.gettimeofday () -. time_zero

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

(* State of an AX12. *)
type ax12 = {
  mutable ax12_position : int;
  mutable ax12_speed : int;
  mutable ax12_torque : int;
}

module StringSet = Set.Make(String)

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

  mutable coins : vertice list;
  (* Position of coins on the table *)

  mutable objects : Krobot_geom.obj list;
  (* objects seen by the urg *)

  mutable bezier_moving : bool;
  (* Is the robot following a bezier curve ? *)

  mutable motors_moving : bool;
  (* Are motors moving ? *)

  mutable curve : curve;
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

  mutable replace : bool;

  mutable init_time : float option;

  mutable delayed_action : (float * Krobot_action.t list) option;

  mutable elevator_left_moving : bool;
  mutable elevator_right_moving : bool;
  mutable elevator_left_homed : bool;
  mutable elevator_right_homed : bool;

  mutable received_finished_ax12_sequence : StringSet.t;

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


let rec blink bus robot f =
  lwt () = if (not robot.emergency_stop) then begin
    lwt () = Krobot_message.send bus (Unix.gettimeofday (),
                                      Switch_request(5,f)) in
    lwt () = Krobot_message.send bus (Unix.gettimeofday (),
                                      Switch_request(6,f)) in
    lwt () = Krobot_message.send bus (Unix.gettimeofday (),
                                      Switch_request(7,f)) in
    Lwt.return ()
    end
  else
    lwt () = Krobot_message.send bus (Unix.gettimeofday (),
                                      Switch_request(5,f)) in
    Lwt.return ()
  in
  lwt () = Lwt_unix.sleep 0.5 in
  blink bus robot (not f)

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
              robot.bezier_moving <- following

          | Motor_status (b1, b2, b3, b4) ->
            let r = b1 || b2 || b3 || b4 in
            if robot.motors_moving <> r
            then
              if r
              then ignore (Lwt_log.info_f "motor start %f" (current_time ()))
              else ignore (Lwt_log.info_f "motor stop %f" (current_time ()));
            robot.motors_moving <- r

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

          | Effector_status(l,r,_,_) ->
            robot.elevator_left_moving <- l;
            robot.elevator_right_moving <- r

          | Homing_status(l,r) ->
            robot.elevator_left_homed <- l;
            robot.elevator_right_homed <- r

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

    | Objects objects ->
      robot.objects <- List.map (fun (pos,size) -> { pos; size }) objects

    | Coins l ->
        robot.coins <-
          List.map
            (fun v ->
              let v = [|v.x;v.y;1.|] in
              let v = mult (rot_mat robot.orientation) v in
              Krobot_geom.translate robot.position { vx = v.(0); vy = v.(1) }) l

    | Finished_ax12_sequence name ->
        robot.received_finished_ax12_sequence <-
          StringSet.add name robot.received_finished_ax12_sequence

    | Strategy_append l -> begin
        ignore (Lwt_log.info "append strategy");
        match robot.append_strategy with
          | Some l' ->
              robot.append_strategy <- Some(l' @ l)
          | None ->
              robot.append_strategy <- Some l
      end

    | Strategy_stop ->
        ignore (Lwt_log.info "stop strategy");
        robot.change_strategy <- Some [Stop]

    | Strategy_set l ->
        ignore (Lwt_log.info "set strategy");
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
  robot.curve <- No_curve;
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

(** cancel the deapest node *)
let rec cancel_node = function
  (* cancel simple node: cancel everything *)
  | (Node (Simple, l)) :: rest ->
    (match cancel_node l with
     | None -> None
     | Some rep ->
       (* if a son handled the cancel: continue *)
       Some(rep@rest))

  | (Node (Next, l)) :: rest ->
    (match cancel_node l with
     | None ->
       (* next failing -> continue with the next one *)
       Some(rest)
     | Some rep ->
       (* if a son handled the cancel: continue *)
       Some(Node(Next,rep)::rest))

  | (Node(Retry(n,t), l)) :: rest ->
    (match cancel_node l with
      | None ->
        (match n with
         | 0 -> None (* like simple *)
         | 1 -> Some (t::rest) (* continue simply with the fallback *)
         | n -> Some (Node(Retry(n-1,t),[t])::rest))
      | Some rep ->
        Some (Node(Retry(n,t),rep)::rest))

  | (Node(Loop t, l)) :: rest ->
    (match cancel_node l with
     | None -> Some (Node(Loop t,[t])::rest)
     | Some rep -> Some (Node(Loop(t),rep)::rest))

  | _ ->
    None

let replace robot =
  ignore (Lwt_log.info "replace");
  let replacer = match cancel_node robot.strategy with
    | None -> [Stop]
    | Some r -> r in
  robot.strategy <- replacer;
  robot.replace <- false;
  reset robot


(* The effect triggered by the execution of the first action of a tree
   of action. *)
type effect =
  | Wait
      (* Wait a bit. *)
  | Send_bus of Krobot_bus.message list
  | Send of Krobot_message.t list
      (* Send messages. *)
  | Send_frame of Krobot_can.frame list
  | Abort

let string_of_test = function
  | `Eq -> "Eq"
  | `Gt -> "Gt"
  | `Ge -> "Ge"
  | `Lt -> "Lt"
  | `Le -> "Le"

let bezier_collide dir objects curve c1 c2 shift_vector curve_parameter =
  let curve = Bezier.mul_d1 curve c1 in
  let curve = Bezier.mul_d2 curve c2 in
  let collisions = ref [] in
  let last_position = ref { x = nan; y = nan } in
  let last_angle = ref nan in
  for n = curve_parameter to 255 do
    let u = float n /. 255. in
    let vert = translate (Bezier.vertice curve u) shift_vector in
    let tangent = Bezier.dt curve u in
    let angle = atan2 tangent.vy tangent.vx in
    let angle = if dir then angle else angle +. pi in
    let sufficiently_different =
      (* only compute collisions if we are sufficiently far from the previous point *)
      let dist = distance vert !last_position in
      let dangle = abs_float (angle -. !last_angle) in
      n = curve_parameter || n = 255 || dist > 0.01 || dangle > 0.01 in
    if sufficiently_different
    then begin
      last_position := vert;
      last_angle := angle;
      if not (Krobot_collision.robot_in_world vert angle) then
        collisions := (u, None) :: !collisions;
      List.iter
        (fun c ->
           if Krobot_collision.collision_robot_circle vert angle c.pos c.size then
             collisions := (u, Some (c.pos, c.size)) :: !collisions)
        objects
    end
  done;
  (curve, !collisions)

let rotation_collide position orig_angle end_angle direction objects =
  let collisions = ref [] in
  let last_angle = ref nan in
  let steps = 100 in
  let dangle = diff_angle direction ~start:orig_angle ~stop:end_angle in
  let step_angle = dangle /. (float steps) in
  for n = 0 to steps do
    let angle = orig_angle +. ((float n) *. step_angle) in
    let sufficiently_different =
      (* only compute collisions if we are sufficiently far from the previous point *)
      let dangle = abs_float (angle -. !last_angle) in
      n = 0 || n = steps || dangle > 0.01 in
    if sufficiently_different
    then begin
      last_angle := angle;
      if not (Krobot_collision.robot_in_world position angle) then
        collisions := (position, Krobot_config.robot_radius) :: !collisions;
      List.iter
        (fun c ->
           if Krobot_collision.collision_robot_circle position angle c.pos c.size then
             collisions := (c.pos, c.size) :: !collisions)
        objects
    end
  done;
  !collisions

let find_rotation position orig_angle end_angle objects =
  let dangle_trigo = diff_angle Trigo ~start:orig_angle ~stop:end_angle in
  let dangle_antitrigo = diff_angle Antitrigo ~start:orig_angle ~stop:end_angle in
  Lwt_log.ign_info_f "angles %f %f" dangle_trigo dangle_antitrigo;
  let (first,second) =
    if dangle_trigo > (-.dangle_antitrigo)
    then Antitrigo, Trigo
    else Trigo, Antitrigo in
  match rotation_collide position orig_angle end_angle first objects with
  | [] -> Some first
  | _::_ ->
    Lwt_log.ign_info "can't turn trigo";
    match rotation_collide position orig_angle end_angle second objects with
    | [] -> Some second
    | _::_ -> None

let build_objects robot =
  let fixed_objects = Krobot_config.fixed_obstacles in

  let l = robot.objects @ fixed_objects in

  let l =
    match robot.beacon with
      | (Some v, None)
      | (None, Some v) ->
        { pos = v; size = beacon_radius } :: l
      | (Some v1, Some v2) ->
        { pos = v1; size = beacon_radius }
        :: { pos = v2; size = beacon_radius }
        :: l
      | (None, None) ->
        l
  in
  l

let not_ignore_probal = 0.7

let ignore_some proba l =
  List.filter (fun _ -> Random.float 1. <= proba) l

(* let path_ignoring proba robot dst last_vector = *)
(*   Krobot_path.find ~src:robot.position ~dst *)
(*     ~beacon:robot.beacon *)
(*     ~objects:(ignore_some proba robot.objects) *)

(* let goto_node v vertices last_vector = *)
(*   (Node (Retry (1,Node (Simple, [Stop; Try_something v; Wait_for 0.1; *)
(*                                  Goto (v,last_vector)])), *)
(*      [Follow_path (vertices,last_vector, true)])) *)

let () = Random.self_init ()

(* TODO: check if the original line is admissible (no collision)
   if it is not the case, it will loop infinitely *)
let correct_bezier sign objects curve shift_vector =
  let rec aux c1 c2 =
    if c1 <= 0.2 || c2 <= 0.2
    then 0.2,0.2 (* not the good solution: improve later: do straight lines *)
    else
      begin
        let _, collisions = bezier_collide sign objects curve c1 c2 shift_vector 0 in
        let collisions = List.map fst collisions in
        let col_1, col_2 = List.partition (fun u -> u <= 0.5) collisions in
        match col_1, col_2 with
          | [], [] -> c1, c2
          | _, [] -> aux (c1/.2.) c2
          | [], _ -> aux c1 (c2/.2.)
          | _, _  -> aux (c1/.2.) (c2/.2.)
      end
   in
  let c1, c2 = aux 1. 1. in
  let curve = Bezier.mul_d1 curve c1 in
  let curve = Bezier.mul_d2 curve c2 in
  curve

let turn_radius = 0.2

let prepare_goto robot dst last_vector =
  let dst_orient = match last_vector with
    | None -> None
    | Some v -> Some (turn_radius, minus v) in
  match Krobot_path.find_with_real_center ~pos:robot.position
          ~orientation:robot.orientation
          ~turn_radius ?dst_orient ~dst ~beacon:robot.beacon robot.objects with
  | Some vertices ->
    Follow_path (vertices,last_vector, true)
  | None ->
    (* try with rotation *)
    let alternate = Krobot_path.find ~src:robot.position ~dst
        ?dst_orient
        ~beacon:robot.beacon robot.objects in
    match alternate with
    | Some ((h1::_) as vertices) ->
      let first_orientation = angle (vector robot.position h1) in
      ignore (Lwt_log.info_f "found another one with rotation %f" first_orientation);
      Node (Simple,
            [Set_orientation first_orientation;
             Follow_path (vertices,last_vector, true)])
    | Some [] -> Fail
    | None -> Fail

(*
    let alternate = Krobot_path.find_with_real_center ~pos:robot.position
        ~orientation:robot.orientation
        ~turn_radius ?dst_orient
        ~dst ~beacon:robot.beacon
        (ignore_some not_ignore_probal robot.objects) in
    match alternate with
    | Some vertices ->
      Follow_path (vertices,last_vector, true)
    | None -> Fail
*)

(* [exec robot actions] searches for the first action to execute in a
   tree of actions and returns a new tree of actions and an effect. *)
let rec exec robot actions =
  match actions with
    | [] ->
        ([], Wait)
    | Node (Loop t,[]) :: rest ->
        ignore (Lwt_log.info_f "Exit/reenter node loop");
        exec robot (Node (Loop t,[t]) :: rest)
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
    | Wait_for_bezier_moving (state, opt) :: rest ->
        if robot.bezier_moving = state then
          (ignore (Lwt_log.info_f "Wait_for_bezier_moving %b done" state);
           exec robot rest)
        else begin
          match opt with
            | None ->
              (actions, Wait)
            | Some date ->
              if Unix.gettimeofday () > date then begin
                ignore (Lwt_log.info_f "Wait_for_bezier_moving %b timeouted" state);
                exec robot rest
              end else
                (actions, Wait)
        end
    | Wait_for_motors_moving (state, opt) :: rest ->
        if robot.motors_moving = state then
          (ignore (Lwt_log.info_f "Wait_for_motors_moving %b done" state);
           exec robot rest)
        else begin
          match opt with
            | None ->
              (actions, Wait)
            | Some date ->
              if Unix.gettimeofday () > date then begin
                ignore (Lwt_log.info_f "Wait_for_motors_moving %b timeouted %f"
                          state (current_time ()));
                exec robot rest
              end else
                (actions, Wait)
        end
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

    | Wait_for_orientation(start, stop) :: rest ->
      let dpi = 2. *. pi in
      let pos = mod_float (dpi +. robot.orientation) dpi in
      let start = mod_float (dpi +. start) dpi in
      let stop = mod_float (dpi +. stop) dpi in
      let between =
        if start <= stop
        then pos >= start && pos <= stop
        else pos >= start || pos <= stop in
      if between
      then (ignore (Lwt_log.info_f "Wait_for_orientation %f <= %f <= %f done"
                      start robot.orientation stop);
            exec robot rest)
      else (actions, Wait)

    | Wait_for_lift_status (
        { moving_left; moving_right; homed_left; homed_right } as status,
                                                                  timeout) :: rest ->
      let aux b = function
        | None -> true
        | Some b' -> b = b' in
      let correct =
        aux robot.elevator_left_moving moving_left &&
        aux robot.elevator_right_moving moving_right &&
        aux robot.elevator_left_homed homed_left &&
        aux robot.elevator_right_homed homed_right in
      if correct
      then (ignore (Lwt_log.info_f "Wait_for_lift_status done");
            exec robot rest)
      else begin match timeout with
        | Timeout_before offset ->
          let endt = Unix.gettimeofday () +. offset in
          (Wait_for_lift_status (status, Timeout_started endt) :: rest, Wait)
        | Timeout_started endt ->
          if Unix.gettimeofday () > endt
          then (ignore (Lwt_log.info_f "Wait_for_lift_status timed out");
                exec robot rest)
          else (actions, Wait)
        | Timeout_none ->
          (actions, Wait)
      end

    | Wait_for_finished_ax12_sequence (name, timeout) :: rest ->
      if StringSet.mem name robot.received_finished_ax12_sequence
      then (ignore (Lwt_log.info_f "Wait_for_finished_ax12_sequence %s done" name);
            exec robot rest)
      else begin match timeout with
        | Timeout_before offset ->
          let endt = Unix.gettimeofday () +. offset in
          (Wait_for_finished_ax12_sequence (name, Timeout_started endt) :: rest, Wait)
        | Timeout_started endt ->
          if Unix.gettimeofday () > endt
          then (ignore (Lwt_log.info_f "Wait_for_lift_status timed out");
                exec robot rest)
          else (actions, Wait)
        | Timeout_none ->
          (actions, Wait)
      end

    | Wait_for t :: rest ->
        ignore (Lwt_log.info_f "Wait_for %f" t);
        exec robot (Wait_until (Unix.gettimeofday () +. t) :: rest)
    | Wait_until t :: rest ->
        if Unix.gettimeofday () >= t then
          (ignore (Lwt_log.info_f "Wait finish");
           exec robot rest)
        else
          (actions, Wait)
    | Set_curve No_curve :: rest ->
        reset robot;
        exec robot rest
    | Set_curve((Curve_bezier _ | Curve_rotation _) as curve) :: rest ->
        robot.curve <- curve;
        exec robot rest

    | Move_straight dist :: rest ->
      let move_vect =
        { vx = cos robot.orientation;
          vy = sin robot.orientation } *| dist
      in
      let dest = translate robot.position move_vect in
      exec robot ((Follow_path([dest],None,false))::rest)

    | Random_move (v1,v2) :: rest ->
      ignore (Lwt_log.info_f "Random_move");
      let min_x = min v1.x v2.x in
      let max_x = max v1.x v2.x in
      let min_y = min v1.y v2.y in
      let max_y = max v1.y v2.y in
      let x = (Random.float (max_x -. min_x)) +. min_x in
      let y = (Random.float (max_y -. min_y)) +. min_y in
      let action = prepare_goto robot {x;y} None in
      exec robot (action::rest)

    | Simple_goto (dst,last_vector) :: rest ->
      ignore (Lwt_log.info_f "Simple_goto");
      let action = prepare_goto robot dst last_vector in
      exec robot (action::rest)

    | Goto ({x;y} as dst,last_vector) :: rest -> begin
      ignore (Lwt_log.info_f "Goto (%.3f,%.3f)" x y);
      let action = prepare_goto robot dst last_vector in
      exec robot
        ((Node (Retry (1,Node (Simple, [Stop; Try_something dst; Wait_for 0.1;
                                        Goto (dst,last_vector)])),
            [action]))::rest)
      end


      (*   (\* Try to find a path to the destination. *\) *)
      (*   match Krobot_path.find ~src:robot.position ~dst:v *)
      (*       ~beacon:robot.beacon ~objects:robot.objects with *)
      (*     | Some vertices -> *)
      (*         exec robot ((goto_node v vertices last_vector) :: rest) *)

      (*     | None -> *)
      (*       match path_ignoring not_ignore_probal robot v last_vector with *)
      (*       | Some vertices -> *)
      (*         exec robot ((goto_node v vertices last_vector) :: rest) *)
      (*       | None -> *)
      (*         ([Stop; Try_something v; Wait_for 0.1; Goto (v,last_vector)] @ rest, *)
      (*          Wait) *)
      (* end *)

    | Can c ::rest ->
        ignore (Lwt_log.info_f "Can");
        (rest, Send_frame[c])
    | Set_limits(vmax,omega_max,atan_max,arad_max) :: rest ->
        ignore (Lwt_log.info_f "Set_limit");
        (rest, Send[Motor_bezier_limits(vmax,omega_max,atan_max,arad_max)])
    | Set_led(led,value) :: rest ->
        ignore (Lwt_log.info_f "Set_led");
        let led = match led with
          | `Red -> 7
          | `Green -> 6
          | `Yellow -> 5 in
        (rest, Send[Switch_request(led,value)])
    | Follow_path (vertices,last_vector, correct_curve ) :: rest -> begin
        ignore (Lwt_log.info_f "Follow_path");
        (* Compute bezier curves. *)
        let vector = { vx = cos robot.orientation; vy = sin robot.orientation } in
(*
        let curves = List.rev (Bezier.fold_vertices ?last:last_vector (fun sign p q r s acc -> (sign, p, q, r, s) :: acc) vector (robot.position :: vertices) []) in
*)
        let objects = build_objects robot in

        let curves = List.rev (Bezier.fold_curves ?last:last_vector
          (fun sign curve acc ->
            let shift_vector = null in
            let c =
              if correct_curve
              then correct_bezier (sign>0.) objects curve shift_vector
              else curve
            in
            (sign,c) :: acc) vector
          (robot.position :: vertices) []) in

        set_path robot (Some (List.map snd curves));

        let curves =
          List.map (fun (sign,t) ->
            let p,q,r,s = Bezier.pqrs t in
            (sign,p,q,r,s))
            curves in

        let rec check curves =
          match curves with
            | (sign, p, q, r, s) as curve :: rest ->
              (* Check if the curve can be followed. *)
              let curve', collisions =
                bezier_collide
                  (sign>0.)
                  (build_objects robot)
                  (Bezier.of_vertices p q r s)
                  1. 1.
                  null
                  0
              in
              if collisions <> [] then begin
                ignore (Lwt_log.info_f "one of the bezier curve is colliding");
                ignore (Krobot_bus.send robot.bus (Unix.gettimeofday (), Collisions (Col_bezier (curve', collisions))));
                ([],
                 [(* Try_something s; *) Fail])
              end else
                let curves, post = check rest in
                (curve :: curves, post)
            | [] ->
              ([], [])
        in


        let curves, post = check curves in

(*
        (* Set the path. *)
        set_path robot (Some (List.map (fun (sign, p, q, r, s) -> Bezier.of_vertices p q r s) curves));
*)

        (* Compute orders. *)
        let rec loop = function
          | [] ->
              Wait_for_bezier_moving (false, None) :: post
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
                (let dir = sign > 0. in
                 Set_curve(Curve_bezier(dir,Bezier.of_vertices p q r s)));
                (* Wait for the end of the new curve. *)
                Wait_for_bezier_moving (false, None);
                (* Remove the current curve. *)
                Set_curve No_curve;
              ] @ post
          | (sign, p, q, r, s) :: rest ->
              ignore (Lwt_log.info_f "add middle %f %f" p.x p.y);
              Wait_for_odometry(`Ge, 128)
              :: Bezier(sign, p, q, r, s, 0.5)
              :: Wait_for_odometry(`Lt, 128)
              :: Set_curve(Curve_bezier(sign > 0., Bezier.of_vertices p q r s))
              :: loop rest
        in
        match curves with
          | [] ->
              exec robot (Node (Simple, post) :: rest)
          | [(sign, p, q, r, s)] ->
            exec robot (Node (Simple,[
              Set_curve(Curve_bezier(sign>0.,Bezier.of_vertices p q r s));
              Bezier(sign, p, q, r, s, 0.01);
              Wait_for_odometry(`Le, 128);
              Wait_for_odometry(`Ge, 128);
              Wait_for_bezier_moving (false, None);
              Set_curve No_curve;
            ] @ post) :: rest)
          | (sign, p, q, r, s) :: curves ->
            exec robot (Node(Simple,
                             Set_curve(Curve_bezier(sign>0.,Bezier.of_vertices p q r s))
                             :: Bezier(sign, p, q, r, s, 0.5)
                             :: Wait_for_odometry(`Le, 128)
                             :: loop curves) :: rest)
      end
    | Bezier(sign, p, q, r, s, v_end) :: rest ->
        ignore (Lwt_log.info "Bezier");
        (* Compute parameters. *)
        let d1 = sign *. distance p q and d2 = distance r s in
        let d1,d2 =
          (if abs_float d1 <= 0.01
           then if d1 < 0. then -.0.1 else 0.1
           else d1),
          (if abs_float d2 <= 0.01
           then if d2 < 0. then -.0.1 else 0.1
           else d2)
        in
        let v = vector r s in
        let theta_end = atan2 v.vy v.vx in
        (rest, Send[Switch_request(5,false);
                    Motor_bezier(s.x, s.y, d1, d2, theta_end, v_end)])
    | Stop :: rest ->
        ignore (Lwt_log.info_f "Stop");
        reset robot;
        (rest, Send[Switch_request(5,true); Motor_stop(1.0, 0.0)])
    | Reset_odometry which :: rest ->
        ignore (Lwt_log.info_f "Reset_odometry");
        (rest,
         Send
           (match which, robot.team with
              | `Red, _ | `Auto, `Red ->
                let { Krobot_geom.x; y }, angle = Krobot_config.red_initial_position in
                [Krobot_message.Set_odometry( x, y, angle );
                 Set_odometry_indep( x, y, angle ); ]
              | `Blue, _ | `Auto, `Blue ->
                let { Krobot_geom.x; y }, angle = Krobot_config.blue_initial_position in
                [Krobot_message.Set_odometry( x, y, angle);
                 Set_odometry_indep( x, y, angle )]))

    | Set_odometry( x, y, orientation )::rest ->
      ignore (Lwt_log.info_f "Set_odometry");
      let aux v = function
        | None -> v
        | Some w -> w in
      let x = aux robot.position.x x in
      let y = aux robot.position.y y in
      let orientation = aux robot.orientation orientation in
      (rest,
       Send [Krobot_message.Set_odometry( x, y, orientation);
             Set_odometry_indep( x, y, orientation )])

    | Think :: rest ->
        exec robot rest
    | Fail :: rest ->
        ignore (Lwt_log.info_f "failing");
        (rest, Abort)
    | Turn (angle, speed, acceleration) :: rest ->
        ignore (Lwt_log.info "Turn");
        (rest, Send[Switch_request(5,false);
                    Motor_turn(angle, speed, acceleration)])
    | Set_orientation orientation :: rest ->
      ignore (Lwt_log.info_f "set orientation %f -> %f" robot.orientation orientation);
      if abs_float ((positive_angle orientation)
                    -. (positive_angle robot.orientation)) < 0.05
      then exec robot rest
      else
        let objects = build_objects robot in
        begin match find_rotation robot.position robot.orientation orientation objects with
          | None ->
            ignore (Lwt_log.info_f "can't turn");
            (rest,Abort)
          | Some direction ->
            let delta = diff_angle direction ~start:robot.orientation ~stop:orientation in
            let start, stop =
              let abs_delta = abs_float delta in
              orientation -. (abs_delta /. 4.), orientation +. (abs_delta /. 4.) in
            ignore (Lwt_log.info_f "can turn %f %f" delta (current_time ()));
            (Stop ::
             Wait_for 0.05 ::
             Wait_for_motors_moving(false,None) ::
             Set_curve (Curve_rotation (direction,orientation))::
             Turn (delta,1.5,1.5) ::
             Wait_for_orientation(start, stop) ::
             Wait_for_motors_moving(false,None)::rest,
             Wait)
        end
    | Calibrate ( approach_position, approach_orientation, distance,
                  supposed_x, supposed_y, supposed_orientation )::rest ->
      Node (Simple,
            ([ Goto (approach_position,
                     Some { vx = cos approach_orientation;
                            vy = cos approach_orientation });
               Set_orientation approach_orientation;
               Can (Krobot_message.encode (Torque_limit(4,100)));
               Can (Krobot_message.encode (Torque_limit(8,100)));
               Can (Krobot_message.encode (Motor_move(distance,0.2,0.5)));
               Wait_for_motors_moving(true,Some(Unix.gettimeofday () +. 2.));
               Wait_for_motors_moving(false,None);
               Set_odometry(supposed_x, supposed_y, supposed_orientation);
               Can (Krobot_message.encode (Torque_limit(4,3600)));
               Can (Krobot_message.encode (Torque_limit(8,3600)));]))
        ::rest,
      Wait

    | Try_something dst :: rest ->
        ignore (Lwt_log.info_f "trying something to reach (%f, %f)" dst.x dst.y);
        let objects = build_objects robot in
        let direction = vector_of_polar ~norm:1. ~angle:robot.orientation in
        if Random.bool () then begin
          let d = if Random.bool () then 0.02 else -0.02 in
          if Krobot_collision.possible objects (translate robot.position (direction *| d)) robot.orientation then
            let _ = Lwt_log.info_f "try move(%f)" d in
            (Wait_for_motors_moving (true, Some (Unix.gettimeofday () +. 1.0)) :: Wait_for_motors_moving (false, None) :: rest,
             Send [Motor_move (d, 0.5, 1.)])
          else
            ((Wait_for 0.1) :: rest, Wait)
        end else begin
          let a = if Random.bool () then -. pi /. 8. else pi /. 8. in
          if Krobot_collision.possible objects robot.position (robot.orientation +. a) then
            let _ = Lwt_log.info_f "try turn_l" in
            (Wait_for_motors_moving (true, Some (Unix.gettimeofday () +. 1.0)) :: Wait_for_motors_moving (false, None) :: rest,
             Send [Motor_turn (a, 0.5, 1.)])
          else
            ((Wait_for 0.1) :: rest, Wait)
        end

    | Elevator_homing :: rest ->
      let act =
        [ Can (Krobot_message.encode (Homing_command (pi,pi)));
          Wait_for_lift_status ({ moving_left = None;
                                  moving_right = None;
                                  homed_left = Some false;
                                  homed_right = Some false },
                                Timeout_before 0.5);
          Wait_for_lift_status ({ moving_left = None;
                                  moving_right = None;
                                  homed_left = Some true;
                                  homed_right = Some true },
                                Timeout_none);
          Can (Krobot_message.encode (Elevator_command (0.01,0.01)));
          Wait_for_lift_status ({ moving_left = Some true;
                                  moving_right = Some true;
                                  homed_left = None;
                                  homed_right = None },
                                Timeout_before 0.5);
          Wait_for_lift_status ({ moving_left = Some false;
                                  moving_right = Some false;
                                  homed_left = None;
                                  homed_right = None },
                                Timeout_none);
          Can (Krobot_message.encode (Homing_command (pi/.4.,pi/.4.)));
          Wait_for_lift_status ({ moving_left = None;
                                  moving_right = None;
                                  homed_left = Some false;
                                  homed_right = Some false },
                                Timeout_before 0.5);
          Wait_for_lift_status ({ moving_left = None;
                                  moving_right = None;
                                  homed_left = Some true;
                                  homed_right = Some true },
                                Timeout_none);
          Can (Krobot_message.encode (Elevator_command (0.03,0.03)));
          Wait_for_lift_status ({ moving_left = Some true;
                                  moving_right = Some true;
                                  homed_left = None;
                                  homed_right = None },
                                Timeout_before 0.5);
          Wait_for_lift_status ({ moving_left = Some false;
                                  moving_right = Some false;
                                  homed_left = None;
                                  homed_right = None },
                                Timeout_none) ] in
      exec robot (act @ rest)

    | Ax12_sequence (name, sequence) :: rest ->
      robot.received_finished_ax12_sequence <-
        StringSet.remove name robot.received_finished_ax12_sequence;
      (rest, Send_bus [Run_ax12_sequence (name, sequence)])

    | Start_timer(delay,action) :: rest ->
      ignore (Lwt_log.info_f "Start_timer(%f)" delay);
      let current_time = Unix.gettimeofday () in
      robot.init_time <- Some (current_time);
      robot.delayed_action <- Some (current_time +. delay, action);
      (rest, Wait)

    | Stop_timer :: rest ->
      ignore (Lwt_log.info_f "Stop_time");
      robot.init_time <- None;
      robot.delayed_action <- None;
      (rest, Wait)

    | End :: rest ->
      ([], Send_bus [Strategy_finished])

    | Start_match :: rest ->
      (rest, Send_bus [Match_start])

(* +-----------------------------------------------------------------+
   | Main loop                                                       |
   +-----------------------------------------------------------------+ *)

let run robot =
  while_lwt true do
    let timestamp = Unix.gettimeofday () in

    (* check if the current time is higher than the delayed action *)
    begin
      match robot.delayed_action with
        | Some (timeout,l) ->
            if timeout <= timestamp
            then begin
              robot.delayed_action <- None;
              robot.strategy <- l
            end
        | None ->
            ()
    end;

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
    if robot.bezier_moving then begin
      match robot.curve with
        | No_curve ->
            ()
        | Curve_bezier (dir,curve) ->
          let problem =
              (* Check that the robot is not too far from the ghost.
                 if it is then stop brutaly:
                 TODO do something interesting after the stop: retry what we were doing *)
            if distance robot.ghost_position robot.position > 0.05 then begin
              ignore (Lwt_log.info_f "Robot too far from the ghost");
              true
            end else
              let shift_vector = vector robot.ghost_position robot.position in
                (* Check that there is no colision between the current
                   position and the end of the current curve. *)
              let curve, collisions = bezier_collide dir (build_objects robot) curve 1. 1. shift_vector robot.curve_parameter in
              match collisions with
                | [] ->
                  false
                | _ ->
                  ignore (Lwt_log.info_f "Obstacle in the trajectory");
                  ignore (Krobot_bus.send robot.bus (Unix.gettimeofday (), Collisions (Col_bezier (curve, collisions))));
                  true
          in
          if problem then
            replace robot
        | Curve_rotation (direction,dest_orientation) ->
          let collisions = rotation_collide robot.position
              robot.orientation dest_orientation direction (build_objects robot) in
          match collisions with
          | [] -> ()
          | _ ->
            ignore (Lwt_log.info_f "Obstacle in the rotation");
            ignore (Krobot_bus.send robot.bus (Unix.gettimeofday (),
                                               Collisions (Col_rotation collisions)));
            replace robot
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
      | Send_frame msgs ->
        Lwt_list.iter_s
          (fun m -> Krobot_bus.send robot.bus (timestamp, (CAN (Info,m))))
          msgs
      | Send_bus msgs ->
        Lwt_list.iter_s
          (fun m -> Krobot_bus.send robot.bus (timestamp, m))
          msgs
      | Abort ->
          replace robot;
          return ()
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

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

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
    coins = [];
    objects = [];
    bezier_moving = false;
    motors_moving = false;
    path = None;
    curve = No_curve;
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
    replace = false;
    init_time = None;
    delayed_action = None;
    elevator_left_moving = false;
    elevator_right_moving = false;
    elevator_left_homed = false;
    elevator_right_homed = false;
    received_finished_ax12_sequence = StringSet.empty;
  } in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message robot) (Krobot_bus.recv bus));

  (* Ask for parameters. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Send) in

  ignore (Lwt_unix.sleep 2.);

  ignore(blink bus robot false);

  (* Run forever. *)
  run robot
