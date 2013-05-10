(*
 * krobot_homologation.ml
 * ------------
 * Copyright : (c) 2012, Pierre Chambart <chambart@crans.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* The simple ai for homologation. *)

open Lwt
open Lwt_react
open Krobot_message
open Krobot_bus
open Krobot_action
open Krobot_geom

type reset = [ `Auto | `Blue | `Red ]

let secure_dist = 0.25

let gift_distance = 0.21

let gift_width = 0.15

let gifts_positions =
  [ { x = 0.60 ; y = 0. };
    { x = 1.2 ; y = 0. };
    { x = 1.8 ; y = 0. };
    { x = 2.4 ; y = 0. }; ]

let team_gift_position team p =
  match team with
  | `Red -> { x = p.x +. (gift_width /. 2. +. 0.01); y = p.y }
  | `Blue -> { x= p.x -. (gift_width /. 2. +. 0.01); y = p.y }

let gift_destination team p =
  let p = team_gift_position team p in
  { x = p.x; y = p.y +. gift_distance }

type status = {
  bus : Krobot_bus.t;
  (* The bus used to communicate with the robot. *)
  mutable team : [ `Red | `Blue ];
  (* The state of the team selector. *)
}

let vmax = 0.3
let omega_max = 3.14 /. 2.
let accel_tan_max = 1.0
let accel_rad_max = 1.0

let ax12_2_base_position = 519
let ax12_2_high_position = 210

let ax12_1_base_position = 518
let ax12_1_high_position = 823

let gonfle_baloon =
  [Can (Krobot_message.encode (Motor_command (2,3600)));
   Wait_for 10.;
   Can (Krobot_message.encode (Motor_command (2,0)));
   Wait_for 0.1]

let start team =
  [ Stop_timer;
    Wait_for 0.1;
    Reset_odometry (team:>reset);
    Can (Krobot_message.encode (Drive_activation true));
    Wait_for_jack true;
    Can (Krobot_message.encode (Ax12_Set_Torque_Enable (2,true)));
    Can (Krobot_message.encode (Ax12_Goto (2, ax12_2_base_position, 100)));
    Can (Krobot_message.encode (Ax12_Goto (1, ax12_1_base_position, 100)));
    Wait_for 1.;
    Wait_for_jack false;
    Start_timer (90.,[Stop] @ gonfle_baloon @ [End]);
    Start_match;
    Set_led(`Red,false);
    Set_led(`Green,false);
    Reset_odometry (team:>reset);
    Wait_for_odometry_reset (team:>reset);
    Set_limits (vmax,omega_max,accel_tan_max,accel_rad_max) ]

let end_ =
  [ End; ]

let hit_ax12_id id =
  let base, high =
    if id = 1
    then ax12_1_base_position, ax12_1_high_position
    else ax12_2_base_position, ax12_2_high_position
  in
  [Can (Krobot_message.encode (Ax12_Set_Torque_Enable (2,true)));
   Wait_for 0.1;
   Can (Krobot_message.encode (Ax12_Goto (id, high, 100)));
   Wait_for 1.;
   Can (Krobot_message.encode (Ax12_Goto (id, base, 100)));
   Wait_for 1.]

let hit_ax12_dir dir =
  (* TODO: test ! *)
  let ax12id = if dir.vx < 0. then 1 else 2 in
  hit_ax12_id ax12id

let retry_follow_node dest dir =
  let rec follow_node =
    Node(Some (Node(None,[Wait_for 0.2; follow_node])),
      [Follow_path ([dest], Some dir, false)]) in
  follow_node

let direction pos = function
  | `Blue ->
    if pos.x >= 2.2
    then { vx = -.1.; vy = 0. }
    else { vx = 1.; vy = 0. }
  | `Red ->
    if pos.x <= 0.8
    then { vx = 1.; vy = 0. }
    else { vx = -.1.; vy = 0. }

let approach_lower_border pos dir =
  let shift_len = 0.1 in
  let shift =
    if dir.vx >= 0.
    then shift_len
    else -. shift_len in
  let shifted_position =
    { x = pos.x +. shift; y = Krobot_config.robot_radius +. 0.01 } in
  let goto_path =
    Node(None,
      [ Goto (shifted_position, Some (minus dir));
        retry_follow_node pos dir; ]) in
  let simple_path = [Follow_path ([pos], Some (minus dir), false)] in
  [Node (Some goto_path, simple_path)]

(* let approach_lower_border pos dir = *)
(*   let shift_len = 0.1 in *)
(*   let shift = *)
(*     if dir.vx >= 0. *)
(*     then shift_len *)
(*     else -. shift_len in *)
(*   let shifted_position = *)
(*     { x = pos.x +. shift; y = Krobot_config.robot_radius +. 0.01 } in *)
(*   [ Goto (shifted_position, Some (minus dir)); *)
(*     (\* Follow_path ([pos], Some dir, false); *\) *)
(*     retry_follow_node pos dir; ] *)

let goto_gift team gift =
  let destination = gift_destination team gift in
  approach_lower_border destination (direction destination team)

let do_gift team gift =
  let destination = gift_destination team gift in
  let dir = direction destination team in
  let goto = approach_lower_border destination dir in
  let hit = hit_ax12_dir dir in
  goto @ hit

let gifts_actions team =
  let gifts = match team with
    | `Red -> List.rev gifts_positions
    | `Blue -> gifts_positions
  in
  List.flatten (List.map (do_gift team) gifts)

let strategy team =
  start team @ gifts_actions team @ end_

(* let strat_base team = *)
(*   let n_gift = match team with *)
(*     | `Red -> 2 *)
(*     | `Blue -> 1 *)
(*   in *)
(*   let gift = List.nth gifts_positions n_gift in *)
(*   let destination = gift_destination team gift in *)
(*   let dst = { destination with y = destination.y +. secure_dist } in *)
(*   [ *)
(*     Stop_timer; *)
(*     Wait_for 0.1; *)
(*     Reset_odometry `Auto; *)
(*     Can (Krobot_message.encode (Drive_activation true)); *)
(*     Wait_for_jack true; *)
(*     Can (Krobot_message.encode (Ax12_Set_Torque_Enable (2,true))); *)
(*     Can (Krobot_message.encode (Ax12_Goto (2, ax12_2_base_position, 100))); *)
(*     Can (Krobot_message.encode (Ax12_Goto (1, ax12_1_base_position, 100))); *)
(*     Wait_for 1.; *)
(*     Wait_for_jack false; *)
(*     Start_timer (90.,[Stop] @ gonfle_baloon @ [End]); *)
(*     Start_match; *)
(*     Set_led(`Red,false); *)
(*     Set_led(`Green,false); *)
(*     Reset_odometry `Auto; *)
(*     Wait_for_odometry_reset `Auto; *)
(*     Set_limits (vmax,omega_max,accel_tan_max,accel_rad_max); *)
(*     Goto (dst, Some { vx = 0.; vy = -.1. }); *)
(*     Stop; *)
(*     Follow_path ([destination], Some { vx = 0.; vy = 1. }, false); *)
(*     Stop; *)
(*     Wait_for 0.1; *)
(*     Can (Krobot_message.encode (Motor_turn(-.(pi/.2.),0.5,1.))); *)
(*     (\* Wait_for_motors_moving (true,None); *\) *)
(*     Wait_for 0.1; *)
(*     (\* Wait_for_motors_moving (false,None); *\) *)
(*     Wait_for 0.1; *)
(*     Wait_for 2.; *)
(*     Can (Krobot_message.encode (Ax12_Set_Torque_Enable (2,true))); *)
(*     Wait_for 0.1; *)
(*     Can (Krobot_message.encode (Ax12_Goto (2, ax12_2_high_position, 100))); *)
(*     Wait_for 0.1; *)
(*     Can (Krobot_message.encode (Ax12_Goto (2, ax12_2_high_position, 100))); *)
(*     Wait_for 0.1; *)
(*     Can (Krobot_message.encode (Ax12_Goto (2, ax12_2_high_position, 100))); *)
(*     Wait_for 3.; *)
(*     Can (Krobot_message.encode (Ax12_Goto (2, ax12_2_base_position, 100))); *)
(*     Wait_for 0.1; *)
(*     Can (Krobot_message.encode (Ax12_Goto (2, ax12_2_base_position, 100))); *)
(*     Wait_for 0.1; *)
(*     Can (Krobot_message.encode (Ax12_Goto (2, ax12_2_base_position, 100))); *)
(*     Wait_for 0.1; *)
(*     Wait_for 2.; *)
(*     Can (Krobot_message.encode (Ax12_Set_Torque_Enable (1,true))); *)
(*     End; *)
(*   ] *)


(* let strat team = *)
(*   [Reset_odometry (team:>reset); *)
(*    Wait_for_odometry_reset (team:>reset)] @ *)
(*     gifts_actions team *)



(* let { y = init_y },_ = Krobot_config.red_initial_position *)


(* let linear a0 an n = *)
(*   let dx = (an -. a0) /. (float (n-1)) in *)
(*   let p n = a0 +. (float n) *. dx in *)
(*   Array.init n p *)

(* let stupid_path init_x = *)
(*   let l = linear init_x 1.5 10 in *)
(*   Array.to_list (Array.map (fun x -> {x; y = init_y }) l) *)

(* let stupid_path = function *)
(*   | `Red -> [{ x = 2.3; y = init_y }] *)
(*   | `Blue -> [{ x = 0.7; y = init_y }] *)

(* let godx = function *)
(*   | `Red -> 1. *)
(*   | `Blue -> -.1. *)

(* let strat_base status = *)
(*   let ({x = init_x},_) = match status.team with *)
(*     | `Red -> Krobot_config.red_initial_position *)
(*     | `Blue -> Krobot_config.blue_initial_position in *)
(*   (\* let path = stupid_path init_x in *\) *)
(*   let path = stupid_path status.team in *)
(*   Printf.printf "update strat\n%!"; *)
(*   let rec node = *)
(*     let follow = *)
(*       Follow_path (path, Some { vx = godx status.team; vy = 0. }, false) *)
(*     in *)
(*     (Node (Some node, [follow])) in *)
(*   [ *)
(*     Stop_timer; *)
(*     Wait_for 0.1; *)
(*     Reset_odometry `Auto; *)
(*     Can (Krobot_message.encode (Drive_activation true)); *)
(*     Wait_for_jack true; *)
(*     Can (Krobot_message.encode (Ax12_Set_Torque_Enable (2,true))); *)
(*     Can (Krobot_message.encode (Ax12_Goto (2, ax12_2_base_position, 100))); *)
(*     Can (Krobot_message.encode (Ax12_Goto (1, ax12_1_base_position, 100))); *)
(*     Wait_for 1.; *)
(*     Wait_for_jack false; *)
(*     Start_timer (90.,[Stop] @ gonfle_baloon @ [End]); *)
(*     Set_led(`Red,false); *)
(*     Set_led(`Green,false); *)
(*     Reset_odometry `Auto; *)
(*     Wait_for_odometry_reset `Auto; *)
(*     Set_limits (vmax,omega_max,accel_tan_max,accel_rad_max); *)
(*     Stop; *)
(*     node; *)
(*     Stop; *)
(*     End; *)
(*   ] *)

let launch bus team =
  Krobot_bus.send bus
    (Unix.gettimeofday (),
     Strategy_set (strategy team))

let update_team_led status =
  let m1,m2 =
    if status.team = `Red then
      Switch_request(7,false), Switch_request(6,true)
    else
      Switch_request(7,true), Switch_request(6,false)
  in
  lwt () = Krobot_message.send status.bus (Unix.gettimeofday (),m1) in
  Krobot_message.send status.bus (Unix.gettimeofday (), m2)

lwt bus = Krobot_bus.get ()

let handle_message status (timestamp, message) =
  match message with
    | CAN(_, frame) -> begin
        match decode frame with
          | Switch1_status(jack, team, emergency, _, _, _, _, _) ->
            let team = if team then `Red else `Blue in
            if team <> status.team
            then
              begin
                status.team <- team;
                ignore (update_team_led status);
                ignore (launch bus status.team)
              end
          | _ ->
              ()
      end

    (* | Strategy_finished -> *)
    (*   ignore (loop bus) *)

    | Kill "homologation" ->
        exit 0

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
Usage: krobot-homologation [options]
options are:"

lwt () =
  Arg.parse options ignore usage;

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  (* Kill any running homologation. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "homologation") in

  let status = {
    bus;
    team = `Red;
  } in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message status) (Krobot_bus.recv bus));

  (* Wait forever. *)
  fst (wait ())
