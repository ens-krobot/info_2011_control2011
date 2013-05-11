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

let gift_width = 0.15

let init_y = 0.244

let ax12_2_base_position = 519
let ax12_2_high_position = 210

let ax12_2_mid_position = 400
let ax12_1_mid_position = 600

let ax12_1_base_position = 518
let ax12_1_high_position = 823

let init_blue_pos =
  Set_odometry (
    Some (Krobot_config.wheels_position +. 0.001),
    Some 24.4,
    Some 0.)

let gifts_positions =
  [ { x = 0.60 ; y = 0. };
    { x = 1.2 ; y = 0. };
    { x = 1.8 ; y = 0. };
    { x = 2.4 ; y = 0. }; ]

let gift_shift = -. (gift_width /. 2.) -. 0.09
let gift_position1 = { x = 0.60 ; y = 0. }
let gift_position2 = { x = 1.2 ; y = 0. }
let gift_position3 = { x = 1.8 ; y = 0. }
let gift_position4 = { x = 2.4 ; y = 0. }

let gift_go pos = { x = pos.x +. gift_shift ; y = init_y }
let gift_go_red pos = { x = pos.x -. gift_shift ; y = init_y }

let gitf_go1 = { x = gift_position1.x +. gift_shift ; y = init_y }
let gift_dir1_blue = { vx = -.1.; vy = 0. }
let gift_dir1_red = { vx = 1.; vy = 0. }

let retry_n n t = Node(Retry(n,Node(Simple,[Stop;Wait_for 0.2;t])),[t])

let node t = Node(Simple,t)

let back_and_retry t =
  Node(Retry(3,
      node ((Node(Next,[Stop;Move_back 0.1])) :: t)),
    t)

let retry_follow n dst dir =
  let follow =
    Follow_path ([dst], Some dir, false) in
  let tmp =
    Node(Retry(n,node [Stop;Wait_for 0.2;follow]),[follow]) in
  Node(Next,[back_and_retry [tmp]])

let act1_blue = retry_follow 15 (gift_go gift_position1) gift_dir1_blue
let act2_blue = retry_follow 15 (gift_go gift_position2) gift_dir1_blue
let act3_blue = retry_follow 15 (gift_go gift_position3) gift_dir1_blue
let act4_blue = retry_follow 15 (gift_go gift_position4) gift_dir1_blue

let act1_red = retry_follow 15 (gift_go_red gift_position4) gift_dir1_red
let act2_red = retry_follow 15 (gift_go_red gift_position3) gift_dir1_red
let act3_red = retry_follow 15 (gift_go_red gift_position2) gift_dir1_red
let act4_red = retry_follow 15 (gift_go_red gift_position1) gift_dir1_red


let hit_ax12_id id =
  let base, high =
    if id = 1
    then ax12_1_base_position, ax12_1_high_position
    else ax12_2_base_position, ax12_2_high_position
  in
  Node (Simple,
    [Can (Krobot_message.encode (Ax12_Goto (id, high, 0)));
     Wait_for 0.3;
     Can (Krobot_message.encode (Ax12_Goto (id, base, 0)));
     Wait_for 0.3])

let ramene p1 p2 dir =
  [ Goto (p1, None);
    Move_back 0.4;
    Goto (p2, Some dir); ]

let pos1 = { x = 0.4 ; y = 0.7 }
let pos2 = { x = 2.6 ; y = 0.7 }
let pos3 = { x = 2.6 ; y = 1.5 }
let pos4 = { x = 2.6 ; y = 1.0 }

let haut = { vx = 0. ; vy = 1. }
let bas = { vx = 0. ; vy = -.1. }

let inner_loop_blue =
  let l =
    ramene pos1 pos2 haut
    @ ramene pos1 pos3 bas
    @ ramene pos1 pos4 haut in
  Node(Loop(node l),l)

let sym_pos p = { p with x = 3. -. p.x }

let pos1_red = sym_pos { x = 0.4 ; y = 0.7 }
let pos2_red = sym_pos { x = 2.6 ; y = 0.7 }
let pos3_red = sym_pos { x = 2.6 ; y = 1.5 }
let pos4_red = sym_pos { x = 2.6 ; y = 1.0 }

let inner_loop_red =
  let l =
    ramene pos1_red pos2_red haut
    @ ramene pos1_red pos3_red bas
    @ ramene pos1_red pos4_red haut in
  Node(Loop(node l),l)


let run_strategy_blue =
  [ act1_blue;
    Stop;
    hit_ax12_id 2;
    act2_blue;
    Stop;
    hit_ax12_id 2;
    act3_blue;
    Stop;
    hit_ax12_id 2;
    act4_blue;
    Stop;
    hit_ax12_id 2;
    inner_loop_blue;
 ]

let run_strategy_red =
  [ act1_red;
    Stop;
    hit_ax12_id 1;
    act2_red;
    Stop;
    hit_ax12_id 1;
    act3_red;
    Stop;
    hit_ax12_id 1;
    act4_red;
    Stop;
    hit_ax12_id 1;
    inner_loop_red;
 ]

let team_gift_position team p =
  match team with
  | `Red -> { x = p.x +. (gift_width /. 2. +. 0.09); y = p.y }
  | `Blue -> { x= p.x -. (gift_width /. 2. +. 0.09); y = p.y }

(* let gift_destination team p = *)
(*   let p = team_gift_position team p in *)
(*   { x = p.x; y = p.y +. gift_distance } *)

type status = {
  bus : Krobot_bus.t;
  (* The bus used to communicate with the robot. *)
  mutable team : [ `Red | `Blue ];
  (* The state of the team selector. *)
}

let vmax = 0.5
let omega_max = 3.14 /. 2.
let accel_tan_max = 1.0
let accel_rad_max = 1.0

let gonfle_baloon =
  [Can (Krobot_message.encode (Motor_command (2,3600)));
   Wait_for 10.;
   Can (Krobot_message.encode (Motor_command (2,0)));
   Wait_for 0.1]

let start team =
  (* assert(team = `Blue); *)
  [ Stop_timer;
    Wait_for 0.1;
    (* init_blue_pos; *)
    Reset_odometry (team:>reset);
    Can (Krobot_message.encode (Drive_activation true));
    Can (Krobot_message.encode (Ax12_Set_Torque_Enable (2,true)));
    Wait_for 0.1;
    Can (Krobot_message.encode (Ax12_Set_Torque_Enable (1,true)));
    Wait_for 0.1;
    Can (Krobot_message.encode (Ax12_Goto (2, ax12_2_mid_position, 0)));
    Wait_for 0.1;
    Can (Krobot_message.encode (Ax12_Goto (1, ax12_1_mid_position, 0)));
    Wait_for 0.1;
    Can (Krobot_message.encode (Ax12_Goto (2, ax12_2_base_position, 0)));
    Wait_for 0.1;
    Can (Krobot_message.encode (Ax12_Goto (1, ax12_1_base_position, 0)));
    Wait_for_jack true;
    Wait_for 0.4;
    Wait_for_jack false;
    Start_timer (90.,[Stop] @ gonfle_baloon @ [End]);
    Start_match;
    Set_led(`Red,false);
    Set_led(`Green,false);
    Reset_odometry (team:>reset);
    Set_led(`Red,true);
    (* init_blue_pos; *)
    (* Wait_for 0.05; *)
    Wait_for_odometry_reset (team:>reset);
    Set_led(`Red,true);
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
    Node(Retry(1, Node(Simple,[Wait_for 0.2; follow_node])),
      [Follow_path ([dest], Some dir, false)]) in
  follow_node

let direction pos = function
  | `Blue ->
    (* if pos.x >= 2.2 *)
    (* then { vx = -.1.; vy = 0. } *)
    (* else *)
    { vx = 1.; vy = 0. }
  | `Red ->
    (* if pos.x <= 0.8 *)
    (* then { vx = 1.; vy = 0. } *)
    (* else *)
    { vx = -.1.; vy = 0. }

let retry_n n t = Node(Retry(n,Node(Simple,[Stop;Wait_for 0.05;t])),[t])

let go_first_gift pos dir =
  let shift_len = 0.2 in
  let shift =
    if dir.vx >= 0.
    then shift_len
    else -. shift_len in
  let back_shifted_position =
    { x = pos.x +. shift; y = Krobot_config.robot_radius +. 0.01 } in
  Node(Simple,
    [ retry_n 2 (Simple_goto (back_shifted_position, Some (minus dir)));
      retry_n 2 (Follow_path ([pos], Some dir, false)); ])

let approach_lower_border retries pos dir =
  let shift_len = 0.2 in
  let shift =
    if dir.vx >= 0.
    then shift_len
    else -. shift_len in
  let shifted_position =
    { x = pos.x -. shift; y = Krobot_config.robot_radius +. 0.01 } in
  let back_shifted_position =
    { x = pos.x +. shift; y = Krobot_config.robot_radius +. 0.01 } in
  let goto_normal =
    Node(Simple,
      [ retry_n retries (Simple_goto (shifted_position, Some (minus dir)));
        retry_n retries (Follow_path ([pos], Some (minus dir), false)); ]) in
  let goto_back =
    Node(Simple,
      [ retry_n retries (Simple_goto (back_shifted_position, Some (minus dir)));
        retry_n retries (Follow_path ([pos], Some dir, false)); ]) in
  [Node(Retry(1,goto_back),[goto_normal])]

  (* let goto_path = *)
  (*   Node(Simple, *)
  (*     [ retry_n retries (Simple_goto (shifted_position, Some (minus dir))); *)
  (*       retry_n retries (Follow_path ([pos], Some dir, false)); ]) in *)
  (* let simple_path = *)
  (*   retry_n 2 (Follow_path ([pos], Some (minus dir), false)) in *)
  (* [Node (Retry(1,goto_path), [simple_path])] *)

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

(* let goto_gift retries team gift = *)
(*   let destination = gift_destination team gift in *)
(*   approach_lower_border retries destination (direction destination team) *)

(* let do_gift retries team gift = *)
(*   let destination = gift_destination team gift in *)
(*   let dir = direction destination team in *)
(*   let goto = approach_lower_border retries destination dir in *)
(*   let hit = hit_ax12_dir dir in *)
(*   Node(Next,goto @ hit) *)

(* let do_first_gift team gift = *)
(*   let destination = gift_destination team gift in *)
(*   let dir = direction destination team in *)
(*   let goto = go_first_gift destination dir in *)
(*   let hit = hit_ax12_dir dir in *)
(*   Node(Next,goto :: hit) *)

(* let gifts_actions retries team = *)
(*   let gifts = match team with *)
(*     | `Red -> List.rev gifts_positions *)
(*     | `Blue -> gifts_positions *)
(*   in *)
(*   let first = List.hd gifts in *)
(*   let rest = List.tl gifts in *)
(*   do_first_gift team first *)
(*   ,(List.map (do_gift retries team) rest) *)

(* let random team = *)
(*   let zone = match team with *)
(*     | `Red -> ({x = 0.3; y = 0.4},{x = 0.6; y = 1.5}) *)
(*     | `Blue -> ({x = 2.2; y = 0.4},{x = 2.5; y = 1.5}) in *)
(*   let home = match team with *)
(*     | `Red -> ({x = 2.5; y = 0.4},{x = 2.4; y = 1.}) *)
(*     | `Blue -> ({x = 0.4; y = 0.4},{x = 0.5; y = 1.}) in *)
(*   let go = Random_move zone in *)
(*   let back' = Random_move home in *)
(*   let back = Node(Retry(3,Node(Simple,[Stop;back'])),[back']) in *)
(*   let r = Node(Simple,[Stop;go;back;Move_back 0.1]) in *)
(*   [Node(Loop(Node(Simple,[Stop;Wait_for 0.1;r])),[r])] *)

(* let loop l = Node(Loop(Node(Simple,l)),l) *)

(* let strategy gift_retries team = *)
(*   let first_gift, rest_gift = gifts_actions gift_retries team in *)
(*   start team @ [first_gift] @ rest_gift @ random team @ end_ *)


(* let launch bus team = *)
(*   Krobot_bus.send bus *)
(*     (Unix.gettimeofday (), *)
(*      Strategy_set (strategy 20 team)) *)


let launch bus team =
  let s = match team with
    | `Red -> run_strategy_red
    | `Blue -> run_strategy_blue
  in
  let strat = start team @ s in
  Krobot_bus.send bus (Unix.gettimeofday (),
    Strategy_set strat)


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
