(*
 * krobot_example.ml
 * ------------
 * Copyright : (c) 2014, Pierre Chambart <chambart@crans.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Lwt_react
open Krobot_message
open Krobot_bus
open Krobot_action
open Krobot_geom

let strategy_0 () = [
  Stop;
  Reset_odometry `Red;
  Wait_for_odometry_reset `Red;
  Wait_for 0.1;
  Node (
    Loop
      (Node (Simple,
             [Goto ({ x = 1. ; y = 0.7 }, None);
              Wait_for 0.1;
              Goto ({ x = 1. ; y = 1.4 }, Some({ vx = -.1.; vy = 0.}));
              Wait_for 0.1;
              Goto ({ x = 2. ; y = 1.4 }, None);
              Wait_for 0.1;
              Goto ({ x = 2. ; y = 0.7 }, Some({ vx = 1.; vy = -.0.1}));
              Wait_for 0.1;
             ])),
    []);
  Stop;
]

let strategy_1 () =
  let timed_actions = Krobot_ax12_format.read_timed_actions_file Sys.argv.(2) in
  let actions = Krobot_ax12_format.to_actions timed_actions in
  [Ax12_sequence("direct", actions);
   Set_led (`Red,false);
   Set_led (`Green,true);
   Wait_for_finished_ax12_sequence("direct", Timeout_none);
   Set_led (`Red,true);
   Set_led (`Green,false);
   End]

let strategy_2 () =

  let move_loop =
    let (init_vertice, theta) = Krobot_config.red_initial_position in
    let move =
      Follow_path ([{init_vertice with y = init_vertice.y -. 0.5}],
                   Some { vx = 0.; vy = -1. },
                   false) in
    (* loop infinitely until move succeed *)
    Node(Retry (-1,move), [Fail])
  in
  [
  Stop;
  Reset_odometry `Red;
  Wait_for_odometry_reset `Red;
  Wait_for 0.1;
  Set_led (`Green,true);
  Set_led (`Red,true);
  Wait_for_jack true;
  Set_led (`Red,false);
  Wait_for 0.4;
  Elevator_homing;
  Wait_for_jack false;
  Set_led (`Red,true);
  Set_led (`Green,false);

  Start_timer (30.,[Stop; End]);

  move_loop;

  Node (
    Loop
      (Node (Simple,
             [Goto ({ x = 1. ; y = 0.7 }, None);
              Wait_for 0.1;
              Goto ({ x = 1. ; y = 1.4 }, Some({ vx = -.1.; vy = 0.}));
              Wait_for 0.1;
              Goto ({ x = 2. ; y = 1.4 }, None);
              Wait_for 0.1;
              Goto ({ x = 2. ; y = 0.7 }, Some({ vx = 1.; vy = -.0.1}));
              Wait_for 0.1;
             ])),
    []);

  Stop;
  End
]

let strategy_3 () =
  let keyframes = Krobot_ax12_format.read_keyframes_file Sys.argv.(2) in
  let sequence = [(0,0); (0, 100)] in
  [Ax12_framed_sequence("goto_start_sequence", keyframes, sequence);
   Set_led (`Red,false);
   Set_led (`Green,true);
   Wait_for_finished_ax12_sequence("goto_start_sequence", Timeout_none);
   Set_led (`Red,true);
   Set_led (`Green,false);
   End]

let strategy_4 () =
  let keyframes = Krobot_ax12_format.read_keyframes_file Sys.argv.(2) in
  let num_keyframes = Krobot_ax12_format.IntMap.cardinal keyframes in
  let rec make_sequence seq id =
    if id < 0 then
      seq
    else
      make_sequence ((id, 100)::seq) (id-1)
  in
  let sequence = make_sequence [(0, 100)] (num_keyframes-1) in
  let rec print_seq = function
    | [] ->
      Printf.printf "\n%!"
    | (id, speed)::t ->
      (Printf.printf "(%d, %d) " id speed;
       print_seq t) in
  print_seq sequence;
  [Ax12_framed_sequence("framed_seq", keyframes, sequence);
   Set_led (`Red,false);
   Set_led (`Green,true);
   Wait_for_finished_ax12_sequence("framed_seq", Timeout_none);
   Set_led (`Red,true);
   Set_led (`Green,false);
   End]

let strategy = [|
  strategy_0;
  strategy_1;
  strategy_2;
  strategy_3;
  strategy_4
|]

let launch i =
  lwt bus = Krobot_bus.get () in
  Krobot_bus.send bus (Unix.gettimeofday (),
                       Strategy_set (strategy.(i) ()))

let () =
  let strat =
    if Array.length Sys.argv = 1
    then 0
    else int_of_string (Sys.argv.(1)) in
  Lwt_unix.run (launch strat)
