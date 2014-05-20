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

let strategy = [|
  strategy_0;
  strategy_1
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
