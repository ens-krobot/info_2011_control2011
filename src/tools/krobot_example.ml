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

let strategy_0 = [
  Stop;
  Wait_for 0.1;
  Goto ({ x = 0.4 ; y = 0.7 }, None);
  Stop;
]

let strategy_1 = []

let strategy = [|
  strategy_0;
  strategy_1
|]

let launch i =
  lwt bus = Krobot_bus.get () in
  Krobot_bus.send bus (Unix.gettimeofday (),
                       Strategy_set strategy.(i))

let () =
  let strat =
    if Array.length Sys.argv = 1
    then 0
    else int_of_string (Sys.argv.(1)) in
  Lwt_unix.run (launch strat)
