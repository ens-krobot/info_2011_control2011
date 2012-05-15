(*
 * krobot_homologation.ml
 * ------------
 * Copyright : (c) 2012, Pierre Chambart <chambart@crans.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* The simple ai for homologation. *)

open Krobot_bus
open Krobot_action
open Krobot_geom

let init_pos, init_angle = Krobot_config.red_initial_position

let path =
  [
    { x = 0.7; y = init_pos.y };
    { x = 0.8; y = 1.5 };
    { x = 0.7; y = 1.15 };
    { x = 0.5; y = 1.15 };
    { x = 0.4; y = 1.15 };
  ]

lwt () =
  lwt bus = Krobot_bus.get () in
  Krobot_bus.send bus
    (Unix.gettimeofday (),
     Strategy_set [
       Reset_odometry `Red;
       Wait_for_jack false;
       Follow_path path;
     ])
