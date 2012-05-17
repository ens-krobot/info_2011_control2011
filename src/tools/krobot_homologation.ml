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
    { x = 0.7; y = init_pos.y -. 0.1};
    { x = 0.85; y = 1.5 };
    { x = 0.75; y = 1.20 };
    { x = 0.55; y = 1.15 };
    { x = 0.4; y = 1.15 };
  ]

let path =
  [
    { x = 1.8; y = init_pos.y -. 0.1};
  ]


lwt () =
  lwt bus = Krobot_bus.get () in
  Krobot_bus.send bus
    (Unix.gettimeofday (),
     Strategy_set [
       Wait_for_jack true;
       Wait_for_jack false;
       Reset_odometry `Auto;
       Wait_for_odometry_reset `Auto;
       Set_limits (0.2,1.0,1.0);
       Goto (true, { x = 0.55; y = 1.15 }, Some { vx = 1. ; vy = 0. });
       Follow_path (true, [{ x = 0.4; y = 1.15 }], None);
     ])
