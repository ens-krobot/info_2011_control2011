(*
 * krobot_ia.ml
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* The IA of the robot. *)

open Krobot_bus
open Krobot_action

lwt () =
  lwt bus = Krobot_bus.get () in
  Krobot_bus.send bus
    (Unix.gettimeofday (),
     Strategy_set [
       Wait_for_jack false;
       Reset_odometry `Auto;
       Think;
     ])
