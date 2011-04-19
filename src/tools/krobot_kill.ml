(*
 * krobot_kill.ml
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Krobot_bus

lwt () =
  lwt bus =  Krobot_bus.get () in

  join
    (List.map
       (fun prog -> Krobot_bus.send bus (Unix.gettimeofday (), Kill prog))
       (List.tl (Array.to_list Sys.argv)))
