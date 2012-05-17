(*
 * krobot_path.mli
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Path utils *)

open Krobot_geom

val find : src : vertice -> dst : vertice -> objects : vertice list -> beacon : vertice option * vertice option -> vertice list option
  (** [goto ~src ~dst ~objects ~beacon] find a path from [src] to
      [dst]. *)

val goto_object : src : vertice -> dst : vertice -> objects : vertice list -> beacon : vertice option * vertice option -> vertice option
  (** [goto_object ~src ~dst ~objects ~beacon] returns the goal
      position to move to to reach an object in a position to take
      it. *)
