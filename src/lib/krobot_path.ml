(*
 * krobot_path.ml
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Krobot_geom
open Krobot_config

let rec prev_last = function
  | [] | [_] ->
      invalid_arg "Krobot_path.last"
  | [x; _] ->
      x
  | _ :: l ->
      prev_last l

let find ~src ~dst ~objects ~beacon =
  (* Remove objects that are near the destination. *)
  let objects = List.filter (fun obj -> distance dst obj >= object_safety_distance) objects in
  (* Remove objects that are near the curent position. *)
  let objects = List.filter (fun obj -> distance src obj >= object_safety_distance +. 0.01) objects in
  let l = List.map (fun v -> (v, object_safety_distance +. 0.01)) objects in
  let l =
    match beacon with
      | (Some v, None)
      | (None, Some v) ->
          (v, beacon_safety_distance +. 0.01) :: l
      | (Some v1, Some v2) ->
          (v1, beacon_safety_distance +. 0.01)
        :: (v2, beacon_safety_distance +. 0.01)
        :: l
      | (None, None) ->
          l
  in
  Krobot_pathfinding.find_path ~src ~dst
    ({ x = border_safety_distance;
       y = border_safety_distance},
     { x = world_width -. border_safety_distance;
       y = world_height -. border_safety_distance})
    l

let goto_object ~src ~dst ~objects ~beacon =
  match find ~src ~dst ~objects ~beacon with
    | Some p ->
        let v = vector dst (prev_last (src :: p)) in
        let v = v /| norm v in
        Some(translate dst (v *| object_safety_distance))
    | None ->
        None
