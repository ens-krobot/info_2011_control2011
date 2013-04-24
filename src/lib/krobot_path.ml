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

let find ~src ~dst ~beacon =

  let fixed_objects = List.map (fun { pos; size } -> pos,
    size +. Krobot_config.robot_radius +. 0.01)
    Krobot_config.fixed_obstacles in

  (* do that in a better way when we have time... *)
  let init_coins = List.map (fun pos -> pos,
    Krobot_config.coin_radius +. Krobot_config.robot_radius +. 0.01)
    Krobot_config.initial_coins in

  let l = fixed_objects @ init_coins in
  let l =
    match beacon with
      | (Some v, None)
      | (None, Some v) ->
          ignore (Lwt_log.info_f "One beacon %f %f" v.x v.y);
          (v, beacon_radius +. safety_margin +. Krobot_config.robot_radius) :: l
      | (Some v1, Some v2) ->
          ignore (Lwt_log.info_f "Two beacons (%f,%f) (%f,%f)" v1.x v1.y v2.x v2.y);
          (v1, beacon_radius +. safety_margin +. Krobot_config.robot_radius)
        :: (v2, beacon_radius +. safety_margin +. Krobot_config.robot_radius)
        :: l
      | (None, None) ->
          ignore (Lwt_log.info_f "no beacon");
          l
  in
  let l = List.map (fun (v,s) -> (v, min s (distance v src -. 0.1))) l in
  let min_distance = Krobot_config.robot_radius +. safety_margin in
  Krobot_pathfinding.find_path ~src ~dst
    ({ x = min_distance;
       y = min_distance},
     { x = world_width -. min_distance;
       y = world_height -. min_distance})
    l
(*
let goto_object ~src ~dst ~beacon =
  match find ~src ~dst ~beacon with
    | Some p ->
        let v = vector dst (prev_last (src :: p)) in
        let v = v /| norm v in
        Some(translate dst (v *| object_safety_distance))
    | None ->
        None
*)
