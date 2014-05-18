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

let object_list ~beacon objects =
  let fixed_objects = List.map (fun { pos; size } -> pos,
    size +. Krobot_config.robot_radius +. 0.01)
    Krobot_config.fixed_obstacles in

  let objects = List.map (fun { pos; size } -> pos,
    size +. Krobot_config.robot_radius +. 0.01)
    objects in

  let l = objects @ fixed_objects in
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
  l

let find ?src_orient ?dst_orient ~src ~dst ~beacon objects =
  let l = object_list ~beacon objects in
  let l = List.map (fun (v,s) -> (v, min s (distance v src -. 0.1))) l in
  let min_distance = Krobot_config.robot_radius +. safety_margin in
  Krobot_pathfinding.find_path ?src_orient ?dst_orient ~src ~dst
    ({ x = min_distance;
       y = min_distance},
     { x = world_width -. min_distance;
       y = world_height -. min_distance})
    l

let sq x = x *. x

let find_with_real_center ~pos:robot_pos ~orientation:robot_angle ?turn_radius ?dst_orient ~dst ~beacon objects =
  let dist_real_center_wheel_center = (robot_length/.2.) -. wheels_position in
  let diff_center = vector_of_polar ~norm:dist_real_center_wheel_center
      ~angle:robot_angle in
  let center = translate robot_pos diff_center in
  let radius = sqrt ((sq (robot_length/.2.)) +. (sq (robot_width/.2.))) in
  let src = center in

  let l = object_list ~beacon objects in
  let l = List.map (fun (v,s) -> (v, min s (distance v src -. 0.1))) l in
  let min_distance = radius +. safety_margin in

  let src_orient = match turn_radius with
    | None -> None
    | Some r ->
      let vect = { vx = cos robot_angle;
                   vy = sin robot_angle } in
      Some (r, vect) in

  Krobot_pathfinding.find_path ?src_orient ?dst_orient ~src ~dst
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
