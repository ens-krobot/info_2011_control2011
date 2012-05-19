(*
 * krobot_collision.ml
 * -------------------
 * Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Krobot_geom
open Krobot_config

let sqr x = x *. x

let point_in_world v =
  v.x >= 0. && v.y >= 0. && v.x < world_width && v.y < world_height

let point_in_world_safe v =
  v.x >= safety_margin && v.y >= safety_margin &&
    v.x < world_width -. safety_margin && v.y < world_height -. safety_margin

let rect pos angle =
  let norm_front = sqrt (sqr (robot_width /. 2.) +. sqr (robot_length -. wheels_position)) in
  let norm_back = sqrt (sqr (robot_width /. 2.) +. sqr wheels_position) in
  let a_front = atan2 (robot_width /. 2.) (robot_length -. wheels_position) in
  let a_back = atan2 (robot_width /. 2.) wheels_position in
  let v1 = translate pos (vector_of_polar ~norm:norm_front ~angle:(angle -. a_front)) in
  let v2 = translate pos (vector_of_polar ~norm:norm_front ~angle:(angle +. a_front)) in
  let v3 = translate pos (vector_of_polar ~norm:norm_back ~angle:(angle -. (pi -. a_back))) in
  let v4 = translate pos (vector_of_polar ~norm:norm_back ~angle:(angle +. (pi -. a_back))) in
  (v1, v2, v3, v4)

let robot_in_world pos angle =
  let v1, v2, v3, v4 = rect pos angle in
  point_in_world_safe v1
  && point_in_world_safe v2
  && point_in_world_safe v3
  && point_in_world_safe v4

let collision_rect_circle v1 v2 v3 v4 center radius =
  let i = normalize (vector v1 v2) in
  let j = normalize (vector v1 v3) in
  let v = vector v1 center in
  let x = prod v i in
  let y = prod v j in
  let len_horz = distance v1 v2
  and len_vert = distance v1 v3 in
  let in_band_horz = x >= 0. && x < len_horz in
  let in_band_vert = y >= 0. && y < len_vert in
  let safety_distance = radius +. safety_margin in
  match in_band_horz, in_band_vert with
    | true, true ->
      true
    | false, false ->
      let d = min (min (distance v1 center) (distance v2 center)) (min (distance v3 center) (distance v4 center)) in
      d < safety_distance
    | true, false ->
      y >= -. safety_distance && y < len_vert +. safety_distance
    | false, true ->
      x >= -. safety_distance && x < len_horz +. safety_distance

let collision_robot_circle pos angle center radius =
  let v1, v2, v3, v4 = rect pos angle in
  collision_rect_circle v1 v2 v3 v4 center radius

let move_possible objects pos angle dist =
  let d_front, d_back =
    if dist > 0. then
      (robot_length -. wheels_position +. dist, wheels_position)
    else
      (robot_length -. wheels_position, wheels_position -. dist)
  in
  let norm_front = sqrt (sqr (robot_width /. 2.) +. sqr d_front) in
  let norm_back = sqrt (sqr (robot_width /. 2.) +. sqr d_back) in
  let a_front = atan2 (robot_width /. 2.) d_front in
  let a_back = atan2 (robot_width /. 2.) d_back in
  let v1 = translate pos (vector_of_polar ~norm:norm_front ~angle:(angle -. a_front)) in
  let v2 = translate pos (vector_of_polar ~norm:norm_front ~angle:(angle +. a_front)) in
  let v3 = translate pos (vector_of_polar ~norm:norm_back ~angle:(angle -. (pi -. a_back))) in
  let v4 = translate pos (vector_of_polar ~norm:norm_back ~angle:(angle +. (pi -. a_back))) in
  if point_in_world_safe v1
    && point_in_world_safe v2
    && point_in_world_safe v3
    && point_in_world_safe v4 then
    List.for_all (fun obj -> not (collision_rect_circle v1 v2 v3 v4 obj.pos obj.size)) objects
  else
    false

let possible objects pos angle =
  robot_in_world pos angle
  && List.for_all
    (fun obj -> not (collision_robot_circle pos angle obj.pos obj.size))
    objects

let rec last_possible objects configs =
  match configs with
    | [] ->
      None
    | (x, pos, angle) :: configs ->
      if possible objects pos angle then
        match last_possible objects configs with
          | None ->
            Some x
          | Some _ as opt ->
            opt
      else
        None
