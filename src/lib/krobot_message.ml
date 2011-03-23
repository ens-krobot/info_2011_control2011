(*
 * krobot_message.ml
 * -----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Krobot_can
open Printf
open Lwt_react

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type direction = Forward | Backward

type t =
  | Encoder_position_direction_3_4 of int * direction * int * direction
  | Encoder_position_speed_3 of float * float
  | Encoder_position_speed_4 of float * float
  | Unknown of frame

(* +-----------------------------------------------------------------+
   | Message --> string                                              |
   +-----------------------------------------------------------------+ *)

let string_of_direction = function
  | Forward -> "Forward"
  | Backward -> "Backward"

let to_string = function
  | Encoder_position_direction_3_4(pos3, dir3, pos4, dir4) ->
      sprintf
        "Encoder_position_direction_3_4(%d, %s, %d, %s)"
        pos3 (string_of_direction dir3)
        pos4 (string_of_direction dir4)
  | Encoder_position_speed_3(pos, speed) ->
      sprintf
        "Encoder_position_speed_3(%f, %f)"
        pos speed
  | Encoder_position_speed_4(pos, speed) ->
      sprintf
        "Encoder_position_speed_4(%f, %f)"
        pos speed
  | Unknown frame ->
      sprintf "Unknown%s" (Krobot_can.string_of_frame frame)

(* +-----------------------------------------------------------------+
   | Encoding                                                        |
   +-----------------------------------------------------------------+ *)

let encode = function
  | Encoder_position_direction_3_4(pos3, dir3, pos4, dir4) ->
      let data = String.create 6 in
      put_uint16 data 0 pos3;
      put_uint16 data 2 pos4;
      put_uint8 data 4 (match dir3 with Forward -> 0 | Backward -> 1);
      put_uint8 data 5 (match dir4 with Forward -> 0 | Backward -> 1);
      frame
        ~identifier:100
        ~kind:Data
        ~remote:false
        ~format:F11bits
        ~data
  | Encoder_position_speed_3(pos, speed) ->
      let data = String.create 8 in
      put_float32 data 0 pos;
      put_float32 data 4 speed;
      frame
        ~identifier:101
        ~kind:Data
        ~remote:false
        ~format:F11bits
        ~data
  | Encoder_position_speed_4(pos, speed) ->
      let data = String.create 8 in
      put_float32 data 0 pos;
      put_float32 data 4 speed;
      frame
        ~identifier:102
        ~kind:Data
        ~remote:false
        ~format:F11bits
        ~data
  | Unknown frame ->
      frame

(* +-----------------------------------------------------------------+
   | Decoding                                                        |
   +-----------------------------------------------------------------+ *)

let decode frame =
  match frame.identifier with
    | 100 ->
        Encoder_position_direction_3_4
          (get_uint16 frame.data 0,
           (if get_uint8 frame.data 4 = 0 then Forward else Backward),
           get_uint16 frame.data 2,
           (if get_uint8 frame.data 5 = 0 then Forward else Backward))
    | 101 ->
        Encoder_position_speed_3
          (get_float32 frame.data 0,
           get_float32 frame.data 4)
    | 102 ->
        Encoder_position_speed_4
          (get_float32 frame.data 0,
           get_float32 frame.data 4)
    | _ ->
        Unknown frame

(* +-----------------------------------------------------------------+
   | Sending/receiving messages                                      |
   +-----------------------------------------------------------------+ *)

let send bus (timestamp, msg) = Krobot_can.send bus (timestamp, encode msg)
let recv bus = E.map (fun (timestamp, frame) -> (timestamp, decode frame)) (Krobot_can.recv bus)
