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

type encoder_state = {
  es_position : int;
  es_direction : direction;
}

type t =
  | Encoder_state_1_2 of encoder_state * encoder_state
  | Encoder_state_3_4 of encoder_state * encoder_state
  | Unknown of frame

(* +-----------------------------------------------------------------+
   | Message --> string                                              |
   +-----------------------------------------------------------------+ *)

let string_of_encoder_state es =
  sprintf "{ es_position = %d; es_direction = %s }" es.es_position (match es.es_direction with Forward -> "Forward" | Backward -> "Backward")

let to_string = function
  | Encoder_state_1_2(c1, c2) -> sprintf "Encoder_state_1_2(%s, %s)" (string_of_encoder_state c1) (string_of_encoder_state c2)
  | Encoder_state_3_4(c3, c4) -> sprintf "Encoder_state_3_4(%s, %s)" (string_of_encoder_state c3) (string_of_encoder_state c4)
  | Unknown frame -> Krobot_can.string_of_frame frame

(* +-----------------------------------------------------------------+
   | Encoding                                                        |
   +-----------------------------------------------------------------+ *)

let encode = function
  | Encoder_state_1_2(c1, c2) ->
      let data = String.create 6 in
      put_uint16 data 0 c1.es_position;
      put_uint16 data 2 c2.es_position;
      put_uint8 data 4 (match c1.es_direction with Forward -> 0 | Backward -> 1);
      put_uint8 data 5 (match c2.es_direction with Forward -> 0 | Backward -> 1);
      frame
        ~identifier:100
        ~kind:Data
        ~remote:false
        ~format:F11bits
        ~data
  | Encoder_state_3_4(c3, c4) ->
      let data = String.create 6 in
      put_uint16 data 0 c3.es_position;
      put_uint16 data 2 c4.es_position;
      put_uint8 data 4 (match c3.es_direction with Forward -> 0 | Backward -> 1);
      put_uint8 data 5 (match c4.es_direction with Forward -> 0 | Backward -> 1);
      frame
        ~identifier:101
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
        Encoder_state_1_2
          ({ es_position = get_uint16 frame.data 0;
             es_direction = if get_uint8 frame.data 4 = 0 then Forward else Backward },
           { es_position = get_uint16 frame.data 2;
             es_direction = if get_uint8 frame.data 5 = 0 then Forward else Backward })
    | 101 ->
        Encoder_state_3_4
          ({ es_position = get_uint16 frame.data 0;
             es_direction = if get_uint8 frame.data 4 = 0 then Forward else Backward },
           { es_position = get_uint16 frame.data 2;
             es_direction = if get_uint8 frame.data 5 = 0 then Forward else Backward })
    | _ ->
        Unknown frame

(* +-----------------------------------------------------------------+
   | Sending/receiving messages                                      |
   +-----------------------------------------------------------------+ *)

let send bus (timestamp, msg) = Krobot_can.send bus (timestamp, encode msg)
let recv bus = E.map (fun (timestamp, frame) -> (timestamp, decode frame)) (Krobot_can.recv bus)
