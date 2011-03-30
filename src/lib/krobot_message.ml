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
open Lwt
open Lwt_react

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type direction = Forward | Backward

type t =
  | Encoder_position_direction_3_4 of int * direction * int * direction
  | Encoder_position_speed_3 of float * float
  | Encoder_position_speed_4 of float * float
  | Motor_status of bool
  | Motor_move of float * float * float
  | Motor_turn of float * float * float
  | Motor_stop
  | Odometry of float * float * float
  | Set_odometry of float * float * float
  | Req_motor_status
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
  | Motor_status moving ->
      sprintf
        "Motor_status(%B)"
        moving
  | Motor_move(dist, speed, acc) ->
      sprintf
        "Motor_move(%f, %f, %f)"
        dist speed acc
  | Motor_turn(angle, speed, acc) ->
      sprintf
        "Motor_turn(%f, %f, %f)"
        angle speed acc
  | Motor_stop ->
      "Motor_stop"
  | Odometry(x, y, theta) ->
      sprintf
        "Odometry(%f, %f, %f)"
        x y theta
  | Set_odometry(x, y, theta) ->
      sprintf
        "Set_odometry(%f, %f, %f)"
        x y theta
  | Req_motor_status ->
      "Req_motor_status"
  | Unknown frame ->
      sprintf "Unknown%s" (Krobot_can.string_of_frame frame)

(* +-----------------------------------------------------------------+
   | Encoding                                                        |
   +-----------------------------------------------------------------+ *)

let pi = 4. *. atan 1.

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
        ~format:F29bits
        ~data
  | Encoder_position_speed_3(pos, speed) ->
      let data = String.create 8 in
      put_float32 data 0 pos;
      put_float32 data 4 speed;
      frame
        ~identifier:101
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Encoder_position_speed_4(pos, speed) ->
      let data = String.create 8 in
      put_float32 data 0 pos;
      put_float32 data 4 speed;
      frame
        ~identifier:102
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Motor_status moving ->
      let data = String.create 1 in
      put_uint8 data 0 (if moving then 1 else 0);
      frame
        ~identifier:103
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Odometry(x, y, theta) ->
      let data = String.create 6 in
      put_sint16 data 0 (truncate (x *. 1000.));
      put_sint16 data 2 (truncate (y *. 1000.));
      put_sint16 data 4 (truncate (theta /. pi *. 18000.));
      frame
        ~identifier:104
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Set_odometry(x, y, theta) ->
      let data = String.create 6 in
      put_sint16 data 0 (truncate (x *. 1000.));
      put_sint16 data 2 (truncate (y *. 1000.));
      put_sint16 data 4 (truncate (theta /. pi *. 18000.));
      frame
        ~identifier:105
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Motor_move(dist, speed, acc) ->
      let data = String.create 8 in
      put_sint32 data 0 (truncate (dist *. 1000.));
      put_uint16 data 4 (truncate (speed *. 1000.));
      put_uint16 data 6 (truncate (acc *. 1000.));
      frame
        ~identifier:201
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Motor_turn(angle, speed, acc) ->
      let data = String.create 8 in
      put_sint32 data 0 (truncate (angle /. pi *. 18000.));
      put_uint16 data 4 (truncate (speed /. pi *. 18000.));
      put_uint16 data 6 (truncate (acc /. pi *. 18000.));
      frame
        ~identifier:202
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Motor_stop ->
      frame
        ~identifier:203
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data:""
  | Req_motor_status ->
      frame
        ~identifier:103
        ~kind:Data
        ~remote:true
        ~format:F29bits
        ~data:""
  | Unknown frame ->
      frame

(* +-----------------------------------------------------------------+
   | Decoding                                                        |
   +-----------------------------------------------------------------+ *)

let decode frame =
  if frame.remote then
    match frame.identifier with
      | 103 ->
          Req_motor_status
      | _ ->
          Unknown frame
  else
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
      | 103 ->
          Motor_status(get_uint8 frame.data 0 <> 0)
      | 104 ->
          Odometry
            (float (get_sint16 frame.data 0) /. 1000.,
             float (get_sint16 frame.data 2) /. 1000.,
             float (get_sint16 frame.data 4) *. pi /. 18000.)
      | 105 ->
          Set_odometry
            (float (get_sint16 frame.data 0) /. 1000.,
             float (get_sint16 frame.data 2) /. 1000.,
             float (get_sint16 frame.data 4) *. pi /. 18000.)
      | 201 ->
          Motor_move
            (float (get_sint32 frame.data 0) /. 1000.,
             float (get_uint16 frame.data 4) /. 1000.,
             float (get_uint16 frame.data 6) /. 1000.)
      | 202 ->
          Motor_turn
            (float (get_sint32 frame.data 0) *. pi /. 18000.,
             float (get_uint16 frame.data 4) *. pi /. 18000.,
             float (get_uint16 frame.data 6) *. pi /. 18000.)
      | 203 ->
          Motor_stop
      | _ ->
          Unknown frame

(* +-----------------------------------------------------------------+
   | Sending/receiving messages                                      |
   +-----------------------------------------------------------------+ *)

let send bus (timestamp, msg) = Krobot_can.send bus (timestamp, encode msg)
let recv bus = E.map (fun (timestamp, frame) -> (timestamp, decode frame)) (Krobot_can.recv bus)

(* +-----------------------------------------------------------------+
   | Calls                                                           |
   +-----------------------------------------------------------------+ *)

let wait_timeout = 0.1

let send_and_recv bus req f =
  let t =
    E.next
      (E.fmap
         (fun (ts, frame) ->
            match f frame with
              | Some x -> Some(`Value(ts, x))
              | None -> None)
         (recv bus))
  in
  let rec loop () =
    lwt () = send bus (Unix.gettimeofday (), req) in
    match_lwt choose [t; Lwt_unix.sleep wait_timeout >|= fun () -> `Timeout] with
      | `Value x -> return x
      | `Timeout -> loop ()
  in
  loop ()

let motor_status bus =
  send_and_recv bus Req_motor_status
    (function
       | Motor_status status -> Some status
       | _ -> None)
