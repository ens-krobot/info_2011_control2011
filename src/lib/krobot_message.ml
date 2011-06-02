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
  | Battery1_voltages of float * float * float * float
  | Battery2_voltages of float * float * float * float
  | Beacon_position of float * float * float
  | Beacon_lowlevel_position of float * float * int
  | Switch1_status of bool * bool * bool * bool * bool * bool * bool * bool
  | Switch2_status of bool * bool * bool * bool * bool * bool * bool * bool
  | Switch_request of int * bool
  | Adc1_values of int * int * int * int
  | Adc2_values of int * int * int * int
  | Ax12_State of int * int * int * int
  | Ax12_Request_State of int
  | Ax12_Goto of int * int * int
  | Ax12_Reset of int
  | Encoder_position_direction_1_2 of int * direction * int * direction
  | Encoder_position_direction_3_4 of int * direction * int * direction
  | Encoder_position_speed_3 of float * float
  | Encoder_position_speed_4 of float * float
  | Motor_status of bool * bool * bool * bool
  | Motor_move of float * float * float
  | Motor_turn of float * float * float
  | Motor_bezier of float * float * float * float * float * float
  | Motor_stop of float * float
  | Odometry of float * float * float
  | Odometry_ghost of float * float * float * int * bool
  | Set_odometry of float * float * float
  | Set_controller_mode of bool
  | Elevator of float * float
  | Req_motor_status
  | Unknown of frame

(* +-----------------------------------------------------------------+
   | Message --> string                                              |
   +-----------------------------------------------------------------+ *)

let string_of_direction = function
  | Forward -> "Forward"
  | Backward -> "Backward"

let to_string = function
  | Battery1_voltages(elem1, elem2, elem3, elem4) ->
      sprintf
        "Battery1_voltages(%f, %f, %f, %f)"
        elem1
        elem2
        elem3
        elem4
  | Battery2_voltages(elem1, elem2, elem3, elem4) ->
      sprintf
        "Battery2_voltages(%f, %f, %f, %f)"
        elem1
        elem2
        elem3
        elem4
  | Beacon_position(angle, distance, period) ->
      sprintf
        "Beacon_position(%f, %f, %f)"
        angle
        distance
        period
  | Beacon_lowlevel_position(angle, width, period) ->
      sprintf
        "Beacon_lowlevel_position(%f, %f, %d)"
        angle
        width
        period
  | Switch1_status(s1, s2, s3, s4, s5, s6, s7, s8) ->
      sprintf
        "Switch1_status(%B, %B, %B, %B, %B, %B, %B, %B)"
        s1 s2 s3 s4 s5 s6 s7 s8
  | Switch2_status(s1, s2, s3, s4, s5, s6, s7, s8) ->
      sprintf
        "Switch2_status(%B, %B, %B, %B, %B, %B, %B, %B)"
        s1 s2 s3 s4 s5 s6 s7 s8
  | Switch_request(switch, status) ->
      sprintf
        "Switch_request(%d, %s)"
        switch
        (if status then "ON" else "OFF")
  | Adc1_values(v1, v2, v3, v4) ->
      sprintf
        "Adc1_values(%d, %d, %d, %d)"
        v1 v2 v3 v4
  | Adc2_values(v1, v2, v3, v4) ->
      sprintf
        "Adc2_values(%d, %d, %d, %d)"
        v1 v2 v3 v4
  | Ax12_State(addr, position, speed, torque) ->
      sprintf
        "Ax12_State(%d, %d, %d, %d)"
        addr position speed torque
  | Ax12_Request_State(addr) ->
      sprintf
        "Ax12_Request_State(%d)"
        addr
  | Ax12_Goto(addr, position, speed) ->
      sprintf
        "Ax12_Goto(%d, %d, %d)"
        addr position speed
  | Ax12_Reset(new_addr) ->
      sprintf
        "Ax12_Reset(%d)"
        new_addr
  | Encoder_position_direction_1_2(pos1, dir1, pos2, dir2) ->
      sprintf
        "Encoder_position_direction_1_2(%d, %s, %d, %s)"
        pos1 (string_of_direction dir1)
        pos2 (string_of_direction dir2)
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
  | Motor_status(m1, m2, m3, m4) ->
      sprintf
        "Motor_status(%B, %B, %B, %B)"
        m1 m2 m3 m4
  | Motor_move(dist, speed, acc) ->
      sprintf
        "Motor_move(%f, %f, %f)"
        dist speed acc
  | Motor_turn(angle, speed, acc) ->
      sprintf
        "Motor_turn(%f, %f, %f)"
        angle speed acc
  | Motor_bezier(x, y, d1, d2, theta, v) ->
      sprintf
        "Motor_bezier(%f, %f, %f, %f, %f, %f)"
        x y d1 d2 theta v
  | Motor_stop(lin_acc, rot_acc) ->
      sprintf
        "Motor_stop(%f, %f)"
        lin_acc rot_acc
  | Odometry(x, y, theta) ->
      sprintf
        "Odometry(%f, %f, %f)"
        x y theta
  | Odometry_ghost(x, y, theta, u, following) ->
      sprintf
        "Odometry_ghost(%f, %f, %f, %d, %B)"
        x y theta u following
  | Set_odometry(x, y, theta) ->
      sprintf
        "Set_odometry(%f, %f, %f)"
        x y theta
  | Set_controller_mode b ->
      sprintf
        "Set_controller_mode(%B)"
        b
  | Elevator(p1, p2) ->
      sprintf
        "Elevator(%f, %f)"
        p1 p2
  | Req_motor_status ->
      "Req_motor_status"
  | Unknown frame ->
      sprintf "Unknown%s" (Krobot_can.string_of_frame frame)

(* +-----------------------------------------------------------------+
   | Encoding                                                        |
   +-----------------------------------------------------------------+ *)

let pi = 4. *. atan 1.

external encode_bezier : int * int * int * int * int * int -> string = "krobot_message_encode_bezier"

let encode = function
  | Encoder_position_direction_1_2(pos1, dir1, pos2, dir2) ->
      let data = String.create 6 in
      put_uint16 data 0 pos1;
      put_uint16 data 2 pos2;
      put_uint8 data 4 (match dir1 with Forward -> 0 | Backward -> 1);
      put_uint8 data 5 (match dir2 with Forward -> 0 | Backward -> 1);
      frame
        ~identifier:99
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
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
  | Motor_status(m1, m2, m3, m4) ->
      let data = String.create 1 in
      let x = 0 in
      let x = if m1 then x lor 1 else x in
      let x = if m2 then x lor 2 else x in
      let x = if m3 then x lor 4 else x in
      let x = if m4 then x lor 8 else x in
      put_uint8 data 0 x;
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
      put_sint16 data 4 (truncate (theta *. 10000.));
      frame
        ~identifier:104
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Odometry_ghost(x, y, theta, u, following) ->
      let data = String.create 8 in
      put_sint16 data 0 (truncate (x *. 1000.));
      put_sint16 data 2 (truncate (y *. 1000.));
      put_sint16 data 4 (truncate (theta *. 10000.));
      put_uint8 data 6 u;
      put_uint8 data 7 (if following then 1 else 0);
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
      put_sint32 data 0 (truncate (angle *. 10000.));
      put_uint16 data 4 (truncate (speed *. 1000.));
      put_uint16 data 6 (truncate (acc *. 1000.));
      frame
        ~identifier:202
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Motor_bezier(x, y, d1, d2, theta, v) ->
      let x = int_of_float (x *. 1000.)
      and y = int_of_float (y *. 1000.)
      and d1 = int_of_float (d1 *. 100.)
      and d2 = int_of_float (d2 *. 100.)
      and theta = int_of_float (theta *. 100.)
      and v = int_of_float (v *. 1000.) in
      frame
        ~identifier:206
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data:(encode_bezier (x, y, d1, d2, theta, v))
  | Set_odometry(x, y, theta) ->
      let data = String.create 6 in
      put_sint16 data 0 (truncate (x *. 1000.));
      put_sint16 data 2 (truncate (y *. 1000.));
      put_sint16 data 4 (truncate (theta *. 10000.));
      frame
        ~identifier:203
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Beacon_position(angle, length, period) ->
      let data = String.create 6 in
      put_uint16 data 0 (truncate (angle *. 10000.));
      put_uint16 data 2 (truncate (length *. 1000.));
      put_uint16 data 4 (truncate (period *. 10000.));
      frame
        ~identifier:301
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Beacon_lowlevel_position(angle, width, period) ->
      let data = String.create 8 in
      put_uint16 data 0 (truncate (angle *. 10000.));
      put_uint16 data 2 (truncate (width *. 100000.));
      put_uint32 data 4 period;
      frame
        ~identifier:302
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Switch1_status(sw1, sw2, sw3, sw4, sw5, sw6, sw7, sw8) ->
      let data = String.create 8 in
      put_uint8 data 0 (if sw1 then 1 else 0);
      put_uint8 data 1 (if sw2 then 1 else 0);
      put_uint8 data 2 (if sw3 then 1 else 0);
      put_uint8 data 3 (if sw4 then 1 else 0);
      put_uint8 data 4 (if sw5 then 1 else 0);
      put_uint8 data 5 (if sw6 then 1 else 0);
      put_uint8 data 6 (if sw7 then 1 else 0);
      put_uint8 data 7 (if sw8 then 1 else 0);
      frame
        ~identifier:311
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data:data
  | Switch2_status(sw1, sw2, sw3, sw4, sw5, sw6, sw7, sw8) ->
      let data = String.create 8 in
      put_uint8 data 0 (if sw1 then 1 else 0);
      put_uint8 data 1 (if sw2 then 1 else 0);
      put_uint8 data 2 (if sw3 then 1 else 0);
      put_uint8 data 3 (if sw4 then 1 else 0);
      put_uint8 data 4 (if sw5 then 1 else 0);
      put_uint8 data 5 (if sw6 then 1 else 0);
      put_uint8 data 6 (if sw7 then 1 else 0);
      put_uint8 data 7 (if sw8 then 1 else 0);
      frame
        ~identifier:312
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data:data
  | Switch_request(switch, status) ->
      let data = String.create 2 in
      put_uint8 data 0 switch;
      put_uint8 data 1 (if status then 1 else 0);
      frame
        ~identifier:313
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data:data
  | Adc1_values(v1, v2, v3, v4) ->
      let data = String.create 8 in
      put_uint16 data 0 v1;
      put_uint16 data 2 v2;
      put_uint16 data 4 v3;
      put_uint16 data 6 v4;
      frame
        ~identifier:321
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data:data
  | Adc2_values(v1, v2, v3, v4) ->
      let data = String.create 8 in
      put_uint16 data 0 v1;
      put_uint16 data 2 v2;
      put_uint16 data 4 v3;
      put_uint16 data 6 v4;
      frame
        ~identifier:322
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data:data
  | Battery1_voltages(elem1, elem2, elem3, elem4) ->
      let data = String.create 8 in
      put_uint16 data 0 (truncate (elem1 *. 10000.));
      put_uint16 data 2 (truncate (elem2 *. 10000.));
      put_uint16 data 4 (truncate (elem3 *. 10000.));
      put_uint16 data 5 (truncate (elem4 *. 10000.));
      frame
        ~identifier:331
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Battery2_voltages(elem1, elem2, elem3, elem4) ->
      let data = String.create 8 in
      put_uint16 data 0 (truncate (elem1 *. 10000.));
      put_uint16 data 2 (truncate (elem2 *. 10000.));
      put_uint16 data 4 (truncate (elem3 *. 10000.));
      put_uint16 data 5 (truncate (elem4 *. 10000.));
      frame
        ~identifier:332
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Ax12_State(address, position, speed, torque) ->
      let data = String.create 7 in
      put_uint8 data 0 address;
      put_uint16 data 1 position;
      put_uint16 data 3 speed;
      put_uint16 data 5 torque;
      frame
        ~identifier:341
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Ax12_Request_State(address) ->
      let data = String.create 1 in
      put_uint8 data 0 address;
      frame
        ~identifier:342
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Ax12_Goto(address, position, speed) ->
      let data = String.create 5 in
      put_uint8 data 0 address;
      put_uint16 data 1 position;
      put_uint16 data 3 speed;
      frame
        ~identifier:343
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Ax12_Reset(future_address) ->
      let data = String.create 1 in
      put_uint8 data 0 future_address;
      frame
        ~identifier:344
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Motor_stop(lin_acc, rot_acc) ->
      let data = String.create 8 in
      put_float32 data 0 lin_acc;
      put_float32 data 4 rot_acc;
      frame
        ~identifier:204
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
  | Set_controller_mode b ->
      frame
        ~identifier:205
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data:(if b then "\x01" else "\x00")
  | Elevator(p1, p2) ->
      let data = String.create 8 in
      put_float32 data 0 p1;
      put_float32 data 4 p2;
      frame
        ~identifier:231
        ~kind:Data
        ~remote:false
        ~format:F29bits
        ~data
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

exception Invalid_frame of Krobot_can.frame

let () =
  Printexc.register_printer
    (function
       | Invalid_frame frame ->
           Some(Printf.sprintf "Invalid_frame%s" (Krobot_can.string_of_frame frame))
       | _ ->
           None)

external decode_bezier : string -> int * int * int * int * int * int = "krobot_message_decode_bezier"

let decode frame =
  try
    if frame.remote then
      match frame.identifier with
        | 103 ->
            Req_motor_status
        | _ ->
            Unknown frame
    else
      match frame.identifier with
        | 99 ->
            Encoder_position_direction_1_2
              (get_uint16 frame.data 0,
               (if get_uint8 frame.data 4 = 0 then Forward else Backward),
               get_uint16 frame.data 2,
               (if get_uint8 frame.data 5 = 0 then Forward else Backward))
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
            let x = get_uint8 frame.data 0 in
            Motor_status(x land 1 <> 0,
                         x land 2 <> 0,
                         x land 4 <> 0,
                         x land 8 <> 0)
        | 104 ->
            Odometry
              (float (get_sint16 frame.data 0) /. 1000.,
               float (get_sint16 frame.data 2) /. 1000.,
               float (get_sint16 frame.data 4) /. 10000.)
        | 105 ->
            Odometry_ghost
              (float (get_sint16 frame.data 0) /. 1000.,
               float (get_sint16 frame.data 2) /. 1000.,
               float (get_sint16 frame.data 4) /. 10000.,
               get_uint8 frame.data 6,
               get_uint8 frame.data 7 <> 0)
        | 201 ->
            Motor_move
              (float (get_sint32 frame.data 0) /. 1000.,
               float (get_uint16 frame.data 4) /. 1000.,
               float (get_uint16 frame.data 6) /. 1000.)
        | 202 ->
            Motor_turn
              (float (get_sint32 frame.data 0) /. 10000.,
               float (get_uint16 frame.data 4) /. 1000.,
               float (get_uint16 frame.data 6) /. 1000.)
        | 203 ->
            Set_odometry
              (float (get_sint16 frame.data 0) /. 1000.,
               float (get_sint16 frame.data 2) /. 1000.,
               float (get_sint16 frame.data 4) /. 10000.)
        | 204 ->
            Motor_stop
              (get_float32 frame.data 0,
               get_float32 frame.data 4)
        | 205 ->
            Set_controller_mode
              (get_uint8 frame.data 0 <> 0)
        | 206 ->
            let x, y, d1, d2, theta, v = decode_bezier frame.data in
            Motor_bezier(float x /. 1000.,
                         float y /. 1000.,
                         float d1 /. 100.,
                         float d2 /. 100.,
                         float theta /. 100.,
                         float v /. 1000.)
        | 231 ->
            Elevator(get_float32 frame.data 0,
                     get_float32 frame.data 4)
        | 301 ->
            Beacon_position
              (float (get_uint16 frame.data 0) /. 10000.,
               float (get_uint16 frame.data 2) /. 1000.,
               float (get_uint16 frame.data 4) /. 10000.)
        | 302 ->
            Beacon_lowlevel_position
              (float (get_uint16 frame.data 0) /. 10000.,
               float (get_uint16 frame.data 2) /. 100000.,
               get_uint32 frame.data 4)
        | 311 ->
            Switch1_status
              (get_uint8 frame.data 0 <> 0,
               get_uint8 frame.data 1 <> 0,
               get_uint8 frame.data 2 <> 0,
               get_uint8 frame.data 3 <> 0,
               get_uint8 frame.data 4 <> 0,
               get_uint8 frame.data 5 <> 0,
               get_uint8 frame.data 6 <> 0,
               get_uint8 frame.data 7 <> 0)
        | 312 ->
            Switch2_status
              (get_uint8 frame.data 0 <> 0,
               get_uint8 frame.data 1 <> 0,
               get_uint8 frame.data 2 <> 0,
               get_uint8 frame.data 3 <> 0,
               get_uint8 frame.data 4 <> 0,
               get_uint8 frame.data 5 <> 0,
               get_uint8 frame.data 6 <> 0,
               get_uint8 frame.data 7 <> 0)
        | 313 ->
            Switch_request
              (get_uint8 frame.data 0,
               get_uint8 frame.data 1 <> 0)
        | 321 ->
            Adc1_values
              (get_uint16 frame.data 0,
               get_uint16 frame.data 2,
               get_uint16 frame.data 4,
               get_uint16 frame.data 6)
        | 322 ->
            Adc2_values
              (get_uint16 frame.data 0,
               get_uint16 frame.data 2,
               get_uint16 frame.data 4,
               get_uint16 frame.data 6)
        | 331 ->
            Battery1_voltages
              (float (get_uint16 frame.data 0) /. 10000.,
               float (get_uint16 frame.data 2) /. 10000.,
               float (get_uint16 frame.data 4) /. 10000.,
               float (get_uint16 frame.data 6) /. 10000.)
        | 332 ->
            Battery2_voltages
              (float (get_uint16 frame.data 0) /. 10000.,
               float (get_uint16 frame.data 2) /. 10000.,
               float (get_uint16 frame.data 4) /. 10000.,
               float (get_uint16 frame.data 6) /. 10000.)
        | 341 ->
            Ax12_State
              (get_uint8 frame.data 0,
               get_uint16 frame.data 1,
               get_uint16 frame.data 3,
               get_uint16 frame.data 5)
        | 342 ->
            Ax12_Request_State
              (get_uint8 frame.data 0)
        | 343 ->
            Ax12_Goto
              (get_uint8 frame.data 0,
               get_uint16 frame.data 1,
               get_uint16 frame.data 3)
        | 344 ->
            Ax12_Reset
              (get_uint8 frame.data 0)
        | _ ->
            Unknown frame
  with Invalid_argument _ ->
    raise (Invalid_frame frame)

(* +-----------------------------------------------------------------+
   | Sending/receiving messages                                      |
   +-----------------------------------------------------------------+ *)

let send bus (timestamp, msg) =
  Krobot_bus.send bus (timestamp, Krobot_bus.CAN(Krobot_bus.Info, encode msg))
let recv bus =
  E.fmap
    (fun (timestamp, message) ->
       match message with
         | Krobot_bus.CAN(_, frame) ->
             Some(timestamp, decode frame)
         | _ ->
             None)
    (Krobot_bus.recv bus)
