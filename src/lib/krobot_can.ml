(*
 * krobot_can.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* +-----------------------------------------------------------------+
   | CAN Frames                                                      |
   +-----------------------------------------------------------------+ *)

exception Invalid_frame of string

type kind =
  | Data
  | Error

type format =
  | F11bits
  | F29bits

type frame = {
  identifier : int;
  kind : kind;
  remote : bool;
  format : format;
  data : string;
}

let identifier frame = frame.identifier
let kind frame = frame.kind
let remote frame = frame.remote
let format frame = frame.format
let data frame = frame.data

let frame ~identifier ~kind ~remote ~format ~data =
  if String.length data > 8 then
    raise (Invalid_frame "too much data");
  if identifier < 0 then
    raise (Invalid_frame "identifier is negative");
  let max_id =
    match format with
      | F11bits -> 1 lsl 11 - 1
      | F29bits -> 1 lsl 29 - 1
  in
  if identifier > max_id then
    raise (Invalid_frame "identifier is too big");
  { identifier; kind; remote; format; data }

(* +-----------------------------------------------------------------+
   | Reading/writing numbers                                         |
   +-----------------------------------------------------------------+ *)

let get_sint8 str ofs =
  let v = Char.code str.[ofs] in
  if v land 0x80 = 0 then
    v
  else
    v - (1 lsl 8)

let get_uint8 str ofs =
  Char.code str.[ofs]

let get_sint16 str ofs =
  let v0 = Char.code str.[ofs + 0]
  and v1 = Char.code str.[ofs + 1] in
  let v = v0 lor (v1 lsl 8) in
  if v1 land 0x80 = 0 then
    v
  else
    v - (1 lsl 16)

let get_uint16 str ofs =
  let v0 = Char.code str.[ofs + 0]
  and v1 = Char.code str.[ofs + 1] in
  v0 lor (v1 lsl 8)

let get_sint32 str ofs =
  let v0 = Char.code str.[ofs + 0]
  and v1 = Char.code str.[ofs + 1]
  and v2 = Char.code str.[ofs + 2]
  and v3 = Char.code str.[ofs + 3] in
  let v = v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24) in
  if v3 land 0x80 = 0 then
    v
  else
    v - (1 lsl 32)

let get_uint32 str ofs =
  let v0 = Char.code str.[ofs + 0]
  and v1 = Char.code str.[ofs + 1]
  and v2 = Char.code str.[ofs + 2]
  and v3 = Char.code str.[ofs + 3] in
  v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24)

let put_sint8 str ofs v =
  str.[ofs] <- Char.unsafe_chr v

let put_uint8 = put_sint8

let put_sint16 str ofs v =
  str.[ofs + 0] <- Char.unsafe_chr v;
  str.[ofs + 1] <- Char.unsafe_chr (v lsr 8)

let put_uint16 = put_sint16

let put_sint32 str ofs v =
  str.[ofs + 0] <- Char.unsafe_chr v;
  str.[ofs + 1] <- Char.unsafe_chr (v lsr 8);
  str.[ofs + 1] <- Char.unsafe_chr (v lsr 16);
  str.[ofs + 1] <- Char.unsafe_chr (v lsr 24)

let put_uint32 = put_sint32

(* +-----------------------------------------------------------------+
   | D-Bus value conversion                                          |
   +-----------------------------------------------------------------+ *)

open OBus_value

let typ = C.structure (C.seq5 C.basic_uint32 C.basic_uint32 C.basic_boolean C.basic_uint32 C.byte_array)

let value_of_frame frame =
  C.make_single
    typ
    (Int32.of_int frame.identifier,
     (match frame.kind with
        | Data -> 0l
        | Error -> 1l),
     frame.remote,
     (match frame.format with
        | F11bits -> 0l
        | F29bits -> 1l),
     frame.data)

let frame_of_values (identifier, kind, remote, format, data) =
  let identifier = Int32.to_int identifier in
  let kind =
    match kind with
      | 0l -> Data
      | 1l -> Error
      | n -> Printf.ksprintf failwith "Krobot_can.frame_of_value: invalid frame kind (%ld)" n
  in
  let format =
    match format with
      | 0l -> F11bits
      | 1l -> F29bits
      | n -> Printf.ksprintf failwith "Krobot_can.frame_of_value: invalid frame format (%ld)" n
  in
  frame ~identifier ~kind ~remote ~format ~data

let frame_of_value v =
  frame_of_values (C.cast_single typ v)

(* +-----------------------------------------------------------------+
   | Sending/receiving frames                                        |
   +-----------------------------------------------------------------+ *)

let send bus frame =
  OBus_connection.send_message
    bus
    (OBus_message.signal
       ~path:["fr"; "krobot"; "CAN"]
       ~interface:"fr.krobot.CAN"
       ~member:"message"
       [value_of_frame frame])

let frames bus =
  let proxy = OBus_proxy.make (OBus_peer.anonymous bus) ["fr"; "krobot"; "CAN"] in
  OBus_signal.map frame_of_values (OBus_signal.make Krobot_interface_can.Fr_krobot_CAN.s_message proxy)
