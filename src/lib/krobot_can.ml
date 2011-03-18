(*
 * krobot_can.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt_react

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

let string_of_frame frame =
  let buf = Buffer.create (String.length frame.data * 4) in
  String.iter (fun ch -> Printf.bprintf buf "\\x%02x" (Char.code ch)) frame.data;
  Printf.sprintf
    "{ identifier = %d; kind = %s; remote = %B; format = %s; data = \"%s\" }"
    frame.identifier
    (match frame.kind with Data -> "Data" | Error -> "Error")
    frame.remote
    (match frame.format with F11bits -> "F11bits" | F29bits -> "F29bits")
    (Buffer.contents buf)

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

let get_float32 str ofs =
  let v0 = Char.code str.[ofs + 0]
  and v1 = Char.code str.[ofs + 1]
  and v2 = Char.code str.[ofs + 2]
  and v3 = Char.code str.[ofs + 3] in
  Int32.float_of_bits
    (Int32.logor
       (Int32.logor
          (Int32.of_int v0)
          (Int32.shift_left (Int32.of_int v1) 8))
       (Int32.logor
          (Int32.shift_left (Int32.of_int v2) 16)
          (Int32.shift_left (Int32.of_int v3) 24)))

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

let put_float32 str ofs v =
  let v = Int32.bits_of_float v in
  str.[ofs + 0] <- Char.unsafe_chr (Int32.to_int v);
  str.[ofs + 1] <- Char.unsafe_chr (Int32.to_int (Int32.shift_right v 8));
  str.[ofs + 2] <- Char.unsafe_chr (Int32.to_int (Int32.shift_right v 16));
  str.[ofs + 3] <- Char.unsafe_chr (Int32.to_int (Int32.shift_right v 24))

(* +-----------------------------------------------------------------+
   | D-Bus value conversion                                          |
   +-----------------------------------------------------------------+ *)

open OBus_value

let typ = C.structure (C.seq6 C.basic_double C.basic_uint32 C.basic_uint32 C.basic_boolean C.basic_uint32 C.byte_array)

let value_of_frame (timestamp, frame) =
  C.make_single
    typ
    (timestamp,
     Int32.of_int frame.identifier,
     (match frame.kind with
        | Data -> 0l
        | Error -> 1l),
     frame.remote,
     (match frame.format with
        | F11bits -> 0l
        | F29bits -> 1l),
     frame.data)

let frame_of_values (timestamp, identifier, kind, remote, format, data) =
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
  (timestamp, frame ~identifier ~kind ~remote ~format ~data)

let frame_of_value v =
  frame_of_values (C.cast_single typ v)

(* +-----------------------------------------------------------------+
   | Sending/receiving frames                                        |
   +-----------------------------------------------------------------+ *)

let send bus arg =
  OBus_connection.send_message
    (Krobot_bus.to_bus bus)
    (OBus_message.signal
       ~path:["fr"; "krobot"; "CAN"]
       ~interface:"fr.krobot.CAN"
       ~member:"message"
       [value_of_frame arg])

let recv bus =
  let proxy = OBus_proxy.make (OBus_peer.anonymous (Krobot_bus.to_bus bus)) ["fr"; "krobot"; "CAN"] in
  E.fmap
    (fun (ctx, frame) ->
       (* Filter messages comming from us. *)
       if OBus_peer.name (OBus_context.sender ctx) = OBus_bus.name (Krobot_bus.to_bus bus) then
         None
       else
         Some frame)
    (E.delay
       (OBus_signal.connect
          (OBus_signal.with_context
             (OBus_signal.map frame_of_values
                (OBus_signal.make Krobot_interface_can.Fr_krobot_CAN.s_message proxy)))))
