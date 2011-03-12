(*
 * krobot_can.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

let get_int8 str ofs =
  let v = Char.code str.[ofs] in
  if v land 0x80 = 0 then
    v
  else
    v - (1 lsl 8)

let get_uint8 str ofs =
  Char.code str.[ofs]

let get_int16 str ofs =
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

let get_int32 str ofs =
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

let put_int8 str ofs v =
  str.[ofs] <- Char.unsafe_chr v

let put_uint8 = put_int8

let put_int16 str ofs v =
  str.[ofs + 0] <- Char.unsafe_chr v;
  str.[ofs + 1] <- Char.unsafe_chr (v lsr 8)

let put_uint16 = put_int16

let put_int32 str ofs v =
  str.[ofs + 0] <- Char.unsafe_chr v;
  str.[ofs + 1] <- Char.unsafe_chr (v lsr 8);
  str.[ofs + 1] <- Char.unsafe_chr (v lsr 16);
  str.[ofs + 1] <- Char.unsafe_chr (v lsr 24)

let put_uint32 = put_int32
