(*
 * CAN.ml
 * ------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt

(* +-----------------------------------------------------------------+
   | Bus types                                                       |
   +-----------------------------------------------------------------+ *)

exception Closed

class t fd =
  let ic = Lwt_io.make ~mode:Lwt_io.input (Lwt_bytes.read fd)
  and oc = Lwt_io.make ~mode:Lwt_io.output (Lwt_bytes.write fd) in
object
  val mutable up = true

  method ic =
    if up then ic else raise Closed

  method oc =
    if up then oc else raise Closed

  method close =
    if up then begin
      up <- false;
      lwt () = Lwt_io.close ic and () = Lwt_io.close oc in
      Lwt_unix.close fd
    end else
      return ()
end

(* +-----------------------------------------------------------------+
   | Openning/closing                                                |
   +-----------------------------------------------------------------+ *)

external open_can_file_descr : string -> Unix.file_descr = "ocaml_can_open_file_descr"

let open_can iface =
  let fd = open_can_file_descr iface in
  return (new t (Lwt_unix.of_unix_file_descr fd))

let close bus = bus#close

(* +-----------------------------------------------------------------+
   | Frames                                                          |
   +-----------------------------------------------------------------+ *)

exception Invalid_frame of string

type frame_type =
  | Type_data
  | Type_error

type frame_format =
  | Format_11bits
  | Format_29bits

type frame = {
  frame_identifier : int;
  frame_type : frame_type;
  frame_remote : bool;
  frame_format : frame_format;
  frame_data : string;
}

external forge_frame : frame -> string = "ocaml_can_forge_frame"
external parse_frame : string -> frame = "ocaml_can_parse_frame"
external get_frame_size : unit -> int = "ocaml_can_get_frame_size" "noalloc"

let frame_size = get_frame_size ()

let check_frame frame =
  if String.length frame.frame_data > 8 then
    raise (Invalid_frame "too much data");
  if frame.frame_identifier < 0 then
    raise (Invalid_frame "identifier is negative");
  let max_id =
    match frame.frame_format with
      | Format_11bits -> 1 lsl 11 - 1
      | Format_29bits -> 1 lsl 29 - 1
  in
  if frame.frame_identifier > max_id then
    raise (Invalid_frame "identifier is too big")

let send bus frame =
  check_frame frame;
  Lwt_io.write bus#oc (forge_frame frame)

let recv bus =
  let buffer = String.create frame_size in
  lwt () = Lwt_io.read_into_exactly bus#ic buffer 0 frame_size in
  return (parse_frame buffer)
