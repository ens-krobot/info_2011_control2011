(*
 * krobot_can_bus.ml
 * -----------------
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

external open_can_file_descr : string -> Unix.file_descr = "ocaml_can_open_can_file_descr"

let open_can iface =
  let fd = open_can_file_descr iface in
  return (new t (Lwt_unix.of_unix_file_descr fd))

let close bus = bus#close

(* +-----------------------------------------------------------------+
   | Sending/receiving frames                                        |
   +-----------------------------------------------------------------+ *)

external forge_frame : Krobot_can.frame -> string = "ocaml_can_forge_frame"
external parse_frame : string -> Krobot_can.frame = "ocaml_can_parse_frame"
external get_frame_size : unit -> int = "ocaml_can_get_frame_size" "noalloc"

let frame_size = get_frame_size ()

let send bus frame =
  Lwt_io.write bus#oc (forge_frame frame)

let recv bus =
  let buffer = String.create frame_size in
  lwt () = Lwt_io.read_into_exactly bus#ic buffer 0 frame_size in
  return (parse_frame buffer)
