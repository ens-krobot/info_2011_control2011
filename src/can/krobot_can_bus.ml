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

class t fd = object
  val mutable up = true

  method fd =
    if up then fd else raise Closed

  method close =
    if up then begin
      up <- false;
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

external can_recv : Unix.file_descr -> float * Krobot_can.frame = "ocaml_can_recv"
external can_send : Unix.file_descr -> Krobot_can.frame -> unit = "ocaml_can_send"

let recv bus =
  Lwt_unix.wrap_syscall Lwt_unix.Read bus#fd (fun () -> can_recv (Lwt_unix.unix_file_descr bus#fd))

let send bus (_, frame) =
  Lwt_unix.wrap_syscall Lwt_unix.Write bus#fd (fun () -> can_send (Lwt_unix.unix_file_descr bus#fd) frame)
