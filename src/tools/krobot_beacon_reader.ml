(*
 * krobot_beacon_reader.ml
 * ----------------
 * Copyright : (c) 2013, Pierre Chambart
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Read beacon messages on serial port and broadcast it on the bus. *)

open Lwt
open Lwt_react
open Lwt_preemptive
open Krobot_bus
open Krobot_message

let section = Lwt_log.Section.make "krobot(beacon_reader)"

let open_serial path rate =
  lwt fd = Lwt_unix.openfile path [Unix.O_RDWR; Unix.O_NONBLOCK] 0o660 in
  let tio = {
    (* taken from minicom *)
    Unix.c_ignbrk = true; Unix.c_brkint = false; Unix.c_ignpar = false;
    Unix.c_parmrk = false; Unix.c_inpck = false; Unix.c_istrip = false;
    Unix.c_inlcr = false; Unix.c_igncr = false; Unix.c_icrnl = false;
    Unix.c_ixon = false; Unix.c_ixoff = false; Unix.c_opost = false;
    Unix.c_obaud = rate; Unix.c_ibaud = rate; Unix.c_csize = 8;
    Unix.c_cstopb = 1; Unix.c_cread = true; Unix.c_parenb = false;
    Unix.c_parodd = false; Unix.c_hupcl = false; Unix.c_clocal = true;
    Unix.c_isig = false; Unix.c_icanon = false; Unix.c_noflsh = false;
    Unix.c_echo = false; Unix.c_echoe = false; Unix.c_echok = false;
    Unix.c_echonl = false; Unix.c_vintr = '\000'; Unix.c_vquit = '\000';
    Unix.c_verase = '\000'; Unix.c_vkill = '\000'; Unix.c_veof = '\000';
    Unix.c_veol = '\000'; Unix.c_vmin = 1; Unix.c_vtime = 5;
    Unix.c_vstart = '\000'; Unix.c_vstop = '\000'
  } in
  lwt () = Lwt_unix.tcsetattr fd Unix.TCSAFLUSH tio in
  return fd

let rec get_packet ic =
  lwt line = Lwt_io.read_line ic in
  try
    let s = Scanf.sscanf line "%i %i %i %i %i %i %i %i %i %i %i "
        (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 ->
          x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) in
    (* let x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11 = s in *)
    (* lwt () = Lwt_log.info_f ~section "%i %i %i %i %i %i %i %i %i %i %i " *)
    (*     x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 in *)
    return s
  with _ -> get_packet ic

(* +-----------------------------------------------------------------+
   | read/send loop                                                  |
   +-----------------------------------------------------------------+ *)

let loop bus ic =
  let rec aux () =
    lwt packet = get_packet ic in
    let time = Unix.gettimeofday () in
    let msg = Beacon_raw packet in
    lwt () = Krobot_bus.send bus (time, msg) in
    aux () in
  aux ()

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message (timestamp, message) =
  match message with
    | Kill "beacon_reader" ->
      exit 0
    | _ ->
        ()

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true
let tty = ref "/dev/ttyUSB0"
let baudrate = ref 57600

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
  "-tty", Arg.Set_string tty, " set tty file";
  "-baudrate", Arg.Set_int baudrate, " set tty baudrate file";
]

let usage = "\
Usage: krobot-beacon-reader [options]
options are:"

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

lwt () =
  Arg.parse options ignore usage;

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  lwt fd = open_serial !tty !baudrate in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in

  lwt _ = get_packet ic in

  lwt () = Lwt_log.info ~section "got first packet" in

  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  (* Handle krobot message. *)
  E.keep (E.map handle_message (Krobot_bus.recv bus));

  (* Kill any running urg_handler. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "beacon_reader") in

  (* Loop forever. *)
  loop bus ic
