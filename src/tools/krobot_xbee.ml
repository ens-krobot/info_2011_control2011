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

type info = {
  bus : Krobot_bus.t;
  (* The bus used to communicate with the robot. *)
  mutable team : [ `Red | `Blue ];
  (* The state of the team selector. *)
  mutable jack : bool;
  (* Status of the jack. *)
  mutable started : bool;
}

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

(* +-----------------------------------------------------------------+
   | read/send loop                                                  |
   +-----------------------------------------------------------------+ *)

let slave_start = "KBSS"
let master_acknowledge = "KBMA"

let slave_start_matching =
  Str.regexp (".*"^slave_start^".*"), String.length slave_start

type matched =
  | Matched
  | Not_matched of string

let string_tail str len =
  if String.length str <= len
  then str
  else String.sub str (String.length str - len) len

let get_string (pattern,length) str =
  if Str.string_match pattern str 0
  then Matched
  else Not_matched (string_tail str (length-1))

let rec read_until_pattern matching acc ic =
  lwt s = Lwt_io.read ic in
  let s = acc ^ s in
  match get_string matching s with
  | Matched -> Lwt.return ()
  | Not_matched acc -> read_until_pattern matching acc ic

let rec loop info ic oc =
  lwt () = read_until_pattern slave_start_matching "" ic in
  lwt () = Lwt_io.write oc master_acknowledge in
  loop info ic oc

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message info (timestamp, message) =
  match message with
    | CAN(_, frame) -> begin
        match decode frame with
        | Switch1_status(jack, team, _, _, _, _, _, _) ->
          info.jack <- not jack;
          info.team <- if team then `Red else `Blue
        | _ ->
          ()
      end

    | Match_start ->
      info.started <- true

    | Kill "xbee" ->
      exit 0
    | _ ->
        ()

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true
let tty = ref "/dev/ttyUSB1"
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
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in

  lwt () = Lwt_log.info ~section "got first packet" in

  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  let info = {
    bus = bus;
    team = `Red;
    jack = false;
    started = false;
  } in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message info) (Krobot_bus.recv bus));

  (* Kill any running urg_handler. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "xbee") in

  (* Loop forever. *)
  loop info ic oc
