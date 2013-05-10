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
  start_condition : unit Lwt_condition.t;
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

let slave_start = "KBSS\r"
let master_acknowledge = "KBMA\r"
let kb_match_start = "KBMS\r"
let kb_start_get_team = "KBSGT\r"
let team_red = "KBMTR\r"
let team_blue = "KBMTB\r"

let slave_start_matching =
  Str.regexp (".*"^slave_start^".*"), String.length slave_start

let slave_get_team =
  Str.regexp (".*"^kb_start_get_team^".*"), String.length kb_start_get_team

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

let rec read_string fd =
  let s = String.create 20 in
  lwt c = Lwt_unix.read fd s 0 (String.length s) in
  if c = 0
  then
    lwt () = Lwt_unix.sleep 0.1 in
    read_string fd
  else
    Lwt.return (String.sub s 0 c)

let send_string fd s =
  let rec iter n =
    lwt c = Lwt_unix.write fd s n (String.length s - n) in
    if c + n < String.length s
    then
      lwt () =
        if c = 0
        then Lwt_unix.sleep 0.1
        else Lwt.return () in
      iter (n + c)
    else Lwt.return ()
  in
  lwt () = iter 0 in
  lwt () = Lwt_log.info_f ~section "sent: %s" s in
  Lwt.return ()

let rec read_until_pattern matching acc fd =
  lwt () = Lwt_log.info_f ~section "wait msg" in
  lwt s = read_string fd in
  lwt () = Lwt_log.info_f ~section "recv %s" s in
  let s = acc ^ s in
  match get_string matching s with
  | Matched -> Lwt.return ()
  | Not_matched acc -> read_until_pattern matching acc fd

let rec answer_slave_start info fd =
  lwt () = Lwt.pick
      [(read_until_pattern slave_start_matching "" fd);
       (Lwt_condition.wait info.start_condition)] in
  lwt () = send_string fd master_acknowledge in
  if info.started
  then Lwt.return ()
  else answer_slave_start info fd

let loop_match_start info fd =
  let answered = ref false in
  let rec send () =
    if !answered
    then Lwt.return ()
    else
      lwt () = Lwt_unix.sleep 0.05 in
      lwt () = send_string fd kb_match_start in
      send ()
  in
  let t = send () in
  let _ =
    lwt () = read_until_pattern slave_get_team "" fd in
    answered := true;
    Lwt.return ()
  in
  t

let rec recv fd =
  lwt s = read_string fd in
  lwt () = Lwt_log.info_f ~section "recv %s" s in
  recv fd

let rec loop_team info fd =
  let msg = match info.team with
  | `Red -> team_red
  | `Blue -> team_blue in
  lwt () = send_string fd msg in
  lwt () = Lwt_unix.sleep 0.2 in
  loop_team info fd

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
      info.started <- true;
      Lwt_condition.broadcast info.start_condition ()

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

  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  let info = {
    bus = bus;
    team = `Red;
    jack = false;
    started = false;
    start_condition = Lwt_condition.create ();
  } in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message info) (Krobot_bus.recv bus));

  (* Kill any running urg_handler. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "xbee") in

  (* Loop forever. *)
  lwt () = Lwt_log.info ~section "wait slave" in
  lwt () = answer_slave_start info fd in
  lwt () = Lwt_log.info ~section "match started" in
  lwt () = loop_match_start info fd in
  let _ = recv fd in
  lwt () = Lwt_log.info ~section "send team" in
  loop_team info fd
