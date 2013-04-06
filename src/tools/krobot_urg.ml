(*
 * krobot_urg.ml
 * ----------------
 * Copyright : (c) 2013, Pierre Chambart
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Service providing raw urg data. *)

open Lwt
open Lwt_react
open Lwt_preemptive
open Krobot_bus
open Krobot_message

let section = Lwt_log.Section.make "krobot(urg)"

(* +-----------------------------------------------------------------+
   | read/send loop                                                  |
   +-----------------------------------------------------------------+ *)

let to_array (b:Urg.point_data) =
  let dim = Bigarray.Array1.dim b in
  let a = Array.create dim 0 in
  for i = 0 to dim - 1 do
    a.(i) <- Nativeint.to_int b.{i};
  done;
  a

let loop bus urg =
  let rec aux () =
    let time = Unix.gettimeofday () in
    lwt _ = Lwt_preemptive.detach Urg_simple.get urg in
    let msg = Urg (to_array urg.Urg_simple.data) in
    lwt () = Krobot_bus.send bus (time, msg) in
    lwt () = Lwt_log.info_f "send things" in
    lwt () = Lwt_unix.sleep 0.01 in
    aux () in
  aux ()

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let urg = ref None

let handle_message (timestamp, message) =
  match message with
    | Kill "urg" ->
      begin match !urg with
        | None -> ()
        | Some urg ->
          Urg.urg_disconnect urg.Urg_simple.urg end;
      exit 0
    | _ ->
        ()

let min_val = 10

let less a b =
  if a < min_val
  then false
  else if b < min_val
  then true
  else a < b

let extrema a cmp =
  let m = ref a.(0) in
  let pos = ref 0 in
  for i = 0 to Array.length a - 1 do
    let v = a.(i) in
    if cmp v !m
    then (m := v; pos := i)
  done;
  !m, !pos

let handle_listener (timestamp, message) =
  match message with
    | Urg data ->
      let min, pos_min = extrema data less in
      let rad_min = Krobot_config.urg_angles.(pos_min) in
      Lwt_io.printf "%i %f\n%!" min rad_min
    | _ -> Lwt.return ()

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true
let listen = ref false

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
  "-listen", Arg.Set listen, " listen results";
]

let usage = "\
Usage: krobot-urg [options]
options are:"

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let run_sender bus =
  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  (* Handle krobot message. *)
  E.keep (E.map handle_message (Krobot_bus.recv bus));

  (* Kill any running urg_handler. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "urg") in

  (* Wait a bit to let the other handler release the connection *)
  lwt () = Lwt_unix.sleep 0.4 in

  let local_urg = Urg_simple.init () in
  urg := Some local_urg;

  (* Loop forever. *)
  loop bus local_urg

let run_listener bus =
  E.keep (E.map_s handle_listener (Krobot_bus.recv bus));
  let t, _ = Lwt.wait () in
  t

lwt () =
  Arg.parse options ignore usage;

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  if !listen
  then run_listener bus
  else run_sender bus
