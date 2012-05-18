(*
 * krobot_sharps.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Service converting raw sharps data. *)

open Lwt
open Lwt_react
open Krobot_bus
open Krobot_message

let section = Lwt_log.Section.make "krobot(sharps)"

(* +-----------------------------------------------------------------+
   | Distance evaluation                                             |
   +-----------------------------------------------------------------+ *)

let find_distance data measured_value =
  let delta, pos =
    List.fold_left
      (fun (min_delta, pos_at_min) (pos, value) ->
         let delta = abs (measured_value - value) in
         if delta < min_delta then
           (delta, pos)
         else
           (min_delta, pos_at_min))
      (max_int, 0.0)
      data
  in
  if delta > 100 then
    -1.0
  else
    pos

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message bus data sharps (timestamp, message) =
  match message with
    | CAN(_, frame) -> begin
        match decode frame with
          | Adc1_values(s1, s2, s3, s4) ->
              sharps.(0) <- find_distance data.(0) s1;
              sharps.(1) <- find_distance data.(1) s2;
              sharps.(2) <- find_distance data.(2) s3;
              sharps.(3) <- find_distance data.(3) s4;
              ignore (Krobot_bus.send bus (timestamp, Sharps sharps))
          | Adc2_values(s5, s6, s7, s8) ->
              sharps.(4) <- find_distance data.(4) s5;
              sharps.(5) <- find_distance data.(5) s6;
              sharps.(6) <- find_distance data.(6) s7;
              sharps.(7) <- find_distance data.(7) s8;
              ignore (Krobot_bus.send bus (timestamp, Sharps sharps))
          | _ ->
              ()
      end

    | Kill "sharps" ->
        exit 0

    | _ ->
        ()

(* +-----------------------------------------------------------------+
   | Data loading                                                    |
   +-----------------------------------------------------------------+ *)

let load file_name =
  try_lwt
    let data = Array.make 8 [] in
    lwt () =
      Lwt_stream.iter
        (fun line ->
           let id, value, distance = Scanf.sscanf line "%d %d %f" (fun id value distance -> (id, value, distance)) in
           data.(id) <- (distance, value) :: data.(id))
        (Lwt_io.lines_of_file file_name)
    in
    return data
  with exn ->
    lwt () = Lwt_log.error_f ~exn ~section "failed to load sharps data from '%s'" file_name in
    exit 1

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true
let file = ref "sharps.data"

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
]

let usage = "\
Usage: krobot-sharps [options]
options are:"

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

lwt () =
  Arg.parse options (fun f -> file := f) usage;

  (* Load sharps data. *)
  lwt data = load !file in

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  let sharps = Array.make 8 0. in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message bus data sharps) (Krobot_bus.recv bus));

  (* Wait forever. *)
  fst (wait ())
