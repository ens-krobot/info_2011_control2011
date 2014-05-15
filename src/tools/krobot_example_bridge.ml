open Lwt
open Lwt_react
open Krobot_bus

exception Malformed_result

let separator = Str.regexp_string "\t"

let read_couple s =
  Scanf.sscanf s "(%i,%i)" (fun i j -> i,j)

let read_couples ic =
  lwt s = Lwt_io.read_line ic in
  let l = Str.split separator s in
  let r =
    try List.map read_couple l with
    | _ -> [] in
  Lwt.return r

let parse_result ic = read_couples ic

let run_command oc ic =
    Lwt_io.atomic
      (fun oc ->
         lwt () = Lwt_io.write_char oc 'C' in
         parse_result ic)
      oc

let run_and_send request_id ident bus oc ic =
  lwt l = run_command oc ic in
  let msg = Vision_find_target_response (request_id, ident, l) in
  Krobot_bus.send bus (Unix.gettimeofday (), msg)

(* +-----------------------------------------------------------------+
   | Launch process                                                  |
   +-----------------------------------------------------------------+ *)

let command = "", [|"demo"|]

let handler : (float * Krobot_bus.message -> unit) ref = ref (fun _ -> ())

let handle_message ident oc ic (timestamp, message) =
  match message with
    | Kill "example-bridge" ->
        exit 0
    | _ -> !handler (timestamp, message)

let process_handler ident oc ic bus (timestamp, message) =
  match message with
    | Vision_find_target (request_id, ident')
      when ident = ident' ->
      let _ : 'a Lwt.t = run_and_send request_id ident bus oc ic in
      ()
    | _ ->
        ()

let callback ident bus (process: Lwt_process.process) : 'a Lwt.t =
  let oc = process#stdin in
  let ic = process#stdout in
  handler := (process_handler ident oc ic bus);
  lwt _ = process#status in
  handler := (fun _ -> ());
  Lwt.return ()

let rec launch ident bus =
  lwt () = Lwt_process.with_process command (callback ident bus) in
  lwt () = Lwt_log.warning "process died" in
  launch ident bus

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
]

let usage = "\
Usage: krobot-example-bridge [options]
options are:"

lwt () =
  Arg.parse options ignore usage;

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  (* Kill any running homologation. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "example-bridge") in

  (* Handle krobot message. *)
  E.keep (E.map handle_message (Krobot_bus.recv bus));

  let ident = "example" in

  (* loop forever. *)
  launch ident bus
