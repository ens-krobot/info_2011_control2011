open Arg
open Krobot_bus
open Krobot_message
open Krobot_ax12_format

let run_action bus = function
  | Sleep t -> Lwt_unix.sleep t
  | Do c ->
    let c = Ax12_Goto (c.id, c.pos, c.speed) in
    Krobot_bus.send bus (Unix.gettimeofday (), CAN (Info, Krobot_message.encode c))

let rec run_actions bus = function
  | [] -> Lwt.return_unit
  | h::t ->
    lwt () = run_action bus h in
    run_actions bus t

module StringSet = Set.Make(String)

let running = ref StringSet.empty

let launch bus name sequence =
  if StringSet.mem name !running
  then Lwt_log.warning_f "dropped already running sequence %s" name
  else begin
    running := StringSet.add name !running;
    lwt () = run_actions bus sequence in
    running := StringSet.remove name !running;
    Krobot_bus.send bus (Unix.gettimeofday (), Finished_ax12_sequence name)
  end

let handle_message bus (timestamp, message) =
  match message with
    | Kill "ax12_bus_control" ->
      exit 0
    | Run_ax12_sequence (name,sequence) ->
      ignore(launch bus name sequence:unit Lwt.t)
    | _ ->
        ()

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
]

let usage = "\
Usage: krobot-ax12-bus-control [options]
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

  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  (* Handle krobot message. *)
  Lwt_react.(E.keep (E.map (handle_message bus) (Krobot_bus.recv bus)));

  (* Kill any running ax12_bus_control. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "ax12_bus_control") in

  (* Loop forever. *)
  fst (Lwt.wait ())
