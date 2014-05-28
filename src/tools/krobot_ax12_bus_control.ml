open Arg
open Krobot_bus
open Krobot_message
open Krobot_ax12_format

let ax12_positions = ref IntMap.empty
let frame_prec = 5

let run_action bus = function
  | Sleep t -> Lwt_unix.sleep t
  | Do c ->
    let c = Ax12_Goto (c.id, c.pos, c.speed) in
    Krobot_bus.send bus (Unix.gettimeofday (), CAN (Info, Krobot_message.encode c))

let run_frame bus cur_frame next_frame speed =
  (* find positions differences *)
  let cur_frame_l = IntMap.bindings cur_frame in
  let next_frame_l = IntMap.bindings next_frame in
  let all_zeros, diffs = List.fold_left2
      (fun (all_zeros, acc) (cur_id, cur_pos) (next_id, next_pos) ->
         let diff = abs (next_pos - cur_pos) in
         let all_zeros = if diff == 0 then all_zeros else false in
         all_zeros, ((cur_id, diff, next_pos)::acc))
      (true, []) cur_frame_l next_frame_l in
  lwt () = if all_zeros then
    Lwt_list.iter_s
      (fun (cur_id, _, next_pos) ->
         let c = Ax12_Goto (cur_id, next_pos, speed) in
         Krobot_bus.send bus (Unix.gettimeofday (), CAN (Info, Krobot_message.encode c)))
      diffs
  else begin
    let max_diff = List.fold_left
        (fun max_diff (cur_id, diff, next_pos) ->
           if diff > max_diff then diff else max_diff)
        0
        diffs in
    Lwt_list.iter_s
      (fun (cur_id, diff, next_pos) ->
         if diff > 0 then
           let axe_speed = speed * diff / max_diff in
           let c = Ax12_Goto (cur_id, next_pos, axe_speed) in
           Krobot_bus.send bus (Unix.gettimeofday (), CAN (Info, Krobot_message.encode c))
         else
           Lwt.return_unit)
      diffs
  end in
  let check_end_mvt () =
    let end_reached, _ =
      List.fold_left
        (fun (end_reached,check_sth) (id, pos) ->
           (*let ax12_real_position = if IntMap.mem id !ax12_positions then IntMap.find id !ax12_positions else -1 in
           Printf.printf "Checking id %d with pos %d against pos %d (%B,%B)\n%!" id pos ax12_real_position end_reached check_sth;*)
           if check_sth then
             if IntMap.mem id !ax12_positions then
               if abs ((IntMap.find id !ax12_positions)-pos) <= frame_prec then
                 (end_reached,false)
               else
                 (false,false)
             else
               (false,false)
           else
             (end_reached,check_sth))
        (true,true)
        next_frame_l in
    end_reached
  in
  let ask_ax12_positions () =
    let request_state_list = match next_frame_l with
      | t::q -> [t]
      | [] -> []
    in
    Lwt_list.iter_s
      (fun (id, _) ->
         lwt () = Krobot_bus.send bus (Unix.gettimeofday (), CAN (Info, Krobot_message.encode (Ax12_Request_State id))) in
         Lwt_unix.sleep 0.01)
      request_state_list
  in
  let rec wait_for_mvt () =
    lwt () = Lwt_unix.sleep 0.2 in
    if not (check_end_mvt ()) then
      lwt () = ask_ax12_positions () in
      lwt () = Lwt_unix.sleep 0.1 in
      wait_for_mvt ()
    else
      Lwt.return_unit
  in
  wait_for_mvt ()

let rec run_actions bus = function
  | [] -> Lwt.return_unit
  | h::t ->
    lwt () = run_action bus h in
    run_actions bus t

let run_framed_actions bus keyframes sequence =
  let rec aux cur_frame = function
    | [] -> Lwt.return_unit
    | (index, speed)::t ->
      Printf.printf "Going to frame %d\n%!" index;
      let next_frame = IntMap.find index keyframes in
      lwt () = run_frame bus cur_frame next_frame speed in
      aux next_frame t
  in
  match sequence with
    | [] -> Lwt.return_unit
    | (index, _)::t ->
      Printf.printf "Starting from frame %d\n%!" index;
      let init_frame = IntMap.find index keyframes in
      aux init_frame t

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

let launch_framed bus name keyframes sequence =
  if StringSet.mem name !running
  then Lwt_log.warning_f "dropped already running sequence %s" name
  else begin
    running := StringSet.add name !running;
    lwt () = run_framed_actions bus keyframes sequence in
    running := StringSet.remove name !running;
    Krobot_bus.send bus (Unix.gettimeofday (), Finished_ax12_sequence name)
  end

let handle_message bus (timestamp, message) =
  match message with
    | Kill "ax12_bus_control" ->
      exit 0
    | Run_ax12_sequence (name,sequence) ->
      ignore(launch bus name sequence:unit Lwt.t)
    | Run_ax12_framed_sequence (name,keyframes,sequence) ->
      ignore(launch_framed bus name keyframes sequence:unit Lwt.t)
    | CAN(_, frame) ->
      begin
        match Krobot_message.decode frame with
        | Ax12_State (id,pos,_,_) ->
          ax12_positions := IntMap.add id pos !ax12_positions
        | _ -> ()
      end
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
