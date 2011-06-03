#require "krobot"

open Lwt_react
open Krobot_message
open Krobot_bus
open Krobot_geom

type ax12 =
    { ax12_pos : int;
      ax12_speed : int;
      ax12_torque : int; }

type ax12s =
    { ax12_low : ax12 * ax12;
      ax12_high : ax12 * ax12; }

type state =
    { objects : vertice list;
      ax12 : ax12s;
      jack : bool;
      moving : bool;
      position : vertice;
      orientation : float;
      beacon : vertice option; }

let ax12_state ax12 (id,pos,speed,torque) =
  let s = { ax12_pos = pos; ax12_speed = speed; ax12_torque = torque } in
  match id with
    | 1 -> { ax12 with ax12_low = s,(snd ax12.ax12_low) }
    | 2 -> { ax12 with ax12_low = (fst ax12.ax12_low),s }
    | 3 -> { ax12 with ax12_high = s,(snd ax12.ax12_low) }
    | 4 -> { ax12 with ax12_high = (fst ax12.ax12_low),s }
    | _ ->
      ignore (Lwt_log.warning_f "bad ax12 id: %i" id:unit Lwt.t);
      ax12

let message_condition = Lwt_condition.create ()

let wait_message () = Lwt_condition.wait message_condition

let handle_message state_ref (timestamp, message) =
  let state = !state_ref in
  let state =
    (match message with
      | Trajectory_moving b ->
	{ state with moving = b }
      | Objects objects ->
	{ state with objects = objects }
      | CAN (_,f) ->
	(match decode f with
	  | Odometry(x, y, theta) ->
            { state with position = { x; y };
              orientation = math_mod_float theta (2. *. pi) }
          | Beacon_position(angle, distance, period) ->
            if distance <> 0. then
              let angle = math_mod_float (state.orientation +. Krobot_config.rotary_beacon_index_pos +. angle) (2. *. pi) in
              { state with beacon = Some {
                x = state.position.x +. distance *. cos angle;
                y = state.position.y +. distance *. sin angle;
              } }
	    else
              { state with beacon = None }
	  | Ax12_State (id,pos,speed,torque) ->
	    { state with ax12 = ax12_state state.ax12 (id,pos,speed,torque) }
	  | Switch1_status(b, _, _, _, _, _, _, _) ->
            { state with jack = not b }
	  | _ -> state)
      | _ -> state)
  in
  state_ref := state;
  Lwt_condition.broadcast message_condition

let run_update bus state_ref =
  E.keep (E.map (handle_message state_ref) (Krobot_bus.recv bus))

type waiter =
  | Time of float
  | Grip_open_low_w
  | Grip_close_low_w

let match_waiter state w =
  List.for_all (function
    | Grip_open_low_w ->
      let (a1,a2) = state.ax12.ax12_low in
      a1.ax12_pos > 630 && a2.ax12_pos < 138
    | Grip_close_low_w -> (* TODO position +
			    torque > ( attention au signe: voir olasd ) *)
      failwith "TODO"
    | Time t -> (Unix.gettimeofday () >= t)
  ) w

type action =
  | Can of Krobot_message.t list
  | Bus of Krobot_bus.message list
  | Load
  | Lift_down
  | Lift_up
  | Open_grip_low
  | Close_grip_low
  | Open_grip_up
  | Close_grip_up
  | Move_to_pawn
  | Clear of action list
  | Goto of vertice
  | Wait of waiter list

let rewrite_action bus = function
  | Load ->
    [ Lift_down;
      Open_grip_low;
      Wait [ Grip_open_low_w; Time 0.5 ];
      (* wait long enouth for lift going down *)
      Move_to_pawn;
      Close_grip_low;
      Lift_up;
      Wait [ Grip_close_low_w; Time 0.5 ]; ]
  | Lift_down ->
    [ Can [Elevator (0.,-.1.)] ]
  | Lift_up ->
    [ Can [Elevator (1.,-.1.)] ]
  | Goto v ->
    [ Bus [Trajectory_goto v]]
  | Move_to_pawn ->
    [ ]
  | Close_grip_low ->
    failwith "TODO"
      (* [ Command [Ax12_Goto (1,_,_); Ax12_Goto (2,_,_) ] *)

  | Can v ->
    [Bus (List.map (fun v -> CAN (Info,encode v)) v)]
  | v -> [v]

let init_ax12 =
  { ax12_pos = 0;
    ax12_speed = 0;
    ax12_torque = 0; }

let ax12s =
    { ax12_low = init_ax12,init_ax12;
      ax12_high = init_ax12,init_ax12; }

let init_state =
  { objects = [];
    ax12 = ax12s;
    jack = true;
    beacon = None;
    moving = false;
    position = origin;
    orientation = 0.;
  }

let run_krobot bus state_ref init =
  let rec loop l =
    match l with
      | [] -> return ()
      | (Bus l)::q ->
	lwt () = Lwt_list.iter_s
          (fun m -> Krobot_bus.send bus (Unix.gettimeofday (),m)) l in
          loop q
      | (Wait w)::q as l ->
	if match_waiter !state_ref w
	then loop q
	else lwt () = wait_message () in
             loop l
      | (Clear l)::q ->
	loop l
      | t::q ->
	let l = rewrite_action bus t in
	loop (l@q)
  in
  loop init

let run () =
  lwt bus = Krobot_bus.get () in
  let state = ref init_state in
  run_update bus state;
  lwt () =
    Krobot_message.send bus
      (Unix.gettimeofday (),
       Set_odometry(0.215 -. Krobot_config.robot_size /. 2. +. Krobot_config.wheels_position, 1.885, 0.))
  in
  lwt () =
      for_lwt i = 1 to 4 do
        Krobot_message.send bus
	  (Unix.gettimeofday (),Ax12_Request_State i)
      done
  in
  lwt () = Lwt_unix.sleep 1.0 in
  lwt () =
    while_lwt !state.jack do
      Lwt_unix.sleep 0.1
    done
  in
  run_krobot bus state [] (* todo: do something else than [] *)

