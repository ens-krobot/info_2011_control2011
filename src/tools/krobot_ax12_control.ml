open Arg
open Krobot_bus
open Krobot_message

type action =
  | Sleep of float
  | Do of Krobot_message.t

let run_file = ref None

let ax12_id = ref 0
let ax12_speed = ref 1
let commands = ref []
let add_goto pos = commands := ( Do (Ax12_Goto (!ax12_id, pos, !ax12_speed)) ) :: !commands
let add_sleep t = commands := ( Sleep t ) :: !commands
let add_torque b = commands := ( Do (Ax12_Set_Torque_Enable (!ax12_id, b)) ) :: !commands
let add_request () = commands := ( Do (Ax12_Request_State !ax12_id) ) :: !commands
let spec =
  [ "-id", Set_int ax12_id, "id of next ax12 command";
    "-i", Set_int ax12_id, "-id";
    "-speed", Set_int ax12_speed, "speed of next ax12 command";
    "-s", Set_int ax12_speed, "-speed";
    "-goto", Int add_goto, "add a goto command";
    "-g", Int add_goto, "-goto";
    "-torque", Bool add_torque, "set torque status";
    "-t", Bool add_torque, "-torque";
    "-state", Unit add_request, "request the ax12 state";
    "-sleep", Float add_sleep, "sleep";
    "-file", String (fun s -> run_file := Some s), "run sequence from file";
  ]

let msg = "do things with ax12"
let message _ = usage spec msg; flush stdout; exit 0
let () = Arg.parse spec message msg
let () = if !commands = [] && !run_file = None then message ()
lwt bus = Krobot_bus.get ()
let run = function
  | Sleep t -> Lwt_unix.sleep t
  | Do c -> Krobot_bus.send bus (Unix.gettimeofday (), CAN (Info, Krobot_message.encode c))

let rec go = function
  | [] -> Lwt.return ()
  | t::q ->
    lwt () = run t in
    go q

type ax12_info =
    { id : int;
      pos : float;
      speed : float;
      torque : float;
      time : float }

let fscan_info ic =
  Scanf.fscanf ic "id %i,pos %f, speed %f, torque %f, time %f\n"
    (fun id pos speed torque time -> { id; pos; speed; torque; time; })

let read ic =
  let rec aux acc =
    try
      let v = fscan_info ic in
      aux (v::acc)
    with
      | End_of_file
      | Scanf.Scan_failure _ -> List.rev acc
  in
  aux []

let to_actions l =
  let f (t,l) v =
    v.time, ( Do (Ax12_Goto (v.id, int_of_float v.pos, int_of_float v.speed)) ) :: (Sleep (v.time -. t)) :: l in
  let _, l = List.fold_left f (0.,[]) l in
  List.rev l

lwt () =
  match !run_file with
    | None -> go (List.rev !commands)
    | Some f ->
      let actions = to_actions (read (open_in f)) in
      go actions
