open Arg
open Krobot_bus
open Krobot_message

type action =
  | Sleep of float
  | Do of Krobot_message.t

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
  ]
let msg = "do things with ax12"
let message _ = usage spec msg; flush stdout; exit 0
let () = Arg.parse spec message msg
let () = if !commands = [] then message ()
lwt bus = Krobot_bus.get ()
let run = function
  | Sleep t -> Lwt_unix.sleep t
  | Do c -> Krobot_bus.send bus (Unix.gettimeofday (), CAN (Info, Krobot_message.encode c))

let rec go = function
  | [] -> Lwt.return ()
  | t::q ->
    lwt () = run t in
    go q

lwt () = go (List.rev !commands)
