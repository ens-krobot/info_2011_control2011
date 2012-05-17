open Krobot_bus
open Krobot_message

let bus = Lwt_unix.run (Krobot_bus.get ())
let send m = Lwt_unix.run (Krobot_message.send bus (Unix.gettimeofday (), m))

let beacon_speed s = send (Motor_command (2,s))

let () = beacon_speed 0
