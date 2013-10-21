#use "topfind";;
#camlp4o;;
#require "lwt.syntax";;
#require "krobot";;
open Krobot_bus;;
open Krobot_message;;
open Krobot_action;;
open Krobot_geom;;

let ax12_2_base_position = 519;;
let ax12_2_high_position = 210;;
let ax12_2_mid_position = 400;;
let ax12_1_mid_position = 600;;
let ax12_1_base_position = 518;;
let ax12_1_high_position = 823;;

let send cmd = ignore(Lwt_unix.run (Krobot_bus.send bus (Unix.gettimeofday (), cmd)))
let send_strat strat = ignore(Lwt_unix.run (Krobot_bus.send bus (Unix.gettimeofday (), Strategy_set strat)))

let update_team_led team =
  let m1,m2 =
    if team = `Red then
      Switch_request(7,false), Switch_request(6,true)
    else
      Switch_request(7,true), Switch_request(6,false)
  in
  lwt () = Krobot_message.send bus (Unix.gettimeofday (),m1) in
  Krobot_message.send bus (Unix.gettimeofday (), m2)

let hit_ax12_id id =
  let base, high =
    if id = 1
    then ax12_1_base_position, ax12_1_high_position
    else ax12_2_base_position, ax12_2_high_position
  in
  Node (Simple,
        [Can (Krobot_message.encode (Ax12_Set_Torque_Enable (id,true)));
         Wait_for 0.1;
         Can (Krobot_message.encode (Ax12_Goto (id, high, 0)));
         Wait_for 0.3;
         Can (Krobot_message.encode (Ax12_Goto (id, base, 0)));
         Wait_for 0.3])

let ax12_activate id status =
  send_strat ([Node (Simple, [
      Can (Krobot_message.encode (Ax12_Set_Torque_Enable (id, status)))
    ])
    ])

let ax12_goto id angle speed =
  send_strat ([Node (Simple, [
      Can (Krobot_message.encode (Ax12_Goto (id, angle, speed)))
    ])
    ])

let hit_side side =
  (if side = 1 then
    run_lwt (update_team_led `Red)
  else
    run_lwt (update_team_led `Blue));
  let strat = [ hit_ax12_id side ] in
  run_lwt (
    Krobot_bus.send bus (Unix.gettimeofday (), Strategy_set strat)
  )

let () = hit_side 1
let () = hit_side 2

let () = ax12_activate 1 true
let () = ax12_goto 1 ax12_1_high_position 300
let () = ax12_goto 1 ax12_1_mid_position 300
let () = ax12_goto 1 ax12_1_base_position 0
let () = ax12_activate 1 false
