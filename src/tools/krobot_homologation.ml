(*
 * krobot_homologation.ml
 * ------------
 * Copyright : (c) 2012, Pierre Chambart <chambart@crans.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* The simple ai for homologation. *)

open Lwt
open Lwt_react
open Krobot_message
open Krobot_bus
open Krobot_action
open Krobot_geom

let init_pos, init_angle = Krobot_config.red_initial_position

let path =
  [
    { x = 0.7; y = init_pos.y -. 0.1};
    { x = 0.85; y = 1.5 };
    { x = 0.75; y = 1.20 };
    { x = 0.55; y = 1.15 };
    { x = 0.4; y = 1.15 };
  ]

let strat_loop =
  [
    Calibrate  ( { x = 0.64 +. 0.477; y = 1. +. 0.125 +. 0.1; },
                 pi /. 2.,
                 0.20,
                 None,
                 Some (1. +. 0.125),
                 Some (pi /. 2.) );

    Calibrate  ( { x = 0.9; y = 1.; },
                 pi,
                 0.20,
                 Some (0.64 +. 0.477 -. 0.125),
                 None,
                 Some (pi) );

    Goto ({ x = 1.5; y = 0.5 }, None);
    Goto ({ x = 1.5; y = 1.5 }, None);
    Goto ({ x = 1.5; y = 0.5 }, None);
    Goto ({ x = 1.5; y = 1.5 }, None);
    Wait_for 2.;
    End;
  ]

let strat_base =
  [
    Can (Krobot_message.encode (Drive_activation true));
    Wait_for_jack true;
    Wait_for 1.;
    Wait_for_jack false;
    Start_timer;
    Set_led(`Red,false);
    Set_led(`Green,false);
    Reset_odometry `Auto;
    Wait_for_odometry_reset `Auto;
    Set_limits (0.3,1.0,1.0);
    End;
  ]

let launch bus =
  Krobot_bus.send bus
    (Unix.gettimeofday (),
     Strategy_set strat_base)

let loop bus =
  Krobot_bus.send bus
    (Unix.gettimeofday (),
     Strategy_set strat_loop)

type status = {
  bus : Krobot_bus.t;
  (* The bus used to communicate with the robot. *)
  mutable team : [ `Red | `Blue ];
  (* The state of the team selector. *)
}

let update_team_led status =
  let m1,m2 =
    if status.team = `Red then
      Switch_request(7,true), Switch_request(6,false)
    else
      Switch_request(7,false), Switch_request(6,true)
  in
  lwt () = Krobot_message.send status.bus (Unix.gettimeofday (),m1) in
  Krobot_message.send status.bus (Unix.gettimeofday (), m2)

lwt bus = Krobot_bus.get ()

let handle_message status (timestamp, message) =
  match message with
    | CAN(_, frame) -> begin
        match decode frame with
          | Switch1_status(jack, team, emergency, _, _, _, _, _) ->
            let team = if team then `Red else `Blue in
            if team <> status.team
            then
              begin
                status.team <- team;
                ignore (update_team_led status);
                ignore (launch bus)
              end
          | _ ->
              ()
      end

    | Strategy_finished ->
      ignore (loop bus)

    | Kill "homologation" ->
        exit 0

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
Usage: krobot-homologation [options]
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
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "homologation") in

  let status = {
    bus;
    team = `Red;
  } in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message status) (Krobot_bus.recv bus));

  (* Wait forever. *)
  fst (wait ())
