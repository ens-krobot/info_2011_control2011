#use "topfind";;
#camlp4o;;
#require "lwt.syntax";;
#require "krobot";;
open Krobot_bus;;
open Krobot_message;;

let moving = ref false
let roting = ref false

(* le hub et driver doivent être lancés *)
let bus = Lwt_unix.run (Krobot_bus.get ())
let send m = Lwt_unix.run (Krobot_message.send bus (Unix.gettimeofday (), m))

let handle_message (timestamp, message) =
  match message with
    | CAN(_, frame) -> begin
        match decode frame with

          | Motor_status(m,t,_,_) ->
            moving := m;
            roting := t

          | _ ->
              ()
      end
    | _ ->
        ()

(* a n'executer qu'une fois *)
let () = Lwt_react.E.keep (Lwt_react.E.map handle_message (Krobot_bus.recv bus))

let move distance speed acceleration =
  send (Motor_move(distance,speed,acceleration))
(** [Motor_move(distance, speed, acceleration)] command to make
    the robot to move.
    - [distance] is in m
    - [speed] is in m/s
    - [acceleration] is in m/s^2
*)
let turn angle speed acceleration =
  send (Motor_turn(angle,speed,acceleration))
      (** [Motor_turn(angle, speed, acceleration)] command to make the
          robot to turn.
          - [angle] is in rad
          - [speed] is in rad/s
          - [acceleration] is in rad/s^2
      *)

let motor_stop lin_acc rot_acc = send (Motor_stop(lin_acc, rot_acc))
      (** [Motor_stop(lin_acc, rot_acc)] command to stop following the
          current Bezier Spline and the queued ones.
          - [lin_acc] in m/s^2
          - [rot_acc] in rad/s^2
      *)

let wait t = Lwt_unix.run (lwt () = Lwt_unix.sleep 0. in Lwt_unix.sleep t)
(* en secondes *)

let wait_ref_false r =
  while !r do
    wait 0.01;
  done

let beacon_speed s = send (Motor_command (2,s))


let pi = 4. *. atan 1.

let fait_un_truc () =
  wait 2.;
  Printf.printf "go\n%!";
  move 0.5 0.2 0.5;
  wait_ref_false moving;
  turn pi (pi/.2.) (pi/.2.);
  wait_ref_false roting;
  move 0.2 0.2 0.5

let () = move 2. 0.2 0.5
let () = turn (pi/.2.) (pi/.2. /. 4.) (pi/.2. /. 4. /. 1.)
let () = send (Set_odometry( 0.1, 0.2, pi))

let () = send (Drive_activation false)
let () = send (Drive_activation true)
let () = send (Drive_torque_limit 100)
let () = send (Drive_torque_limit 3600)
let () = send (Torque_limit(4,5))
let () = send (Torque_limit(8,5))

let () = move 1. 0.2 0.5

let () = move (-.1.) 0.2 0.5

let () = turn (20.*.pi) pi 0.5

let () = turn (-. 20.*.pi) pi 0.5

let () = motor_stop 0.5 0.5

(*lancer:
  let () = fait_un_truc ()
*)

(* commande de la vm / planner *)
open Krobot_geom

let send_bus m = Lwt_unix.run (Krobot_bus.send bus (Unix.gettimeofday (), m))
let stop () = send_bus Strategy_stop
let clear () = send_bus (Trajectory_set_vertices [])
(* nettoie la trajectoire enregistree *)
let set_vertice x y = send_bus (Trajectory_set_vertices [{x;y}])
(* enregistre la trajectoire *)
let go () = send_bus Trajectory_go

