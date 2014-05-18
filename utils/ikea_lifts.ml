#!/usr/bin/ocaml

#use "topfind";;
#camlp4o;;
#require "lwt.syntax";;
#require "krobot";;
open Krobot_bus;;
open Krobot_message;;
open Lwt;;

let pi = 4. *. atan 1.

let homed_left = ref false
let homed_right = ref false
let moving_left = ref false
let moving_right = ref false

(* le hub et driver doivent être lancés *)
let bus = Lwt_unix.run (Krobot_bus.get ())
let send m = Lwt_unix.run (Krobot_message.send bus (Unix.gettimeofday (), m))

let handle_message (timestamp, message) =
  match message with
    | CAN(_, frame) -> begin
        match decode frame with
          | Effector_status(l,r,_,_) ->
            moving_left := l;
            moving_right := r
          | Homing_status(l,r) ->
            homed_left := l;
            homed_right := r
          | _ ->
              ()
      end
    | _ ->
        ()

(* a n'executer qu'une fois *)
let () = Lwt_react.E.keep (Lwt_react.E.map handle_message (Krobot_bus.recv bus))

let wait t = Lwt_unix.run (Lwt_unix.sleep 0. >>= fun () -> Lwt_unix.sleep t)
(* en secondes *)

let rec wait_ref_to_be_false r =
  if !r then
    Lwt_unix.sleep 0.01 >>= fun () -> wait_ref_to_be_false r
  else
    Lwt.return ()

let rec wait_ref_to_be_true r =
  if (not !r) then
    Lwt_unix.sleep 0.01 >>= fun ()-> wait_ref_to_be_true r
  else
    Lwt.return ()

let lift_goto s1 s2 = send (Elevator_command (s1,s2))

let homing s1 s2 = send (Homing_command (s1,s2))

let home () =
  Printf.printf "Starting fast homing\n%!";
  homing (pi) (pi);
  wait 0.1;
  Printf.printf "Waiting for homing...\n%!";
  Lwt_unix.run (wait_ref_to_be_true homed_left);
  Lwt_unix.run (wait_ref_to_be_true homed_right);
  Printf.printf "Homing done, moving up a little\n%!";
  lift_goto 0.01 0.01;
  wait 0.1;
  Printf.printf "Waiting for end of movement...\n%!";
  Lwt_unix.run (wait_ref_to_be_false moving_left);
  Lwt_unix.run (wait_ref_to_be_false moving_right);
  Printf.printf "Starting slow homing\n%!";
  homing (pi/.4.) (pi/.4.);
  wait 0.1;
  Printf.printf "Waiting for homing...\n%!";
  Lwt_unix.run (wait_ref_to_be_true homed_left);
  Lwt_unix.run (wait_ref_to_be_true homed_right);
  Printf.printf "Homing done, moving to stand-by position\n%!";
  lift_goto 0.03 0.03;
  wait 0.1;
  Printf.printf "Waiting for end of movement...\n%!";
  Lwt_unix.run (wait_ref_to_be_false moving_left);
  Lwt_unix.run (wait_ref_to_be_false moving_right);
  Printf.printf "done\n%!"

let () = wait 0.5
let () = if (not !homed_left) || (not !homed_right) then home ()
let () =
  while true do
    begin
      lift_goto 0.05 0.01;
      wait 0.5;
      Lwt_unix.run (wait_ref_to_be_false moving_left);
      Lwt_unix.run (wait_ref_to_be_false moving_right);
      Printf.printf ".%!";
      wait 1.0;
      lift_goto 0.01 0.05;
      wait 0.5;
      Lwt_unix.run (wait_ref_to_be_false moving_left);
      Lwt_unix.run (wait_ref_to_be_false moving_right);
      Printf.printf ".\n%!";
      wait 1.0
    end
  done


