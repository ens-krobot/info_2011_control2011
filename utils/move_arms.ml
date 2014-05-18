#!/usr/bin/ocaml

#use "topfind";;
#camlp4o;;
#require "lwt.syntax";;
#require "krobot";;
open Krobot_bus;;
open Krobot_message;;

(* le hub et driver doivent être lancés *)
let bus = Lwt_unix.run (Krobot_bus.get ())
let send m = Lwt_unix.run (Krobot_message.send bus (Unix.gettimeofday (), m))

let lift_goto s1 s2 = send (Elevator_command (s1,s2))

let goal = 0.03

let () = lift_goto goal goal
