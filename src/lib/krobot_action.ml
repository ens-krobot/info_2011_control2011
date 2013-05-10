(*
 * krobot_action.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Printf
open Krobot_geom

type node_kind =
  | Simple
  | Retry of int * t
  | Loop of t
  | Next

and t =
  | Node of node_kind * t list
  | Stop
  | Think
  | Goto of vertice * vector option
  | Simple_goto of vertice * vector option
  | Random_move of vertice * vertice
  | Set_limits of float * float * float * float
  | Follow_path of vertice list * vector option * bool
  | Bezier of float * vertice * vertice * vertice * vertice * float
  | Set_curve of (bool * Bezier.curve) option
  | Wait_for_jack of bool
  | Wait_for_bezier_moving of bool * float option
  | Wait_for_motors_moving of bool * float option
  | Reset_odometry of [ `Red | `Blue | `Auto ]
  | Wait_for_odometry of [ `Eq | `Gt | `Ge | `Lt | `Le ] * int
  | Try_something of vertice
  | Fail
  | Wait_for_odometry_reset of [ `Red | `Blue | `Auto ]
  | Wait_for of float
  | Wait_until of float
  | Start_timer of float * t list
  | Stop_timer
  | Start_match
  | Can of Krobot_can.frame
  | Set_led of [ `Red | `Yellow | `Green ] * bool
  | Set_orientation of float
  | Set_odometry of float option * float option * float option
  | Calibrate of vertice * float * float * float option * float option * float option
  | End

let string_of_vertice { x; y } = sprintf "{ x = %f; y = %f }" x y
let string_of_vector { vx; vy } = sprintf "{ vx = %f; vy = %f }" vx vy

let string_of_option f = function
  | None -> "None"
  | Some v -> Printf.sprintf "Some (%s)" (f v)

let string_of_face = function
  | `Front -> "`Front"
  | `Back -> "`Back"

let rec to_string = function
  | Node (Simple,l) ->
      sprintf "Node [%s]" (list_to_string l)
  | Node (Next,l) ->
      sprintf "Node [Next,%s]" (list_to_string l)
  | Node (Retry(n,l'),l) ->
      sprintf "Node [%i, %s, %s]"
        n (to_string l') (list_to_string l)
  | Node (Loop t,l) ->
      sprintf "Node [loop, %s, %s]" (to_string t) (list_to_string l)
  | Stop ->
      "Stop"
  | Think ->
      "Think"
  | Random_move (v1,v2) ->
      sprintf "Random move %s %s" (string_of_vertice v1) (string_of_vertice v2)
  | Goto (v,vect) ->
      sprintf "Goto %s %s" (string_of_vertice v) (string_of_option string_of_vector vect)
  | Simple_goto (v,vect) ->
      sprintf "Simple_goto %s %s" (string_of_vertice v) (string_of_option string_of_vector vect)
  | Set_limits (vmax,omega_max,atan_max, arad_max) ->
      sprintf "Set_limits(%f, %f, %f, %f)" vmax omega_max atan_max arad_max
  | Set_led (_,_) -> "Set_led"
  | Follow_path (l,vect, correct) ->
      sprintf "Follow_path [%s, %s, %b]"
        (String.concat "; " (List.map string_of_vertice l))
        (string_of_option string_of_vector vect)
        correct
  | Bezier(sign, p, q, r, s, end_velocity) ->
      sprintf
        "Bezier(%f, %s, %s, %s, %s, %f)"
        sign
        (string_of_vertice p)
        (string_of_vertice q)
        (string_of_vertice r)
        (string_of_vertice s)
        end_velocity
  | Set_curve(Some (dir,c)) ->
      sprintf "Set_curve(Some (%b, %s))" dir (Bezier.string_of_curve c)
  | Set_curve None ->
      "Set_curve None"
  | Wait_for_jack st ->
      sprintf "Wait_for_jack %B" st
  | Wait_for_bezier_moving (st, opt) ->
      sprintf "Wait_for_moving (%B, %s)" st (string_of_option string_of_float opt)
  | Wait_for_motors_moving (st, opt) ->
      sprintf "Wait_for_moving (%B, %s)" st (string_of_option string_of_float opt)
  | Reset_odometry `Red ->
      "Reset_odometry `Red"
  | Reset_odometry `Blue ->
      "Reset_odometry `Blue"
  | Reset_odometry `Auto ->
      "Reset_odometry `Auto"
  | Wait_for_odometry(test, value) ->
      sprintf
        "Wait_for_odometry(`%s, %d)"
        (match test with
           | `Eq -> "Eq"
           | `Gt -> "Gt"
           | `Ge -> "Ge"
           | `Lt -> "Lt"
           | `Le -> "Le")
        value
  | Wait_for_odometry_reset `Red ->
    "Wait_for_odometry_reset `Red"
  | Wait_for_odometry_reset `Blue ->
    "Wait_for_odometry_reset `Blue"
  | Wait_for_odometry_reset `Auto ->
    "Wait_for_odometry_reset `Auto"
  | Wait_for t ->
      sprintf "Wait_for %f" t
  | Wait_until t ->
      sprintf "Wait_until %f" t
  | Start_timer (delay,t) ->
      sprintf "Start_timer(%f,%s)" delay (list_to_string t)
  | Stop_timer ->
      sprintf "Stop_timer"
  | Can c -> "Can"
  | Try_something v ->
      sprintf "Try_something %s" (string_of_vertice v)
  | Fail ->
      "Fail"
  | Set_orientation (o) -> "Set_orientation"
  | Set_odometry (x,y,o) ->
    Printf.sprintf "Set_odometry %s %s %s"
      (string_of_option string_of_float x)
      (string_of_option string_of_float y)
      (string_of_option string_of_float o)
  | Calibrate (_,_,_,_,_,_) -> "Calibrate"
  | End -> "End"
  | Start_match -> "Start_match"

and list_to_string l = String.concat "; " (List.map to_string l)
