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

type t =
  | Node of t option * t list
  | Stop
  | Think
  | Goto of bool * vertice * vector option
  | Set_limits of (float * float * float)
  | Follow_path of bool * vertice list * vector option * bool
  | Bezier of float * vertice * vertice * vertice * vertice * float
  | Set_curve of Bezier.curve option
  | Wait_for_jack of bool
  | Wait_for_bezier_moving of bool * float option
  | Wait_for_motors_moving of bool * float option
  | Reset_odometry of [ `Red | `Blue | `Auto ]
  | Wait_for_odometry of [ `Eq | `Gt | `Ge | `Lt | `Le ] * int
  | Try_something of vertice
  | Fail
  | Wait_for_odometry_reset of [ `Red | `Blue | `Auto ]
  | Load of [ `Front | `Back ]
  | Lift_down of [ `Front | `Back ]
  | Lift_up of [ `Front | `Back ]
  | Open_grip_low of [ `Front | `Back ]
  | Close_grip_low of [ `Front | `Back ]
  | Open_grip_high of [ `Front | `Back ]
  | Close_grip_high of [ `Front | `Back ]
  | Wait_for of float
  | Wait_until of float
  | Wait_for_grip_open_low of [ `Front | `Back ]
  | Wait_for_grip_close_low of [ `Front | `Back ]
  | Set_led of ( [ `Red | `Yellow | `Green ] * bool )
  | Start_timer
  | Can of Krobot_can.frame

let string_of_vertice { x; y } = sprintf "{ x = %f; y = %f }" x y
let string_of_vector { vx; vy } = sprintf "{ vx = %f; vy = %f }" vx vy

let string_of_option f = function
  | None -> "None"
  | Some v -> Printf.sprintf "Some (%s)" (f v)

let string_of_face = function
  | `Front -> "`Front"
  | `Back -> "`Back"

let rec to_string = function
  | Node (t,l) ->
      sprintf "Node [%s, %s]" (string_of_option to_string t) (String.concat "; " (List.map to_string l))
  | Stop ->
      "Stop"
  | Think ->
      "Think"
  | Goto (reverted,v,vect) ->
      sprintf "Goto %b %s %s" reverted (string_of_vertice v) (string_of_option string_of_vector vect)
  | Set_limits (vmax,atan_max, arad_max) ->
      sprintf "Set_limits(%f, %f, %f)" vmax atan_max arad_max
  | Set_led (_,_) -> "Set_led"
  | Follow_path (reverted,l,vect, correct) ->
      sprintf "Follow_path [%b, %s, %s, %b]" reverted
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
  | Set_curve(Some c) ->
      sprintf "Set_curve(Some %s)" (Bezier.string_of_curve c)
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
  | Load face ->
      sprintf "Load %s" (string_of_face face)
  | Lift_down face ->
      sprintf "Lift_down %s" (string_of_face face)
  | Lift_up face ->
      sprintf "Lift_up %s" (string_of_face face)
  | Open_grip_low face ->
      sprintf "Open_grip_low %s" (string_of_face face)
  | Close_grip_low face ->
      sprintf "Close_grip_low %s" (string_of_face face)
  | Open_grip_high face ->
      sprintf "Open_grip_high %s" (string_of_face face)
  | Close_grip_high face ->
      sprintf "Close_grip_high %s" (string_of_face face)
  | Wait_for t ->
      sprintf "Wait_for %f" t
  | Wait_until t ->
      sprintf "Wait_until %f" t
  | Wait_for_grip_open_low face ->
      sprintf "Wait_for_grip_open_low %S" (string_of_face face)
  | Wait_for_grip_close_low face ->
      sprintf "Wait_for_grip_close_low %S" (string_of_face face)
  | Start_timer -> "Start_timer"
  | Can c -> "Can"
  | Try_something v ->
      sprintf "Try_something %s" (string_of_vertice v)
  | Fail ->
      "Fail"
