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
  | Node of t list
  | Stop
  | Think
  | Goto of vertice
  | Follow_path of vertice list
  | Bezier of float * vertice * vertice * vertice * vertice * float
  | Set_curve of Bezier.curve option
  | Wait_for_jack of bool
  | Wait_for_moving of bool
  | Reset_odometry of [ `Red | `Blue | `Auto ]
  | Wait_for_odometry of [ `Eq | `Gt | `Ge | `Lt | `Le ] * int
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

let string_of_vertice { x; y } = sprintf "{ x = %f; y = %f }" x y

let string_of_face = function
  | `Front -> "`Front"
  | `Back -> "`Back"

let rec to_string = function
  | Node l ->
      sprintf "Node [%s]" (String.concat "; " (List.map to_string l))
  | Stop ->
      "Stop"
  | Think ->
      "Think"
  | Goto v ->
      sprintf "Goto %s" (string_of_vertice v)
  | Follow_path l ->
      sprintf "Follow_path [%s]" (String.concat "; " (List.map string_of_vertice l))
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
  | Wait_for_moving st ->
      sprintf "Wait_for_moving %B" st
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
