(*
 * krobot_joystick.ml
 * ------------------
 * Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Krobot_bus
open Krobot_message
open Sdljoystick
open Sdlevent
open Sdlkey

(* +-----------------------------------------------------------------+
   | Joystick events                                                 |
   +-----------------------------------------------------------------+ *)

type button =
  | ButtonCross
  | ButtonSquare
  | ButtonTriangle
  | ButtonCircle
  | ButtonDown
  | ButtonLeft
  | ButtonUp
  | ButtonRight
  | ButtonSelect
  | ButtonStart
  | ButtonR1
  | ButtonR2
  | ButtonL1
  | ButtonL2
  | ButtonPS3
  | ButtonLAxis
  | ButtonRAxis

type event =
  | JoyRAxisV of float
  | JoyLAxisV of float
  | JoyRAxisH of float
  | JoyLAxisH of float
  | JoyButtonPressed of button
  | JoyButtonReleased of button
  | KeyPressed of Sdlkey.t
  | KeyReleased of Sdlkey.t

(* +-----------------------------------------------------------------+
   | int --> button                                                  |
   +-----------------------------------------------------------------+ *)

let raxis_v = 3
let raxis_h = 2
let laxis_v = 1
let laxis_h = 0

let axis_min = -32768.0
let axis_max = 32767.0

let button_of_num = function
  | 14 -> Some ButtonCross
  | 15 -> Some ButtonSquare
  | 12 -> Some ButtonTriangle
  | 13 -> Some ButtonCircle
  | 6 -> Some ButtonDown
  | 7 -> Some ButtonLeft
  | 4 -> Some ButtonUp
  | 5 -> Some ButtonRight
  | 0 -> Some ButtonSelect
  | 3 -> Some ButtonStart
  | 11 -> Some ButtonR1
  | 9 -> Some ButtonR2
  | 10 -> Some ButtonL1
  | 8 -> Some ButtonL2
  | 16 -> Some ButtonPS3
  | 1 -> Some ButtonLAxis
  | 2 -> Some ButtonRAxis
  | n -> None

(* +-----------------------------------------------------------------+
   | SDL events (executed in a child process)                        |
   +-----------------------------------------------------------------+ *)

let child_loop pipe joy =
  let axis_state = Array.make (num_axes joy) 0.0 in
  let send ev =
    Pervasives.output_value pipe ev;
    Pervasives.flush pipe
  in
  while true do
    match wait_event () with
      | KEYDOWN { keysym = key } ->
          send (KeyPressed key);
          if key = KEY_ESCAPE then begin
            Sdl.quit ();
            exit 0
          end
      | JOYAXISMOTION { jae_axis = axis; jae_value = value } ->
          let value = 0.1 -. ((float_of_int value -. axis_min) *. 0.2 /. (axis_max -. axis_min)) in
          if value <> axis_state.(axis) then begin
            axis_state.(axis) <- value;
            if axis = laxis_h then
              send (JoyLAxisH value)
            else if axis = laxis_v then
              send (JoyLAxisV value)
            else if axis = raxis_h then
              send (JoyRAxisH value)
            else if axis = raxis_v then
              send (JoyRAxisV value)
            else
              ()
          end
      | JOYBUTTONUP { jbe_button = button } -> begin
          match button_of_num button with
            | Some button ->
                send (JoyButtonReleased button)
            | None ->
                ()
        end
      | JOYBUTTONDOWN { jbe_button = button } -> begin
          match button_of_num button with
            | Some button ->
                send (JoyButtonPressed button)
            | None ->
                ()
        end
      | _ ->
          ()
  done

(* +-----------------------------------------------------------------+
   | Handling events (in the parent process)                         |
   +-----------------------------------------------------------------+ *)

let motor_l = 4
let motor_r = 8

let axis_coef = 6.0
let axis_coef_turn = 4.0
let duration = 0.2

let velocity_l = ref 0.
let velocity_r = ref 0.

let max_vel = 1000.
let map_vel v =
  match truncate (v /. axis_coef *. max_vel *. 10.) with
    | 0 -> 1
    | n -> n

let send_speeds bus =
  let vl = map_vel !velocity_l in
  let vr = map_vel !velocity_r in
  lwt () = Lwt_log.notice_f "speeds = %d, %d" vl vr in
  let ts = Unix.gettimeofday () in
  lwt () = Krobot_message.send bus (ts, Motor_command (motor_l, vl))
  and () = Krobot_message.send bus (ts, Motor_command (motor_r, vr)) in
  return ()

let rec loop_speeds bus =
  lwt () = send_speeds bus in
  lwt () = Lwt_unix.sleep 0.1 in
  loop_speeds bus

let parent_loop bus pipe =
  lwt () = Krobot_message.send bus (Unix.gettimeofday (), Drive_activation false) in
  lwt () = Krobot_message.send bus (Unix.gettimeofday (), Motor_activation (motor_l, true)) in
  lwt () = Krobot_message.send bus (Unix.gettimeofday (), Motor_activation (motor_r, true)) in
  let raxis_h = ref 0.0
  and raxis_v = ref 0.0
  and laxis_h = ref 0.0
  and laxis_v = ref 0.0 in
  let set_velocities () =
    velocity_l := !laxis_v *. axis_coef -. !raxis_h *. axis_coef_turn;
    velocity_r := !laxis_v *. axis_coef +. !raxis_h *. axis_coef_turn;
    send_speeds bus
  in
  let rec loop () =
    Lwt_io.read_value pipe >>= function
      | KeyPressed KEY_ESCAPE ->
          return ()
      | JoyLAxisV n ->
          laxis_v := n;
          lwt () = set_velocities () in
          loop ()
      | JoyLAxisH n ->
          laxis_h := n;
          lwt () = set_velocities () in
          loop ()
      | JoyRAxisV n ->
          raxis_v := n;
          lwt () = set_velocities () in
          loop ()
      | JoyRAxisH n ->
          raxis_h := n;
          lwt () = set_velocities () in
          loop ()
      | _ ->
          loop ()
  in
  loop ()

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let () =
  let fd_r, fd_w = Unix.pipe () in
  match Unix.fork () with
    | 0 ->
        Unix.close fd_r;
        Sdl.init [`JOYSTICK;`VIDEO];
        Sdljoystick.set_event_state true;
        let joy =
          try
            open_joystick 0
          with exn ->
            Printf.eprintf "cannot open joystick: %s\n%!" (Printexc.to_string exn);
            raise exn
        in
        child_loop (Unix.out_channel_of_descr fd_w) joy
    | pid ->
        Unix.close fd_w;
        Lwt_main.run begin
          lwt bus = Krobot_bus.get () in
          lwt () = Lwt_log.notice "ready to process event" in
          ignore (loop_speeds bus);
          parent_loop bus (Lwt_io.of_unix_fd ~mode:Lwt_io.input fd_r)
        end
