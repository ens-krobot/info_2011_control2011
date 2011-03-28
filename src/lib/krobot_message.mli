(*
 * krobot_message.mli
 * ------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Krobot messages. *)

type direction = Forward | Backward
    (** Type of directions. *)

(** Type of messages. *)
type t =
  | Encoder_position_direction_3_4 of int * direction * int * direction
      (** The position and direction of encoders 3 and 4. *)
  | Encoder_position_speed_3 of float * float
      (** The position and speed of encoder 3. *)
  | Encoder_position_speed_4 of float * float
      (** The position and speed of encoder 4. *)
  | Motor_status of bool
      (** [true] iff a movement is in progress. *)
  | Motor_move of float * float * float
      (** [Motor_move(distance, speed, acceleration)] command to make
          the robot to move.
          - [distance] is in m
          - [speed] is in m/s
          - [acceleration] is in m/s^2
      *)
  | Motor_turn of float * float * float
      (** [Motor_turn(angle, speed, acceleration)] command to make the
          robot to turn.
          - [angle] is in rad
          - [speed] is in rad/s
          - [acceleration] is in rad/s^2
      *)
  | Odometry of float * float * float
      (** [Odometry(x, y, theta)] the position of the robot on the
          table. *)

  | Req_motor_status
      (** Request the status of the motors. *)

  | Unknown of Krobot_can.frame
      (** An unknown can frame. *)

val to_string : t -> string
  (** Returns the string representation of the given message. *)

val encode : t -> Krobot_can.frame
  (** Encode the given message into a CAN frame. *)

val decode : Krobot_can.frame -> t
  (** Decode the given CAN frame into a message. *)

val send : Krobot_bus.t -> (float * t) -> unit Lwt.t
  (** [send bus (timestamp, message)] sends the given message over
      D-Bus. *)

val recv : Krobot_bus.t -> (float * t) React.event
  (** [recv bus] is the event which receive messages. *)

(** {6 Calls} *)

(** The following functions send a request and wait for the result. *)

val motor_status : Krobot_bus.t -> (float * bool) Lwt.t
