(*
 * krobot_message.mli
 * ------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Krobot CAN messages. *)

type direction = Forward | Backward
    (** Type of directions. *)

(** Type of messages. *)
type t =
  | Battery1_voltages of float * float * float * float
      (** The voltages of the elements of the first battery *)
  | Battery2_voltages of float * float * float * float
      (** The voltages of the elements of the second battery *)
  | Beacon_position of float * float * float
      (** The position of the beacon relative to the robot *)
  | Beacon_lowlevel_position of float * float * int
      (** The position of the beacon as internally stored (for calibration) *)
  | Switch1_status of bool * bool * bool * bool * bool * bool * bool * bool
      (** The status of the first 8 switches *)
  | Switch2_status of bool * bool * bool * bool * bool * bool * bool * bool
      (** The status of the other 8 switches *)
  | Switch_request of int * bool
      (** A request to switch something on/off *)
  | Adc1_values of int * int * int * int
      (** The values of the first 4 ADCs *)
  | Adc2_values of int * int * int * int
      (** The values of the other 4 ADCs *)
  | Encoder_position_direction_1_2 of int * direction * int * direction
      (** The position and direction of encoders 1 and 2. *)
  | Encoder_position_direction_3_4 of int * direction * int * direction
      (** The position and direction of encoders 3 and 4. *)
  | Encoder_position_speed_3 of float * float
      (** The position and speed of encoder 3. *)
  | Encoder_position_speed_4 of float * float
      (** The position and speed of encoder 4. *)
  | Motor_status of bool * bool * bool * bool
      (** Status of the 4 motors. *)
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
  | Motor_bezier of float * float * float * float * float * float
      (** [Motor_bezier(x_end, y_end, d1, d2, theta_end, v_end)] *)
  | Motor_stop of float * float
      (** [Motor_stop(lin_acc, rot_acc)] command to stop following the
          current Bezier Spline and the queued ones.
          - [lin_acc] in m/s^2
          - [rot_acc] in rad/s^2
      *)
  | Odometry of float * float * float
      (** [Odometry(x, y, theta)] the position of the robot on the
          table. *)
  | Odometry_ghost of float * float * float * int * bool
      (** [Odometry_ghost(x, y, theta, following)]. [following] is
          [true] iff the robot is following the ghost. *)
  | Set_odometry of float * float * float
      (** [set_odometry(x, y, theta)] sets the parameters of the
          odometry to the given ones. *)
  | Set_controller_mode of bool
      (** Put the card into simulation mode or not. *)

  | Req_motor_status
      (** Request the status of the motors. *)

  | Unknown of Krobot_can.frame
      (** An unknown can frame. *)

val to_string : t -> string
  (** Returns the string representation of the given message. *)

exception Invalid_frame of Krobot_can.frame
  (** Exception raised when an invalid frame is encountered. *)

val encode : t -> Krobot_can.frame
  (** Encode the given message into a CAN frame. *)

val decode : Krobot_can.frame -> t
  (** Decode the given CAN frame into a message. *)

val send : Krobot_bus.t -> (float * t) -> unit Lwt.t
  (** [send bus (timestamp, message)] sends the given message over
      D-Bus. *)

val recv : Krobot_bus.t -> (float * t) React.event
  (** [recv bus] is the event which receive messages. *)
