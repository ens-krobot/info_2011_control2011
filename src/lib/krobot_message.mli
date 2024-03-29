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

type simulation_mode = Sim_no | Sim_normal | Sim_HIL
    (** Type of simulation. *)

(** Type of messages. *)
type t =
  | Battery1_voltages of float * float * float * float
      (** The voltages of the elements of the first battery *)
  | Battery2_voltages of float * float * float * float
      (** The voltages of the elements of the second battery *)
  | Beacon_position of float * float * float * float
      (** The position of the beacon relative to the robot *)
  | Beacon_lowlevel_position of float * float * int
      (** The position of the beacon as internally stored (for calibration) *)
  | Beacon_angles of float * float * float * float
      (** The angles of the beacons seen by the robot *)
  | Beacon_widths of float * float * float * float
      (** The widths of the beacons seen by the robot *)
  | Switch1_status of bool * bool * bool * bool * bool * bool * bool * bool
      (** The status of the first 8 switches
          1 : start
          2 : team
          3 : emergency stop *)
  | Switch2_status of bool * bool * bool * bool * bool * bool * bool * bool
      (** The status of the other 8 switches *)
  | Switch_request of int * bool
      (** A request to switch something on/off
          4 : buzzer
          5 : yellow led
          6 : red led
          7 : green led *)
  | Adc1_values of int * int * int * int
      (** The values of the first 4 ADCs *)
  | Adc2_values of int * int * int * int
      (** The values of the other 4 ADCs *)
  | Ax12_State of int * int * int * int
      (** The state of the AX-12 servo (position, speed, torque) *)
  | Ax12_Request_State of int
      (** Request the state of the AX-12 servo *)
  | Ax12_Goto of int * int * int
      (** Move the AX-12 servo in position at the given speed *)
  | Ax12_Set_Torque_Enable of int * bool
      (** Set the torque status of the AX-12 servo *)
  | Encoder_position_direction_1_2 of int * direction * int * direction
      (** The position and direction of encoders 1 and 2. *)
  | Encoder_position_direction_3_4 of int * direction * int * direction
      (** The position and direction of encoders 3 and 4. *)
  | Encoder_position_speed_3 of float * float
      (** The position and speed of encoder 3. *)
  | Encoder_position_speed_4 of float * float
      (** The position and speed of encoder 4. *)
  | Controller_activation of int * bool
      (** Activate/Deactivate some controllerts *)
  | Drive_activation of bool
      (** Activate/Deactivate differential drive *)
  | Torque_limit of int * int
      (** Adjust torque limitation on a some MOTORs *)
  | Drive_torque_limit of int
      (** Adjust torque limitation of the propulsion drive *)
  | Control_error of int * int
      (** Errors generated by a motor controller card. *)
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
  | Motor_command of int * int
      (** [Motor_command(motor_id, PWM_value)] *)
  | Motor_activation of int * bool
      (** [Motor_activation (id, enabled)] *)
  | Motor_stop of float * float
      (** [Motor_stop(lin_acc, rot_acc)] command to stop following the
          current Bezier Spline and the queued ones.
          - [lin_acc] in m/s^2
          - [rot_acc] in rad/s^2
      *)
  | Motor_bezier_limits of float * float * float * float
      (** [Motor_bezier_limits(v_max, omega_max, a_tan_max, a_rad_max)]
          - [v_max] in m/s
          - [omega_max] in rad/s
          - [a_tan_max] in m/s^2
          - [a_rad_max] in m/s^2 *)
  | Odometry of float * float * float
      (** [Odometry(x, y, theta)] the position of the robot on the
          table.
          - [x, y] in m
          - [theta] in rad *)
  | Odometry_indep of float * float * float
      (** [Odometry_ident(x, y, theta)] the position of the robot on the
          table, obtained from independent encoder wheels.
          - [x, y] in m
          - [theta] in rad *)
  | Odometry_ghost of float * float * float * int * bool
      (** [Odometry_ghost(x, y, theta, following)]. [following] is
          [true] iff the robot is following the ghost. *)
  | Set_odometry of float * float * float
      (** [set_odometry(x, y, theta)] sets the parameters of the
          odometry to the given ones. *)
  | Set_odometry_indep of float * float * float
      (** [set_odometry_ident(x, y, theta)] sets the parameters of the
          independant odometry to the given ones. *)
  | Set_simulation_mode of simulation_mode
      (** Put the cards into a given simulation mode. *)
  | Elevator_command of float * float
      (** Set the position of the elevators, between 0 and 1. Negative
          positions are ignored. *)
  | Pump_command of int * int
      (** Set the voltages applyed to the pumps, between 0 and 3600.
          Negative values are ignored *)
  | Elevator_encoders of int * direction * int * direction
      (** The position and direction of encoders from the lifts. *)
  | Pump_state of int * int
      (** The current voltages applyed to the pumps. *)
  | Effector_status of bool * bool * bool * bool
      (** Status of the 4 motors. *)
  | Elevator_positions of float * float
      (** The current awaited positions of the lifts *)
  | Homing_status of bool * bool
      (** Have the elevators been homed yet ? *)
  | Homing_command of float * float
      (** Home elevators at given speeds (a speed of 0 won't home) *)

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
