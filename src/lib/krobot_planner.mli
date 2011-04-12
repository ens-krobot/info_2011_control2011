(*
 * krobot_planner.mli
 * ------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

type t = Krobot_bus.t
    (** Type of the planner. *)

val vertices : t -> (Krobot_geom.vertice list, [ `readable | `writable ]) OBus_property.t
  (** The property holding the current trajectory. *)

val add_vertice : t -> Krobot_geom.vertice -> unit Lwt.t
  (** Add a vertice at the end of the current trajectory. *)

val simplify : t -> float -> unit Lwt.t
  (** [simplify planner tolerance] simplify the trajectory. *)

val moving : t -> (bool, [ `readable ]) OBus_property.t
  (** Is the robot currently following a trajectory ? *)

val go : t -> float -> float -> float -> float -> unit Lwt.t
  (** [go planner rotation_speed rotation_acceleration moving_speed
      moving_acceleration] make the robot to follow the trajectory. *)

val stop : t -> unit Lwt.t
  (** Stop the current trajectory. *)
