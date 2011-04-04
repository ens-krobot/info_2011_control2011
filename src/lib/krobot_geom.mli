(*
 * krobot_geom.mli
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Geometry *)

(** {6 Basic geometry} *)

type vector = { vx : float; vy : float }
type vertice = { x : float; y : float }

val null : vector
val origin : vertice

val add : vector -> vector -> vector
val sub : vector -> vector -> vector
val minus : vector -> vector
val mul : vector -> float -> vector
val div : vector -> float -> vector

val ( +| ) : vector -> vector -> vector
val ( -| ) : vector -> vector -> vector
val ( ~| ) : vector -> vector
val ( *| ) : vector -> float -> vector
val ( /| ) : vector -> float -> vector

val translate : vertice -> vector -> vertice
val vector : vertice -> vertice -> vector

val norm : vector -> float
val distance : vertice -> vertice -> float

val tangent : vertice -> vertice -> vertice -> vector
  (** [tangent a b c] returns the tangent to the triangle abc in b. *)

(** {6 Cubic Bezier curves} *)

module Bezier : sig
  type curve
    (** Type of cubic Bezier curves. *)

  val of_vertices : vertice -> vertice -> vertice -> vertice -> curve
    (** [of_vertices p q r s] creates a bezier curve from the given
        four control points. [p] and [s] are the first and end point
        of the curve. *)

  val make : p : vertice -> s : vertice -> vp : vector -> vs : vector -> a : float -> error_max : float -> curve
    (** [make p s vp vs sp ss a] creates a bezier curve.
        @param p is the first control point
        @param s is the last control point
        @param vp is the speed vector in [p]
        @param vs is the speed vector in [s]
        @param a is the radial acceleration of the robot
        @param error_max is the maximum allowed error or the
        computation of intermediate control points *)

  val vertice : curve -> float -> vertice
    (** [vertice curve u] returns the vertice on the given curve for
        the given paramter [u] which must be in the range [0..1]. *)
end
