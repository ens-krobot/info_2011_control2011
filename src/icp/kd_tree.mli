
type 'a t

type vertice = { x : float; y : float; }

val make : ('a * vertice) array -> 'a t

val nearest_neighbor : vertice -> 'a t -> 'a * vertice * float
(** [nearest_neighbor v kd] finds the closest vertice [val] of
   [v]. returns [val,vert,dist_sqr], with [val] the value associated
   to [vert] and [dist_sqr] the square of the distance between [v] and
   [vert] *)

val depth : 'a t -> int

val closer : float -> vertice -> 'a t -> bool
(** [closer dist vert t] returns true if there is a point in [t]
   closer than [dist] of [vert] *)

val closer_points : float -> vertice -> 'a t -> 'a list
(** [closer_points dist vert t] returns the lis tof all points in [t]
   closer than [dist] of [vert] *)
