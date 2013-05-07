
type 'a t

type vertice = { x : float; y : float; }

val make : ('a * vertice) array -> 'a t

val nearest_neighbor : vertice -> 'a t -> 'a * vertice * float
(** [nearest_neighbor v kd] finds the closest vertice [val] of
   [v]. returns [val,vert,dist_sqr], with [val] the value associated
   to [vert] and [dist_sqr] the square of the distance between [v] and
   [vert] *)

val depth : 'a t -> int
