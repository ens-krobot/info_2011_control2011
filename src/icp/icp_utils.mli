
val table : width:float -> length:float -> int -> Icp_minimisation.data

val load_file : ?min_dist:float -> ?max_dist:float -> string ->
  (float * Icp_minimisation.data) array
(** load a dump file in the format dumpped by [krobot_urg -listen] *)

