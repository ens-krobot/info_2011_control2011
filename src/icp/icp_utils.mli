open Icp_minimisation

val table : width:float -> length:float -> int -> data

val real_table : int -> data

val load_file : ?min_dist:float -> ?max_dist:float -> string ->
  (float * data) array
(** load a dump file in the format dumpped by [krobot_urg -listen] *)

val load_raw_file : string -> (float * data) array
(** load a dump file in the format dumpped by [krobot_urg -listen] *)

(* filtering *)

val far_enougth_filter : 'a Kd_tree.t -> float -> data -> data
(** [far_enougth_filter kd min_dist data] filter out values of [data]
   that are closer than [min_dist] to a vertex of kd *)

val invert_transform : a -> a
