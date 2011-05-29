
val mult : float array array -> float array -> float array
(** [mult m v] matrix multiplication *)

val solve : float array array -> float array -> float array
(** [solve m v] finds [x] such that [mult m x = v] *) 
