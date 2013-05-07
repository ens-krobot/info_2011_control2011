
type a = { ath : float; ax : float; ay : float; }
(** representing rigid transformation: rotation by [ath] then
    translation by [ax,ay] *)

type data = { dx : float array; dy : float array; }

type kernel = float -> float

val transform : a -> data -> data
(** apply transformation to data *)

val make_kd_tree : data -> int Kd_tree.t
(** create a kd tree containting index in the data *)

val distance_transform : kernel -> 'a Kd_tree.t -> a -> data -> float array
(** [distance_transform kernel kd a data] Calculates the distance for
   each point of [data] after transformation to the closest point in
   the kd tree. the kernel is applied to the result after *)

val default_kernel : kernel
(** Huber kernel *)

(**********)

val register_simple : ?kernel:kernel -> int Kd_tree.t -> data -> a -> int -> a
(** Gauss-Newton gradient descent *)

val register : ?kernel:kernel -> ?lambda:Lacaml.D.num_type ->
  int Kd_tree.t -> data -> a -> int -> a
(** Levenberg-Marquardt gradient descent *)
