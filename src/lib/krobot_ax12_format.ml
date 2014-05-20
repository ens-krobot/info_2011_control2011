
type ax12_action =
  { id: int;
    pos: int;
    speed: int }

type action =
  | Sleep of float
  | Do of ax12_action

type timed_action = float * ax12_action

type ax12_info =
    { id : int;
      pos : float;
      speed : float;
      torque : float;
      time : float }

let to_actions l =
  let f (t,l) (time, v) =
    time, ( Do v ) :: (Sleep (time -. t)) :: l in
  let _, l = List.fold_left f (0.,[]) l in
  List.rev l

let to_timed_actions l =
  let f v =
    v.time, { id = v.id; pos = int_of_float v.pos; speed = int_of_float v.speed }
  in
  List.map f l

let fscan_info ic =
  Scanf.fscanf ic "id %i,pos %f, speed %f, torque %f, time %f\n"
    (fun id pos speed torque time -> { id; pos; speed; torque; time; })

let read ic =
  let rec aux acc =
    try
      let v = fscan_info ic in
      aux (v::acc)
    with
      | End_of_file
      | Scanf.Scan_failure _ -> List.rev acc
  in
  aux []

let read_timed_actions_file f =
  to_timed_actions (read (open_in f))

let rec merge l1 l2 = match (l1,l2) with
  | l, [] | [], l -> l
  | (ti1,h1)::t1, (ti2,h2)::t2 ->
    if (ti1:float) <= ti2
    then (ti1,h1) :: merge t1 l2
    else (ti2,h2) :: merge l1 t2
