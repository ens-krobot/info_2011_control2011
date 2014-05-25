type ax12_info =
    { id : int;
      pos : float;
      speed : float;
      torque : float;
      time : float }

type ax12_action =
  { id: int;
    pos: int;
    speed: int }

type action =
  | Sleep of float
  | Do of ax12_action

module IntMap = Map.Make(struct type t = int let compare = compare end)

type keyframe_dict = (int IntMap.t) IntMap.t

type timed_action = float * ax12_action

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

let to_keyed_actions l =
  let f (k,v) =
    ignore(v.time);
    k, { id = v.id; pos = int_of_float v.pos; speed = int_of_float v.speed }
  in
  List.map f l

let fscan_info ic =
  Scanf.fscanf ic "id %i,pos %f, speed %f, torque %f, time %f\n"
    (fun id pos speed torque time -> { id; pos; speed; torque; time; })

let fscan_keyframe_info ic =
  Scanf.fscanf ic "key %i, id %i, pos %f, speed %f, torque %f, time %f\n"
    (fun key id pos speed torque time -> (key,{ id; pos; speed; torque; time; }))

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

let read_keyframes ic =
  let rec aux acc =
    try
      let v = fscan_keyframe_info ic in
      aux (v::acc)
    with
      | End_of_file
      | Scanf.Scan_failure _ -> List.rev acc
  in
  aux []

let read_timed_actions_file f =
  to_timed_actions (read (open_in f))

let read_keyframes_file f =
  let infos = read_keyframes (open_in f) in
  let infos = to_keyed_actions infos in
  List.fold_left
    (fun dict (key_id, info) ->
       if IntMap.mem key_id dict then
         IntMap.add key_id (IntMap.add info.id info.pos (IntMap.find key_id dict)) dict
       else
         IntMap.add key_id (IntMap.singleton info.id info.pos) dict)
    IntMap.empty
    infos

let rec merge l1 l2 = match (l1,l2) with
  | l, [] | [], l -> l
  | (ti1,h1)::t1, (ti2,h2)::t2 ->
    if (ti1:float) <= ti2
    then (ti1,h1) :: merge t1 l2
    else (ti2,h2) :: merge l1 t2
