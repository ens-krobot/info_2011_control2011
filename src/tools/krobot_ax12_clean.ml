open Arg

type ax12_info =
    { id : int;
      pos : float;
      speed : float;
      torque : float;
      time : float }

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

module IntMap = Map.Make(struct type t = int let compare = compare end)

let separate l =
  let f map v =
    if IntMap.mem v.id map
    then let r = IntMap.find v.id map in
         r := v :: !r;
         map
    else IntMap.add v.id (ref [v]) map in
  IntMap.map (fun r -> List.rev !r) (List.fold_left f IntMap.empty l)

let soft window l =
  let rec aux (l,v_win) v =
    let v_win = List.filter (fun t -> v.time -. t.time <= window) l in
    let v_win = v::v_win in
    let mean = (List.fold_left (fun s v -> s +. v.pos) 0. v_win) /. float (List.length v_win) in
    { v with pos = mean }::l, v_win in
  List.rev (fst (List.fold_left aux ([],[]) l))

let remove_useless remove_below l =
  let rec aux prev acc = function
    | [] -> List.rev acc
    | t::q ->
      if abs_float (t.pos -. prev.pos) <= remove_below
      then aux prev acc q
      else aux t (t::acc) q
  in
  aux (List.hd l) [List.hd l] (List.tl l)

let add_speed speed_coef l =
  let rec aux prev acc = function
    | [] -> List.rev acc
    | t::q ->
      let dt = t.time -. prev.time in
      let dpos = t.pos -. prev.pos in
      let speed = (dpos /. dt) *. speed_coef in
      if abs_float speed > 200.
      then aux prev acc q
      else aux t ({ t with speed }::acc) q
  in
  aux (List.hd l) [List.hd l] (List.tl l)

let window = 0.1
let remove_below = 1.
let speed_coef = 1. (* ratio ax12 speed -> rad /. second *)

let prepare l =
  let l = List.map (fun v -> { v with speed = 0.; torque = 0. }) l in
  let l = List.sort (fun v1 v2 -> compare v1.time v2.time) l in
  let first_time = (List.hd l).time in
  let l = List.map (fun v -> { v with time = v.time -. first_time }) l in
  let map = separate l in
  let map = IntMap.map (soft window) map in
  let map = IntMap.map (remove_useless remove_below) map in
  let map = IntMap.map (add_speed speed_coef) map in
  let l = List.flatten (IntMap.fold (fun _ l acc -> l::acc) map []) in
  let l = List.sort (fun v1 v2 -> compare v1.time v2.time) l in
  l

let map_speed l c =
  let f v = { v with speed = abs_float (v.speed *. c); time = v.time /. c } in
  List.map f l

let print_info { id; pos; speed; torque; time } =
  Printf.printf "id %i,pos %f, speed %f, torque %f, time %f\n%!" id pos speed torque time

let file = ref None
let speed = ref 1.

let spec = ["-s", Set_float speed, "speed";]
let msg = "clean ax12 command sequence"
let message _ = usage spec msg; flush stdout; exit 0
let () = Arg.parse spec (fun s -> file := Some s) msg
let () =
  match !file with
    | None -> message ()
    | Some f ->
      let l = read (open_in f) in
      let l2 = prepare l in
      let l3 = map_speed l2 !speed in
      List.iter print_info l3
