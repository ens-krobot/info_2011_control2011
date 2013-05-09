open Lwt
open Lwt_react
open Lwt_preemptive
open Krobot_bus
open Krobot_message
open Krobot_geom
open Icp_minimisation

let section = Lwt_log.Section.make "krobot(urg_extract)"

type info = {
  bus : Krobot_bus.t;
  (* The message bus used to communicate with the robot. *)

  mutable position : vertice;
  (* The position of the robot on the table. *)

  mutable orientation : float;
  (* The orientation of the robot. *)

  mutable urg : Krobot_geom.vertice array;
}


(**********************)

let default_obstacle_diameter = 0.05
let keep_above_dist = 0.1

(* let table = Icp_utils.table 2. 3. 200 *)
let table = Icp_utils.real_table 100
let table_kd = make_kd_tree table

let a0 = { ath = 0.; ax = 0.; ay = 0. }

let filter_data keep_above_dist robot_transform data =
  let back_transform = Icp_utils.invert_transform robot_transform in
  let data = transform back_transform data in
  Icp_utils.far_enougth_filter table_kd keep_above_dist data

let mark_close kd marking dist v marking_val =
  let close_points = Kd_tree.closer_points dist v kd in
  List.iter (fun i -> marking.(i) <- Some marking_val) close_points

let mark_circles diameter data =
  let kd = make_kd_tree data in
  let marking = Array.map (fun _ -> None) data.dx in
  let count = ref 0 in
  let a = Array.mapi (fun i -> function
      | Some c -> c
      | None ->
        let v = { Kd_tree.x = data.dx.(i); y = data.dy.(i) } in
        mark_close kd marking diameter v !count;
        let c = !count in
        incr count;
        c) marking in
  !count, a

let baricenter { dx; dy } l =
  let len = List.length l in
  let accx = List.fold_left (fun acc i -> acc +. dx.(i)) 0. l in
  let accy = List.fold_left (fun acc i -> acc +. dy.(i)) 0. l in
  { Krobot_geom.x = accx /. (float len); Krobot_geom.y = accy /. (float len) }

let circles_points (count,marking) data =
  let a = Array.make count [] in
  Array.iteri (fun index i -> a.(i) <- index :: a.(i)) marking;
  let l = Array.to_list a in
  let l = List.filter (fun pts -> List.length pts >= 4) l in
  Array.of_list (List.map (baricenter data) l)

let extract_obstacles trans data =
  let tr = Icp_utils.invert_transform trans in
  let data_rest = filter_data keep_above_dist tr data in
  let filtered = transform tr data_rest in
  let marked = mark_circles default_obstacle_diameter filtered in
  circles_points marked filtered

let transform_vertice { ath; ax; ay } { x; y } =
  let co = cos ath in
  let si = sin ath in
  let x' = x *. co -. y *. si +. ax in
  let y' = x *. si +. y *. co +. ay in
  { x = x'; y = y' }

let run_extract info urg =
  let dxl, dyl = List.split (List.map (fun {x;y} -> x,y) (Array.to_list urg)) in
  let data = { dx = Array.of_list dxl; dy = Array.of_list dyl } in
  let trans = { ax = info.position.x; ay = info.position.y;
                ath = info.orientation } in
  let obstacles = extract_obstacles trans data in
  let ts = Unix.gettimeofday () in
  let objects = Array.map (fun v ->
      transform_vertice trans v,
      default_obstacle_diameter) obstacles in
  Krobot_bus.send info.bus (ts, Objects (Array.to_list objects))

(*******************)

(* +-----------------------------------------------------------------+
   | read/send loop                                                  |
   +-----------------------------------------------------------------+ *)

let loop bus =
  let rec aux () =
    lwt () = Lwt_unix.sleep 0.1 in
    aux () in
  aux ()

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message info (timestamp, message) =
  match message with
    | CAN(_, frame) -> begin
        match decode frame with
          | Odometry(x, y, theta) ->
              info.position <- { x; y };
              info.orientation <- math_mod_float theta (2. *. pi)
          | _ ->
              ()
      end

    | Urg data ->
      info.urg <- data;
      ignore (run_extract info data: unit Lwt.t)

    | Kill "urg_extract" ->
      exit 0

    | _ ->
        ()

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true
let listen = ref false

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
]

let usage = "\
Usage: krobot-urg-extract [options]
options are:"

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let run bus =
  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  (* Kill any running urg_handler. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "urg_extract") in

  let info = {
    bus = bus;
    position = { x = 0.; y = 0. };
    orientation = 0.;
    urg = [||];
  } in

  E.keep (E.map (handle_message info) (Krobot_bus.recv bus));

  (* Loop forever. *)
  loop info

lwt () =
  Arg.parse options ignore usage;

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  run bus
