open Lwt
open Lwt_react
open Krobot_bus
open Krobot_geom
open Krobot_config
open Krobot_message
open CvCore
open CvHighGui

let section = Lwt_log.Section.make "krobot(see_coin)"

(* +-----------------------------------------------------------------+
   | vision initialisation                                           |
   +-----------------------------------------------------------------+ *)

let calibration_file = ref None
let homography_file = ref None
let color_filter_file = ref None

let file_string file =
  let file = open_in file in
  let l = in_channel_length file in
  let s = String.create l in
  ignore(Pervasives.input file s 0 l);
  s

type color_filter =
    { s_bounds : int * int;
      v_bounds : int *int; }

let load_color_filter () = match !color_filter_file with
  | None ->
    lwt () = Lwt_log.warning "No color config provided" in
    return { s_bounds = 0, 100;
             v_bounds = 150, 255; }
  | Some file ->
    try_lwt
      let f = file_string file in
      Scanf.sscanf f "s %i %i\nv %i %i\n"
        (fun s1 s2 v1 v2 ->
          return { v_bounds = v1, v2; s_bounds = s1, s2 })
    with
      | exn ->
        lwt () = Lwt_log.error ~exn "can't load color file" in
        raise_lwt exn

let load_calibration () = match !calibration_file with
  | None ->
    lwt () = Lwt_log.warning "No calibration config provided" in
    return ([|[|1.;0.;1.;|];
              [|0.;1.;1.;|];
              [|0.;0.;1.;|]|],
            [|0.;0.;0.;0.;0.|])
  | Some file ->
    try_lwt
      return (load_calibration_file file)
    with
      | exn ->
        lwt () = Lwt_log.error ~exn "can't load calibration file" in
        raise_lwt exn

let load_homography () = match !homography_file with
  | None ->
    lwt () = Lwt_log.warning "No homography config provided" in
    return [|[|1.;0.;0.;|];
             [|0.;1.;0.;|];
             [|0.;0.;1.;|]|]
  | Some file ->
    try_lwt
      return (load_homography_file file)
    with
      | exn ->
        lwt () = Lwt_log.error ~exn "can't load homography file" in
        raise_lwt exn

let rec get_cam i m =
  if i <= m
  then
    try
      Some (capture_from_cam i)
    with
      | _ -> get_cam (i+1) m
  else None

type vision = {
  capture : cvCapture;
  size : cvSize;
  color_filter : color_filter;
  remap : remap;
  homography : float array array;
  inv_homography : float array array;
}

let init_capture () =
  match get_cam 0 10 with
    | None ->
      lwt () = Lwt_log.error "Can't acquire webcam" in
      raise_lwt (Failure "Can't acquire webcam")
    | Some capture ->
      set_capture_property capture CV_CAP_PROP_FRAME_HEIGHT 480.;
      set_capture_property capture CV_CAP_PROP_FRAME_WIDTH 640.;
      let i = query_frame capture in
      let size = image_size i in
      lwt color_filter = load_color_filter () in
      lwt calibration = load_calibration () in
      let remap_info = init_undistort_map size calibration in
      lwt homography = load_homography () in
      let det, inv_homography = invert homography in
      return { capture;
               size;
               color_filter;
               remap = remap_info;
               homography;
               inv_homography; }

(* +-----------------------------------------------------------------+
   | Main vision code                                                |
   +-----------------------------------------------------------------+ *)

let rec filter_map f l =
  match l with
    | [] -> []
    | t::q ->
      match f t with
        | None -> filter_map f q
        | Some t -> t::(filter_map f q)

let detect_ellipse vision =
  let src = query_frame vision.capture in
  let src = remap vision.remap src in
  let hsv = convert_color src bgr2hsv in
  let h,s,v = split hsv in
  let vmin, vmax = vision.color_filter.v_bounds in
  let v = between v vmin vmax in
  let smin, smax = vision.color_filter.s_bounds in
  let s = between s smin smax in

  let filtered = copy s (Some v) in
  let filtered = erode filtered (5,5) in
  let filtered = dilate filtered (5,5) in
  let contours = contours (find_contours filtered) in
  let ellipses = filter_map (fun (Contour (c,l)) ->
    fit_ellipse c) contours in
  (* TODO: filter good ellipses *)
  ellipses

let loop bus vision =
  let rec aux () =
    let time = Unix.gettimeofday () in
    let ellipses = detect_ellipse vision in
    let time2 = Unix.gettimeofday () in
    lwt () = Lwt_log.info_f "vision compute time: %f" (time2 -. time) in
    let norm v =
      match v with
        | [|a;b;c|] -> { x = a/.c; y = b/.c }
        | _ -> assert false in
    let vertices = List.map
      (fun e -> let x,y = e.ellipse_center in
             norm (mult vision.inv_homography [|x;y;1.|]) )
      ellipses in
    let msg = Coins vertices in
    lwt () = Krobot_bus.send bus (time, msg) in
    lwt () = Lwt_unix.sleep 0.01 in
    aux () in
  aux ()

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message looker (timestamp, message) =
  match message with
    | Kill "see_coin" ->
        exit 0
    | _ ->
        ()

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
  "-calibration", Arg.String (fun s -> calibration_file:=Some s), "calibration config file";
  "-cam", Arg.String (fun s -> calibration_file:=Some s), "-calibration";
  "-homography", Arg.String (fun s -> homography_file:=Some s), "homography config file";
  "-h", Arg.String (fun s -> homography_file:=Some s), "-homography";
  "-color", Arg.String (fun s -> color_filter_file:=Some s), "color filter config file";
  "-c", Arg.String (fun s -> color_filter_file:=Some s), "-color";
]

let usage = "\
Usage: krobot-see-coin [options]
options are:"

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

lwt () =
  Arg.parse options ignore usage;

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  let looker = () in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message looker) (Krobot_bus.recv bus));

  (* Kill any running planner. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "see_coin") in

(*
  (* Ask for initial parameters. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Send) in
*)

  lwt vision = init_capture () in

  (* Loop forever. *)
  loop bus vision
