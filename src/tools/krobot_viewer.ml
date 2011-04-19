(*
 * krobot_viewer.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Lwt_react
open Krobot_message
open Krobot_geom
open Krobot_bus
open Krobot_config

let section = Lwt_log.Section.make "krobot(viewer)"

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type state = {
  pos : vertice;
  theta : float;
}

type beacon = {
  xbeacon : float;
  ybeacon : float;
  valid : bool;
}

type viewer = {
  bus : Krobot_bus.t;
  (* The D-Bus message bus used by this viwer. *)

  ui : Krobot_viewer_ui.window;
  (* The UI of the viewer. *)

  statusbar_context : GMisc.statusbar_context;
  (* The context of the statusbar. *)

  mutable state : state;
  (* The state of the robot. *)

  mutable ghost : state;
  (* The state of the ghost. *)

  mutable beacon : beacon;
  (* The state of the beacon. *)

  mutable origin : vertice * vector;
  (* The origin of the trajectory. *)

  mutable vertices : vertice list;
  (* The current trajectory. *)
}

(* +-----------------------------------------------------------------+
   | Drawing                                                         |
   +-----------------------------------------------------------------+ *)

type color =
  | Black
  | White
  | Green
  | Red
  | Blue
  | Yellow
  | Purple

let set_color ctx color =
  let r, g, b = match color with
    | Black -> (0., 0., 0.)
    | White -> (255., 255., 255.)
    | Green -> (36., 145., 64.)
    | Red -> (199., 23., 18.)
    | Blue -> (0., 59., 128.)
    | Yellow -> (252., 189., 31.)
    | Purple -> (128., 0., 128.)
  in
  Cairo.set_source_rgb ctx (r /. 255.) (g /. 255.) (b /. 255.)

let optimal_size width height =
  if width /. height >= (world_width +. 0.204) /. (world_height +. 0.204) then
    ((world_width +. 0.204) /. (world_height +. 0.204) *. height, height)
  else
    (width, width /. (world_width +. 0.204) *. (world_height +. 0.204))

let draw viewer =
  let { Gtk.width; Gtk.height } = viewer.ui#scene#misc#allocation in
  let surface = Cairo.image_surface_create Cairo.FORMAT_ARGB32 width height in
  let ctx = Cairo.create surface in
  let width = float width and height = float height in

  Cairo.set_antialias ctx Cairo.ANTIALIAS_NONE;

  (* Draw the background *)
  Cairo.rectangle ctx 0. 0. width height;
  set_color ctx White;
  Cairo.fill ctx;

  (* Compute the optimal width and height *)
  let dw, dh = optimal_size width height in

  (* Translation to have the viewer at the center and scaling to match the window sizes *)
  let x0 = (width -. dw) /. 2. and y0 = (height -. dh) /. 2. in
  let scale = dw /. (world_width +. 0.204) in
  Cairo.translate ctx (x0 +. 0.102 *. scale) (y0 +. dh -. 0.102 *. scale);
  Cairo.scale ctx scale (-.scale);

  Cairo.set_line_width ctx (1. /. scale);

  (* Draw the borders *)
  Cairo.rectangle ctx (-0.022) (-0.022) (world_width +. 0.044) (world_height +. 0.044);
  set_color ctx Black;
  Cairo.fill ctx;

  (* Draw beacon supports *)
  Cairo.rectangle ctx (-0.102) (-0.102) 0.08 0.08;
  Cairo.fill ctx;

  Cairo.rectangle ctx (-0.102) (world_height /. 2. -. 0.04) 0.08 0.08;
  Cairo.fill ctx;

  Cairo.rectangle ctx (-0.102) (world_height +. 0.022) 0.08 0.08;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width +. 0.022) (-0.102) 0.08 0.08;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width +. 0.022) (world_height /. 2. -. 0.04) 0.08 0.08;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width +. 0.022) (world_height +. 0.022) 0.08 0.08;
  Cairo.fill ctx;

  (* Draw the viewer background *)
  Cairo.rectangle ctx 0. 0. world_width world_height;
  set_color ctx Green;
  Cairo.fill ctx;

  (* Draw the starting areas *)
  Cairo.rectangle ctx 0. (world_height -. 0.4) 0.4 0.4;
  set_color ctx Red;
  Cairo.fill ctx;
  Cairo.rectangle ctx (world_width -. 0.4) (world_height -. 0.4) 0.4 0.4;
  set_color ctx Blue;
  Cairo.fill ctx;

  (* Draw the paving *)
  for i = 0 to 5 do
    for j = 0 to 5 do
      let x = 0.45 +. 0.35 *. float i
      and y = 0.35 *. float j in
      Cairo.rectangle ctx x y 0.35 0.35;
      set_color ctx (if (i + j) mod 2 = 0 then Red else Blue);
      Cairo.fill ctx
    done
  done;

  (* Draw the bands *)
  set_color ctx Black;

  Cairo.rectangle ctx 0.4 0. 0.05 world_height;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width -. 0.45) 0. 0.05 world_height;
  Cairo.fill ctx;

  Cairo.rectangle ctx 0.45 0.33 0.7 0.02;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width -. 1.15) 0.33 0.7 0.02;
  Cairo.fill ctx;

  Cairo.rectangle ctx 1.13 0. 0.02 0.35;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width -. 1.15) 0. 0.02 0.35;
  Cairo.fill ctx;

  Cairo.rectangle ctx 0.45 0. 0.7 0.12;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width -. 1.15) 0. 0.7 0.12;
  Cairo.fill ctx;

  Cairo.rectangle ctx 0.45 0. 0.02 0.25;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width -. 0.47) 0. 0.02 0.25;
  Cairo.fill ctx;

  Cairo.move_to ctx 0. (world_height -. 0.4);
  Cairo.rel_line_to ctx 0.4 0.;
  Cairo.stroke ctx;

  Cairo.move_to ctx 0. (world_height -. 0.422);
  Cairo.rel_line_to ctx 0.4 0.;
  Cairo.stroke ctx;

  Cairo.move_to ctx (world_width -. 0.4) (world_height -. 0.4);
  Cairo.rel_line_to ctx 0.4 0.;
  Cairo.stroke ctx;

  Cairo.move_to ctx (world_width -. 0.4) (world_height -. 0.422);
  Cairo.rel_line_to ctx 0.4 0.;
  Cairo.stroke ctx;

  Cairo.set_antialias ctx Cairo.ANTIALIAS_DEFAULT;

  (* Draw circles on bonus cases *)
  Cairo.arc ctx 0.975 0.875 0.05 0. (2. *. pi);
  Cairo.fill ctx;

  Cairo.arc ctx 0.975 1.575 0.05 0. (2. *. pi);
  Cairo.fill ctx;

  Cairo.arc ctx 2.025 0.875 0.05 0. (2. *. pi);
  Cairo.fill ctx;

  Cairo.arc ctx 2.025 1.575 0.05 0. (2. *. pi);
  Cairo.fill ctx;

  Cairo.arc ctx 1.325 0.175 0.05 0. (2. *. pi);
  Cairo.fill ctx;

  Cairo.arc ctx 1.675 0.175 0.05 0. (2. *. pi);
  Cairo.fill ctx;

  Cairo.save ctx;

  List.iter
    (fun (state, alpha) ->
       (* Draw the robot *)
       Cairo.translate ctx state.pos.x state.pos.y;
       Cairo.rotate ctx state.theta;
       Cairo.rectangle ctx (-. wheels_position) (-. robot_size /. 2.) robot_size robot_size;
       Cairo.set_source_rgba ctx 1. 1. 1. alpha;
       Cairo.fill ctx;

       (* Draw an arrow on the robot *)
       let d = robot_size /. 2. -. wheels_position in
       Cairo.move_to ctx 0. 0.;
       Cairo.line_to ctx (d +. robot_size /. 4.) 0.;
       Cairo.line_to ctx d (-. robot_size /. 4.);
       Cairo.line_to ctx d (robot_size /. 4.);
       Cairo.line_to ctx (d +. robot_size /. 4.) 0.;
       Cairo.set_source_rgba ctx 0. 0. 0. 0.5;
       Cairo.stroke ctx)
    [(viewer.ghost, 0.5);
     (viewer.state, 1.0)];

  Cairo.restore ctx;

  (* Draw the beacon *)
  if viewer.beacon.valid then begin
    Cairo.arc ctx viewer.beacon.xbeacon viewer.beacon.ybeacon 0.04 0. (2. *. pi);
    set_color ctx Purple;
    Cairo.fill ctx;
    Cairo.arc ctx viewer.beacon.xbeacon viewer.beacon.ybeacon 0.04 0. (2. *. pi);
    set_color ctx Black;
    Cairo.stroke ctx
  end;

  let o, v = viewer.origin in

  (* Draw points. *)
  Cairo.set_source_rgb ctx 255. 255. 0.;
  Cairo.move_to ctx o.x o.y;
  List.iter (fun { x; y } -> Cairo.line_to ctx x y) viewer.vertices;
  Cairo.stroke ctx;

  (* Draw bezier curves. *)
  Cairo.set_source_rgb ctx 255. 0. 255.;
  Bezier.fold_curves
    (fun curve () ->
       let { x; y } = Bezier.vertice curve 0. in
       Cairo.move_to ctx x y;
       for i = 1 to 100 do
         let { x; y } = Bezier.vertice curve (float i /. 100.) in
         Cairo.line_to ctx x y
       done;
       Cairo.stroke ctx)
    v (o :: viewer.vertices) ();

  let ctx = Cairo_lablgtk.create viewer.ui#scene#misc#window in
  Cairo.set_source_surface ctx surface 0. 0.;
  Cairo.rectangle ctx 0. 0. width height;
  Cairo.fill ctx;
  Cairo.surface_finish surface

let queue_draw viewer =
  GtkBase.Widget.queue_draw viewer.ui#scene#as_widget

let add_point viewer x y =
  let { Gtk.width; Gtk.height } = viewer.ui#scene#misc#allocation in
  let width = float width and height = float height in
  let dw, dh = optimal_size width height in
  let scale = dw /. (world_width +. 0.204) in
  let x0 = (width -. dw) /. 2. and y0 = (height -. dh) /. 2. in
  let x = (x -. x0) /. scale -. 0.102 and y = world_height -. ((y -. y0) /. scale -. 0.102) in
  if x >= 0. && x < world_width && y >= 0. && y < world_height then
    ignore (Krobot_bus.send viewer.bus (Unix.gettimeofday (), Trajectory_add_vertice { x; y }))

let clear viewer =
  ignore (Krobot_bus.send viewer.bus (Unix.gettimeofday (), Trajectory_set_vertices []))

let rec last = function
  | [] -> failwith "Krobot_viewer.last"
  | [p] -> p
  | _ :: l -> last l

let simplify viewer =
  let tolerance = viewer.ui#tolerance#adjustment#value in
  ignore (Krobot_bus.send viewer.bus (Unix.gettimeofday (), Trajectory_simplify tolerance))

(* +-----------------------------------------------------------------+
   | Message handling                                                |
   +-----------------------------------------------------------------+ *)

let handle_message viewer (timestamp, message) =
  match message with
    | CAN frame -> begin
        match decode frame with
          | Odometry(x, y, theta) ->
              let angle = math_mod_float (theta) (2. *. pi) in
              let state = { pos = { x; y }; theta = angle } in
              if state <> viewer.state then begin
                viewer.state <- state;
                viewer.ui#entry_x#set_text (string_of_float x);
                viewer.ui#entry_y#set_text (string_of_float y);
                viewer.ui#entry_theta#set_text (string_of_float theta);
                queue_draw viewer
              end

          | Odometry_ghost(x, y, theta, following) ->
              let angle = math_mod_float (theta) (2. *. pi) in
              let ghost = { pos = { x; y }; theta = angle } in
              if ghost <> viewer.ghost then begin
                viewer.ghost <- ghost;
                queue_draw viewer
              end

          | Motor_status(m1, m2, m3, m4) ->
              let moving = m1 || m2 in
              if moving then begin
                viewer.statusbar_context#pop ();
                let _ = viewer.statusbar_context#push
                  (if m1 then
                     "Moving..."
                   else
                     (if m2 then
                        "Turning..."
                      else
                        "")
                  ) in ();
              end else
                viewer.statusbar_context#pop ();
              viewer.ui#entry_moving1#set_text (if m1 then "yes" else "no");
              viewer.ui#entry_moving2#set_text (if m2 then "yes" else "no");
              viewer.ui#entry_moving3#set_text (if m3 then "yes" else "no");
              viewer.ui#entry_moving4#set_text (if m4 then "yes" else "no")

          | Beacon_position(angle, distance, period) ->
              let newangle = math_mod_float (viewer.state.theta +. Krobot_config.rotary_beacon_index_pos +. angle) (2. *. pi) in
              let x = viewer.state.pos.x +. distance *. cos (newangle) in
              let y = viewer.state.pos.y +. distance *. sin (newangle) in
              let valid = distance <> 0. in
              let beacon = { xbeacon = x; ybeacon = y; valid; } in
              if beacon <> viewer.beacon then begin
                viewer.beacon <- beacon;
                viewer.ui#beacon_status#set_text (if valid then "valid" else "-");
                viewer.ui#beacon_distance#set_text (string_of_float distance);
                viewer.ui#beacon_angle#set_text (string_of_float angle);
                viewer.ui#beacon_period#set_text (string_of_float period);
                queue_draw viewer
              end

          | Set_controller_mode hil ->
              if hil then
                viewer.ui#menu_mode_hil#set_active true
              else
                viewer.ui#menu_mode_normal#set_active true

          | _ ->
              ()
      end

    | Kill "viewer" ->
        exit 0

    | Trajectory_origin(o, v) ->
        viewer.origin <- (o, v);
        queue_draw viewer

    | Trajectory_vertices l ->
        viewer.vertices <- l;
        queue_draw viewer

    | Trajectory_moving moving ->
        viewer.ui#button_go#misc#set_sensitive (not moving)

    | Log line ->
        viewer.ui#logs#buffer#insert (line ^ "\n");
        viewer.ui#scrolled_logs#vadjustment#set_value viewer.ui#scrolled_logs#vadjustment#upper

    | _ ->
        ()

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

lwt () =
  Lwt_log.default :=
    Lwt_log.channel
      ~template:"$(date).$(milliseconds) $(name): $(section): $(message)"
      ~close_mode:`Keep
      ~channel:Lwt_io.stderr
      ();

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  lwt bus = Krobot_bus.get () in
  ignore (GMain.init ());
  Lwt_glib.install ();

  let waiter, wakener = wait () in

  let ui = new Krobot_viewer_ui.window () in
  ignore (ui#window#connect#destroy ~callback:(wakeup wakener));
  ui#window#show ();

  (* Write logs to the log buffer. *)
  Lwt_log.default :=
    Lwt_log.broadcast [
      !Lwt_log.default;
      Lwt_log.make
        ~output:(fun section level lines ->
                   List.iter
                     (fun line ->
                        ui#logs#buffer#insert
                          (Printf.sprintf "krobot-viewer[%s]: %s\n" (Lwt_log.Section.name section) line))
                     lines;
                   ui#scrolled_logs#vadjustment#set_value ui#scrolled_logs#vadjustment#upper;
                   return ())
        ~close:return
    ];

  (* Create the viewer. *)
  let init = {
    pos = { x = 0.2;
            y = 1.9 +. Krobot_config.robot_size /. 2. -. Krobot_config.wheels_position };
    theta = -0.5 *. pi
  } in
  let viewer ={
    bus;
    ui;
    state = init;
    ghost = init;
    beacon = { xbeacon = 1.; ybeacon = 1.; valid = false };
    origin = ({ x = 0.; y = 0. }, { vx = 0.; vy = 0. });
    vertices = [];
    statusbar_context = ui#statusbar#new_context "";
  } in

  (* Handle messages. *)
  E.keep (E.map (fun msg -> handle_message viewer msg) (Krobot_bus.recv bus));

  (* Ask for initial parameters. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Send) in

  (* Adjusts The position of paned. *)
  viewer.ui#scene_paned#set_position ((viewer.ui#window#default_width * 5) / 8);

  ignore (ui#scene#event#connect#expose (fun ev -> draw viewer; true));
  ignore
    (ui#scene#event#connect#button_press
       (fun ev ->
          add_point viewer (GdkEvent.Button.x ev) (GdkEvent.Button.y ev);
          true));
  ignore
    (ui#scene#event#connect#motion_notify
       (fun ev ->
          add_point viewer (GdkEvent.Motion.x ev) (GdkEvent.Motion.y ev);
          true));

  ignore
    (ui#button_clear#event#connect#button_release
       (fun ev ->
          if GdkEvent.Button.button ev = 1 then
            clear viewer;
          false));

  ignore
    (ui#button_simplify#event#connect#button_release
       (fun ev ->
          if GdkEvent.Button.button ev = 1 then
            simplify viewer;
          false));

  ignore
    (ui#button_go#event#connect#button_release
       (fun ev ->
          if GdkEvent.Button.button ev = 1 then
            ignore (
              Krobot_bus.send bus
                (Unix.gettimeofday (),
                 Trajectory_go(ui#rotation_speed#adjustment#value,
                               ui#rotation_acceleration#adjustment#value,
                               ui#moving_speed#adjustment#value,
                               ui#moving_acceleration#adjustment#value))
            );
          false));

  ignore
    (ui#button_start_red#event#connect#button_release
       (fun ev ->
          if GdkEvent.Button.button ev = 1 then
            ignore (
              Krobot_message.send bus
                (Unix.gettimeofday (),
                 Set_odometry(0.215 -. Krobot_config.robot_size /. 2. +. Krobot_config.wheels_position, 1.885, 0.))
            );
          false));

  ignore
    (ui#button_start_blue#event#connect#button_release
       (fun ev ->
          if GdkEvent.Button.button ev = 1 then
            ignore_result (
              Krobot_message.send bus
                (Unix.gettimeofday (),
                 Set_odometry(Krobot_config.world_width -. (0.215 -. Krobot_config.robot_size /. 2. +. Krobot_config.wheels_position), 1.885, pi))
            );
          false));

  ignore
    (ui#button_stop#event#connect#button_release
       (fun ev ->
          if GdkEvent.Button.button ev = 1 then
            ignore (Krobot_bus.send bus (Unix.gettimeofday (), Trajectory_stop));
          false));

  ignore
    (ui#menu_mode_normal#event#connect#button_release
       (fun ev ->
          if GdkEvent.Button.button ev = 1 then
            ignore (
              Krobot_message.send bus
                (Unix.gettimeofday (),
                 Set_controller_mode false)
            );
          false));

  ignore
    (ui#menu_mode_hil#event#connect#button_release
       (fun ev ->
          if GdkEvent.Button.button ev = 1 then
            ignore_result (
              Krobot_message.send bus
                (Unix.gettimeofday (),
                 Set_controller_mode true)
            );
          false));

  pick [
    waiter;
    (* Sends motor status request continously. *)
    while_lwt true do
      lwt () = Krobot_message.send bus (Unix.gettimeofday (), Req_motor_status) in
      Lwt_unix.sleep 0.2
    done;
  ]
