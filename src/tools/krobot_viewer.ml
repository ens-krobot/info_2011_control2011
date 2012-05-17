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

type viewer = {
  bus : Krobot_bus.t;
  (* The bus used by this viwer. *)

  ui : Krobot_viewer_ui.window;
  (* The UI of the viewer. *)

  statusbar_context : GMisc.statusbar_context;
  (* The context of the statusbar. *)

  mutable state : state;
  (* The state of the robot. *)

  mutable state_indep : state;
  (* The state of the robot according to indep coder. *)

  mutable ghost : state;
  (* The state of the ghost. *)

  mutable beacon : vertice option;
  (* The position of the beacon, if any. *)

  mutable planner_path : Bezier.curve list;
  (* The path of the planner. *)

  mutable vm_path : Bezier.curve list option;
  (* The path of the VM. *)

  mutable motor_status : bool * bool * bool *bool;
  (* Status of the four motor controller. *)

  mutable objects : vertice list;
  (* The objects on the table. *)

  mutable coins : vertice list;
  (* The coins on the table *)
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
  | Brown

let set_color ctx color =
  let r, g, b = match color with
    | Black -> (0., 0., 0.)
    | White -> (255., 255., 255.)
    | Green -> (36., 200., 64.)
    | Red -> (220., 23., 18.)
    | Blue -> (20., 80., 170.)
    | Yellow -> (232., 232., 0.)
    | Purple -> (180., 0., 128.)
    | Brown -> (175., 89., 67.)
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
  set_color ctx Purple;
  Cairo.fill ctx;

  Cairo.rectangle ctx (-0.102) (world_height /. 2. -. 0.04) 0.08 0.08;
  set_color ctx Red;
  Cairo.fill ctx;

  Cairo.rectangle ctx (-0.102) (world_height +. 0.022) 0.08 0.08;
  set_color ctx Purple;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width +. 0.022) (-0.102) 0.08 0.08;
  set_color ctx Red;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width +. 0.022) (world_height /. 2. -. 0.04) 0.08 0.08;
  set_color ctx Purple;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width +. 0.022) (world_height +. 0.022) 0.08 0.08;
  set_color ctx Red;
  Cairo.fill ctx;

  (* Draw the viewer background *)
  Cairo.rectangle ctx 0. 0. world_width world_height;
  set_color ctx Blue;
  Cairo.fill ctx;

  (* Draw the starting areas *)
  Cairo.rectangle ctx 0. (world_height -. 0.5) 0.5 0.5;
  set_color ctx Purple;
  Cairo.fill ctx;
  Cairo.rectangle ctx (world_width -. 0.5) (world_height -. 0.5) 0.5 0.5;
  set_color ctx Red;
  Cairo.fill ctx;

  (* draw black lines *)
(*
  Cairo.move_to ctx  0.5;
  Cairo.line_to ctx (world_height -. 0.45) (0.5 +. 0.15);
  Cairo.line_to ctx 0. (0.5 +. 0.15);
*)
  Cairo.move_to ctx 0.5 (world_height -. 0.45);
  Cairo.line_to ctx (0.5 +. 0.15) (world_height -. 0.45);
  Cairo.line_to ctx (0.5 +. 0.15) 0.;
  Cairo.move_to ctx (world_width -. 0.5) (world_height -. 0.45);
  Cairo.line_to ctx (world_width -. (0.5 +. 0.15)) (world_height -. 0.45);
  Cairo.line_to ctx (world_width -. (0.5 +. 0.15)) 0.;
  set_color ctx Black;
  Cairo.set_line_width ctx (5. /. scale);
  Cairo.stroke ctx;
  Cairo.set_line_width ctx (1. /. scale);

  (* draw the homes *)
  Cairo.move_to ctx 0. 0.;
  Cairo.line_to ctx 0.325 0.;
  Cairo.line_to ctx 0.4 (world_height -. 0.5);
  Cairo.line_to ctx 0. (world_height -. 0.5);
  set_color ctx Brown;
  Cairo.fill ctx;

  Cairo.move_to ctx world_width 0.;
  Cairo.line_to ctx (world_width -. 0.325) 0.;
  Cairo.line_to ctx (world_width -. 0.4) (world_height -. 0.5);
  Cairo.line_to ctx world_width (world_height -. 0.5);
  set_color ctx Brown;
  Cairo.fill ctx;

  Cairo.set_antialias ctx Cairo.ANTIALIAS_DEFAULT;

  (* Draw plages *)
  Cairo.arc ctx 1.5 world_height 0.4 pi (2.*.pi);
  set_color ctx Yellow;
  Cairo.fill ctx;

  Cairo.arc ctx 1.5 world_height 0.3 pi (2.*.pi);
  set_color ctx Green;
  Cairo.fill ctx;

  Cairo.arc ctx (0.64 +. 0.477) (world_height -. 1.) 0.3 0. (2.*.pi);
  set_color ctx Yellow;
  Cairo.fill ctx;

  Cairo.arc ctx (world_width -. (0.64 +. 0.477)) (world_height -. 1.) 0.3 0. (2.*.pi);
  set_color ctx Yellow;
  Cairo.fill ctx;

  Cairo.rectangle ctx (0.64 +. 0.477) 0.75 0.8 0.5;
  set_color ctx Yellow;
  Cairo.fill ctx;

  Cairo.arc ctx 1.5 0.24 0.55 (pi /. 4.) (3.*.pi/.4.);
  set_color ctx Blue;
  Cairo.fill ctx;

  Cairo.arc ctx 1.5 (world_height -. 0.24) 0.55 (5. *. pi /. 4.) (7.*.pi/.4.);
  set_color ctx Blue;
  Cairo.fill ctx;

  Cairo.arc ctx (0.64 +. 0.477) (world_height -. 1.) 0.2 0. (2.*.pi);
  set_color ctx Green;
  Cairo.fill ctx;

  Cairo.arc ctx (world_width -. (0.64 +. 0.477)) (world_height -. 1.) 0.2 0. (2.*.pi);
  set_color ctx Green;
  Cairo.fill ctx;

  Cairo.arc ctx 1.5 1. 0.1 0. (2.*.pi);
  set_color ctx Green;
  Cairo.fill ctx;

  Cairo.rectangle ctx (0.64 +. 0.477 -. 0.125) (1. -. 0.125) 0.25 0.25;
  set_color ctx Brown;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width -. (0.64 +. 0.477 +. 0.125)) (1. -. 0.125) 0.25 0.25;
  set_color ctx Brown;
  Cairo.fill ctx;

  Cairo.rectangle ctx 0.54 (-.0.1) 0.2 0.1;
  set_color ctx Purple;
  Cairo.fill ctx;

  Cairo.rectangle ctx (0.54 +. 0.47)  (-.0.1) 0.2 0.1;
  set_color ctx Red;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width -. 0.74) (-.0.1) 0.2 0.1;
  set_color ctx Red;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width -. (0.74 +. 0.47))  (-.0.1) 0.2 0.1;
  set_color ctx Purple;
  Cairo.fill ctx;

  (* Draw objects *)
  List.iter
    (fun { x; y } ->
       set_color ctx Yellow;
       Cairo.arc ctx x y 0.1 0. (2. *. pi);
       Cairo.fill ctx;

       set_color ctx Black;
       Cairo.arc ctx x y 0.1 0. (2. *. pi);
       Cairo.stroke ctx)
    viewer.objects;

  (* Draw coins *)
  List.iter
    (fun { x; y } ->
       set_color ctx White;
       Cairo.arc ctx x y Krobot_config.coin_radius 0. (2. *. pi);
       Cairo.fill ctx;

       set_color ctx Black;
       Cairo.arc ctx x y Krobot_config.coin_radius 0. (2. *. pi);
       Cairo.stroke ctx)
    viewer.coins;

  (* Draw obstacles *)
  Cairo.set_source_rgba ctx 255. 255. 255. 0.5;
  let () =
    let open Krobot_geom in
    List.iter
      (fun { pos = { x; y }; size } ->
        Cairo.arc ctx x y size 0. (2. *. pi);
        Cairo.fill ctx)
      Krobot_config.fixed_obstacles
  in

  (* Draw the robot and the ghost *)
  List.iter
    (fun (state, (r,g,b,alpha)) ->
       Cairo.save ctx;

       (* Draw the robot *)
       Cairo.translate ctx state.pos.x state.pos.y;
       Cairo.rotate ctx state.theta;
       Cairo.rectangle ctx (-. wheels_position) (-. robot_size /. 2.) robot_size robot_size;
       Cairo.set_source_rgba ctx r g b alpha;
       Cairo.fill ctx;

       (* Draw an arrow on the robot *)
       let d = robot_size /. 2. -. wheels_position in
       Cairo.move_to ctx 0. 0.;
       Cairo.line_to ctx (d +. robot_size /. 4.) 0.;
       Cairo.line_to ctx d (-. robot_size /. 4.);
       Cairo.line_to ctx d (robot_size /. 4.);
       Cairo.line_to ctx (d +. robot_size /. 4.) 0.;
       Cairo.set_source_rgba ctx 0. 0. 0. 0.5;
       Cairo.stroke ctx;

       Cairo.restore ctx)
    [(viewer.ghost, (1., 1., 1., 0.5));
     (viewer.state_indep, (0.8, 0.8, 1., 0.8));
     (viewer.state, (1., 1., 1., 1.5));];

  (* Draw the beacon *)
  begin
    match viewer.beacon with
      | Some v ->
          Cairo.arc ctx v.x v.y 0.04 0. (2. *. pi);
          set_color ctx Purple;
          Cairo.fill ctx;
          Cairo.arc ctx v.x v.y 0.04 0. (2. *. pi);
          set_color ctx Black;
          Cairo.stroke ctx
      | None ->
          ()
  end;

  (* Draw the path of the VM if any or the path of the planner if the
     VM is not following a trajectory. *)
  let path =
    match viewer.vm_path with
      | Some path -> path
      | None -> viewer.planner_path
  in

  (* Draw points. *)
  Cairo.set_source_rgb ctx 255. 255. 0.;
  (match path with
     | [] ->
         ()
     | curve :: curves ->
         let src = Bezier.src curve and dst = Bezier.dst curve in
         Cairo.move_to ctx src.x src.y;
         Cairo.line_to ctx dst.x dst.y;
         List.iter (fun curve -> let v = Bezier.dst curve in Cairo.line_to ctx v.x v.y) curves;
         Cairo.stroke ctx);

  (* Draw bezier curves. *)
  Cairo.set_source_rgb ctx 255. 0. 255.;
  List.iter
    (fun curve ->
       let { x; y } = Bezier.vertice curve 0. in
       Cairo.move_to ctx x y;
       for i = 1 to 100 do
         let { x; y } = Bezier.vertice curve (float i /. 100.) in
         Cairo.line_to ctx x y
       done;
       Cairo.stroke ctx)
    path;

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
    | CAN(_, frame) -> begin
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

          | Odometry_indep(x, y, theta) ->
              let angle = math_mod_float (theta) (2. *. pi) in
              let state = { pos = { x; y }; theta = angle } in
              if state <> viewer.state_indep then begin
                viewer.state_indep <- state;
                viewer.ui#entry_x_indep#set_text (string_of_float x);
                viewer.ui#entry_y_indep#set_text (string_of_float y);
                viewer.ui#entry_theta_indep#set_text (string_of_float theta);
                queue_draw viewer
              end

          | Odometry_ghost(x, y, theta, u, following) ->
              let angle = math_mod_float (theta) (2. *. pi) in
              let ghost = { pos = { x; y }; theta = angle } in
              if ghost <> viewer.ghost then begin
                viewer.ghost <- ghost;
                queue_draw viewer
              end

          | Motor_status(m1, m2, m3, m4) ->
              if (m1, m2, m3, m4) <> viewer.motor_status then begin
                viewer.motor_status <- (m1, m2, m3, m4);
                if m1 || m2 then begin
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
              end

          | Beacon_position(angle1, angle2, distance1, distance2) ->
              let compute_beacon angle distance =
                if distance <> 0. then begin
                  let angle = math_mod_float (viewer.state.theta +. rotary_beacon_index_pos +. angle) (2. *. pi) in
                  Some {
                    x = viewer.state.pos.x +. distance *. cos angle;
                    y = viewer.state.pos.y +. distance *. sin angle;
                  }
                end else
                  None
              in
              let beacon1 = compute_beacon angle1 distance1 in
              (*let beacon2 = compute_beacon angle2 distance2 in*)
              if beacon1 <> viewer.beacon then begin
                viewer.beacon <- beacon1;
                viewer.ui#beacon_status#set_text (if beacon1 = None then "-" else "valid");
                viewer.ui#beacon_distance#set_text (string_of_float distance1);
                viewer.ui#beacon_angle#set_text (string_of_float angle1);
                viewer.ui#beacon_period#set_text "-";
                queue_draw viewer
              end

          | Beacon_lowlevel_position(_, _, period) ->
            viewer.ui#beacon_period#set_text (string_of_int period);
            queue_draw viewer

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

    | Trajectory_path curves ->
        viewer.planner_path <- curves;
        queue_draw viewer

    | Strategy_path None ->
        viewer.vm_path <- None;
        viewer.ui#button_go#misc#set_sensitive true;
        queue_draw viewer

    | Strategy_path(Some curves) ->
        viewer.vm_path <- Some curves;
        viewer.ui#button_go#misc#set_sensitive false;
        queue_draw viewer

    | Log line ->
        viewer.ui#logs#buffer#insert (line ^ "\n");
        viewer.ui#scrolled_logs#vadjustment#set_value viewer.ui#scrolled_logs#vadjustment#upper

    | Objects l ->
        viewer.objects <- l;
        queue_draw viewer

    | Coins l ->
        viewer.coins <-
          List.map
            (fun v ->
              let v = [|v.x;v.y;1.|] in
              let v = mult (rot_mat viewer.state.theta) v in
              Krobot_geom.translate viewer.state.pos { vx = v.(0); vy = v.(1) }) l;
        queue_draw viewer

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
    state_indep = init;
    ghost = init;
    beacon = None;
    planner_path = [];
    vm_path = None;
    statusbar_context = ui#statusbar#new_context "";
    motor_status = (false, false, false, false);
    objects = [];
    coins = [];
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
    (ui#button_find#event#connect#button_release
       (fun ev ->
          if GdkEvent.Button.button ev = 1 then
            ignore (Krobot_bus.send viewer.bus (Unix.gettimeofday (), Trajectory_find_path));
          false));

  ignore
    (ui#button_go#event#connect#button_release
       (fun ev ->
          if GdkEvent.Button.button ev = 1 then
            ignore (Krobot_bus.send bus (Unix.gettimeofday (), Trajectory_go));
          false));

  ignore
    (ui#button_goto#event#connect#button_release
       (fun ev ->
          if GdkEvent.Button.button ev = 1 then begin
            match viewer.planner_path with
              | curve :: _ ->
                  ignore (Krobot_bus.send bus (Unix.gettimeofday (), Strategy_set [Krobot_action.Goto(Bezier.dst curve)]))
              | _ ->
                  ()
          end;
          false));

  ignore
    (ui#button_start_red#event#connect#button_release
       (fun ev ->
          if GdkEvent.Button.button ev = 1 then
            ignore (Krobot_bus.send bus (Unix.gettimeofday (), Strategy_set [Krobot_action.Reset_odometry `Red]));
          false));

  ignore
    (ui#button_start_blue#event#connect#button_release
       (fun ev ->
          if GdkEvent.Button.button ev = 1 then
            ignore (Krobot_bus.send bus (Unix.gettimeofday (), Strategy_set [Krobot_action.Reset_odometry `Blue]));
          false));

  ignore
    (ui#button_stop#event#connect#button_release
       (fun ev ->
          if GdkEvent.Button.button ev = 1 then
            ignore (Krobot_bus.send bus (Unix.gettimeofday (), Strategy_stop));
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


  let send_motor_limit () =
    let v_max = ui#v_max#adjustment#value in
    let a_tan_max = ui#a_tan_max#adjustment#value in
    let a_rad_max = ui#a_rad_max#adjustment#value in
    ignore (Krobot_bus.send viewer.bus
              (Unix.gettimeofday (),
               CAN (Info,
                    Krobot_message.encode
                      (Motor_bezier_limits (v_max, a_tan_max, a_rad_max))))) in

  ignore
    (ui#v_max#connect#value_changed
       (fun () -> send_motor_limit ()));
  ignore
    (ui#a_tan_max#connect#value_changed
       (fun () -> send_motor_limit ()));
  ignore
    (ui#a_rad_max#connect#value_changed
       (fun () -> send_motor_limit ()));

  waiter
