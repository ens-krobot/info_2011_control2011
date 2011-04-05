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

let utf8 code =
  let set_byte s o x = String.unsafe_set s o (Char.unsafe_chr x) in
  if code < 0x80 then begin
    let s = String.create 1 in
    set_byte s 0 code;
    s
  end else if code <= 0x800 then begin
    let s = String.create 2 in
    set_byte s 0 ((code lsr 6) lor 0xc0);
    set_byte s 1 ((code land 0x3f) lor 0x80);
    s
  end else if code <= 0x10000 then begin
    let s = String.create 3 in
    set_byte s 0 ((code lsr 12) lor 0xe0);
    set_byte s 1 (((code lsr 6) land 0x3f) lor 0x80);
    set_byte s 2 ((code land 0x3f) lor 0x80);
    s
  end else if code <= 0x10ffff then begin
    let s = String.create 4 in
    set_byte s 0 ((code lsr 18) lor 0xf0);
    set_byte s 1 (((code lsr 12) land 0x3f) lor 0x80);
    set_byte s 2 (((code lsr 6) land 0x3f) lor 0x80);
    set_byte s 3 ((code land 0x3f) lor 0x80);
    s
  end else
    invalid_arg "utf8"

let pi = 4. *. atan 1.

let math_mod_float a b =
  let b2 = b /. 2. in
  let modf = mod_float a b in
  if modf > b2 then
    modf -. b
  else if modf < -. b2 then
    modf +. b
  else
    modf;;

(* +-----------------------------------------------------------------+
   | LCD                                                             |
   +-----------------------------------------------------------------+ *)

module LCD = struct
  type t = {
    ui : Krobot_viewer_ui.window;
    chars : char array array;
    mutable line : int;
    mutable column : int;
    mutable cursor : bool;
    mutable backlight : bool;
  }

  let lines = 3
  let columns = 20
  let inter = 4.
  let border = 2.

  let create ui = {
    ui;
    chars = Array.make_matrix lines columns ' ';
    line = 0;
    column = 0;
    cursor = true;
    backlight = true;
  }

  type colors = {
    background : float * float * float;
    text_background : float * float * float;
    text_foreground : float * float * float;
  }

  let colors_light = {
    background = (0.4, 0.4, 1.0);
    text_background = (0.0, 0.0, 0.7);
    text_foreground = (1.0, 1.0, 1.0);
  }

  let colors_dark = {
    background = (0.1, 0.1, 0.25);
    text_background = (0.0, 0.0, 0.7 /. 4.);
    text_foreground = (0.25, 0.25, 0.25);
  }

  let set_color ctx (r, g, b) =
    Cairo.set_source_rgb ctx r g b

  let draw lcd =
    let colors = if lcd.backlight then colors_light else colors_dark in
    let { Gtk.width; Gtk.height } = lcd.ui#lcd#misc#allocation in
    let surface = Cairo.image_surface_create Cairo.FORMAT_ARGB32 width height in
    let ctx = Cairo.create surface in
    Cairo.select_font_face ctx "Monospace" Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
    Cairo.set_font_size ctx 20.;
    set_color ctx colors.background;
    Cairo.rectangle ctx 0. 0. (float width) (float height);
    Cairo.fill ctx;
    let { Cairo.max_x_advance = fw;
          Cairo.font_height = fh;
          Cairo.descent = descent } = Cairo.font_extents ctx in
    for line = 0 to lines - 1 do
      for column = 0 to columns - 1 do
        let x = inter +. (fw +. inter +. border *. 2.0) *. float column
        and y = inter +. (fh +. inter +. border *. 2.0) *. float line in
        set_color ctx colors.text_background;
        Cairo.rectangle ctx x y (fw +. border *. 2.0) (fh +. border *. 2.0);
        Cairo.fill ctx;
        Cairo.move_to ctx (x +. border) (y +. fh -. descent +. border);
        set_color ctx colors.text_foreground;
        Cairo.show_text ctx (utf8 (Char.code lcd.chars.(line).(column)))
      done
    done;
    if lcd.cursor then begin
      let x = inter +. (fw +. inter +. border *. 2.0) *. float lcd.column
      and y = inter +. (fh +. inter +. border *. 2.0) *. float lcd.line in
      set_color ctx colors.text_foreground;
      Cairo.rectangle ctx x y (fw +. border *. 2.0) (fh +. border *. 2.0);
      Cairo.fill ctx
    end;
    let ctx = Cairo_lablgtk.create lcd.ui#lcd#misc#window in
    Cairo.set_source_surface ctx surface 0. 0.;
    Cairo.rectangle ctx 0. 0. (float width) (float height);
    Cairo.fill ctx;
    Cairo.surface_finish surface

  let add_char lcd ch =
    lcd.chars.(lcd.line).(lcd.column) <- ch;
    if lcd.column + 1 < columns then
      lcd.column <- lcd.column + 1
    else if lcd.line + 1 < lines then begin
      lcd.line <- lcd.line + 1;
      lcd.column <- 0
    end else begin
      lcd.line <- 0;
      lcd.column <- 0
    end

  let clear lcd =
    Array.iter (fun line -> Array.fill line 0 columns ' ') lcd.chars;
    lcd.column <- 0;
    lcd.line <- 0

  let set_cursor lcd state =
    lcd.cursor <- state

  let set_backlight lcd state =
    lcd.backlight <- state

  let goto lcd line column =
    lcd.line <- line mod lines;
    lcd.column <- column mod columns

  let write lcd text =
    String.iter (add_char lcd) text

  let write_line lcd ~line ~text =
    lcd.line <- line mod lines;
    lcd.column <- 0;
    write lcd text
end

(* +-----------------------------------------------------------------+
   | The board                                                       |
   +-----------------------------------------------------------------+ *)

module Board = struct
  open Krobot_config

  type state = {
    pos : vertice;
    theta : float;
  }

  type beacon = {
    xbeacon : float;
    ybeacon : float;
    valid : bool;
  }

  type t = {
    bus : Krobot_bus.t;
    ui : Krobot_viewer_ui.window;
    mutable state : state;
    mutable beacon : beacon;
    mutable points : vertice list;
    mutable bezier : vertice array list;
    mutable event : unit event;
    mutable moving : bool;
  }

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

  let draw board =
    let { Gtk.width; Gtk.height } = board.ui#scene#misc#allocation in
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

    (* Translation to have the board at the center and scaling to match the window sizes *)
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

    (* Draw the board background *)
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

    (* Draw the robot *)
    Cairo.translate ctx board.state.pos.x board.state.pos.y;
    Cairo.rotate ctx board.state.theta;
    Cairo.rectangle ctx (-. wheels_position) (-. robot_size /. 2.) robot_size robot_size;
    set_color ctx White;
    Cairo.fill ctx;

    (* Draw an arrow on the robot *)
    let d = robot_size /. 2. -. wheels_position in
    Cairo.move_to ctx 0. 0.;
    Cairo.line_to ctx (d +. robot_size /. 4.) 0.;
    Cairo.line_to ctx d (-. robot_size /. 4.);
    Cairo.line_to ctx d (robot_size /. 4.);
    Cairo.line_to ctx (d +. robot_size /. 4.) 0.;
    set_color ctx Black;
    Cairo.stroke ctx;

    Cairo.restore ctx;

    (* Draw the beacon *)
    if board.beacon.valid then begin
      Cairo.arc ctx board.beacon.xbeacon board.beacon.ybeacon 0.04 0. (2. *. pi);
      set_color ctx Purple;
      Cairo.fill ctx;
      Cairo.arc ctx board.beacon.xbeacon board.beacon.ybeacon 0.04 0. (2. *. pi);
      set_color ctx Black;
      Cairo.stroke ctx
    end;

    (* Draw points. *)
    Cairo.set_source_rgb ctx 255. 255. 0.;
    Cairo.move_to ctx board.state.pos.x board.state.pos.y;
    List.iter (fun { x; y } -> Cairo.line_to ctx x y) board.points;
    Cairo.stroke ctx;
    Cairo.set_source_rgb ctx 255. 0. 255.;
    List.iter (Array.iter (fun { x; y } -> Cairo.line_to ctx x y)) board.bezier;
    Cairo.stroke ctx;

    let ctx = Cairo_lablgtk.create board.ui#scene#misc#window in
    Cairo.set_source_surface ctx surface 0. 0.;
    Cairo.rectangle ctx 0. 0. width height;
    Cairo.fill ctx;
    Cairo.surface_finish surface

  let queue_draw board =
    GtkBase.Widget.queue_draw board.ui#scene#as_widget

  let add_point board x y =
    let { Gtk.width; Gtk.height } = board.ui#scene#misc#allocation in
    let width = float width and height = float height in
    let dw, dh = optimal_size width height in
    let scale = dw /. (world_width +. 0.204) in
    let x0 = (width -. dw) /. 2. and y0 = (height -. dh) /. 2. in
    let x = (x -. x0) /. scale -. 0.102 and y = world_height -. ((y -. y0) /. scale -. 0.102) in
    if x >= 0. && x < world_width && y >= 0. && y < world_height then begin
      board.points <- board.points @ [{ x; y }];
      queue_draw board
    end

  let clear board =
    board.points <- [];
    board.bezier <- [];
    queue_draw board

  let rec last = function
    | [] -> failwith "Krobot_viewer.Board.last"
    | [p] -> p
    | _ :: l -> last l

  let smooth board =
    let points = Array.of_list ({ x = board.state.pos.x; y = board.state.pos.y } :: board.points) in
    let tolerance = board.ui#tolerance#adjustment#value in
    let rec loop = function
      | i1 :: i2 :: rest ->
          let { x = x1; y = y1 } = points.(i1) and { x = x2; y = y2 } = points.(i2) in
          let a = y2 -. y1 and b = x1 -. x2 and c = x2 *. y1 -. x1 *. y2 in
          let r = sqrt (a *. a +. b *. b) in
          if r <> 0. then begin
            (* Search the furthest point from the line passing by (x1,
               y1) and (x2, y2) *)
            let max_dist = ref 0. and at_max = ref i1 in
            for i = i1 + 1 to i2 - 1 do
              let { x; y } = points.(i) in
              let d = abs_float (a *. x +. b *. y +. c) /. r in
              if d > !max_dist then begin
                max_dist := d;
                at_max := i
              end
            done;
            if !max_dist > tolerance then
              (* The furthest point is out of tolerance, we split the
                 current region with it. *)
              loop (i1 :: !at_max :: i2 :: rest)
            else
              (* The point is acceptable, we pass the next region. *)
              i1:: loop (i2 :: rest)
          end else
            (* The two point are the same so we drop one. *)
            loop (i2 :: rest)
      | rest ->
          rest
    in
    let result = List.tl (loop [0; Array.length points - 1]); in
    board.points <- List.map (fun i -> points.(i)) result;

    (* Compute cubic bezier curves. *)
    let rec loop = function
      |  p :: (q :: r :: s :: _ as rest) ->
           (* Computes tangents with a length that is half of the
              minimum length of the adjacent segments. *)
           let _, v1 = tangents p q r and v2, _ = tangents q r s in
           let v1 = v1 *| (min (distance p q) (distance q r) /. 2.)
           and v2 = v2 *| (min (distance q r) (distance r s) /. 2.) in

           (* Create the bezier curve. *)
           let curve = Bezier.of_vertices q (translate q v1) (translate r v2) r in

           (* Create vertices. *)
           let vertices = Array.create 101 origin in
           for i = 0 to 100 do
             vertices.(i) <- Bezier.vertice curve (float i /. 100.)
           done;

           vertices :: loop rest
      | _ ->
          []
    in
    board.bezier <- [||] :: loop ({ x = board.state.pos.x; y = board.state.pos.y } :: board.points);

    queue_draw board

  let wait_done board =
    lwt () = Lwt_log.info "waiting for the robot to stop moving" in
    lwt () = Lwt_unix.sleep 0.3 in
    lwt () =
      while_lwt board.moving do
        Lwt_unix.sleep 0.2
      done
    in
    Lwt_log.info "trajectory done"

  let go board =
    let rec loop () =
      match board.points with
        | { x; y } :: rest ->
            let sqr x = x *. x in
            let radius = sqrt (sqr (max wheels_position (robot_size -. wheels_position)) +. sqr (robot_size /. 2.)) in
            if x >= radius && x <= world_width -. radius && y >= radius && y <= world_height -. radius then begin
              (* Turn the robot. *)
              let alpha = math_mod_float (atan2 (y -. board.state.pos.y) (x -. board.state.pos.x) -. board.state.theta) (2. *. pi) in
              lwt () = Lwt_log.info_f "turning by %f radiants" alpha in
              lwt () = Krobot_message.send board.bus (Unix.gettimeofday (),
                                                      Motor_turn(alpha,
                                                                 board.ui#rotation_speed#adjustment#value,
                                                                 board.ui#rotation_acceleration#adjustment#value)) in
              lwt () = wait_done board in

              (* Move the robot. *)
              let dist = sqrt (sqr (x -. board.state.pos.x) +. sqr (y -. board.state.pos.y)) in
              lwt () = Lwt_log.info_f "moving by %f meters" dist in
              lwt () = Krobot_message.send board.bus (Unix.gettimeofday (),
                                                      Motor_move(dist,
                                                                 board.ui#moving_speed#adjustment#value,
                                                                 board.ui#moving_acceleration#adjustment#value)) in
              lwt () = wait_done board in

              (* Remove the point. *)
              (match board.points with
                 | _ :: l -> board.points <- l
                 | [] -> ());
              (match board.bezier with
                 | _ :: l -> board.bezier <- l
                 | [] -> ());

              (* Redraw everything without the last point. *)
              queue_draw board;

              loop ()
            end else
              Lwt_log.warning_f "can not move to (%f, %f)" x y
        | [] ->
            return ()
    in
    loop ()

  let create bus ui =
    let board ={
      bus;
      ui;
      state = {
        pos = { x = 0.2;
                y = 1.9 +. Krobot_config.robot_size /. 2. -. Krobot_config.wheels_position };
        theta = -0.5 *. pi
      };
      beacon = { xbeacon = 1.; ybeacon = 1.; valid = false };
      points = [];
      bezier = [];
      event = E.never;
      moving = false;
    } in
    board.ui#beacon_status#set_text "-";
    board.ui#beacon_distance#set_text "-";
    board.ui#beacon_angle#set_text "-";
    board.ui#beacon_period#set_text "-";
    queue_draw board;
    (* Move the robot on the board when we receive odometry
       informations. *)
    board.event <- (
      E.map
        (fun (ts, frame) ->
           match frame with
             | Odometry(x, y, theta) ->
                 let angle = math_mod_float (theta) (2. *. pi) in
                 let state = { pos = { x; y }; theta = angle } in
                 if state <> board.state then begin
                   board.state <- state;
                   board.ui#entry_x#set_text (string_of_float x);
                   board.ui#entry_y#set_text (string_of_float y);
                   board.ui#entry_theta#set_text (string_of_float theta);
                   queue_draw board
                 end
             | Motor_status(m1, m2, m3, m4) ->
                 board.moving <- m1 || m2;
                 board.ui#entry_moving1#set_text (if m1 then "yes" else "no");
                 board.ui#entry_moving2#set_text (if m2 then "yes" else "no");
                 board.ui#entry_moving3#set_text (if m3 then "yes" else "no");
                 board.ui#entry_moving4#set_text (if m4 then "yes" else "no")
             | Beacon_position(angle, distance, period) ->
                 let newangle = math_mod_float (board.state.theta +. Krobot_config.rotary_beacon_index_pos +. angle) (2. *. pi) in
                 let x = board.state.pos.x +. distance *. cos (newangle) in
                 let y = board.state.pos.y +. distance *. sin (newangle) in
                 let valid = distance <> 0. in
                 let beacon = { xbeacon = x; ybeacon = y; valid; } in
                 if beacon <> board.beacon then begin
                   board.beacon <- beacon;
                   board.ui#beacon_status#set_text (if valid then "valid" else "-");
                   board.ui#beacon_distance#set_text (string_of_float distance);
                   board.ui#beacon_angle#set_text (string_of_float angle);
                   board.ui#beacon_period#set_text (string_of_float period);
                   queue_draw board
                 end
             | _ ->
                 ())
        (Krobot_message.recv bus)
    );
    board
end

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
  Lwt_log.Section.set_level Lwt_log.Section.main Lwt_log.Info;

  lwt bus = Krobot_bus.get () in
  ignore (GMain.init ());
  Lwt_glib.install ();

  let waiter, wakener = wait () in

  let ui = new Krobot_viewer_ui.window () in
  ignore (ui#window#connect#destroy ~callback:(wakeup wakener));
  ui#window#show ();

  Lwt_log.default :=
    Lwt_log.broadcast [
      !Lwt_log.default;
      Lwt_log.make
        ~output:(fun section level lines ->
                   List.iter
                     (fun line ->
                        ui#logs#buffer#insert
                          (Printf.sprintf "%s: %s\n" (Lwt_log.Section.name section) line))
                     lines;
                   ui#scrolled_logs#vadjustment#set_value ui#scrolled_logs#vadjustment#upper;
                   return ())
        ~close:return
    ];

  let lcd = LCD.create ui in
  ignore (ui#lcd#event#connect#expose (fun ev -> LCD.draw lcd; true));

  let board = Board.create bus ui in
  ignore (ui#scene#event#connect#expose (fun ev -> Board.draw board; true));
  ignore
    (ui#scene#event#connect#button_press
       (fun ev ->
          Board.add_point board (GdkEvent.Button.x ev) (GdkEvent.Button.y ev);
          true));
  ignore
    (ui#scene#event#connect#motion_notify
       (fun ev ->
          Board.add_point board (GdkEvent.Motion.x ev) (GdkEvent.Motion.y ev);
          true));

  ignore
    (ui#button_clear#event#connect#button_release
       (fun ev ->
          if GdkEvent.Button.button ev = 1 then
            Board.clear board;
          false));

  ignore
    (ui#button_smooth#event#connect#button_release
       (fun ev ->
          if GdkEvent.Button.button ev = 1 then
            Board.smooth board;
          false));

  let thread_go = ref (return ()) in
  ignore
    (ui#button_go#event#connect#button_release
       (fun ev ->
          if GdkEvent.Button.button ev = 1 then
            ignore_result (
              ui#button_go#misc#set_sensitive false;
              try_lwt
                thread_go := Board.go board;
                !thread_go
              with
                | Canceled ->
                    return ()
              finally
                ui#button_go#misc#set_sensitive true;
                return ()
            );
          false));

  ignore
    (ui#button_start_red#event#connect#button_release
       (fun ev ->
          if GdkEvent.Button.button ev = 1 then
            ignore_result (
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
          if GdkEvent.Button.button ev = 1 then begin
            Board.clear board;
            cancel !thread_go;
            ignore_result (
              Krobot_message.send bus (Unix.gettimeofday (), Motor_stop)
            )
          end;
          false));

  pick [
    waiter;
    (* Sends motor status request continously. *)
    while_lwt true do
      lwt () = Krobot_message.send bus (Unix.gettimeofday (), Req_motor_status) in
      Lwt_unix.sleep 0.2
    done;
  ]
