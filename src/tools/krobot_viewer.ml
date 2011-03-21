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

(* +-----------------------------------------------------------------+
   | LCD                                                             |
   +-----------------------------------------------------------------+ *)

module LCD = struct
  type t = {
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

  let create () = {
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

  let draw widget lcd =
    let colors = if lcd.backlight then colors_light else colors_dark in
    let { Gtk.width; Gtk.height } = widget#misc#allocation in
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
    let ctx = Cairo_lablgtk.create widget#misc#window in
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
  type state = {
    x : float;
    y : float;
    theta : float;
  }

  type t = {
    mutable state : state;
  }

  let world_height = 2.1
  let world_width = 3.
  let robot_size = 0.3
  let wheels_diam = 0.098
  let wheels_dist = 0.259
  let sim_step = 0.01
  let time = ref 0.

  type color =
    | Black
    | White
    | Green
    | Red
    | Blue
    | Yellow

  let set_color ctx color =
    let r, g, b = match color with
      | Black -> (0., 0., 0.)
      | White -> (255., 255., 255.)
      | Green -> (36., 145., 64.)
      | Red -> (199., 23., 18.)
      | Blue -> (0., 59., 128.)
      | Yellow -> (252., 189., 31.)
    in
    Cairo.set_source_rgb ctx (r /. 255.) (g /. 255.) (b /. 255.)

  let pi = 4. *. atan 1.

  let draw widget board =
    let { Gtk.width; Gtk.height } = widget#misc#allocation in
    let surface = Cairo.image_surface_create Cairo.FORMAT_ARGB32 width height in
    let ctx = Cairo.create surface in
    let width = float width and height = float height in

    (* Draw the background *)
    Cairo.rectangle ctx 0. 0. width height;
    set_color ctx White;
    Cairo.fill ctx;

    (* Compute the optimal width and height *)
    let dw, dh =
      if width /. height >= (world_width +. 0.204) /. (world_height +. 0.204) then
        ((world_width +. 0.204) /. (world_height +. 0.204) *. height, height)
      else
        (width, width /. (world_width +. 0.204) *. (world_height +. 0.204))
    in

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

    (* Draw the robot *)
    Cairo.translate ctx board.state.x board.state.y;
    Cairo.rotate ctx board.state.theta;
    Cairo.rectangle ctx (-. robot_size /. 2.) (-. robot_size /. 2.) robot_size robot_size;
    set_color ctx White;
    Cairo.fill ctx;

    (* Draw an arrow on the robot *)
    Cairo.move_to ctx (-. robot_size /. 4.) 0.;
    Cairo.line_to ctx (robot_size /. 4.) 0.;
    Cairo.line_to ctx 0. (-. robot_size /. 4.);
    Cairo.line_to ctx 0. (robot_size /. 4.);
    Cairo.line_to ctx (robot_size /. 4.) 0.;
    set_color ctx Black;
    Cairo.stroke ctx;

    let ctx = Cairo_lablgtk.create widget#misc#window in
    Cairo.set_source_surface ctx surface 0. 0.;
    Cairo.rectangle ctx 0. 0. width height;
    Cairo.fill ctx;
    Cairo.surface_finish surface
end

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

lwt () =
  lwt bus = Krobot_bus.get () in
  ignore (GMain.init ());
  Lwt_glib.install ();

  let waiter, wakener = wait () in

  let ui = new Krobot_viewer_ui.window () in
  ignore (ui#window#connect#destroy ~callback:(wakeup wakener));
  ui#window#show ();

  let lcd = LCD.create () in
  ignore (ui#lcd#event#connect#expose (fun ev -> LCD.draw ui#lcd lcd; true));

  let board = Board.({ state = { x = 0.2; y = 1.9; theta = 2. *. atan (-1.) } }) in
  ignore (ui#scene#event#connect#expose (fun ev -> Board.draw ui#scene board; true));

  waiter
