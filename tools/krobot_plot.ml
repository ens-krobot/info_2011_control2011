(*
 * krobot_plot.ml
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Krobot_can

type direction = Forward | Backward

type point = { time : float; position : int; direction : direction }

(* +-----------------------------------------------------------------+
   | Graphs                                                          |
   +-----------------------------------------------------------------+ *)

let graph_duration = 10.0
  (* Which amount of data to keep in graphs. *)

(* Type of graphs. *)
type graph = {
  points : point Queue.t array;
  (* Queue of points with their time. They are ordered by increasing
     date. *)
  mutable max : int;
  (* The maximum reached. *)
}

(* Remove old points. *)
let update_graph graph time =
  Array.iter
    (fun q ->
       while not (Queue.is_empty q) && (Queue.top q).time +. graph_duration < time do
         ignore (Queue.take q)
       done)
    graph.points

(* +-----------------------------------------------------------------+
   | Plotting                                                        |
   +-----------------------------------------------------------------+ *)

let rec colors = (1., 0., 0.) :: (0., 1., 0.) :: (0., 0., 1.) :: (1., 1., 0.) :: colors

let plot ctx width height graph time =
  Cairo.set_source_rgb ctx 1. 1. 1.;
  Cairo.rectangle ctx 0. 0. width height;
  Cairo.fill ctx;
  let colors = ref colors in
  Array.iter
    (fun q ->
       let r, g, b = List.hd !colors in
       colors := List.tl !colors;
       Cairo.set_source_rgb ctx r g b;
       let prev = ref None in
       Queue.iter
         (fun point ->
            let x = (point.time -. (time -. graph_duration)) /. graph_duration *. width
            and y = height -. height *. (float point.position /. float graph.max) in
            match !prev with
              | None ->
                  prev := Some(x, y)
              | Some(x', y') ->
                  prev := Some(x, y);
                  Cairo.move_to ctx x' y';
                  Cairo.line_to ctx x y;
                  Cairo.stroke ctx)
         q)
    graph.points

(* +-----------------------------------------------------------------+
   | Decoding frames                                                 |
   +-----------------------------------------------------------------+ *)

let decode_frame time frame =
  let data = Krobot_can.data frame in
  ({ time = time;
     position = get_uint16 data 0;
     direction = if get_uint8 data 4 = 0 then Forward else Backward },
   { time = time;
     position = get_uint16 data 2;
     direction = if get_uint8 data 5 = 0 then Forward else Backward })

(* +-----------------------------------------------------------------+
   | Main-loop                                                       |
   +-----------------------------------------------------------------+ *)

lwt () =
  if Array.length Sys.argv <> 2 then begin
    print_endline "usage: krobot-plot <interface>";
    exit 2;
  end;
  ignore (GMain.init ());
  Lwt_glib.install ();

  let window = GWindow.window ~title:"Krobot coders positions" () in
  ignore (window#connect#destroy ~callback:(fun () -> exit 0));
  window#show ();

  let graph = { points = Array.init 4 (fun _ -> Queue.create ()); max = 1 } in

  try_lwt
    lwt bus = Krobot_can_bus.open_can Sys.argv.(1) in
    join [
      (* Data reading. *)
      while_lwt true do
        (* Read coder positions. *)
        lwt coder1, coder2 = Krobot_can_bus.recv bus >|= (fun frame -> decode_frame (Unix.gettimeofday ()) frame) in
        lwt coder3, coder4 = Krobot_can_bus.recv bus >|= (fun frame -> decode_frame (Unix.gettimeofday ()) frame) in
        (* Compute the new maximum. *)
        graph.max <- List.fold_left max graph.max [coder1.position; coder2.position; coder3.position; coder4.position];
        (* Add points to the graph. *)
        Queue.push coder1 graph.points.(0);
        Queue.push coder2 graph.points.(1);
        Queue.push coder3 graph.points.(2);
        Queue.push coder4 graph.points.(3);
        (* Remove old points. *)
        update_graph graph (Unix.gettimeofday ());
        return ()
      done;
      (* Drawing. *)
      while_lwt true do
        let { Gtk.width; Gtk.height } = window#misc#allocation in
        let surface = Cairo.image_surface_create Cairo.FORMAT_ARGB32 width height in
        let ctx = Cairo.create surface in
        plot ctx (float width) (float height) graph (Unix.gettimeofday ());
        let ctx = Cairo_lablgtk.create window#misc#window in
        Cairo.set_source_surface ctx surface 0. 0.;
        Cairo.rectangle ctx 0. 0. (float width) (float height);
        Cairo.fill ctx;
        Cairo.surface_finish surface;
        Lwt_unix.sleep 0.05
      done;
    ]
  with Unix.Unix_error(error, func, arg) ->
    Lwt_log.error_f "'%s' failed with: %s" func (Unix.error_message error)
