(*
 * krobot_plot.ml
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Lwt_react
open Krobot_message

(* +-----------------------------------------------------------------+
   | Graphs                                                          |
   +-----------------------------------------------------------------+ *)

let graph_duration = 10.0
  (* Which amount of data to keep in graphs. *)

(* Type of graphs. *)
type graph = {
  points : (float * int) Queue.t array;
  (* Queue of points with their time. They are ordered by increasing
     date. *)
  mutable max : int;
  (* The maximum reached. *)
}

(* Remove old points. *)
let update_graph graph time =
  Array.iter
    (fun q ->
       while not (Queue.is_empty q) && fst (Queue.top q) +. graph_duration < time do
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
         (fun (date, position) ->
            let x = (date -. (time -. graph_duration)) /. graph_duration *. width
            and y = height -. height *. (float position /. float graph.max) in
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
   | Drawing                                                         |
   +-----------------------------------------------------------------+ *)

let draw window graph =
  while true do
    let { Gtk.width; Gtk.height } = window#misc#allocation in
    let surface = Cairo.image_surface_create Cairo.FORMAT_ARGB32 width height in
    let ctx = Cairo.create surface in
    plot ctx (float width) (float height) graph (Unix.gettimeofday ());
    let ctx = Cairo_lablgtk.create window#misc#window in
    Cairo.set_source_surface ctx surface 0. 0.;
    Cairo.rectangle ctx 0. 0. (float width) (float height);
    Cairo.fill ctx;
    Cairo.surface_finish surface;
    ignore (Unix.select [] [] [] (1. /. 25.))
  done

(* +-----------------------------------------------------------------+
   | Main-loop                                                       |
   +-----------------------------------------------------------------+ *)

lwt () =
  ignore (GMain.init ());
  Lwt_glib.install ();

  let waiter, wakener = wait () in

  (* GTK stuff. *)
  let window = GWindow.window ~title:"Krobot coders positions" () in
  ignore (window#connect#destroy ~callback:(wakeup wakener));
  window#show ();

  (* Create the graph. *)
  let graph = { points = Array.init 4 (fun _ -> Queue.create ()); max = 1 } in

  (* Draw in a separate thread. *)
  ignore (Thread.create (fun () -> draw window graph) ());

  lwt bus = Krobot_bus.get () in

  E.keep
    (E.map
       (function
          | Encoder_state_1_2(enc1, enc2) ->
              let time = Unix.gettimeofday () in
              graph.max <- max graph.max (max enc1.es_position enc2.es_position);
              Queue.push (time, enc1.es_position) graph.points.(0);
              Queue.push (time, enc2.es_position) graph.points.(1);
              update_graph graph time
          | Encoder_state_3_4(enc3, enc4) ->
              let time = Unix.gettimeofday () in
              graph.max <- max graph.max (max enc3.es_position enc4.es_position);
              Queue.push (time, enc3.es_position) graph.points.(2);
              Queue.push (time, enc4.es_position) graph.points.(3);
              update_graph graph time
          | _ ->
              ())
       (Krobot_message.recv bus));

  waiter
