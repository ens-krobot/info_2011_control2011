(*
 * krobot_graph.ml
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* +-----------------------------------------------------------------+
   | Graphs                                                          |
   +-----------------------------------------------------------------+ *)

let graph_duration = 10.0
  (* Which amount of data to keep in graphs. *)

(* Type of graphs. *)
type graph = {
  points : (float * float) Queue.t array;
  (* Queue of points with their time. They are ordered by increasing
     date. *)
  mutable max : float;
  (* The maximum reached. *)
}

(* Remove old points. *)
let update_graph graph time =
  Array.iter
    (fun q ->
       while not (Queue.is_empty q) && fst (Queue.top q) +. graph_duration *. 1.5 < time do
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
            and y = height -. height *. (position /. graph.max) in
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
  let { Gtk.width; Gtk.height } = window#misc#allocation in
  let surface = Cairo.image_surface_create Cairo.FORMAT_ARGB32 width height in
  let ctx = Cairo.create surface in
  plot ctx (float width) (float height) graph (Unix.gettimeofday ());
  let ctx = Cairo_lablgtk.create window#misc#window in
  Cairo.set_source_surface ctx surface 0. 0.;
  Cairo.rectangle ctx 0. 0. (float width) (float height);
  Cairo.fill ctx;
  Cairo.surface_finish surface
