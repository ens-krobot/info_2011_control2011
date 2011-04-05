(*
 * krobot_plot_battery.ml
 * ----------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Lwt_react
open Krobot_message

lwt () =
  lwt bus = Krobot_bus.get () in
  ignore (GMain.init ());
  Lwt_glib.install ();

  let waiter, wakener = wait () in

  (* GTK stuff. *)
  let window = GWindow.window ~title:"Krobot batteries" () in
  ignore (window#connect#destroy ~callback:(wakeup wakener));
  window#show ();

  (* Create the graph. *)
  let graph = { Krobot_graph.points = Array.init 2 (fun _ -> Queue.create ());
                Krobot_graph.max = 14. } in

  E.keep
    (E.map
       (fun (timestamp, msg) ->
          match msg with
            | Battery1_voltages(x1, x2, x3, x4) ->
                let s = x1 +. x2 +. x3 +. x4 in
                graph.Krobot_graph.max <- max graph.Krobot_graph.max s;
                Queue.push (timestamp, s) graph.Krobot_graph.points.(0);
                Krobot_graph.update_graph graph timestamp
            | Battery2_voltages(x1, x2, x3, x4) ->
                let s = x1 +. x2 +. x3 +. x4 in
                graph.Krobot_graph.max <- max graph.Krobot_graph.max s;
                Queue.push (timestamp, s) graph.Krobot_graph.points.(1);
                Krobot_graph.update_graph graph timestamp
            | _ ->
                ())
       (Krobot_message.recv bus));

  pick [
    waiter;
    while_lwt true do
      Krobot_graph.draw window graph;
      Lwt_unix.sleep (1. /. 25.)
    done;
  ]
