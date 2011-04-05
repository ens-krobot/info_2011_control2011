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
open Krobot_graph

(* +-----------------------------------------------------------------+
   | Main-loop                                                       |
   +-----------------------------------------------------------------+ *)

lwt () =
  lwt bus = Krobot_bus.get () in
  ignore (GMain.init ());
  Lwt_glib.install ();

  let waiter, wakener = wait () in

  (* GTK stuff. *)
  let window = GWindow.window ~title:"Krobot coders positions" () in
  ignore (window#connect#destroy ~callback:(wakeup wakener));
  window#show ();

  (* Create the graph. *)
  let graph = { points = Array.init 2 (fun _ -> Queue.create ()); max = 1. } in
  E.keep
    (E.map
       (fun (timestamp, msg) ->
          match msg with
            | Encoder_position_direction_3_4(pos3, dir3, pos4, dir4) ->
                let pos3 = float pos3 and pos4 = float pos4 in
                graph.max <- max graph.max (max pos3 pos4);
                Queue.push (timestamp, pos3) graph.points.(0);
                Queue.push (timestamp, pos4) graph.points.(1);
                update_graph graph timestamp
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
