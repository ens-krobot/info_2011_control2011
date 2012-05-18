(*
 * krobot_bus.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Lwt_react
open Krobot_geom

let section = Lwt_log.Section.make "krobot(bus)"
let port = 50000

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type frame_source = Elec | Info

type message =
  | CAN of frame_source * Krobot_can.frame
  | Log of string
  | Send
  | Kill of string
  | Trajectory_path of Bezier.curve list
  | Trajectory_set_vertices of vertice list
  | Trajectory_add_vertice of vertice
  | Trajectory_simplify of float
  | Trajectory_go
  | Trajectory_find_path
  | Objects of vertice list
  | Sharps of float array
  | Strategy_append of Krobot_action.t list
  | Strategy_stop
  | Strategy_set of Krobot_action.t list
  | Strategy_path of Bezier.curve list option
  | Set_fake_beacons of vertice option * vertice option
  | Coins of vertice list

type t = {
  oc : Lwt_io.output_channel;
  recv : (float * message) event;
}

(* +-----------------------------------------------------------------+
   | Message printer                                                 |
   +-----------------------------------------------------------------+ *)

open Printf

let string_of_vertice v =
  sprintf "{ x = %f; y = %f }" v.x v.y

let string_of_vector v =
  sprintf "{ vx = %f; vy = %f }" v.vx v.vy

let string_of_option f v =
  match v with
    | Some x ->
      "Some " ^ (f x)
    | None ->
      "None"

let string_of_message = function
  | CAN(source, frame) ->
      sprintf
        "CAN(%s, %s)"
        (match source with Elec -> "Elec" | Info -> "Info")
        (Krobot_can.string_of_frame frame)
  | Log str ->
      sprintf
        "Log %S"
        str
  | Send ->
      "Send"
  | Kill name ->
      sprintf
        "Kill %S"
        name
  | Trajectory_path curves ->
      sprintf
        "Trajectory_path [%s]"
        (String.concat "; " (List.map Bezier.string_of_curve curves))
  | Trajectory_set_vertices l ->
      sprintf
        "Trajectory_set_vertices [%s]"
        (String.concat "; " (List.map string_of_vertice l))
  | Trajectory_add_vertice v ->
      sprintf
        "Trajectory_add_vertice %s"
        (string_of_vertice v)
  | Trajectory_simplify tolerance ->
      sprintf
        "Trajectory_simplify %f"
        tolerance
  | Trajectory_go ->
      "Trajectory_go"
  | Trajectory_find_path ->
      "Trajectory_find_path"
  | Objects objects ->
      sprintf
        "Objects [%s]"
        (String.concat "; " (List.map string_of_vertice objects))
  | Sharps a ->
      sprintf
        "Sharps [|%s|]"
        (String.concat "; " (List.map string_of_float (Array.to_list a)))
  | Strategy_append l ->
      sprintf "Strategy_append [%s]" (String.concat "; " (List.map Krobot_action.to_string l))
  | Strategy_stop ->
      "Strategy_stop"
  | Strategy_set l ->
      sprintf "Strategy_set [%s]" (String.concat "; " (List.map Krobot_action.to_string l))
  | Strategy_path None ->
      "Strategy_path None"
  | Strategy_path(Some curves)  ->
      sprintf
        "Strategy_path(Some [%s])"
        (String.concat "; " (List.map Bezier.string_of_curve curves))
  | Set_fake_beacons (b1, b2) ->
      sprintf
        "Set_fake_beacons (%s, %s)"
        (string_of_option string_of_vertice b1)
        (string_of_option string_of_vertice b2)
  | Coins coins ->
      sprintf
        "Coins [%s]"
        (String.concat "; " (List.map string_of_vertice coins))

(* +-----------------------------------------------------------------+
   | Sending/receiving messages                                      |
   +-----------------------------------------------------------------+ *)

let send bus v = Lwt_io.write_value bus.oc v
let recv bus = bus.recv

(* +-----------------------------------------------------------------+
   | Dispatching incomming messages                                  |
   +-----------------------------------------------------------------+ *)

let dispatch ic emit =
  try_lwt
    while_lwt true do
      (* Read one message. *)
      lwt v = Lwt_io.read_value ic in

      (* Emit it. *)
      begin
        try
          emit v
        with exn ->
          ignore (Lwt_log.error ~section ~exn "message handler failed with")
      end;

      return ()
    done
  with exn ->
    ignore (Lwt_log.error ~section ~exn "lost connection");
    exit 1

(* +-----------------------------------------------------------------+
   | Logger                                                          |
   +-----------------------------------------------------------------+ *)

let logger bus =
  Lwt_log.make
    (fun section level lines ->
      let buf = Buffer.create 42 in
      List.iter
        (fun line ->
          Buffer.clear buf;
          Lwt_log.render ~buffer:buf ~template:"$(name)[$(section)]: $(message)" ~section ~level ~message:line;
          ignore (send bus (Unix.gettimeofday (), Log(Buffer.contents buf))))
        lines;
      return ())
    return

(* +-----------------------------------------------------------------+
   | Creation                                                        |
   +-----------------------------------------------------------------+ *)

let bus = lazy(
  try_lwt
    (* Open a connection to the local HUB. *)
    lwt ic, oc = Lwt_io.open_connection (Unix.ADDR_INET(Unix.inet_addr_loopback, port)) in

    (* The event for incomming message. *)
    let recv, emit = E.create () in

    (* Dispatch message forever. *)
    ignore (dispatch ic emit);

    (* Create the bus. *)
    let bus = { oc; recv } in

    (* Send logs over the bus. *)
    Lwt_log.default := Lwt_log.broadcast [!Lwt_log.default; logger bus];

    return bus
  with exn ->
    ignore (Lwt_log.error ~section ~exn "failed to connect to the local hub");
    exit 1
)

let get () = Lazy.force bus
