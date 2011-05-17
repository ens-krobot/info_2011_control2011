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
  | Trajectory_vertices of vertice list * (vertice * vertice * vertice * vertice) list
  | Trajectory_set_vertices of vertice list
  | Trajectory_add_vertice of vertice
  | Trajectory_simplify of float
  | Trajectory_go of float * float * float * float
  | Trajectory_stop
  | Trajectory_moving of bool
  | Objects of vertice list

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
  | Trajectory_vertices(vertices, curves) ->
      sprintf
        "Trajectory_vertices([%s], [%s])"
        (String.concat "; " (List.map string_of_vertice vertices))
        (String.concat "; " (List.map
                               (fun (p, q, r, s) ->
                                  Printf.sprintf
                                    "(%s, %s, %s, %s)"
                                    (string_of_vertice p)
                                    (string_of_vertice q)
                                    (string_of_vertice r)
                                    (string_of_vertice s))
                               curves))
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
  | Trajectory_go(a, b, c, d) ->
      sprintf
        "Trajectory_go(%f, %f, %f, %f)"
        a b c d
  | Trajectory_stop ->
      "Trajectory_stop"
  | Trajectory_moving b ->
      sprintf
        "Trajectory_moving %B"
        b
  | Objects objects ->
      sprintf
        "Objects [%s]"
        (String.concat "; " (List.map string_of_vertice objects))

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
    let logger =
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
    in

    Lwt_log.default := Lwt_log.broadcast [!Lwt_log.default; logger];

    return bus
  with exn ->
    ignore (Lwt_log.error ~section ~exn "failed to connect to the local hub");
    exit 1
)

let get () = Lazy.force bus
