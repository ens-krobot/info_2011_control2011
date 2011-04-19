(*
 * krobot_hub.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* The krobot HUB. *)

open Lwt

let section = Lwt_log.Section.make "krobot(hub)"
let port = 50000

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type hub = {
  connections : (Lwt_io.input_channel * Lwt_io.output_channel) Lwt_sequence.t;
  (* Connections to programs or other HUBs. *)
}

(* +-----------------------------------------------------------------+
   | Dispatching                                                     |
   +-----------------------------------------------------------------+ *)

let dispatch hub node =
  let ic, oc = Lwt_sequence.get node in
  let header = String.create Marshal.header_size in
  try_lwt
    while_lwt true do
      (* Read one header. *)
      lwt () = Lwt_io.read_into_exactly ic header 0 Marshal.header_size in

      (* Allocate a buffer for the whole the message. *)
      let data_size = Marshal.data_size header 0 in
      let buffer = String.create (Marshal.header_size + data_size) in

      (* Copy the header into the buffer. *)
      String.blit header 0 buffer 0 Marshal.header_size;

      (* Read the rest of the message. *)
      lwt () = Lwt_io.read_into_exactly ic buffer Marshal.header_size data_size in

      (* Broadcast the message on other connections. *)
      Lwt_sequence.iter_node_l
        (fun node' ->
           if node != node' then
             ignore begin
               let _, oc = Lwt_sequence.get node' in
               try_lwt
                 Lwt_io.write_from_exactly oc buffer 0 (String.length buffer)
               with exn ->
                 Lwt_log.error_f ~section ~exn "failed to send message"
             end)
        hub.connections;

      return ()
    done
  with exn ->
    Lwt_sequence.remove node;
    ignore (Lwt_log.error_f ~section ~exn "reception error");
    try_lwt
      Lwt_io.close ic <&> Lwt_io.close oc
    with exn ->
      return ()

(* +-----------------------------------------------------------------+
   | Connection to another HUB                                       |
   +-----------------------------------------------------------------+ *)

let link hub host =
  while_lwt true do
    try_lwt
      (* Resolve the host. *)
      lwt entry = Lwt_unix.gethostbyname host in

      (* Create the address of the host. *)
      let addr = Unix.ADDR_INET(entry.Unix.h_addr_list.(0), port) in

      (* Try to connect to the remote HUB. *)
      lwt channels = Lwt_io.open_connection addr in

      (* Add the connection to the local HUB. *)
      let node = Lwt_sequence.add_r channels hub.connections in

      (* Dispatch until error. *)
      dispatch hub node
    with exn ->
      ignore (Lwt_log.error_f ~section ~exn "cannot connect to '%s'" host);

      (* Wait a bit before retrying. *)
      Lwt_unix.sleep 1.0
  done

(* +-----------------------------------------------------------------+
   | Connection handler                                              |
   +-----------------------------------------------------------------+ *)

let handle_connection hub channels =
  (* Add the connection to the local HUB. *)
  let node = Lwt_sequence.add_r channels hub.connections in

  (* Dispatch until error. *)
  ignore (dispatch hub node)

(* +-----------------------------------------------------------------+
   | Command line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
]

let usage = "\
Usage: krobot-hub [options] [addresses]
options are:"

let hosts = ref []

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

lwt () =
  Arg.parse options (fun host -> hosts := host :: !hosts) usage;

  (* Create the local HUB. *)
  let hub = { connections = Lwt_sequence.create () } in

  (* Establish the local server. *)
  ignore (Lwt_io.establish_server (Unix.ADDR_INET(Unix.inet_addr_any, port)) (fun channels -> handle_connection hub channels));

  (* Launch link to other HUBs. *)
  List.iter (fun host -> ignore (link hub host)) !hosts;

  (* Fork if not prevented. *)
  if !fork then Lwt_daemon.daemonize ();

  (* Wait forever. *)
  fst (wait ())
