(*
 * krobot_remote.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt

let rec copy ta tb =
  lwt msg = OBus_transport.recv ta in
  lwt () = OBus_transport.send tb msg in
  copy ta tb

let handle_connection server ta =
  ignore (
    let process = Lwt_process.open_process ("ssh", [|"ssh"; "krobot"; "krobot-local"|]) in
    try_lwt
      lwt _ = Lwt_io.read_char process#stdout in
      let tb =
        OBus_transport.make
          ~send:(fun msg -> OBus_wire.write_message process#stdin msg)
          ~recv:(fun () -> OBus_wire.read_message process#stdout)
          ~shutdown:(fun () -> process#close >> return ())
          ()
      in
      ignore (
        try_lwt
          copy ta tb <&> copy tb ta
        with exn ->
          OBus_transport.shutdown ta <&> OBus_transport.shutdown tb
      );
      return ()
    with exn ->
      lwt _ = process#close and () = OBus_transport.shutdown ta in
      lwt () = Lwt_log.error_f ~exn "failed to open connection" in
      return ()
  )

lwt () =
  lwt server =
    OBus_server.make_lowlevel
      ~addresses:[OBus_address.make "unix" [("abstract", "krobot")]]
      handle_connection
  in
  fst (wait ())
