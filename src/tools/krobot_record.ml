(*
 * krobot_record.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Record CAN frames. *)

open Lwt
open Lwt_react
open Krobot_bus

(* Queue of messages to write. *)
let queue = Queue.create ()

(* Condition used to signal the writer thread. *)
let cond = Condition.create ()

(* Mutex used to protect [queue] and [cond]. *)
let mutex = Mutex.create ()

(* We use another system thread to dump things to the log file because
   disk IO are blocking and we do not want to block the whole system
   because of it. Also it is faster than using unix jobs. *)
let writer () =
  let tmp = Queue.create () in
  while true do
    Mutex.lock mutex;
    if Queue.is_empty queue then Condition.wait cond mutex;
    (* Take everything from the queue. *)
    Queue.transfer queue tmp;
    Mutex.unlock mutex;
    (* Write values to the log file. *)
    Queue.iter (output_value stdout) tmp;
    Queue.clear tmp
  done

lwt () =
  ignore (Thread.create writer ());
  lwt bus = Krobot_bus.get () in

  (* Messages to send to the writer thread. *)
  let to_send = Queue.create () in

  E.keep
    (E.map
       (fun x ->
         (* Do not send the message immediatly but wait a bit before
            so if other messages are available they are all sent in
            one batch. *)
         if Queue.is_empty to_send then
           ignore (
             Lwt.on_success (Lwt.pause ())
               (fun () ->
                 Mutex.lock mutex;
                 Queue.transfer to_send queue;
                 Condition.signal cond;
                 Mutex.unlock mutex)
           );
         Queue.add x to_send)
       (Krobot_bus.recv bus));

  fst (wait ())
