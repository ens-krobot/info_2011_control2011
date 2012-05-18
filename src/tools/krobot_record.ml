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
  while true do
    Mutex.lock mutex;
    if Queue.is_empty queue then Condition.wait cond mutex;
    (* Take everything from the queue. *)
    let l = List.rev (Queue.fold (fun l x -> x :: l) [] queue) in
    Queue.clear queue;
    Mutex.unlock mutex;
    (* Write values to the log file. *)
    List.iter (output_value stdout) l
  done

lwt () =
  ignore (Thread.create writer ());
  lwt bus = Krobot_bus.get () in

  (* Add all messages to the queue. *)
  E.keep
    (E.map
       (fun x ->
         Mutex.lock mutex;
         Queue.add x queue;
         Condition.signal cond;
         Mutex.unlock mutex)
       (Krobot_bus.recv bus));

  fst (wait ())
