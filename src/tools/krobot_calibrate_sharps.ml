(*
 * krobot_calibrate_sharps.ml
 * --------------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Calibrate the sharps. *)

open Lwt
open Lwt_react
open Krobot_message

let step = 0.01

let handle_message sharps (ts, message) =
  match message with
    | Adc1_values(s1, s2, s3, s4) ->
        sharps.(0) <- s1;
        sharps.(1) <- s2;
        sharps.(2) <- s3;
        sharps.(3) <- s4
    | Adc2_values(s5, s6, s7, s8) ->
        sharps.(4) <- s5;
        sharps.(5) <- s6;
        sharps.(6) <- s7;
        sharps.(7) <- s8
    | _ ->
        ()

(* Record sharps continously. *)
let rec record bus oc sharps distances =
  (* Wait a bit to be sure the sharps are stabilized. *)
  lwt () = Lwt_unix.sleep 1.0 in

  (* Save the values measured by the sharps. *)
  lwt () =
    for_lwt i = 0 to 2 do
      Lwt_io.fprintlf oc "%d %d %f" i sharps.(i) distances.(i)
    done
  in
  lwt () = Lwt_io.flush oc in

  (* Move backward. *)
  lwt () = Krobot_message.send bus (Unix.gettimeofday (), Motor_move(-.step, 0.2, 0.4)) in

  (* Update distances. *)
  for i = 0 to 7 do
    distances.(i) <- distances.(i) +. step
  done;

  record bus oc sharps distances

lwt () =
  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  (* Create the array containing values measured by the sharps. *)
  let sharps = Array.make 8 0 in

  (* The array of distances. *)
  let distances = [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|] in

  (* Handle messsages. *)
  E.keep (E.map (handle_message sharps) (Krobot_message.recv bus));

  lwt oc = Lwt_io.open_file "sharps.data" ~mode:Lwt_io.output in

  record bus oc sharps distances
