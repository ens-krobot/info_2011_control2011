open Lwt
open Lwt_react
open Krobot_config
open Krobot_geom
open Krobot_bus
open Krobot_message

type robot = {
  bus : Krobot_bus.t;
  (* The bus used to communicate with the robot. *)
  mutable beacon : vertice option * vertice option;
  (* The detected position of the beacon, if any. *)
  mutable position : vertice option;
  (* The position of the robot on the table. *)
  mutable orientation : float option;
  (* The orientation of the robot. *)
  mutable destination : vertice option;
}

let handle_message robot (timestamp, message) =
  match message with
    | CAN(_, frame) -> begin
        match decode frame with
          | Beacon_position(angle1, angle2, distance1, distance2) ->
            begin
              match robot.position, robot.orientation with
                | Some position, Some orientation ->
                  let compute_beacon angle distance =
                    if distance <> 0. then begin
                      let angle = math_mod_float (orientation +. rotary_beacon_index_pos +. angle) (2. *. pi) in
                      Some{
                        x = position.x +. distance *. cos angle;
                        y = position.y +. distance *. sin angle;
                      }
                    end else
                      None
                  in
                  robot.beacon <- (compute_beacon angle1 distance1,
                                  compute_beacon angle2 distance2)
                | _, _ -> ()
            end
          | Odometry(x, y, theta) ->
            robot.position <- Some { x; y };
            robot.orientation <- Some (math_mod_float theta (2. *. pi))

          | _ ->
              ()
      end

    | Kill "pet" ->
        exit 0

    | _ ->
        ()

let set_leds bus f =
  lwt () = Krobot_message.send bus (Unix.gettimeofday (),
                                    Switch_request(5,f)) in
  lwt () = Krobot_message.send bus (Unix.gettimeofday (),
                                    Switch_request(6,f)) in
  lwt () = Krobot_message.send bus (Unix.gettimeofday (),
                                    Switch_request(7,f)) in
  return ()

let rec blink robot =
  lwt () = match robot.beacon with
    | None, None -> set_leds robot.bus false
    | _, _ -> set_leds robot.bus true in
  lwt () = Lwt_unix.sleep 0.01 in
  blink robot

(* +-----------------------------------------------------------------+
   | Main loop                                                       |
   +-----------------------------------------------------------------+ *)

let avoid_distance = 0.6
let reach_distance = 1.
let destination_change = 0.4

let goto_position robot origin orientation pos =
  robot.destination <- Some pos;
  let vect = { vx = cos orientation; vy = sin orientation } in
  let curves = List.rev
    (Bezier.fold_vertices
       (fun sign p q r s acc -> (sign, p, q, r, s) :: acc) vect
       [origin; pos] []) in
  let (sign, p, q, r, s) = List.hd curves in
  (* yurk *)
  let d1 = sign *. distance p q
  and d2 = distance r s in
  let d1,d2 =
    (if abs_float d1 <= 0.01
     then if d1 < 0. then -.0.1 else 0.1
     else d1),
    (if abs_float d2 <= 0.01
     then if d2 < 0. then -.0.1 else 0.1
     else d2)
  in
  let v = vector r s in
  let theta_end = atan2 v.vy v.vx in
  let command = Motor_bezier(s.x, s.y, d1, d2, theta_end, 0.2) in
  lwt () =
      Krobot_message.send robot.bus
        (Unix.gettimeofday (), Motor_stop (0.01, 0.01)) in
  lwt () = Lwt_unix.sleep 0.05 in
  lwt () =
      Krobot_message.send robot.bus
        (Unix.gettimeofday (),
         command) in
  return ()

let stop robot =
  (robot.destination <- None;
   Krobot_message.send robot.bus (Unix.gettimeofday (),
                                  Motor_stop (0.4, 1.)))
let run robot =
  while_lwt true do
  lwt () =
  match robot.position, robot.orientation with
    | Some position, Some orientation ->
      begin
        match robot.beacon with
          | None, None ->
            begin
              match robot.destination with
                | None -> return ()
                | Some destination ->
                  if distance position destination < avoid_distance
                  then stop robot
                  else return ()
            end
          | Some beacon, _
          | None, Some beacon ->
            if distance beacon position < avoid_distance
            then stop robot
            else
              match robot.destination with
                | None ->
                  if distance beacon position > reach_distance
                  then goto_position robot origin orientation beacon
                  else return ()
                | Some destination ->
                  if distance beacon destination > reach_distance
                  then goto_position robot origin orientation beacon
                  else return ()
      end
    | _ -> return ()
  in
  Lwt_unix.sleep 0.05
  done

(* +-----------------------------------------------------------------+
   | Command-line arguments                                          |
   +-----------------------------------------------------------------+ *)

let fork = ref true

let options = Arg.align [
  "-no-fork", Arg.Clear fork, " Run in foreground";
]

let usage = "\
Usage: krobot-pet [options]
options are:"

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

lwt () =
  Arg.parse options ignore usage;

  (* Display all informative messages. *)
  Lwt_log.append_rule "*" Lwt_log.Info;

  (* Open the krobot bus. *)
  lwt bus = Krobot_bus.get () in

  (* Fork if not prevented. *)
  if !fork then Krobot_daemon.daemonize bus;

  (* Kill any running vm. *)
  lwt () = Krobot_bus.send bus (Unix.gettimeofday (), Krobot_bus.Kill "pet") in

  (* Create a new robot. *)
  let robot = {
    bus;
    position = None;
    orientation = None;
    beacon = None, None;
    destination = None;
  } in

  (* Handle krobot message. *)
  E.keep (E.map (handle_message robot) (Krobot_bus.recv bus));

  ignore(blink robot);
  ignore(Krobot_message.send bus (Unix.gettimeofday (),
                                  Drive_activation true));
  ignore(Krobot_message.send bus (Unix.gettimeofday (),
                                  Motor_command (2,1000)));

  (* Run forever. *)
  run robot
