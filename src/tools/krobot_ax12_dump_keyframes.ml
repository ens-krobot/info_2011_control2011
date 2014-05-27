open Krobot_bus
open Krobot_message
open Lwt_react

let keyframe_timeout = Some 1.

lwt bus = Krobot_bus.get ()

type ax12_info =
    { id : int;
      pos : int;
      speed : int;
      torque : int;
      time : float }

type timeout_state =
  | No_timeout
  | Timed_out

module IntMap = Map.Make(struct type t = int let compare = compare end)

let infos = ref IntMap.empty
let messages_to_receive = ref 0
let wait_for_messages timeout =
  let end_date = match timeout with
    | None -> None
    | Some t -> Some ((Unix.gettimeofday ()) +. t)
  in
  let rec aux () =
    if (!messages_to_receive != 0) then
      match end_date with
        | Some date ->
          if Unix.gettimeofday () > date then
            Lwt.return Timed_out
          else
            lwt () = Lwt_unix.sleep 0.01 in aux ()
        | None ->
            lwt () = Lwt_unix.sleep 0.01 in aux ()
      else
        Lwt.return No_timeout
  in
  aux ()

let previous = ref IntMap.empty
let is_previous info =
  try
    let v = IntMap.find info.id !previous in
    (v.pos,v.speed) = (info.pos,info.speed)
  with
    | Not_found -> false

let print_info (key_id, { id; pos; speed; torque; time }) =
  (*previous := IntMap.add id info !previous;*)
  Printf.printf "key %i, id %i, pos %i, speed %i, torque %i, time %f\n%!" key_id id pos speed torque time

let log () =
  E.keep
    (E.map
       (fun (time, message) ->
         match message with
           | CAN(_, frame) ->
             begin
               match Krobot_message.decode frame with
                 | Ax12_State (id,pos,speed,torque) ->
                   let info = { id;pos;speed;torque;time } in
                   (*if not (is_previous info)
                   then print_info info*)
                   infos := IntMap.add id info !infos;
                   (*print_info info;*)
                   if !messages_to_receive > 0 then messages_to_receive := !messages_to_receive - 1
                 | _ -> ()
             end
           | _ -> ())
       (Krobot_bus.recv bus))

let ax12_id = ref []
let ax12_delay = ref 0.02
let spec =
  [ "-id", Arg.Int (fun i -> ax12_id := i::(!ax12_id)), "id of the recorded ax12";
    "-delay", Arg.Set_float ax12_delay, "delay between to points"; ]

let msg = "record ax12 movement ax12"
let message _ = Arg.usage spec msg; flush stdout; exit 0
let () = Arg.parse spec message msg

let rec loop_request key_idx =
  (*lwt () = Lwt_unix.sleep 0.01 in*)
  Printf.eprintf "Waiting for keyframe %d : press ENTER... %!" key_idx;
  lwt _ = Lwt_io.read_line Lwt_io.stdin in
  let rec retry_messages () =
    messages_to_receive := (List.length !ax12_id);
    lwt () = Lwt_list.iter_s (fun i ->
      lwt () = Krobot_bus.send bus (Unix.gettimeofday (), CAN (Info, Krobot_message.encode (Ax12_Request_State i))) in
      Lwt_unix.sleep !ax12_delay)
        !ax12_id in
    lwt () = Lwt_unix.sleep 0.1 in
    lwt timeout = wait_for_messages keyframe_timeout in
    match timeout with
      | No_timeout ->
        (if (IntMap.cardinal !infos) != (List.length !ax12_id) then
           (Printf.eprintf "Not all AX-12 responded, retrying...\n%!";
            retry_messages ())
         else
           Lwt.return ())
      | Timed_out ->
        (Printf.eprintf "Com. error, retrying...\n%!";
         retry_messages ());
  in
  lwt () = retry_messages () in
  IntMap.iter (fun _ info -> print_info (key_idx, info)) !infos;
  Printf.eprintf "Keyframe %d OK.\n%!" key_idx;
  loop_request (key_idx+1)

let t = loop_request 0
let () = log ()
lwt () = fst (Lwt.wait ())

