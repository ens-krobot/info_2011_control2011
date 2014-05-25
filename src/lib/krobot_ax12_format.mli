type ax12_action =
  { id: int;
    pos: int;
    speed: int }

type action =
  | Sleep of float
  | Do of ax12_action

module IntMap : Map.S with type key = int

type keyframe_dict = (int IntMap.t) IntMap.t

type timed_action = float * ax12_action

val to_actions : timed_action list -> action list

val read_timed_actions_file : string -> timed_action list

val read_keyframes_file : string -> keyframe_dict

val merge : timed_action list -> timed_action list -> timed_action list
