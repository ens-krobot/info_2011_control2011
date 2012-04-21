open Lwt
open Krobot_can_decoder

type frame_identifier =
  | Text of string
  | Num of int

let waiter, wakener = wait ()
let quit () = Lwt.wakeup wakener ()

class packet_display ~packing =
  let box = GPack.vbox ~packing () in
  let current_child = ref (GPack.vbox ~packing:box#add ()) in
object (self)

  method new_box () =
    let old_child = !current_child in
    let tmp_box = GPack.vbox ~packing:box#add () in
    current_child := tmp_box;
    old_child#destroy ();
    tmp_box

  method set_packet result =
    let tmp_box = self#new_box () in
    let _ = List.map (fun (name, result) ->
      GMisc.label ~text:(Printf.sprintf "%s : %s" name (result_to_string result))
        ~packing:tmp_box#add ()) result in
    ()

  method clear () = ignore (self#new_box ())

end

let type_name type_ = match type_ with
  | Text t -> t
  | Num i -> string_of_int i

class packet_list ~packing =
  let box = GPack.vbox ~packing () in
  let hbox = GPack.hbox ~packing:box#add () in
  let packet_display = new packet_display ~packing:(box#pack ~expand:false) in
  let scroll = GBin.scrolled_window ~vpolicy:`ALWAYS ~hpolicy:`NEVER ~packing:hbox#add () in

  let model = new GTree.column_list in
  let packet_time = model#add Gobject.Data.double in
  let packet_type = model#add Gobject.Data.string in
  let packet_id = model#add Gobject.Data.int in
  let packet_content = model#add Gobject.Data.caml in

  let packet_store = GTree.list_store model in

  let column_packet_time = GTree.view_column ~title:"time" ~renderer:(GTree.cell_renderer_text [], ["text", packet_time]) () in
  let column_packet_type = GTree.view_column ~title:"type" ~renderer:(GTree.cell_renderer_text [], ["text", packet_type]) () in
  let column_packet_id = GTree.view_column ~title:"id" ~renderer:(GTree.cell_renderer_text [], ["text", packet_id]) () in

  let view = GTree.view ~packing:scroll#add_with_viewport ~model:packet_store () in

  let _ = view#append_column column_packet_time in
  let _ = view#append_column column_packet_type in
  let _ = view#append_column column_packet_id in

  let selection_changed selection () =
    let update path =
      let row = packet_store#get_iter path in
      let result = packet_store#get ~row ~column:packet_content in
      packet_display#set_packet result
    in
    List.iter update selection#get_selected_rows in

object

  method add_packet (type_:frame_identifier) id timestamp (content:result) =
    let type_name = type_name type_ in
    let iter = packet_store#append () in
    packet_store#set ~row:iter ~column:packet_time timestamp;
    packet_store#set ~row:iter ~column:packet_id id;
    packet_store#set ~row:iter ~column:packet_type type_name;
    packet_store#set ~row:iter ~column:packet_content content

  method clear () =
    packet_store#clear ();
    packet_display#clear ()

  initializer
    ignore (view#selection#connect#changed ~callback:(selection_changed view#selection));

end

module StringMap = Map.Make(String)

class basic_field ~packing =
object
  val widget = GMisc.label ~packing ()
  method clear () = widget#set_label ""
end

class value_field ~packing =
object
  inherit basic_field ~packing
  method set_result result =
    widget#set_label (result_to_string result)
end

class text_field ~packing text =
object
  inherit basic_field ~packing
  method set_result (result:result_field) =
    widget#set_label text
  method! clear () = ()
end

class filter_field ~packing filter =
  let old_v = ref None in
object
  inherit basic_field ~packing
  method set_result result =
    match result_to_float result with
      | None -> ()
      | Some f ->
        let new_v =
          match !old_v with
            | None -> f
            | Some old -> filter old f in
        old_v := Some new_v;
        widget#set_label (string_of_float new_v)
end

let cap_field ~packing = function
  | Value -> new value_field ~packing
  | C_text t -> new text_field ~packing t
  | Min -> new filter_field ~packing min
  | Max -> new filter_field ~packing max

class field_info ~packing field_name caps =
  let box = GPack.hbox ~packing () in
  let _ = GMisc.label ~packing:box#add ~text:field_name () in
  let result_widgets = List.map (cap_field ~packing:box#add) caps in
object
  method set_result result =
    List.iter (fun widget -> widget#set_result result) result_widgets
  method clear () = List.iter (fun widget -> widget#clear ()) result_widgets
end

class kind_info ~packing type_ (options:Krobot_can_decoder.opt list) =
  let box = GPack.hbox ~packing () in
  let _ = GMisc.label ~packing:box#add ~text:(type_name type_) () in
  let count_widget = GMisc.label ~packing:box#add ~text:"0" () in
  let count = ref 0 in
  let id = ref 0 in
  let timestamp = ref 0. in
  let field_widgets = List.fold_left
    (fun map -> function
      | Field (name,cap) ->
        let field_info = new field_info ~packing:box#add name cap in
        StringMap.add name field_info map)
    StringMap.empty options in
object (self)
  method add_packet p_id p_timestamp (content:result) =
    id := p_id;
    timestamp := p_timestamp;
    count := !count + 1;
    count_widget#set_label (string_of_int !count);
    List.iter (fun (name,result) ->
      try (StringMap.find name field_widgets)#set_result result
      with
        | Not_found -> ()) content
end

class display_box ~packing =
  let box = GPack.vbox ~packing () in
  let displayed = Hashtbl.create 0 in
object
  method add_packet (type_:frame_identifier) id timestamp (content:result) =
    try
      let kind_info = Hashtbl.find displayed type_ in
      kind_info#add_packet id timestamp content;
      true
    with
      | Not_found -> false
  method add_kind type_ options =
    Hashtbl.add displayed type_ (new kind_info ~packing:box#add type_ options)
  method clear () = Hashtbl.clear displayed
end

class ui () =
  (* The toplevel window. *)
  let window = GWindow.window ~title:"can debug" ~width:800 ~height:600 () in
  let main_vbox = GPack.vbox ~packing:window#add () in
  let packet_list = new packet_list ~packing:main_vbox#add in
  let packet_store = ref [] in
  let decode_table = init_decode_table [] in
  let packet_count = ref 0 in
  let display_box = new display_box ~packing:main_vbox#add in

object (self)
  method window = window
  method main_vbox = main_vbox
  method packet_list = packet_list
  method display_box = display_box

  method display_frame id timestamp frame =
    let result,name = decode_frame frame decode_table in
    let ident = match name with
      | None -> Num frame.Krobot_can.identifier
      | Some n -> Text n
    in
    if not (display_box#add_packet ident id timestamp result)
    then self#packet_list#add_packet ident id timestamp result

  method add_packet (timestamp:float) frame =
    let id = !packet_count in
    incr packet_count;
    packet_store := (id,timestamp,frame) :: !packet_store;
    try
      self#display_frame id timestamp frame;
      return ()
    with
      | exn ->
        Lwt_log.warning_f ~exn "display_packet failed"

  method refresh () =
    packet_list#clear ();
    display_box#clear ();
    List.iter (fun (id,timestamp,frame) -> self#display_frame id timestamp frame)
      (List.rev !packet_store)

  method decode_table = decode_table

  initializer
    ignore (window#connect#destroy quit);

end

let decode_table = ref None
let iface = ref None
let config_file = ref None
let use_krobot_bus = ref false

let parse_arg () =
  let desc =
    [ "-p", Arg.String (fun s -> decode_table := Some s), "file containing the protocol";
      "-c", Arg.String (fun s -> config_file := Some s), "file containing the configuration";
      "-i", Arg.String (fun s -> iface := Some s), "can interface";
      "-b", Arg.Set use_krobot_bus, "use krobot bus"; ] in
  Arg.parse desc (function "" -> () | _ -> Arg.usage desc ""; exit 2) ""

let report_error f s e lexbuf =
  let open Lexing in
      let p1 = lexeme_start_p lexbuf in
      let p2 = lexeme_end_p lexbuf in
      let exn = Printexc.to_string e in
      let off1 = p1.pos_cnum - p1.pos_bol in
      let off2 = p2.pos_cnum - p1.pos_bol in
      Printf.eprintf "File \"%s\", line %i, characters %i-%i:\nError: %s %s\n"
	f p1.pos_lnum off1 off2
	s exn;
      exit 1

let load_frames_desc f =
  let channel = open_in f in
  let lexbuf = Lexing.from_channel channel in
  try
    let r =
      Krobot_can_desc_parser.file
        Krobot_can_desc_lexer.token
        lexbuf in
    close_in channel;
    r
  with
    | e -> report_error f "while parsing frame descriptions" e lexbuf

let load_conf f =
  match f with
    | None -> []
    | Some f ->
      let channel = open_in f in
      let lexbuf = Lexing.from_channel channel in
      try
        let r =
          Krobot_can_desc_parser.config
            Krobot_can_desc_lexer.token
            lexbuf in
        close_in channel;
        r
      with
      | e -> report_error f "while parsing configuration" e lexbuf

let loop_can bus ui =
  let rec aux () =
    lwt (timestamp, frame) = Krobot_can_bus.recv bus in
    lwt () = ui#add_packet timestamp frame in
    aux ()
  in
  aux ()

let loop_krobot_bus bus ui =
  let ev = Krobot_bus.recv bus in
  Lwt_react.E.keep
    (Lwt_react.E.map_s
       (function
         | timestamp, Krobot_bus.CAN (_, frame) ->
           ui#add_packet timestamp frame
         | _ -> return ()) ev);
  let s,_ = Lwt.wait () in
  s

let init loop =
  ignore (GMain.init ~setlocale:false ());
  Lwt_glib.install ();
  let ui = new ui () in
  begin match !decode_table with
    | None -> ()
    | Some f ->
      let l = load_frames_desc f in
      List.iter (fun d ->
        match Krobot_can_decoder.check_description d with
          | None -> ()
          | Some d -> Krobot_can_decoder.set_description ui#decode_table d) l
  end;
  let config = load_conf !config_file in
  List.iter (fun c -> ui#display_box#add_kind (Text c.frame) c.options) config;
  ui#window#show ();
  pick
    [waiter;
     loop ui]

lwt () =
  parse_arg ();
  try_lwt
    if !use_krobot_bus
    then
      lwt bus = Krobot_bus.get () in
      init (loop_krobot_bus bus)
    else
      let iface =
        match !iface with
          | None -> "slcan0"
          | Some s -> s in
      lwt bus = Krobot_can_bus.open_can iface in
      init (loop_can bus)
  with
    | Unix.Unix_error(error, func, arg) ->
      Lwt_log.error_f "'%s' failed with: %s" func (Unix.error_message error)
    | exn -> Lwt_log.error_f ~exn "Uncaught exception"
