open Lwt
open Krobot_can_decoder

type frame_identifie =
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

  method add type_ id timestamp (content:result) =
    let type_name = match type_ with
      | Text t -> t
      | Num i -> string_of_int i in
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

class ui () =
  (* The toplevel window. *)
  let window = GWindow.window ~title:"can debug" ~width:800 ~height:600 () in
  let main_vbox = GPack.vbox ~packing:window#add () in
  let packet_list = new packet_list ~packing:main_vbox#add in
  let packet_store = ref [] in
  let decode_table = init_decode_table () in
  let packet_count = ref 0 in

object (self)
  method window = window
  method main_vbox = main_vbox
  method packet_list = packet_list

  method display_frame id timestamp frame =
    let result,name = decode_frame frame decode_table in
    let ident = match name with
      | None -> Num frame.Krobot_can.identifier
      | Some n -> Text n
    in
    self#packet_list#add ident id timestamp result

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
    List.iter (fun (id,timestamp,frame) -> self#display_frame id timestamp frame)
      (List.rev !packet_store)

  initializer
    ignore (window#connect#destroy quit);

end

let decode_table = ref None
let iface = ref None

let parse_arg () =
  let desc =
    [ "-c", Arg.String (fun s -> decode_table := Some s), "file containing the configuration";
      "-i", Arg.String (fun s -> iface := Some s), "can interface";] in
  Arg.parse desc (function "" -> () | _ -> Arg.usage desc ""; exit 2) ""

let loop ui bus =
  let rec aux () =
    lwt (timestamp, frame) = Krobot_can_bus.recv bus in
    lwt () = ui#add_packet timestamp frame in
    aux ()
  in
  aux ()

let init iface =
  lwt bus = Krobot_can_bus.open_can iface in
  ignore (GMain.init ~setlocale:false ());
  Lwt_glib.install ();
  let ui = new ui () in
  ui#window#show ();
  pick
    [waiter;
     loop ui bus]

lwt () =
  parse_arg ();
  let iface =
    match !iface with
      | None -> "slcan0"
      | Some s -> s in
  try_lwt
    init iface
  with Unix.Unix_error(error, func, arg) ->
    Lwt_log.error_f "'%s' failed with: %s" func (Unix.error_message error)

