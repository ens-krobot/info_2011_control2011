open Sexplib.Conv
open Krobot_can

type signedness =
  | Signed
  | Unsigned with sexp

type display =
  | Bit
  | Hex
  | Int of signedness
  | Float of float option
  | Char
  | No with sexp

type size = int with sexp

type endian = Bitstring.endian = BigEndian | LittleEndian | NativeEndian with sexp

type field =
    { name : string;
      display : display;
      size : size;
      endian : endian;
      field_description : string option } with sexp

type frame_desc =
    { frame_name : string;
      frame_id : int;
      frame_data : field list;
      frame_description : string option } with sexp

type description = frame_desc with sexp

type result_field =
  | R_bit of bool
  | R_hex of int
  | R_int of int
  | R_float of float
  | R_char of char with sexp

type result = ( string * result_field ) list with sexp

type decode_table = (int,frame_desc) Hashtbl.t

let is_description_correct desc =
  ( List.for_all (fun { display; size } ->
    size > 0 &&
      ( match display with
        | Bit -> size = 1
        | Char -> size <= 8
        | _ -> true )) desc ) &&
    let size = List.fold_left (fun acc t -> acc + t.size) 0 desc in
    if size > 64
    then false
    else true

let check_description desc =
  if is_description_correct desc.frame_data
  then Some desc
  else None

let split_bitstring (acc,bitstring) field =
  let (s,start,len) = bitstring in
  if len < field.size
  then (acc,(s,start + len,0))
  else let value = Bitstring.takebits field.size bitstring in
       let rest = Bitstring.dropbits field.size bitstring in
       ((value,field)::acc,rest)

let bit n i = ((1 lsl i) land n) lsr i
let ones n = max_int mod (1 lsl n)
let resize_signed ~n ~size =
  if bit n (size - 1) = 1
  then n lor ((lnot 0) lsl size)
  else n

let read_field ((str,start,end_),field) =
  let i = Bitstring.extract_int_ee_unsigned field.endian str start end_ field.size in
  match field.display with
    | Bit ->
      begin
        let b = match i with
          | 0 -> false
          | 1 -> true
          | _ -> failwith "incorrect description" in
        Some (field.name,R_bit b)
      end
    | Int sign ->
      let i = match sign with
        | Unsigned -> i
        | Signed -> resize_signed ~n:i ~size:field.size in
      Some (field.name,R_int i)
    | Hex -> Some (field.name,R_hex i)
    | Char -> Some (field.name,R_char (Char.chr i))
    | Float coef ->
      Some (field.name,R_float
              (match coef with
                | None -> float i
                | Some c -> c *. (float i)))
    | No -> None

let filter_map f l =
  let rec aux = function
    | [] -> []
    | t::q -> match f t with
        | None -> aux q
        | Some v -> v :: (aux q) in
  aux l

let read_fields bitstring description =
  let fields,rest = List.fold_left split_bitstring ([],bitstring) description in
  filter_map read_field (List.rev fields)

let decode_frame' frame descriptions =
  read_fields (Bitstring.bitstring_of_string frame.Krobot_can.data) descriptions

let default_desc =
  let field =
    { name = "";
      display = Hex;
      size = 8;
      endian = BigEndian;
      field_description = None } in
  [ field; field; field; field;
    field; field; field; field ]

let decode_frame frame table =
  let desc,name =
    try
      let desc = Hashtbl.find table frame.identifier in
      desc.frame_data, Some desc.frame_name
    with
      | Not_found -> default_desc,None in
  decode_frame' frame desc,name

let set_description t desc =
  Hashtbl.replace t desc.frame_id desc

let init_decode_table l : decode_table =
  let t = Hashtbl.create 0 in
  List.iter (set_description t) l;
  t

let result_to_string = function
  | R_bit b -> string_of_bool b
  | R_hex i -> Printf.sprintf "%X" i
  | R_int i -> string_of_int i
  | R_float f -> string_of_float f
  | R_char c -> Printf.sprintf "%c" c

let result_to_float = function
  | R_bit b -> None
  | R_hex i -> Some (float i)
  | R_int i -> Some (float i)
  | R_float f -> Some f
  | R_char c -> None

(* configuration *)
type cap =
  | Value
  | Min
  | Max
  | C_text of string

let cap_of_string = function
  | "min" -> Min
  | "max" -> Max
  | "value" -> Value
  | s -> failwith (Printf.sprintf "unknown option: %s" s)

type opt =
    | Field of (string * cap list)

type config =
    { frame : string;
      options : opt list; }
