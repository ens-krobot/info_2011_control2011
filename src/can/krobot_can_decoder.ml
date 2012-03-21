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

type desc = field list with sexp
type description = field list with sexp

type frame_desc =
    { frame_name : string;
      frame_id : int;
      frame_data : field list;
      frame_description : string option }

type result_field =
  | R_bit of bool
  | R_hex of int
  | R_int of int
  | R_float of float
  | R_char of char with sexp

type result = ( string * result_field ) list with sexp

type decode_table =
    (int * kind,desc * string option) Hashtbl.t

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
  if is_description_correct desc
  then Some desc
  else None

let split_bitstring (acc,bitstring) field =
  let (s,start,len) = bitstring in
  if len < field.size
  then (acc,(s,start + len,0))
  else let value = Bitstring.takebits field.size bitstring in
       let rest = Bitstring.dropbits field.size bitstring in
       ((value,field)::acc,rest)

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
      failwith "handle sign here...";
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
  let desc,name = try Hashtbl.find table (frame.identifier, frame.kind) with
    | Not_found -> default_desc,None in
  decode_frame' frame desc,name

let init_decode_table () =
  Hashtbl.create 0

let set_description t (i,kind) ?name desc =
  Hashtbl.replace t (i,kind) (desc,name)

let result_to_string = function
  | R_bit b -> string_of_bool b
  | R_hex i -> Printf.sprintf "%X" i
  | R_int i -> string_of_int i
  | R_float f -> string_of_float f
  | R_char c -> Printf.sprintf "%c" c

