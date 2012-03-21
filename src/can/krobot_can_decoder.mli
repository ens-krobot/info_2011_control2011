open Sexplib.Conv

type signedness =
  | Signed
  | Unsigned with sexp

type display =
  | Bit
  | Hex
  | Int of signedness
  | Float of float option
  | Char
  | No
 with sexp

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
      frame_description : string option }

type desc = field list with sexp
type description with sexp

type result_field =
  | R_bit of bool
  | R_hex of int
  | R_int of int
  | R_float of float
  | R_char of char with sexp

type result = ( string * result_field ) list with sexp

type decode_table

val check_description : desc -> description option

val decode_frame  : Krobot_can.frame -> decode_table -> result * string option

val init_decode_table : unit -> decode_table

val set_description : decode_table -> (int * Krobot_can.kind) -> ?name:string -> description -> unit

val result_to_string : result_field -> string
