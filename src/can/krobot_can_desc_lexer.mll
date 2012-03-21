{
  open Lexing
  open Krobot_can_decoder
  open Krobot_can_desc_parser
  type pos = Lexing.position * Lexing.position
  exception Unexpected_character of char * pos

  let string_buffer = Buffer.create 0
}

let number =
    ['0'-'9']+
  | "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
  | "0b" ['0' '1']+

let alpha = ['a'-'z' 'A'-'Z' '_']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_' ]
let ident = alpha alphanum*

rule token = parse
  | [' ' '\t' ]     { token lexbuf }     (* skip blanks *)
  | ['\n']          { Lexing.new_line lexbuf; token lexbuf }
  | number as num   { NUM (int_of_string num) }
  | "bit" { BIT }
  | "int" { INT }
  | "float" { FLOAT }
  | "double" { DOUBLE }
  | "signed" { SIGNED }
  | "unsigned" { UNSIGNED }
  | "bigendian" { BIGENDIAN }
  | "littleendian" { LITTLEENDIAN }
  | '{'   { LCBRACKET }
  | '}'   { RCBRACKET }
  | ';'   { SEMICOLON }
  | '"'   { let s = text lexbuf in DESCRIPTION s }
  | ident { IDENT (lexeme lexbuf) }
  | eof   { EOF }
  | "(*"           { comments 0 lexbuf }
  | _ as c         { raise (Unexpected_character
			      (c,(Lexing.lexeme_start_p lexbuf,
				  Lexing.lexeme_end_p lexbuf))) }

and comments level = parse
  | "*)"	{ if level = 0 then token lexbuf
		  else comments (level-1) lexbuf }
  | "(*"	{ comments (level+1) lexbuf }
  | eof		{ raise End_of_file }
  | _		{ comments level lexbuf }

and text = parse
  | '"'         { let r = Buffer.contents string_buffer in
                  Buffer.clear string_buffer;
                  r }
  | eof		{ raise End_of_file }
  | _ as c      { Buffer.add_char string_buffer c; text lexbuf }
