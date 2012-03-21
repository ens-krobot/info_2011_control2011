%{
  open Krobot_can_decoder
%}

%token <int> NUM
%token <string> IDENT
%token <string> DESCRIPTION
%token INT, BIT, FLOAT, DOUBLE, SIGNED, UNSIGNED, LITTLEENDIAN, BIGENDIAN
%token LCOLON, LCBRACKET, RCBRACKET, SEMICOLON, DBLQUOTE
%token EOF

%start file
%type <Krobot_can_decoder.frame_desc list> file
%%

file :
  | frame EOF { [$1] }
  | frame file { $1 :: $2 }
;

frame :
  | IDENT NUM LCBRACKET fields RCBRACKET
    { { frame_name = $1; frame_id = $2; frame_data = $4; frame_description = None } }
  | IDENT NUM DESCRIPTION LCBRACKET fields RCBRACKET
    { { frame_name = $1; frame_id = $2; frame_data = $5; frame_description = Some $3 } }

fields :
  | field { [$1] }
  | field SEMICOLON { [$1] }
  | field SEMICOLON fields { $1 :: $3 }

field :
  | IDENT field_type
    { let type_, size = $2 in
      { name = $1; display = type_; size = size; endian = LittleEndian;
        field_description =  None } }
  | IDENT field_type DESCRIPTION
    { let type_, size = $2 in
      { name = $1; display = type_; size = size; endian = LittleEndian;
        field_description = Some $3 } }

field_type :
  | BIT { Bit, 1 }
  | INT signedness NUM { Int $2, $3 }
  | FLOAT { Float None, 32 }
  | DOUBLE { Float None, 64 }

signedness :
  | SIGNED { Signed }
  | UNSIGNED { Unsigned }

/*
endianness :
  | BIGENDIAN { BigEndian }
  | LITTLEENDIAN  { LittleEndian }
*/

%%
