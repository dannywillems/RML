{
  exception IllegalCharacter of char
  exception UnterminatedComment

  let next_line lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      {
        pos with Lexing.pos_bol = lexbuf.Lexing.lex_curr_pos;
                 Lexing.pos_lnum = pos.Lexing.pos_lnum + 1
      }

  let inner_comments_number = ref 0

}

let subtype = "<:"
let supertype = ":>"

let top = "Any"
let bottom = "Nothing"

let unimplemeted = "Unimplemented "
let white = [' ' '\t' '\r']
let newline = ['\n']

let alpha = ['a'-'z' 'A' - 'Z']
let alpha_capitalize = ['A' - 'Z']
let alpha_num = ['A' - 'Z' 'a' - 'z' '0' - '9']
let alpha_lowercase = ['a' - 'z']

let lowercase_ident =
  (alpha_lowercase | '_')+ (alpha_num | '_' | '\'')*
let uppercase_ident =
  alpha_capitalize (alpha_num | '_')*

let integer = ['0' - '9']*

let let_ = "let"
let in_ = "in"

let sig_ = "sig"
let struct_ = "struct"
let obj = "obj"
let end_ = "end"
let type_ = "type"
let val_ = "val"
let with_ = "with"

let fun_ = "fun"

let forall = "forall"
let intersection = "&"

let unimplemented = "Unimplemented"

let left_square_bracket = '['
let right_square_bracket = ']'
let at = '@'

rule prog = parse
  | white { prog lexbuf }
  | newline { next_line lexbuf;
              prog lexbuf
            }

  (* Use for tests *)
  | "!<:" { Parser.NOT_SUBTYPE }

  | "(*" {
      comment lexbuf;
      prog lexbuf
    }

  | integer as n { Parser.INTEGER (int_of_string n) }

  | '.' { Parser.DOT }
  | ',' { Parser.COMMA }
  | '=' { Parser.EQUAL }

  | subtype { Parser.SUBTYPE }
  | supertype { Parser.SUPERTYPE }

  | sig_ { Parser.SIG }
  | struct_ { Parser.STRUCT }
  | end_ { Parser.END }
  | type_ { Parser.TYPE }
  | val_ { Parser.VAL }
  | "module" { Parser.MODULE }

  | "<:" { Parser.SUBTYPE }
  | ":>" { Parser.SUPERTYPE }

  | '{' { Parser.LEFT_BRACKET }
  | '}' { Parser.RIGHT_BRACKET }

  | '(' { Parser.LEFT_PARENT }
  | ')' { Parser.RIGHT_PARENT }

  | at { Parser.AT }
  | left_square_bracket { Parser.LEFT_SQUARE_BRACKET }
  | right_square_bracket { Parser.RIGHT_SQUARE_BRACKET }
  | ';' { Parser.SEMICOLON }

  | let_ { Parser.LET }
  | in_ { Parser.IN }

  | unimplemented { Parser.UNIMPLEMENTED_TERM}

  | fun_ { Parser.FUN }
  | ':' { Parser.COLON }
  | "->" { Parser.ARROW_RIGHT }

  | forall { Parser.FORALL }


  (* Top and bottom types *)
  | top { Parser.TYPE_TOP }
  | bottom { Parser.TYPE_BOTTOM }
  | intersection { Parser.INTERSECTION }
  | with_ { Parser.WITH }
  (* Method and type labels, variable *)
  | uppercase_ident as l { Parser.ID_CAPITALIZE l }
  | lowercase_ident as l { Parser.ID l }

  | _ as l { raise (IllegalCharacter l) }
  | eof { Parser.EOF }

and comment = parse
  | "*)" {
   if (!inner_comments_number) = 0
   then ()
   else (
     inner_comments_number := (!inner_comments_number) - 1;
     comment lexbuf
   )
  }
  | newline { next_line lexbuf;
              comment lexbuf
            }
  | "(*" {
   incr inner_comments_number;
   comment lexbuf
  }
  | eof { raise UnterminatedComment }
  | _ { comment lexbuf }
