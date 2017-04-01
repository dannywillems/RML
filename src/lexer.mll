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

let let_ = "let"
let in_ = "in"

let sig_ = "sig"
let struct_ = "struct"
let obj = "obj"
let end_ = "end"
let type_ = "type"
let val_ = "val"

let lambda = "lambda"
let fun_ = "fun"

let forall = "forall"

let unimplemented = "Unimplemented"

rule prog = parse
  | white { prog lexbuf }
  | newline { next_line lexbuf;
              prog lexbuf
            }

  | "(*" {
      comment lexbuf;
      prog lexbuf
    }

  | '.' { Parser.DOT }
  | '=' { Parser.EQUAL }

  | subtype { Parser.SUBTYPE }
  | supertype { Parser.SUPERTYPE }

  | sig_ { Parser.SIG }
  | struct_ { Parser.STRUCT }
  | end_ { Parser.END }
  | type_ { Parser.TYPE }
  | val_ { Parser.VAL }

  | "<:" { Parser.SUBTYPE }
  | ":>" { Parser.SUPERTYPE }

  | "=>" { Parser.DOUBLE_RIGHT_ARROW }
  | '{' { Parser.LEFT_BRACKET }
  | '}' { Parser.RIGHT_BRACKET }

  | '(' { Parser.LEFT_PARENT }
  | ')' { Parser.RIGHT_PARENT }

  | ';' { Parser.SEMICOLON }

  | let_ { Parser.LET }
  | in_ { Parser.IN }

  | unimplemented { Parser.UNIMPLEMENTED_TERM}

  | fun_ { Parser.FUN }
  | lambda { Parser.ABSTRACTION }
  | ':' { Parser.COLON }
  | "->" { Parser.ARROW_RIGHT }

  | forall { Parser.FORALL }


  (* Top and bottom types *)
  | top { Parser.TYPE_TOP }
  | bottom { Parser.TYPE_BOTTOM }
  (* Method and type labels, variable *)
  | alpha_capitalize (alpha_num | ''')* as l { Parser.ID_CAPITALIZE l }
  | alpha (alpha_num | ''')* as l { Parser.ID l }

  | _ as l { raise (IllegalCharacter l) }
  | eof { Parser.EOF }

and comment = parse
  | "*)" { () }
  | eof { raise UnterminatedComment }
  | _ { comment lexbuf }
