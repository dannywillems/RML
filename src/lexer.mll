{
  let next_line lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      {
        pos with Lexing.pos_bol = lexbuf.Lexing.lex_curr_pos;
                 Lexing.pos_lnum = pos.Lexing.pos_lnum + 1
      }
}

let top = "Any"
let bottom = "Nothing"

let white = [' ' '\t' '\r']
let newline = ['\n']

let alpha = ['a'-'z' 'A' - 'Z']
let alpha_capitalize = ['A' - 'Z']
let alpha_num = ['A' - 'Z' 'a' - 'z' '0' - '9']

rule prog = parse
  | white { prog lexbuf }
  | newline { next_line lexbuf;
              prog lexbuf
            }
  | ':' { Parser.COLON }
  | '.' { Parser.DOT }
  | '=' { Parser.EQUAL }
  | '{' { Parser.LEFT_BRACKET }
  | '}' { Parser.RIGHT_BRACKET }
  | '(' { Parser.LEFT_PARENT }
  | ')' { Parser.RIGHT_PARENT }
  | ';' { Parser.SEMICOLON }
  | "=>" as l { Parser.DOUBLE_RIGHT_ARROW }
  (* Top and bottom types *)
  | top { Parser.TYPE_TOP }
  | bottom { Parser.TYPE_BOTTOM }
  (* Intersection and union *)
  | "&" { Parser.TYPE_INTERSECTION }
  | "|" { Parser.TYPE_UNION }
  (*
  | alpha alpha_num+ '.' alpha_capitalize alpha_num* as l {
        let split_str = String.split_on_char '.' l in
        match 
        Parser.TYPE_MEMBER_SELECTION()
      }
  *)
  (* Method and type labels, variable *)
  | alpha_capitalize alpha_num* as l { Parser.ID_CAPITALIZE l }
  | alpha alpha_num* as l { Parser.ID l }
  | _ { failwith "Illegal character" }
  | eof { Parser.EOF }
