exception Undefined_action of string

type t =
  | Check_typing
  | Eval
  | WellFormed
  | Read_term
  | Read_type
  | Subtype
  | Typing

let t_of_string = function
  | "check_typing" -> Check_typing
  | "read_term" -> Read_term
  | "read_type" -> Read_type
  | "eval" -> Eval
  | "subtype" -> Subtype
  | "typing" -> Typing
  | "wellFormed" -> WellFormed
  | s -> raise (Undefined_action s)

let available = [
  "check_typing";
  "read_term";
  "read_type";
  "subtype";
  "typing";
  "wellFormed";
]

