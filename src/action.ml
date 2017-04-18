exception Undefined_action of string

type t =
  | Check_typing
  | Eval
  | WellFormed
  | Subtype
  | Typing

let t_of_string = function
  | "check_typing" -> Check_typing
  | "eval" -> Eval
  | "subtype" -> Subtype
  | "typing" -> Typing
  | "wellFormed" -> WellFormed
  | s -> raise (Undefined_action s)

let available = [
  "subtype";
  "typing";
]

