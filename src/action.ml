exception Undefined_action of string

type t =
  | Eval
  | Subtype
  | Typing

let t_of_string = function
  | "eval" -> Eval
  | "subtype" -> Subtype
  | "typing" -> Typing
  | s -> raise (Undefined_action s)

let available = [
  "subtype";
  "typing";
]

