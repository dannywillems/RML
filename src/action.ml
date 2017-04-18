exception Undefined_action of string

type t =
  | Check_typing
  | Eval
  | WellFormed
  | Subtype
  | Subtype_with_REFL
  | Subtype_same_output
  | Typing

let t_of_string = function
  | "check_typing" -> Check_typing
  | "eval" -> Eval
  | "subtype" -> Subtype
  | "subtype_with_REFL" -> Subtype_with_REFL
  | "subtype_same_output" -> Subtype_same_output
  | "typing" -> Typing
  | "wellFormed" -> WellFormed
  | s -> raise (Undefined_action s)

let available = [
  "subtype";
  "subtype_with_REFL";
  "subtype_same_output";
  "typing";
]

