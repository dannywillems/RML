exception Undefined_action of string

type t =
  | Check_typing
  | Eval
  | WellFormed
  | Read_term
  | Read_type
  | Subtype
  | Subtype_with_REFL
  | Subtype_same_output
  | Typing

val t_of_string : string -> t

val available : string list
