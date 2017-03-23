exception Undefined_action of string

type t =
  | Check_typing
  | Eval
  | WellFormed
  | Read_term
  | Read_type
  | Subtype
  | Typing

val t_of_string : string -> t

val available : string list
