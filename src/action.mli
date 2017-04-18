exception Undefined_action of string

type t =
  | Check_typing
  | Eval
  | WellFormed
  | Subtype
  | Typing

val t_of_string : string -> t

val available : string list
