exception Undefined_action of string

type t =
  | Eval
  | Subtype
  | Typing

val t_of_string : string -> t

val available : string list
