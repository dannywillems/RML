(** This module provides an abstraction of a nominal type.
    The purpose of a nominal type is to assign an unique ID to a string.
    It can be used for example to assign a unique ID to variables in lambda
    abstractions.

    The actual implementation used a positive integer, incremented each time
    `t_of_string` is called.
*)

(** A nominal type. *)
type t

(** The unique ID of the nominal type. *)
val uid_of_t : t -> int

(** The initial string which corresponds to the ID. *)
val string_of_t : t -> string

(** Create a nominal type from a string. It is essentially assigning a unique ID
    to the string.
*)
val t_of_string : string -> t
