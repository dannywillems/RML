type env

type t

val empty : env

val lookup : env -> string -> t

(** [fresh_string_of_t str env] returns a fresh string not present in the
    environment [env].
    If [str] is in the environment, nested quotes are added until the new string
    is not in the environment.
*)
val fresh_string_of_t : env -> string -> string

val add : env -> string -> t -> unit
