(** {1 Function to convert to a string} *)
val string_of_raw_term : Grammar.raw_term -> string

val string_of_raw_typ : Grammar.raw_typ -> string

val string_of_term : ('a -> string) -> 'a Grammar.term -> string

val string_of_typ : ('a -> string) -> 'a Grammar.typ -> string

(** {2 Print functions} *)
val print_raw_term : Grammar.raw_term -> unit

val print_raw_typ : Grammar.raw_typ -> unit

val print_term : ('a -> string) -> 'a Grammar.term -> unit

val print_typ : ('a -> string) -> 'a Grammar.typ -> unit
