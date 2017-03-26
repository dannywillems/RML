(** Returns a string representation of a raw term *)
val string_of_raw_term : Grammar.raw_term -> string

(** Returns a string representation of a raw typ *)
val string_of_raw_typ : Grammar.raw_typ -> string

(** Returns a string representation of a nominal term with some additional
    annotation about variables (coming from AlphaLib)
*)
val string_of_nominal_term : Grammar.nominal_term -> string

(** Returns a string representation of a nominal typ with some additional
    annotation about variables (coming from AlphaLib)
*)
val string_of_nominal_typ : Grammar.nominal_typ -> string

(** Print a raw term using {!string_of_raw_term} *)
val raw_term : Grammar.raw_term -> unit

(** Print a raw typ using {!string_of_raw_typ} *)
val raw_typ : Grammar.raw_typ -> unit

(** Print a nominal term using {!string_of_nominal_term} *)
val nominal_term : Grammar.nominal_term -> unit

(** Print a nominal typ using {!string_of_nominal_typ} *)
val nominal_typ : Grammar.nominal_typ -> unit

(** This module defines the same printing functions than above but allows to use
    color with [ANSITerminal] library.
*)
module Style : sig
  (** Returns a string representation of a raw term with the given styles list. *)
  val string_of_raw_term :
    ANSITerminal.style list ->
    Grammar.raw_term ->
    string

  (** Returns a string representation of a raw typ with the given styles list. *)
  val string_of_raw_typ :
    ANSITerminal.style list ->
    Grammar.raw_typ ->
    string

  (** Returns a string representation of a nominal term with some additional
      annotation about variables (coming from AlphaLib) with the given styles list.
  *)
  val string_of_nominal_term :
    ANSITerminal.style list ->
    Grammar.nominal_term ->
    string

  (** Returns a string representation of a nominal typ with some additional
      annotation about variables (coming from AlphaLib) with the given styles list.
  *)
  val string_of_nominal_typ :
    ANSITerminal.style list ->
    Grammar.nominal_typ ->
    string

  (** Print a raw term using {!string_of_raw_term} with the given styles list. *)
  val raw_term : ANSITerminal.style list -> Grammar.raw_term -> unit

  (** Print a raw typ using {!string_of_raw_typ} with the given styles list. *)
  val raw_typ : ANSITerminal.style list -> Grammar.raw_typ -> unit

  (** Print a nominal term using {!string_of_nominal_term} with the given styles list. *)
  val nominal_term : ANSITerminal.style list -> Grammar.nominal_term -> unit

  (** Print a nominal typ using {!string_of_nominal_typ} with the given styles list. *)
  val nominal_typ : ANSITerminal.style list -> Grammar.nominal_typ -> unit
end
