(** This module provides an abstraction of context for algorithm about types for DOT.
    A context is a list of couple (x, T) where x is a term variable and T is a type.
    It is supposed to be used with [AlphaLib] and that terms are represented in
    nominal form (with {AlphaLib.Atom.t}).

    This abstraction of contexts is useful for subtyping and type
    inference algorithms.
    For an abstraction of context for evaluation, see {!ContextEvaluation}.
*)

(** The type of term variable. *)
type key = AlphaLib.Atom.t

type t = Grammar.nominal_typ

type context

exception NotInEnvironment of key * context

(** Create a empty context. *)
val empty : unit -> context

(** [add atom_x typ_of_atom_x context] adds the mapping of the atom [atom_x] to
    [typ_of_atom_x] in the context
*)
val add : key -> t -> context -> context

(** Return [true] if the context is empty. *)
val is_empty : context -> bool

(** [find atom_x context] returns the type of the atom [atom_x] if [atom_x] is
    in the context. Else, an exception [Not_found] is raised.
*)
val find : key -> context -> t

(** Return a string representation of the given context.
    NOTE: Use unicode character ∅.
*)
val string_of_context : context -> string

(** This module allows to use [ANSITerminal] to add style to converters. *)
module Style : sig
  (** Same than {!string_of_context} with a style. *)
  val string_of_context : ANSITerminal.style list -> context -> string
end
