module SetFieldDeclaration : sig
  include Set.S with type elt = string
end

val extract_label_from_declaration :
  Grammar.nominal_decl ->
  SetFieldDeclaration.t

val domain : Grammar.nominal_term -> SetFieldDeclaration.t

(** [tuple_of_dependent_function typ] returns the tuple (s, (x, t)) if the
    given type [typ] is a dependent function (TypeDependentFunction) where [s]
    is the type of the parameter [x] and [t] the return type.
    If it's not a dependent function, an exception [NotADependentFunction] is
    raised with [typ] as parameter.
*)
val tuple_of_dependent_function :
  Grammar.nominal_typ ->
  (Grammar.nominal_typ * (AlphaLib.Atom.atom * Grammar.nominal_typ))

(** [is_value term] returns [true] if [term] is a value (a lambda abstraction or
    a type tag).
*)
val is_value :
  Grammar.nominal_term ->
  bool
