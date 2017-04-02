(** Utilities about types and terms. *)

(** [least_upper_bound_of_recursive_type ~label context typ] returns the least upper bound
    (as an option) appearing in a type declaration
    for the given type [typ]. If no such type exists, it returns [None].

    In other words, this algorithm returns
    the least U such as [typ] <: { A : L .. U }.

    The parameter [~label] is to check the type label.
*)
val least_upper_bound_of_recursive_type :
  label:Grammar.type_label ->
  ContextType.context ->
  Grammar.nominal_typ ->
  Grammar.nominal_typ option

(** [greatest_lower_bound_of_recursive_type ~label context typ] returns the
    greatest lower bound (as an option) appearing in a type declaration for the
    given type [typ]. If no such type exists, it returns [None].

    In other words, this algorithm returns
    the greatest L such as { A : L .. U } <: [typ].

    The parameter [~label] is to check the type label.
*)
val greatest_lower_bound_of_recursive_type :
  label:Grammar.type_label ->
  ContextType.context ->
  Grammar.nominal_typ ->
  Grammar.nominal_typ option

(** [least_upper_bound_of_dependent_function ctx L] returns the least upper
    bound of L which has the form ∀(x : S) T.
*)
val least_upper_bound_of_dependent_function :
  ContextType.context ->
  Grammar.nominal_typ ->
  (Grammar.nominal_typ * (AlphaLib.Atom.atom * Grammar.nominal_typ)) option

(** [is_value term] returns [true] if [term] is a value (a lambda abstraction or
    a type tag).
*)
val is_value :
  Grammar.nominal_term ->
  bool

module SetFieldDeclaration : sig
  include Set.S with type elt = string
end

(** [domain decl] returns all labels in the nominal term decl *)
val domain : Grammar.nominal_term -> SetFieldDeclaration.t

val labels_of_declaration :
  Grammar.nominal_decl ->
  SetFieldDeclaration.t

val labels_of_recursive_type :
  ContextType.context ->
  Grammar.nominal_typ ->
  SetFieldDeclaration.t

val is_type_intersection :
  Grammar.nominal_typ -> bool

val is_term_intersection :
  Grammar.nominal_decl -> bool
