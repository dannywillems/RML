(** [typ env typ] returns [true] if [typ] is a well formed type. We say
    a type [T] is well formed if [T] is the form [x.A] where [x] is a sub-type
    of a type declaration. To make the connection with the module language, a
    type is well formed if when we want to access to a type defining a module
    through a variable [x], the variable [x] is of type module, or at least, a
    sub-type.

    As in the definition we allow sub-types, if [x] is of type [Nothing], [x.A] is
    well-typed.

    In addition to that, if [~use_subtyping] is set to [true] in the case of a
    type declaration, the lower bound must be a sub-type of the upper bound.
    By default, we don't check it because it's never mention in official papers
    the lower bound must be a subtype of the upper bound to have a well formed
    type.
*)
val typ :
  ?use_subtyping:bool ->
  ContextType.context ->
  Grammar.nominal_typ ->
  bool
