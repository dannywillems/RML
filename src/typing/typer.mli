(** [type_of context term] returns the type of terms with the typing derivation tree. *)
val type_of :
  ?context:ContextType.context ->
  Grammar.nominal_term ->
  DerivationTree.typing_node DerivationTree.t * Grammar.nominal_typ
