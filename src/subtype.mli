(** [subtype s t] returns [true] is [s] is a subtype of [t] *)
val subtype :
  ?context:ContextType.context ->
  Grammar.nominal_typ ->
  Grammar.nominal_typ ->
  (DerivationTree.subtyping_node DerivationTree.t * bool)
