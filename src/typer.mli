(** [subtype s t] returns [true] is [s] is a subtype of [t] *)
val subtype :
  Grammar.nominal_typ ->
  Grammar.nominal_typ ->
  bool
