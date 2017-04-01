val check_well_formedness :
  ContextType.context ->
  Grammar.nominal_typ ->
  unit

val check_avoidance_problem :
  AlphaLib.Atom.t ->
  Grammar.nominal_typ ->
  unit

val check_type_match :
  ContextType.context ->
  Grammar.nominal_term ->
  Grammar.nominal_typ ->
  Grammar.nominal_typ ->
  unit

val check_subtype :
  ContextType.context ->
  Grammar.nominal_typ ->
  Grammar.nominal_typ ->
  unit

val check_disjoint_domains :
  Grammar.nominal_decl ->
  Grammar.nominal_decl ->
  unit
