val check_well_formedness :
  ContextType.context ->
  Grammar.nominal_typ ->
  unit

val check_avoidance_problem :
  string ->
  ContextType.context ->
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

val check_record_contains_field :
  Grammar.nominal_decl ->
  Grammar.field_label ->
  bool

val check_record_contains_type :
  Grammar.nominal_decl ->
  Grammar.type_label ->
  bool

val check_type_contains_label :
  ContextType.context ->
  Grammar.nominal_typ ->
  string ->
  bool

val check_type_contains_field :
  ContextType.context ->
  Grammar.nominal_typ ->
  string ->
  bool

val check_type_contains_type :
  ContextType.context ->
  Grammar.nominal_typ ->
  string ->
  bool
