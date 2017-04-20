(** Check if the given type is well-formed. If not, raise {Error.NotWellFormed} *)
val check_well_formedness :
  ContextType.context ->
  Grammar.nominal_typ ->
  unit

(** Check if the avoidance problem arises. If it's the case, raise {Error.AvoidanceProblem} *)
val check_avoidance_problem :
  string ->
  ContextType.context ->
  AlphaLib.Atom.t ->
  Grammar.nominal_typ ->
  unit

(** [check_subtype ctx s t] raises {Error.Subtype} if [s] is not a subtype of
    [t] *)
val check_subtype :
  ContextType.context ->
  Grammar.nominal_typ ->
  Grammar.nominal_typ ->
  unit

(** [check_disjoint_domains d d'] raises {Error.AggregationIntersectionNotEmpty}
if d and d' have a common declaration. *)
val check_disjoint_domains :
  Grammar.nominal_decl ->
  Grammar.nominal_decl ->
  unit
