(** Module for the subtyping algorithm. *)

(** [subtype ~with_refl ~context s t] returns a tuple (derivation_tree,
    is_subtype) where [is_subtype] is [true] if [s] is a subtype of [t] and
    [derivation_tree] is the corresponding subtyping derivation tree.

    In the rules of DSub, the reflexivity is defined but this rule can be
    derived by adding a single rule about type projection (x.A <: y.A when x and
    y are the same atom). Both implementations (without or with the reflexivity
    rules) are implemented.
    The optinal argument [~with_refl] gives you the choice to use the algorithm
    you want. By default, reflexivity is not used.

    The optional argument [~context] allows you to give an non empty context. By
    default, it's empty.

    See WF 2016 - The Essence of Dependent Object types for the description of
    the rules.
*)
val subtype :
  ?with_refl:bool ->
  ?context:ContextType.context ->
  Grammar.nominal_typ ->
  Grammar.nominal_typ ->
  (DerivationTree.subtyping_node DerivationTree.t * bool)

(** [is_subtype ~with_refl ~context s t] returns [true] is [s] is a subtype of [t]. Arguments are the same than {!subtype}.
*)
val is_subtype :
  ?with_refl:bool ->
  ?context:ContextType.context ->
  Grammar.nominal_typ ->
  Grammar.nominal_typ ->
  bool
