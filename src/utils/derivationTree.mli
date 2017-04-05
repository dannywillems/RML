type subtyping_node = {
  rule : string;
  is_true : bool;
  env : ContextType.context;
  s : Grammar.nominal_typ;
  t : Grammar.nominal_typ;
}

type term_typing_node =
  | Declaration of Grammar.nominal_decl
  | Term of Grammar.nominal_term

type typing_node = {
  rule : string;
  env : ContextType.context;
  term : term_typing_node;
  typ : Grammar.nominal_typ;
}

type 'node_value t =
  | Empty
  | Node of 'node_value * 'node_value t list

val create_subtyping_node :
  rule:string ->
  is_true:bool ->
  env:ContextType.context ->
  s:Grammar.nominal_typ ->
  t:Grammar.nominal_typ ->
  history:subtyping_node t list ->
  (subtyping_node t * bool)

val create_typing_node :
  rule:string ->
  env:ContextType.context ->
  term:term_typing_node ->
  typ:Grammar.nominal_typ ->
  history:typing_node t list ->
  (typing_node t * Grammar.nominal_typ)

val string_of_subtyping_derivation_tree :
  ?print_context:bool ->
  int ->
  subtyping_node t ->
  string

val string_of_typing_derivation_tree :
  int ->
  typing_node t ->
  string

val print_subtyping_derivation_tree :
  ?print_context:bool ->
  subtyping_node t ->
  unit

val print_typing_derivation_tree :
  typing_node t ->
  unit
