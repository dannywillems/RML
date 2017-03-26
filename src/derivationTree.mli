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

val string_of_subtyping_derivation_tree :
  int ->
  subtyping_node t ->
  string

val string_of_typing_derivation_tree :
  int ->
  typing_node t ->
  string

val print_subtyping_derivation_tree :
  subtyping_node t ->
  unit

val print_typing_derivation_tree :
  typing_node t ->
  unit
