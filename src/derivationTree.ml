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

let rec ( ^* ) s n = match n with
  | 0 -> ""
  | 1 -> s
  | n when n > 0 -> s ^ (s ^* (n - 1))
  | _ -> s

let rec string_of_subtyping_derivation_tree level (t : subtyping_node t) = match t with
  | Empty -> ""
  | Node (v, children) ->
    Printf.sprintf
      "%s%s (%s ⊦ %s <: %s)\n%s"
      (" " ^* (level * 2))
      (ANSITerminal.sprintf
         (if v.is_true then [ANSITerminal.green] else [ANSITerminal.red])
         "%s"
         v.rule
      )
      (ContextType.Style.string_of_context [ANSITerminal.magenta] v.env)
      (Print.Style.string_of_raw_typ [ANSITerminal.cyan] (Grammar.show_typ v.s))
      (Print.Style.string_of_raw_typ [ANSITerminal.cyan] (Grammar.show_typ v.t))
      (String.concat "\n" (List.map (string_of_subtyping_derivation_tree (level + 1)) children))

let rec string_of_typing_derivation_tree level t = match t with
  | Empty -> ""
  | Node(v, children) ->
    let string_of_term_or_decl = match v.term with
      | Declaration decl -> (Print.string_of_nominal_decl decl)
      | Term term -> (Print.string_of_nominal_term term)
    in
    Printf.sprintf
      "%s%s (%s ⊦ %s : %s)\n%s"
      (" " ^* (level * 2))
      v.rule
      (ContextType.Style.string_of_context [ANSITerminal.magenta] v.env)
      (ANSITerminal.sprintf [ANSITerminal.cyan] "%s" string_of_term_or_decl)
      (Print.Style.string_of_raw_typ [ANSITerminal.blue] (Grammar.show_typ v.typ))
      (String.concat "\n" (List.map (string_of_typing_derivation_tree (level + 1)) children))

let print_subtyping_derivation_tree tree =
  print_string (string_of_subtyping_derivation_tree 0 tree)

let print_typing_derivation_tree tree =
  print_string (string_of_typing_derivation_tree 0 tree)
