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

let create_subtyping_node ~rule ~is_true ~env ~s ~t ~history =
  let subtyping_node = { rule; is_true; env; s; t } in
  Node (subtyping_node, history), is_true

let create_typing_node ~rule ~env ~term ~typ ~history =
  let typing_node = { rule; env; term; typ } in
  Node(typing_node, history), typ

(* Please, improve this code, it's so ugly! *)
let rec string_of_subtyping_derivation_tree
    ?(print_context=true) level (t : subtyping_node t)
  = match t with
  | Empty -> ""
  | Node (v, children) ->
    let string_of_rule =
      Printf.sprintf
        "%s%s ("
        (" " ^* (level * 2))
        (ANSITerminal.sprintf
           (if v.is_true then [ANSITerminal.green] else [ANSITerminal.red])
           "%s"
           v.rule
        )
    in
    let string_of_context =
      if print_context
      then (
        ContextType.Style.string_of_context_with_assert
          [ANSITerminal.magenta]
          v.env
      )
      else ""
    in
    let subtype =
      Printf.sprintf
        "%s <: %s)\n%s"
        (Print.Style.string_of_raw_typ [ANSITerminal.cyan] (Grammar.show_typ v.s))
        (Print.Style.string_of_raw_typ [ANSITerminal.cyan] (Grammar.show_typ v.t))
        (String.concat
           "\n"
           (List.map
              (string_of_subtyping_derivation_tree ~print_context (level + 1))
              children
           )
        )
    in
    string_of_rule ^ string_of_context ^ subtype

let rec string_of_typing_derivation_tree ?(print_context=true) level t = match t with
  | Empty -> ""
  | Node(v, children) ->
    let string_of_term_or_decl = match v.term with
      | Declaration decl -> (Print.string_of_nominal_decl decl)
      | Term term -> (Print.string_of_nominal_term term)
    in
    let string_of_rule =
      Printf.sprintf
        "%s%s ("
        (" " ^* (level * 2))
        (Printf.sprintf
           "%s"
           v.rule
        )
    in
    let string_of_context =
      if print_context
      then (
        ContextType.Style.string_of_context_with_assert
          [ANSITerminal.magenta]
          v.env
      )
      else ""
    in
    let typing =
      Printf.sprintf
        "%s : %s)\n%s"
        (ANSITerminal.sprintf [ANSITerminal.cyan] "%s" string_of_term_or_decl)
        (Print.Style.string_of_raw_typ [ANSITerminal.blue] (Grammar.show_typ v.typ))
        (String.concat "\n" (List.map (string_of_typing_derivation_tree ~print_context (level + 1)) children))
    in
    string_of_rule ^ string_of_context ^ typing

let print_subtyping_derivation_tree ?(print_context=true) tree =
  print_string (
    string_of_subtyping_derivation_tree
      ~print_context
      0
      tree
  )

let print_typing_derivation_tree ?(print_context=true) tree =
  print_string (string_of_typing_derivation_tree ~print_context 0 tree)
