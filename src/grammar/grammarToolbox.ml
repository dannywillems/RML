let ba_raw_term nominal_term =
  let ba_term = Grammar.ba_term nominal_term in
  List.rev
    (AlphaLib.Atom.Set.fold
       (fun elt l -> AlphaLib.Atom.hint elt :: l)
       ba_term
       [])

let fa_raw_term nominal_term =
  let fa_term = Grammar.fa_term nominal_term in
  List.rev
    (AlphaLib.Atom.Set.fold
       (fun elt l -> AlphaLib.Atom.hint elt :: l)
       fa_term
       [])

let rec string_of_term_for_fresh_variable t =
  match t with
  | Grammar.TermVariable x -> Printf.sprintf "VAR_%s" x
  | Grammar.TermAbstraction (s, (x, t)) ->
      Printf.sprintf "ABS_%s_%s" x (string_of_term_for_fresh_variable t)
  | Grammar.TermVarApplication (x, y) -> Printf.sprintf "APP_%s_%s" x y
  | Grammar.TermLet (s, (x, t)) ->
      Printf.sprintf
        "LET_%s_%s_%s"
        x
        (string_of_term_for_fresh_variable s)
        (string_of_term_for_fresh_variable t)
  | Grammar.TermAscription (term, typ) ->
      Printf.sprintf "ASC_%s" (string_of_term_for_fresh_variable term)
  | Grammar.TermUnimplemented -> "UNIMP"
  | Grammar.TermRecursiveRecord (typ, (z, decl)) ->
      Printf.sprintf "REC_%s_%s" z (string_of_decl_for_fresh_variable decl)
  | Grammar.TermFieldSelection (x, a) -> Printf.sprintf "F-SEL_%s_%s" x a
  | Grammar.TermRecursiveRecordUntyped (z, decl) ->
      Printf.sprintf "UN-REC_%s_%s" z (string_of_decl_for_fresh_variable decl)
  | Grammar.TermInteger n -> Printf.sprintf "UN-INT_%d" n

and string_of_decl_for_fresh_variable decl =
  match decl with
  | Grammar.TermTypeDeclaration (x, _) -> Printf.sprintf "T-DECL_%s" x
  | Grammar.TermFieldDeclaration (label, term) ->
      Printf.sprintf "F-DECL_%s" label
  | Grammar.TermAggregateDeclaration (d1, d2) ->
      Printf.sprintf
        "AGG_%s_%s"
        (string_of_decl_for_fresh_variable d1)
        (string_of_decl_for_fresh_variable d2)

let is_raw_variable t =
  match t with Grammar.TermVariable _ -> true | _ -> false

let extract_variable t =
  match t with Grammar.TermVariable x -> Some x | _ -> None
