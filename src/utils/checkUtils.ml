let check_well_formedness context typ =
  if not (WellFormed.typ context typ)
  then raise (Error.NotWellFormed(context, typ))

let check_avoidance_problem rule context x s =
  if Grammar.occurs_typ x s
  then Error.raise_avoidance_problem rule context x s

let check_subtype context s t =
  let history, is_subtype =
    Subtype.subtype ~context s t
  in
  if not is_subtype
  then Error.raise_subtype history s t

let check_disjoint_domains d1 d2 =
  let domain_d1 = TypeUtils.labels_of_declaration d1 in
  let domain_d2 = TypeUtils.labels_of_declaration d2 in
  if not (
      TypeUtils.SetFieldDeclaration.is_empty (
        TypeUtils.SetFieldDeclaration.inter domain_d1 domain_d2
      )
    )
  then Error.raise_aggregate_intersection_not_empty d1 d2
