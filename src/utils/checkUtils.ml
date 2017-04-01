let check_well_formedness context typ =
  if not (WellFormed.typ context typ)
  then raise (Error.NotWellFormed(context, typ))

let check_avoidance_problem x s =
  if Grammar.occurs_typ x s
  then raise (Error.AvoidanceProblem(
      (Printf.sprintf
         "%s appears in %s."
         (AlphaLib.Atom.show x)
         (Print.string_of_nominal_typ s)
      ),
      x,
      s
    ))

let check_type_match context term s t =
  if not (Subtype.is_subtype ~context s t)
  then raise
      (Error.TypeMismatch (
          Printf.sprintf
            "ALL-E: %s must be a subtype of %s but it's of type %s."
            (Print.string_of_nominal_term term)
            (Print.string_of_raw_typ (Grammar.show_typ s))
            (Print.string_of_raw_typ (Grammar.show_typ t)),
          (s, t)
          )
        )

let check_subtype context s t =
  if not (Subtype.is_subtype ~context s t)
  then (
    let str =
      Printf.sprintf
        "%s is not a subtype of %s"
        (Print.string_of_nominal_typ s)
        (Print.string_of_nominal_typ t)
    in
    raise (Error.Subtype (str, s, t))
  )

let check_disjoint_domains d1 d2 =
  let domain_d1 = TypeUtils.extract_label_from_declaration d1 in
  let domain_d2 = TypeUtils.extract_label_from_declaration d2 in
  if not (
      TypeUtils.SetFieldDeclaration.is_empty (
        TypeUtils.SetFieldDeclaration.inter domain_d1 domain_d2
      )
    )
  then raise (
      Error.AggregationIntersectionNotEmpty(
        "When defining an aggregation, the domains must be disjoint.",
        d1,
        d2
      ))


