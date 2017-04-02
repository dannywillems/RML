let check_well_formedness context typ =
  if not (WellFormed.typ context typ)
  then raise (Error.NotWellFormed(context, typ))

let check_avoidance_problem x s =
  if Grammar.occurs_typ x s
  then Error.raise_avoidance_problem x s

let check_type_match context term s t =
  if not (Subtype.is_subtype ~context s t)
  then Error.raise_type_mismatch term s t

let check_subtype context s t =
  if not (Subtype.is_subtype ~context s t)
  then Error.raise_subtype s t

let check_disjoint_domains d1 d2 =
  let domain_d1 = TypeUtils.labels_of_declaration d1 in
  let domain_d2 = TypeUtils.labels_of_declaration d2 in
  if not (
      TypeUtils.SetFieldDeclaration.is_empty (
        TypeUtils.SetFieldDeclaration.inter domain_d1 domain_d2
      )
    )
  then Error.raise_aggregate_intersection_not_empty d1 d2

(* Get labels from a declaration *)
let check_record_contains_label record label =
  let labels_of_record =
    TypeUtils.labels_of_declaration record
  in
  TypeUtils.SetFieldDeclaration.mem label labels_of_record

let check_record_contains_field =
  check_record_contains_label

let check_record_contains_type =
  check_record_contains_label

(* Get labels from a type *)
let check_type_contains_label context recursive_type field_label =
  let labels_of_recursive_type =
    TypeUtils.labels_of_recursive_type context recursive_type
  in
  TypeUtils.SetFieldDeclaration.mem field_label labels_of_recursive_type

let check_type_contains_type =
  check_type_contains_label

let check_type_contains_field =
  check_type_contains_label
