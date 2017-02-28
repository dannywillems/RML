let rec field_labels_of_object_internal method_fields type_fields fields =
  match l with
  | [] -> (method_fields, type_fields)
  | field :: fields ->
    (match field with
    | Grammar.TermTypeAssignment (typ_label, _) ->
      fields_of_object_internal method_fields (typ_label :: type_fields) fields
    | Grammar.TermMethodAssignment (method_name, _) ->
      fields_of_object_internal (method_name :: method_fields) type_fields fields
    )

let field_labels_of_object l =
  fields_of_object_internal [] [] l

let rec has_distinct_field_labels_internal elem l =
  match l with
  | [] -> true
  | head :: tail ->
    if head = elem
    then false
    else has_distinct_field_labels_internal head tail

let has_distinct_field_labels l =
  let sorted_l = List.sort ( = ) l in
  match sorted_l with
  | [] -> true
  | head :: tail ->
    has_distinct_field_labels_internal head tail
