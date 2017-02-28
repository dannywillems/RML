open Grammar

exception SameFieldLabels

let rec raw_typ_of_typ f nominal_typ =
  match nominal_typ with
  | TypeIntersection (typ1, typ2) ->
    TypeIntersection (raw_typ_of_typ f typ1, raw_typ_of_typ f typ2)

  | TypeUnion (typ1, typ2) ->
    TypeUnion (raw_typ_of_typ f typ1, raw_typ_of_typ f typ2)

  | TypeRecursiveRecord (var, typ) ->
    TypeRecursiveRecord (f var, raw_typ_of_typ f typ)
  | TypeMember (type_label, lower_bound_typ, upper_bound_typ) ->
    TypeMember (
      f type_label,
      raw_typ_of_typ f lower_bound_typ,
      raw_typ_of_typ f upper_bound_typ
    )
  | TypeMethodMember (method_name, var, arg_type, returned_type) ->
    TypeMethodMember(
      f method_name,
      f var,
      raw_typ_of_typ f arg_type,
      raw_typ_of_typ f returned_type
    )
  | TypePathDependent (var, label) ->
    TypePathDependent (f var, f label)
  | TypeTop -> TypeTop
  | TypeBottom -> TypeBottom

and raw_term_of_term f nominal_term =
  match nominal_term with
  | TermVar var -> TermVar (f var)
  | TermRecursiveRecord (var, l) ->
    TermRecursiveRecord (
      f var,
      List.map (raw_obj_of_obj f) l
    )
  | TermMethodApp (term1, method_name, term2) ->
    TermMethodApp (
      raw_term_of_term f term1,
      f method_name,
      raw_term_of_term f term2
    )

and raw_obj_of_obj f nominal_obj =
  match nominal_obj with
  | TermTypeAssignment (typ_label, typ) ->
    TermTypeAssignment (f typ_label, raw_typ_of_typ f typ)
  | TermMethodAssignment (method_name, var, typ, term) ->
    TermMethodAssignment (
      f method_name,
      f var,
      raw_typ_of_typ f typ,
      raw_term_of_term f term
    )

let rec nominal_typ_of_typ_internal env_obj env_term env_type raw_typ =

let rec nominal_term_of_term_internal env_obj env_term env_type raw_term =
  match raw_term with
  | TermVar var ->
    let n = Nominal.t_of_string var in
    Env.add env_term var n;
    TermVar n
  | TermRecursiveRecord (var, l) ->

    let (method_fields, type_fields) =
      Grammar_utils.field_labels_of_object l
    in
    if (has_distinct_field_labels type_fields) &&
       (has_distinct_field_labels method_fields)
    then (
      let method_fields =
        List.map (( ^ ) var ) method_fields
      in
      let type_fields =
        List.map (( ^ ) var ) type_fields
      in
      let nominal_obj = Nominal.t_of_string var in
      Env.add env_obj x nominal_obj;
      List.iter (fun elem ->
          let n = Nominal.t_of_string elem in
          Env.add env_term elem n
        )
        method_fields;
      List.iter (fun elem ->
          let n = Nominal.t_of_string elem in
          Env.add env_type elem n
        )
        type_fields;
      let nominal_obj_list =
        List.map (nominal_obj_of_obj_internal env_obj env_term env_typ) l
      in
      TermRecursiveRecord(fresh_var, nominal_obj_list)
    )
    else (raise SameFieldLabels)
  | TermMethodApp (term1, method_name, term2) ->

and nominal_obj_of_obj_internal env_obj env_term env_type raw_obj =
  | TermTypeAssignment (typ_label, typ) ->
    TermTypeAssignment (
      typ_label,
      nominal_typ_of_typ_internal env_obj env_term env_type
    )
  | TermMethodAssignment (method_name, var, typ, term) ->
    let n = Nominal.t_of_string var in
    Env.add env_term var n;
    TermMethodAssignment (
      method_name,
      n,
      nominal_typ_of_typ_internal env_obj env_term env_type typ,
      nominal_typ_of_typ_internal env_obj env_term env_type typ,
    )

let nominal_term_of_term raw_term =
  nominal_term_of_term_internal
    (Env.empty ())
    (Env.empty ())
    (Env.empty ())
    raw_term

let raw_term_of_nominal_term nominal_term =
  raw_term_of_term Nominal.string_of_t nominal_term

let raw_typ_of_nominal_typ nominal_typ =
  raw_typ_of_typ Nominal.string_of_t nominal_typ
