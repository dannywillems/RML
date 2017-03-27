module SetFieldDeclaration = Set.Make(String)

let rec extract_label_from_declaration decl = match decl with
  | Grammar.DeclarationType(label, _) ->
    SetFieldDeclaration.singleton label
  | Grammar.DeclarationField(label, _) ->
    SetFieldDeclaration.singleton label
  | Grammar.DeclarationAggregate(d, d') ->
    SetFieldDeclaration.union
      (extract_label_from_declaration d)
      (extract_label_from_declaration d')

let domain term = match term with
  | Grammar.TermRecursiveRecord(z, decl) ->
    extract_label_from_declaration decl
  | _ -> SetFieldDeclaration.empty

let tuple_of_dependent_function t = match t with
  | Grammar.TypeDependentFunction(s, (x, t)) ->
    (s, (x, t))
  | _ -> raise (Error.NotADependentFunction t)

let is_value t = match t with
  | Grammar.TermRecursiveRecord(_) | Grammar.TermAbstraction(_) -> true
  | _ -> false

let rec field_type_of_record field typ = match typ with
  | Grammar.DeclarationField(a, t) | Grammar.DeclarationType(a, t) ->
    if String.equal field a
    then Some t
    else None
  | Grammar.DeclarationAggregate(d1, d2) ->
    let field_d1 = field_type_of_record field d1 in
    let field_d2 = field_type_of_record field d2 in
    match field_d1, field_d2 with
    | Some t, None | None, Some t -> t
    | None, None -> t
    | Some t1, Some t2 ->
      raise (Error.MultipleDefinitionOfField(field, t1, t2))

let record_of_variable context x =
  let type_of_x = ContextType.find x context in
  match type_of_x with
  (* x is has the form { z => T^{x} }, it returns T. *)
  | Grammar.TypeRecursive(z, t) -> t
    (* If x has the y.A, we
       1. do a recursive call to get the type of y which must be a record.
       2. check the field exist in the record.
    *)
  | Grammar.TypeProjection(y, a) ->
    let type_of_y = ContextType.find y context in
    let y_as_record = record_of_variable context y in
    let field_type_of_x = field_type_of_record a y_as_record in
    match field_type_of_x with
    | None -> raise (NotWellFormed(x, type_of_x))
    | Some t ->

