let rec string_of_raw_term t = match t with
  | Grammar.TermVar x -> x
  | Grammar.TermRecursiveRecord (x, obj_list) ->
    Printf.sprintf
      "{%s => %s}"
      x
      (List.fold_left ( ^ ) "" (List.map string_of_raw_obj obj_list))
  | Grammar.TermMethodApp (term1, method_name, term2) ->
    Printf.sprintf
      "%s.%s(%s)"
      (string_of_raw_term term1)
      method_name
      (string_of_raw_term term2)

and string_of_raw_obj t = match t with
  | Grammar.DeclarationType (x, typ) ->
    Printf.sprintf
      "%s = %s ; "
      x
      (string_of_raw_typ typ)
  | Grammar.DeclarationMethod (method_name, typ, (var, term)) ->
    Printf.sprintf
      "%s(%s : %s) = %s ; "
      method_name
      var
      (string_of_raw_typ typ)
      (string_of_raw_term term)

and string_of_raw_typ t = match t with
  | Grammar.TypeTop -> "⊤"
  | Grammar.TypeBottom -> "⟂"
  | Grammar.TypeIntersection (typ1, typ2) ->
    Printf.sprintf
      "%s ∧ %s"
      (string_of_raw_typ typ1)
      (string_of_raw_typ typ2)
  | Grammar.TypeUnion (typ1, typ2) ->
    Printf.sprintf
      "%s ∨ %s"
      (string_of_raw_typ typ1)
      (string_of_raw_typ typ2)
  | Grammar.TypeRecursive (x, typ1) ->
    Printf.sprintf
      "{ %s ⇒ %s }"
      x
      (string_of_raw_typ typ1)
  | Grammar.TypeTypeMember (type_label, typ1, typ2) ->
    Printf.sprintf
      "%s : %s .. %s"
      type_label
      (string_of_raw_typ typ1)
      (string_of_raw_typ typ2)
  | Grammar.TypeMethodMember (method_name, typ1, (var, typ2)) ->
    Printf.sprintf
      "%s(%s : %s) : %s"
      method_name
      var
      (string_of_raw_typ typ1)
      (string_of_raw_typ typ2)
  | Grammar.TypePathDependent (x, typ) ->
    Printf.sprintf
      "%s.%s"
      x
      typ

let raw_term raw_term =
  print_endline @@ string_of_raw_term raw_term

let raw_typ raw_typ =
  print_endline @@ string_of_raw_typ raw_typ
