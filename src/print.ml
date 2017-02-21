let rec string_of_raw_term t = match t with
  | Grammar.TermVar x -> x
  | Grammar.TermRecursiveRecord (x, term_list) ->
    Printf.sprintf
      "{%s => %s}"
      x
      (List.fold_left ( ^ ) "" (List.map string_of_raw_term term_list))
  | Grammar.TermMethodApp (term1, term2) ->
    Printf.sprintf
      "%s.m(%s)"
      (string_of_raw_term term1)
      (string_of_raw_term term2)
  | Grammar.TermTypeAssignment (x, typ) ->
    Printf.sprintf
      "%s = %s"
      x
      (string_of_raw_typ typ)
  | Grammar.TermMethodAssignment ((x, term), typ) ->
    Printf.sprintf
      "m(%s : %s) = %s"
      x
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
  | Grammar.TypeRecursiveRecord (x, typ1) ->
    Printf.sprintf
      "{ %s ⇒ %s }"
      x
      (string_of_raw_typ typ1)
  | Grammar.TypeMember (x, typ1, typ2) ->
    Printf.sprintf
      "%s : %s .. %s"
      x
      (string_of_raw_typ typ1)
      (string_of_raw_typ typ2)
  | Grammar.TypeMethodMember (x, typ1, typ2) ->
    Printf.sprintf
      "m(%s : %s) : %s"
      x
      (string_of_raw_typ typ1)
      (string_of_raw_typ typ2)
  | Grammar.TypePathDependent (x, typ) ->
    Printf.sprintf
      "%s.%s"
      x
      typ

let print_raw_term raw_term =
  print_endline @@ string_of_raw_term raw_term

let print_raw_typ raw_typ =
  print_endline @@ string_of_raw_typ raw_typ
