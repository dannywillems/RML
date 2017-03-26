let rec string_of_raw_term t = match t with
  | Grammar.TermVariable x -> x
  | Grammar.TermAbstraction (typ, (x, term)) ->
    Printf.sprintf
      "λ(%s : %s) %s"
      x
      (string_of_raw_typ typ)
      (string_of_raw_term term)
  | Grammar.TermVarApplication (x, y) ->
    Printf.sprintf
      "(%s %s)"
      x
      y
  | Grammar.TermLet (t, (x, u)) ->
    Printf.sprintf
      "let %s = %s in %s"
      x
      (string_of_raw_term t)
      (string_of_raw_term u)
  (* ----- Unofficial terms ----- *)
  (* t : T *)
  | Grammar.TermAscription (t, typ_of_t) ->
    Printf.sprintf
      "%s : %s"
      (string_of_raw_term t)
      (string_of_raw_typ typ_of_t)
  (* Add the unimplemented term allows to define terms without given their
     implementation. Useful for testing.
  *)
  | Grammar.TermUnimplemented ->
    "Unimplemented"

  (* ----- Beginning of DOT terms ----- *)
  (* {x => t^{x}} *)
  | Grammar.TermRecursiveRecord (x, decl) ->
    Printf.sprintf
      "{%s => %s}"
      x
      (string_of_raw_decl decl)
  (* x.a *)
  | Grammar.TermFieldSelection (x, a) ->
    Printf.sprintf
      "%s.%s"
      x
      a

(* Objects. Type tag becomes DeclarationType (instead of TermTypeTag) *)
and string_of_raw_decl t = match t with
  (* L = T *)
  | Grammar.DeclarationType (l, typ) ->
    Printf.sprintf
      "%s = %s"
      l
      (string_of_raw_typ typ)
  (* a = t *)
  | Grammar.DeclarationField (a, term) ->
    Printf.sprintf
      "%s = %s"
      a
      (string_of_raw_term term)
  | Grammar.DeclarationAggregate (d, d') ->
    Printf.sprintf
      "%s ∧ %s"
      (string_of_raw_decl d)
      (string_of_raw_decl d')

and string_of_raw_typ t = match t with
  (* Top type : ⊤ *)
  | Grammar.TypeTop -> "⊤"
  (* Bottom type : ⟂ *)
  | Grammar.TypeBottom -> "⟂"
  (* L : S..T --> (L, S, T) *)
  | Grammar.TypeDeclaration (type_label, typ1, typ2) ->
    Printf.sprintf
      "%s : %s .. %s"
      type_label
      (string_of_raw_typ typ1)
      (string_of_raw_typ typ2)
  (* x.L *)
  | Grammar.TypeProjection (x, typ) ->
    Printf.sprintf
      "%s.%s"
      x
      typ
  (* ∀(x : S) T --> (S, (x, T)) *)
  | Grammar.TypeDependentFunction (typ1, (x, typ2)) ->
    Printf.sprintf
      "∀(%s : %s) %s"
      x
      (string_of_raw_typ typ1)
      (string_of_raw_typ typ2)
  (* ----- Beginning of DOT types ----- *)
  (* T ∧ T *)
  | Grammar.TypeIntersection (typ1, typ2) ->
    Printf.sprintf
      "%s ∧ %s"
      (string_of_raw_typ typ1)
      (string_of_raw_typ typ2)
  (* { z => T^{z} } *)
  | Grammar.TypeRecursive (x, typ1) ->
    Printf.sprintf
      "{ %s ⇒ %s }"
      x
      (string_of_raw_typ typ1)
  (* a : T *)
  | Grammar.TypeFieldDeclaration(a, t) ->
    Printf.sprintf
      "%s : %s"
      a
      (string_of_raw_typ t)


let string_of_nominal_term t =
  string_of_raw_term (Grammar.show_term t)

let string_of_nominal_typ t =
  string_of_raw_typ (Grammar.show_typ t)

let raw_term t =
  Printf.printf "%s" (string_of_raw_term t)

let raw_typ t =
  Printf.printf "%s" (string_of_raw_typ t)

let nominal_term t =
  Printf.printf "%s" (string_of_raw_term (Grammar.show_term t))

let nominal_typ t =
  Printf.printf "%s" (string_of_raw_typ (Grammar.show_typ t))

module Style = struct
  let string_of_raw_term style t =
    ANSITerminal.sprintf
      style
      "%s"
      (string_of_raw_term t)

  let string_of_raw_typ style t =
    ANSITerminal.sprintf
      style
      "%s"
      (string_of_raw_typ t)

  let string_of_nominal_term style t =
    (string_of_raw_term style (Grammar.show_term t))

  let string_of_nominal_typ style t =
    (string_of_raw_typ style (Grammar.show_typ t))

  let raw_term style t =
    ANSITerminal.printf
      style
      "%s"
      (string_of_raw_term style t)

  let raw_typ style t =
    ANSITerminal.printf
      style
      "%s"
      (string_of_raw_typ style t)

  let nominal_term style t =
      raw_term style (Grammar.show_term t)

  let nominal_typ style t =
      raw_typ style (Grammar.show_typ t)
end
