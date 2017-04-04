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
  (* {x : T => t^{x}} *)
  | Grammar.TermRecursiveRecord(t, (x, d)) ->
    Printf.sprintf
      "%s : %s => struct %s end"
      x
      (string_of_raw_typ t)
      (string_of_raw_decl d)
  (* Unofficial -- {x => t^{x}} *)
  | Grammar.TermRecursiveRecordUntyped(x, d) ->
    Printf.sprintf
      "%s => struct %s end"
      x
      (string_of_raw_decl d)
  (* x.a *)
  | Grammar.TermFieldSelection (x, a) ->
    Printf.sprintf
      "%s.%s"
      x
      a

(* Objects. Type tag becomes DeclarationType (instead of TermTypeTag) *)
and string_of_raw_decl t = match t with
  (* L = T *)
  | Grammar.TermTypeDeclaration (l, typ) ->
    Printf.sprintf
      "type %s = %s"
      l
      (string_of_raw_typ typ)
  (* a = t *)
  | Grammar.TermFieldDeclaration (a, term) ->
    Printf.sprintf
      "let %s = %s"
      a
      (string_of_raw_term term)
  | Grammar.TermAggregateDeclaration (d, d') ->
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
      "type %s : %s .. %s"
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
      "%s ⇒ sig %s end"
      x
      (string_of_raw_typ typ1)
  (* a : T *)
  | Grammar.TypeFieldDeclaration(a, t) ->
    Printf.sprintf
      "val %s : %s"
      a
      (string_of_raw_typ t)


let string_of_nominal_term t =
  string_of_raw_term (Grammar.show_term t)

let string_of_nominal_decl t =
  string_of_raw_decl (Grammar.show_decl t)

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

let nominal_term_endline t =
  Printf.printf "%s\n" (string_of_raw_term (Grammar.show_term t))

let nominal_typ_endline t =
  Printf.printf "%s\n" (string_of_raw_typ (Grammar.show_typ t))

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

module Pretty = struct
  open PPrint
  open PrettyPrinter

  let rec document_of_nominal_term ?(remove_identity_of_atom=true)t =
    let show_atom =
      if remove_identity_of_atom
      then AlphaLib.Atom.hint
      else AlphaLib.Atom.show
    in
    match t with
    | Grammar.TermVariable x -> string (show_atom x)
    | Grammar.TermAbstraction (typ, (x, term)) ->
      fun_ ^^
      lparen ^^
      string (show_atom x) ^^
      string " : " ^^
      document_of_nominal_typ typ ^^
      rparen ^^
      arrow_right ^^
      document_of_nominal_term term
    | Grammar.TermVarApplication (x, y) ->
      app
        (string (show_atom x))
        (string (show_atom y))
    | Grammar.TermLet (t, (x, u)) ->
      block
        (let_ ^/^ (string (show_atom x)) ^/^ equals ^^ space)
        (document_of_nominal_term t)
        (in_)
      ^/^
      document_of_nominal_term u
    (* ----- Unofficial terms ----- *)
    (* t : T *)
    | Grammar.TermAscription (t, typ_of_t) ->
      binding (document_of_nominal_term t) (document_of_nominal_typ typ_of_t)
    (* Add the unimplemented term allows to define terms without given their
       implementation. Useful for testing.
    *)
    | Grammar.TermUnimplemented ->
      unimplemented

    (* ----- Beginning of DOT terms ----- *)
    (* {x : T => t^{x}} *)
    | Grammar.TermRecursiveRecord(t, (x, d)) ->
      string (show_atom x) ^^ (document_of_nominal_typ ~remove_identity_of_atom t) ^^
      (module_block
         (string " : " ^^ struct_ )
         (document_of_nominal_term_declaration d)
         (end_)
      )
    (* Unofficial -- {x => t^{x}} *)
    | Grammar.TermRecursiveRecordUntyped(x, d) ->
      module_block
        (
          (string (show_atom x)) ^^
          double_arrow_right ^^
          struct_
        )
        (document_of_nominal_term_declaration d)
        (end_)
    (* x.a *)
    | Grammar.TermFieldSelection (x, a) ->
      (string (show_atom x)) ^^
      string a

  (* Objects. Type tag becomes DeclarationType (instead of TermTypeTag) *)
  and document_of_nominal_term_declaration ?(remove_identity_of_atom=true) t =
    match t with
    (* L = T *)
    | Grammar.TermTypeDeclaration (l, typ) ->
      type_ ^^ string l ^^ string " = " ^^ document_of_nominal_typ typ
    (* a = t *)
    | Grammar.TermFieldDeclaration (a, term) ->
      let_ ^^ string a ^^ string " = " ^^ document_of_nominal_term term
    | Grammar.TermAggregateDeclaration (d, d') ->
      (document_of_nominal_term_declaration d)
      ^^
      hardline
      ^^
      (document_of_nominal_term_declaration d')

  and document_of_nominal_typ_declaration ?(remove_identity_of_atom=true) t =
    match t with
    (* L : S..T --> (L, S, T) *)
    | Grammar.TypeDeclaration (type_label, typ1, typ2) ->
      type_ ^^
      string type_label ^^
      string " = " ^^
      document_of_nominal_typ ~remove_identity_of_atom typ1 ^^
      string " .. " ^^
      document_of_nominal_typ ~remove_identity_of_atom typ2
    (* T ∧ T *)
    | Grammar.TypeIntersection (typ1, typ2) ->
      document_of_nominal_typ ~remove_identity_of_atom typ1
      ^^
      hardline
      ^^
      document_of_nominal_typ ~remove_identity_of_atom typ2
    (* a : T *)
    | Grammar.TypeFieldDeclaration(a, t) ->
      val_ ^^
      string a ^^
      string " : " ^^
      document_of_nominal_typ ~remove_identity_of_atom t
    | _ -> document_of_nominal_typ ~remove_identity_of_atom t

  and document_of_nominal_typ ?(remove_identity_of_atom=true) t =
    let show_atom =
      if remove_identity_of_atom
      then AlphaLib.Atom.hint
      else AlphaLib.Atom.show
    in
    match t with
    (* Top type : ⊤ *)
    | Grammar.TypeTop -> string "⊤"
    (* Bottom type : ⟂ *)
    | Grammar.TypeBottom -> string "⟂"
    (* x.L *)
    | Grammar.TypeProjection (x, typ) ->
      (string (show_atom x)) ^^
      dot ^^
      string typ
    (* ∀(x : S) T --> (S, (x, T)) *)
    | Grammar.TypeDependentFunction (typ1, (x, typ2)) ->
      let argument_document =
        if String.equal (AlphaLib.Atom.hint x) "_"
        then (
          document_of_nominal_typ typ1 ^^
          arrow_right
        )
        else (
          forall ^^
          lparen ^^
          (binding
             (string (show_atom x))
             (document_of_nominal_typ typ1)
          ) ^^
          rparen
        )
      in
      argument_document ^^ document_of_nominal_typ typ2
      (* ----- Beginning of DOT types ----- *)
      (* T ∧ T *)
    | Grammar.TypeIntersection (typ1, typ2) ->
      document_of_nominal_typ typ1
      ^^
      hardline
      ^^
      document_of_nominal_typ typ2
    (* { z => T^{z} } *)
    | Grammar.TypeRecursive (x, typ1) ->
      module_block
        ((string (show_atom x)) ^^ double_arrow_right ^^ sig_)
        (document_of_nominal_typ_declaration typ1)
        (end_)
    | _ -> document_of_nominal_typ_declaration ~remove_identity_of_atom t

  let nominal_term ?(remove_identity_of_atom=true) () =
    adapt (document_of_nominal_term ~remove_identity_of_atom)

  let nominal_typ ?(remove_identity_of_atom=true) () =
    adapt (document_of_nominal_typ ~remove_identity_of_atom)

  let nominal_term_declaration ?(remove_identity_of_atom=true) () =
    adapt (document_of_nominal_term_declaration ~remove_identity_of_atom)

  let nominal_typ_declaration ?(remove_identity_of_atom=true) () =
    adapt (document_of_nominal_typ_declaration ~remove_identity_of_atom)
end
