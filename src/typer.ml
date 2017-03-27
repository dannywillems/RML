let rec type_of_internal history context term = match term with
  (* ALL-I
     Γ, x : S ⊦ t : U ∧ x \notin FV(S) =>
     Γ ⊦ λ(x : S) t ⊦ ∀(x : S) U'
  *)
  | Grammar.TermAbstraction(s, (x, t)) ->
    let context' = ContextType.add x s context in
    let u_history, u = type_of_internal history context' t in
    let typ = Grammar.TypeDependentFunction(s, (x, u)) in
    let typing_node = DerivationTree.{
        rule = "ALL-I";
        env = context;
        term = DerivationTree.Term(term);
        typ = typ
      }
    in
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
    else (
      DerivationTree.Node(
        typing_node,
        [u_history]
      ),
      typ
    )
  (* LET
     Γ ⊦ t : T ∧
     Γ, x : T ⊦ u : U ∧
     x \notin FV(U)
     =>
     Γ ⊦ let x = t in u : U
  *)
  | Grammar.TermLet(t, (x, u)) ->
    let left_history, t_typ = type_of_internal history context t in
    (* It implies that x has the type of t, i.e. t_typ *)
    let x_typ = t_typ in
    let context' = ContextType.add x x_typ context in
    let right_history, u_typ = type_of_internal history context' u in
    let typing_node = DerivationTree.{
        rule = "LET";
        env = context;
        term = DerivationTree.Term(term);
        typ = u_typ
      }
    in
    if Grammar.occurs_typ x u_typ
    then raise (Error.AvoidanceProblem(
        (Printf.sprintf
           "%s appears in %s."
           (AlphaLib.Atom.show x)
           (Print.string_of_nominal_typ u_typ)
        ),
        x,
        u_typ
      ))
    else (
      DerivationTree.Node(
        typing_node,
        [left_history ; right_history]
      ),
      u_typ
    )
  (* ALL-E.
     Γ ⊦ x : ∀(z : S) : T ∧
     Γ ⊦ y : S
     =>
     Γ ⊦ xy : [y := z]T
  *)
  | Grammar.TermVarApplication(x, y) ->
    (* Hypothesis, get the corresponding types of x and y *)
    (* We can simply use [ContextType.find x context], but it's to avoid
       duplicating code for the history construction.
    *)
    let history_x, type_of_x =
      type_of_internal history context (Grammar.TermVariable x)
    in
    let history_y, type_of_y =
      type_of_internal history context (Grammar.TermVariable y)
    in
    (* Check if [x] is a dependent function. *)
    let (s, (z, t)) =
      TypeUtils.tuple_of_dependent_function type_of_x
    in
    let _, is_subtype =
      Subtype.subtype ~context type_of_y s
    in
    if is_subtype
    then (
      (* Here, we rename the variable [x1] (which is the variable in the for all
         type, by the given variable [y]). We don't substitute the variable by
         the right types because it doesn't work with not well formed types (like
         x.A when x is of types Any).
      *)
      let typ = Grammar.rename_typ (AlphaLib.Atom.Map.singleton z y) t in
      let typing_node = DerivationTree.{
          rule ="ALL-E";
          env = context;
          term = DerivationTree.Term(term);
          typ = typ
        }

      in
      let node = DerivationTree.Node(
          typing_node,
          [history_x ; history_y]
        )
      in
      node, typ
    )
    else raise
        (Error.TypeMismatch (
            Printf.sprintf
              "ALL-E: %s must be a subtype of %s but it's of type %s."
              (AlphaLib.Atom.show y)
              (Print.string_of_raw_typ (Grammar.show_typ s))
              (Print.string_of_raw_typ (Grammar.show_typ type_of_y)),
          (s, type_of_y)
          )
        )
  (* ----- Unofficial typing rules ----- *)
  (* UN-ASC
     Γ ⊦ t : T
  *)
  | Grammar.TermAscription(t, typ_of_t) ->
    let typing_node = DerivationTree.{
        rule = "UN-ASC";
        env = context;
        term = DerivationTree.Term(term);
        typ = typ_of_t;
      }
    in
    let node = DerivationTree.Node(
        typing_node,
        history
      )
    in
    node, typ_of_t
  (* UN-UNIMPLEMENTED
     Γ ⊦ Unimplemented : ⟂
  *)
  | Grammar.TermUnimplemented ->
    let typing_node = DerivationTree.{
        rule = "UN-UNIMPLEMENTED";
        env = context;
        term = DerivationTree.Term(term);
        typ = Grammar.TypeBottom;
      }
    in
    let node = DerivationTree.Node(
        typing_node,
        history
      )
    in
    node, Grammar.TypeBottom
  (* ----- Typing rules for DOT ----- *)
  (* {}-I
     Γ, x : T ⊦ d : T
     =>
     Γ ⊦ ν(x : T) d : μ(x : T)
  *)
  | Grammar.TermRecursiveRecord(z, d) ->
    let history_d, type_of_d =
      type_of_decl_internal history context d
    in
    let typing_node = DerivationTree.{
        rule = "{}-I";
        env = context;
        term = DerivationTree.Term(term);
        typ = type_of_d
      }
    in
    let node = DerivationTree.Node(
        typing_node,
        [history_d]
      )
    in
    node, type_of_d
  (* FLD-E
     Γ ⊦ x : { a : T}
     =>
     Γ ⊦ x.a : T
  *)
  | Grammar.TermFieldSelection(x, a) ->
    (* TODO Check that x is a recursive record and contains the field a *)
    let type_of_x =
      ContextType.find x context
    in
    let typing_node = DerivationTree.{
        rule = "FLD-E";
        env = context;
        term = DerivationTree.Term(term);
        typ = type_of_x
      }
    in
    let node = DerivationTree.Node(
        typing_node,
        []
      )
    in
    node, type_of_x
  (* REC-I with REC-E (both to avoid to recompute the type of x)

     REC-I
     Γ ⊦ x : T
     =>
     Γ ⊦ x : μ(x : T)

     REC-E
     Γ ⊦ x : μ(x : T)
     =>
     Γ ⊦ x : T
  *)
  | Grammar.TermVariable(x) ->
    let type_of_x =
      ContextType.find x context
    in
    let (typ, rule) = (match type_of_x with
     | Grammar.TypeRecursive(z, type_of_z) ->
       (* TODO: type_of_z must be the same than type_of_x *)
       type_of_x, "REC-E"
     | _ ->
       Grammar.TypeRecursive(x, type_of_x), "REC-I"
      )
    in
    let typing_node = DerivationTree.{
        rule = rule;
        env = context;
        term = DerivationTree.Term(term);
        typ = typ
      }
    in
    let node = DerivationTree.Node(
        typing_node,
        history
      )
    in
    node, typ

and type_of_decl_internal history context decl = match decl with
  (* TYP-I
     Γ ⊦ { A = T } : { A : T .. T }
  *)
  | Grammar.DeclarationType(a, typ) ->
    let typ = Grammar.TypeDeclaration(a, typ, typ) in
    let typing_node = DerivationTree.{
        rule = "TYP-I";
        env = context;
        term = DerivationTree.Declaration(decl) ;
        typ = typ ;
      }
    in (
      DerivationTree.Node(
        typing_node,
        history
      ),
      typ
    )
  (* FLD-I
     Γ ⊦ t : T
     =>
     Γ ⊦ { a = t } : { a : T }
  *)
  | Grammar.DeclarationField(a, term) ->
    let history_type_of_term, type_of_term =
      type_of_internal history context term
    in
    let typ =
      Grammar.TypeFieldDeclaration(a, type_of_term)
    in
    let typing_node = DerivationTree.{
        rule = "FLD-I";
        env = context;
        term = DerivationTree.Declaration(decl) ;
        typ = typ ;
      }
    in (
      DerivationTree.Node(
        typing_node,
        [history_type_of_term]
      ),
      typ
    )
  (* ANDDEF-I
     Γ ⊦ d1 : T1 ∧ Γ ⊦ d2 : T2 ∧ dom(d1) ∩ dom(d2) = ∅
     Γ ⊦ d1 ∧ d2 : T1 ∧ T2
  *)
  | Grammar.DeclarationAggregate(d1, d2) ->
    let domain_d1 = TypeUtils.extract_label_from_declaration d1 in
    let domain_d2 = TypeUtils.extract_label_from_declaration d2 in
    if TypeUtils.SetFieldDeclaration.is_empty
        (TypeUtils.SetFieldDeclaration.inter domain_d1 domain_d2)
    then
      let history_d1, type_of_d1 =
        type_of_decl_internal history context d1
      in
      let history_d2, type_of_d2 =
        type_of_decl_internal history context d2
      in
      let typ =
        Grammar.TypeIntersection(type_of_d1, type_of_d2)
      in
      let typing_node = DerivationTree.{
          rule = "ANDDEF-I";
          env = context;
          term = DerivationTree.Declaration(decl) ;
          typ = typ ;
        }
      in (
        DerivationTree.Node(
          typing_node,
          [history_d1 ; history_d2]
        ),
        typ
      )
    else raise (Error.AggregationIntersectionNotEmpty(
        "When defining an aggregation, the domains must be disjoint.",
        d1,
        d2
      ))

let type_of ?(context = ContextType.empty ()) term =
  type_of_internal [] context term
