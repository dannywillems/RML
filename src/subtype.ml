(*
let rec subtype_internal history context s t =
  match (s, t) with
  (* TOP
     Γ ⊦ S <: ⊤
  *)
  | (_, Grammar.TypeTop) ->
    let subtyping_node =
      DerivationTree.{
        rule = "TOP";
        is_true = true;
        env = context;
        s = s;
        t = t
    } in
    (DerivationTree.Node (subtyping_node, history), true)
  (* BOTTOM
     Γ ⊦ ⟂ <: S
  *)
  | (Grammar.TypeBottom, _) ->
    let subtyping_node =
      DerivationTree.{
        rule = "BOTTOM";
        is_true = true;
        env = context;
        s = s;
        t = t
    } in
    (DerivationTree.Node (subtyping_node, history), true)
  (* UN-REFL-TYP.
     This rule is added from the official rule to be able to remove REFL.
     The missing typing rules was for type projections. We only need to check
     that the variables are represented by the same atom.

     NOTE: The when statement is mandatory!
     If we don't mention it, and do the atom equality checking in the body of
     the expression for this pattern, it won't work because the algorithm choose
     this pattern instead of SEL <: or <: SEL.
     Γ ⊦ x.A <: x.A.
  *)
  | Grammar.TypeProjection(x, label_x), Grammar.TypeProjection(y, label_y)
    when (String.equal label_x label_y) && (AlphaLib.Atom.equal x y) ->
    let subtyping_node =
      DerivationTree.{
        rule = "UN-REFL-TYP";
        is_true = true;
        env = context;
        s = s;
        t = t
    } in
    (
      DerivationTree.Node (subtyping_node, history),
      true
    )
  (* UN-<: SEL <:*)
  | Grammar.TypeProjection(x, label_x), Grammar.TypeProjection(y, label_y) ->
    (* We first try SEL <: *)
    let type_of_x = ContextType.find x context in
    let (label, l, u) =
      TypeUtils.tuple_of_type_declaration context type_of_x
    in
    let derivation_tree_subtype, is_subtype =
      subtype_internal history context u (Grammar.TypeProjection(y, label_y))
    in
    let subtyping_node =
      DerivationTree.{
        rule = "UN-SEL <:";
        is_true = String.equal label label_x && is_subtype;
        env = context;
        s = s;
        t = t
    } in (
      (* If we succeed in proving x.A <: y.A by beginning with SEL <:, we return
         this derivation
      *)
      if is_subtype
      then (
        DerivationTree.Node (subtyping_node, [derivation_tree_subtype]),
        String.equal label label_x && is_subtype
      )
      else (
        (* Else we try <: SEL *)
        let type_of_y = ContextType.find y context in
        let (label, l, u) =
          TypeUtils.tuple_of_type_declaration context type_of_y
        in
        let derivation_tree_subtype, is_subtype =
          subtype_internal history context (Grammar.TypeProjection(x, label_x)) l
        in
        let subtyping_node =
          DerivationTree.{
            rule = "UN-<: SEL";
            is_true = String.equal label label_y && is_subtype;
            env = context;
            s = s;
            t = t
          }
        in
        (* And in every case, we return this derivation. *)
        (* IMPROVEME:
           It is more interesting to return all possible derivation
           trees
        *)
        DerivationTree.Node (subtyping_node, [derivation_tree_subtype]),
        String.equal label label_y && is_subtype
      )
    )

  (* TYP <: TYP
     Γ ⊦ S2 <: S1 ∧ Γ ⊦ T1 <: T2 =>
     Γ ⊦ { A : S1 .. T1 } <: { A : S2 .. T2 }
  *)
  | Grammar.TypeDeclaration(tag1, s1, t1), Grammar.TypeDeclaration(tag2, s2, t2) ->
    let left_derivation_tree, left_is_subtype =
      subtype_internal history context s2 s1
    in
    let right_derivation_tree, right_is_subtype =
      subtype_internal history context t1 t2
    in
    let subtyping_node =
      DerivationTree.{
        rule = "TYP <: TYP";
        is_true = String.equal tag1 tag2 && left_is_subtype && right_is_subtype;
        env = context;
        s = s;
        t = t
    } in
    (
      DerivationTree.Node (
        subtyping_node,
        [left_derivation_tree ; right_derivation_tree]
      ),
      String.equal tag1 tag2 && left_is_subtype && right_is_subtype
    )
  (* SEL <:.
     SUB is allowed for upper bound. This rule unifies official SEL <: and SUB.
     Γ ⊦ x : { A : L .. U } => Γ ⊦ x.A <: U
     becomes
     Γ ⊦ x : { A : L .. U } and Γ ⊦ U <: U' => Γ ⊦ x.A <: U'
  *)
  | (Grammar.TypeProjection(x, label_selected), u') ->
    let type_of_x = ContextType.find x context in
    let (label, l, u) =
      TypeUtils.tuple_of_type_declaration context type_of_x
    in
    let derivation_tree_subtype, is_subtype =
      subtype_internal history context u u'
    in
    let subtyping_node =
      DerivationTree.{
        rule = "SEL <:";
        is_true = String.equal label label_selected && is_subtype;
        env = context;
        s = s;
        t = t
    } in
    (
      DerivationTree.Node (subtyping_node, [derivation_tree_subtype]),
      String.equal label label_selected && is_subtype
    )
  (* <: SEL.
     SUB is allowed for lower bound. This rule unifies official <: SEL and SUB.
     Γ ⊦ x : { A : L .. U } =>
     Γ ⊦ L <: x.A
     becomes
     Γ ⊦ x : { A : L' .. U } ∧ Γ ⊦ L <: L' =>
     Γ ⊦ L <: x.A
  *)
  | (l, Grammar.TypeProjection(x, label_selected)) ->
    (* We get the corresponding label, lower bound and upper bound for the given
       variable x from the environment and we check if s1 and the lower bound are
       equivalent.
    *)
    let type_of_x =
      ContextType.find x context
    in
    let (label, l', u) =
      TypeUtils.tuple_of_type_declaration context type_of_x
    in
    let derivation_tree_subtype, is_subtype =
      subtype_internal history context l l'
    in
    let subtyping_node =
      DerivationTree.{
        rule = "<: SEL";
        is_true = String.equal label label_selected && is_subtype;
        env = context;
        s = s;
        t = t
    } in
    (
      DerivationTree.Node (subtyping_node, [derivation_tree_subtype]),
      String.equal label label_selected && is_subtype
    )

  (* ALL <: ALL
     Γ ⊦ S2 <: S1 ∧ Γ, x : S2 ⊦ T1 <: T2 =>
     Γ ⊦ ∀(x : S1) T1 <: ∀(x : S2) T2
  *)
  | (Grammar.TypeDependentFunction(s1, (x1, t1)),
     Grammar.TypeDependentFunction(s2, (x2, t2))
    ) ->
    (* We create a new variable x and replace x1 (resp. x2) in t1 (resp. t2) by
       the resulting variable. With this method, we can only add (x : S2) in the
       environment.
    *)
    let x = AlphaLib.Atom.copy x1 in
    let t1' = Grammar.rename_typ (AlphaLib.Atom.Map.singleton x1 x) t1 in
    let t2' = Grammar.rename_typ (AlphaLib.Atom.Map.singleton x2 x) t2 in
    let context' = ContextType.add x s2 context in
    let left_derivation_tree, left_is_subtype =
      subtype_internal history context s2 s1
    in
    let right_derivation_tree, right_is_subtype =
      subtype_internal history context' t1' t2'
    in
    let subtyping_node =
      DerivationTree.{
        rule = "ALL <: ALL";
        is_true = left_is_subtype && right_is_subtype;
        env = context;
        s = s;
        t = t
    } in
    (
      DerivationTree.Node (
        subtyping_node,
        [left_derivation_tree ; right_derivation_tree]
      ),
      left_is_subtype && right_is_subtype
    )
  (* ----- Beginning of DOT rules ----- *)
  (* AND1 <:
     Γ ⊦ T ∧ U <: T
  *)
  | (Grammar.TypeIntersection(t, u), t')
    when Grammar.equiv_typ t t' ->
    let subtyping_node =
      DerivationTree.{
        rule = "AND1 <:";
        is_true = true;
        env = context;
        s = s;
        t = t';
      }
    in
    (
      DerivationTree.Node(
        subtyping_node,
        history
      ),
      true
    )
  (* AND2 <:
     Γ ⊦ T ∧ U <: U
  *)
  | (Grammar.TypeIntersection(t, u), u')
    when Grammar.equiv_typ u u' ->
    let subtyping_node =
      DerivationTree.{
        rule = "AND2 <:";
        is_true = true;
        env = context;
        s = s;
        t = u';
      }
    in
    (
      DerivationTree.Node(
        subtyping_node,
        history
      ),
      true
    )
  (* AND2 <:
     Γ ⊦ S <: T ∧ Γ ⊦ S <: U
     =>
     Γ ⊦ S <: T ∧ U
  *)
  | (s, Grammar.TypeIntersection(t, u)) ->
    let left_history_subtype, left_is_subtype =
      subtype_internal history context s t
    in
    let right_history_subtype, right_is_subtype =
      subtype_internal history context s u
    in
    let subtyping_node =
      DerivationTree.{
        rule = "<: AND";
        is_true = true;
        env = context;
        s = s;
        t = Grammar.TypeIntersection(t, u);
      }
    in
    (
      DerivationTree.Node(
        subtyping_node,
        [left_history_subtype, right_history_subtype]
      ),
      left_is_subtype && right_is_subtype
    )
  (* FLD <: FLD
     Γ ⊦ T <: U => Γ ⊦ { a : T } <: { a : U }
  *)
  | (Grammar.TypeFieldDeclaration(a, t), Grammar.TypeFieldDeclaration(b, u))
    when String.equal a b ->
    let history_subtype, is_subtype =
      subtype_internal history context t u
    in
    let subtyping_node =
      DerivationTree.{
        rule = "FLD <: FLD";
        is_true = true;
        env = context;
        s = Grammar.DeclarationFieldDeclaration(a, t);
        t = Grammar.DeclarationFieldDeclaration(b, u);
      }
    in
    (
      DerivationTree.Node(
        subtyping_node,
        [history_subtype]
      ),
      true
    )
  | _ ->
      let subtyping_node =
        DerivationTree.{
          rule = "WRONG";
          is_true = false;
          env = context;
          s = s;
          t = t
        }
      in
      DerivationTree.Node (
        subtyping_node,
        []
      ),
      false
*)
let subtype ?(context = ContextType.empty ()) s t =
  (DerivationTree.Empty, true)
(* subtype_internal (ContextType.empty ()) s t *)

