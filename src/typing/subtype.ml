(* This sum type will be used to distinguish which selection rule we want to
   use. It is useful to avoid duplicated code for selection rules. It is not in the
   interface because it's only to use internally.
*)
type rule_sel =
  | SEL_SUB
  | SUB_SEL

(* Used to create the derivation tree *)
let string_of_rule_sel = function
  | SEL_SUB -> "SEL <:"
  | SUB_SEL -> "<: SEL"

let rec var_unpack context x t =
  match t with
  | Grammar.TypeRecursive(z, t_z) ->
    Grammar.rename_typ (AlphaLib.Atom.Map.singleton z x) t_z
  | t -> t

(* As selection rules are very close, we abstract it with this function.

   [rule_sel SEL_SUB history context (x, label) t] will apply the rule SEL <:
   for x.label <: t.

   [rule_sel SUB_SEL history context (x, label) t] will apply the rule <: SEL
   for t <: x.label.
*)
let rec rule_sel rule history context (x, label) t =
  let type_of_x =
    var_unpack context x (ContextType.find x context)
  in
  let l, u =
    match rule with
    | SEL_SUB -> Grammar.TypeProjection(x, label), t
    | SUB_SEL -> t, Grammar.TypeProjection(x, label)
  in
  let s =
    match rule with
    | SEL_SUB ->
      TypeUtils.least_upper_bound_of_recursive_type
        ~x
        ~label
        context
        type_of_x
    | SUB_SEL ->
      TypeUtils.greatest_lower_bound_of_recursive_type
        ~x
        ~label
        context
        type_of_x
  in
  match s with
  | Some s ->
    let derivation_tree_subtype, is_subtype =
      match rule with
      | SEL_SUB -> subtype_internal history context s t
      | SUB_SEL -> subtype_internal history context t s
    in
    DerivationTree.create_subtyping_node
      ~rule:(string_of_rule_sel rule)
      ~is_true:is_subtype
      ~env:context
      ~s:l
      ~t:u
      ~history:[derivation_tree_subtype]
  | None ->
    DerivationTree.create_subtyping_node
      ~rule:(string_of_rule_sel rule)
      ~is_true:false
      ~env:context
      ~s:l
      ~t:u
      ~history

(* The main subtyping algorithm. Rules are very close to rules given in official
   papers.
*)
and subtype_internal history context s t =
  match (s, t) with
  (* TOP
     Γ ⊦ S <: ⊤
  *)
  | (_, Grammar.TypeTop) ->
    DerivationTree.create_subtyping_node
      ~rule:"TOP"
      ~is_true:true
      ~env:context
      ~s
      ~t
      ~history:history
  (* BOTTOM
     Γ ⊦ ⟂ <: S
  *)
  | (Grammar.TypeBottom, _) ->
    DerivationTree.create_subtyping_node
      ~rule:"BOTTOM"
      ~is_true:true
      ~env:context
      ~s
      ~t
      ~history:history
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
    DerivationTree.create_subtyping_node
      ~rule:"UN-REFL-TYP"
      ~is_true:true
      ~env:context
      ~s
      ~t
      ~history
  (* <: SEL <:
     This pattern is used when we have x.A <: y.A. In this case, we first try
     SEL <:. If it succeeds, we return this solution, else we try <: SEL. If <:
     SEL succeeds, we return the solution, else it implies x.A is not a subtype
     of y.A.
  *)
  | Grammar.TypeProjection(x, label_x), Grammar.TypeProjection(y, label_y) ->
    (* We first try SEL <: *)
    let node_sel_sub, is_subtype_sel_sub =
      rule_sel SEL_SUB history context (x, label_x) t
    in
    if is_subtype_sel_sub
    then (node_sel_sub, is_subtype_sel_sub)
    else (
      let node_sub_sel, is_subtype_sub_sel =
        rule_sel SUB_SEL history context (y, label_y) s
      in
      node_sub_sel, is_subtype_sub_sel
    )
  (* TYP <: TYP
     Γ ⊦ S2 <: S1 ∧ Γ ⊦ T1 <: T2 =>
     Γ ⊦ { A : S1 .. T1 } <: { A : S2 .. T2 }
  *)
  | Grammar.TypeDeclaration(tag1, s1, t1), Grammar.TypeDeclaration(tag2, s2, t2)
    when String.equal tag1 tag2 ->
    let left_derivation_tree, left_is_subtype =
      subtype_internal history context s2 s1
    in
    let right_derivation_tree, right_is_subtype =
      subtype_internal history context t1 t2
    in
    DerivationTree.create_subtyping_node
      ~rule:"TYP <: TYP"
      ~is_true:(left_is_subtype && right_is_subtype)
      ~env:context
      ~s
      ~t
      ~history:[left_derivation_tree ; right_derivation_tree]
  (* FLD <: FLD
     Γ ⊦ T <: U => Γ ⊦ { a : T } <: { a : U }
  *)
  | (Grammar.TypeFieldDeclaration(a, t), Grammar.TypeFieldDeclaration(b, u))
    when String.equal a b ->
    let history_subtype, is_subtype =
      subtype_internal history context t u
    in
    DerivationTree.create_subtyping_node
      ~rule:"FLD <: FLD"
      ~is_true:is_subtype
      ~env:context
      ~s:(Grammar.TypeFieldDeclaration(a, t))
      ~t:(Grammar.TypeFieldDeclaration(b, u))
      ~history:[history_subtype]
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
    DerivationTree.create_subtyping_node
      ~rule:"ALL <: ALL"
      ~is_true:(left_is_subtype && right_is_subtype)
      ~env:context
      ~s
      ~t
      ~history:[left_derivation_tree ; right_derivation_tree]
  (* UN-REC
     We compare two recursive types. We use VAR-UNPACK on both sides by renaming
     the internal variable with a fresh atom.
  *)
  | (Grammar.TypeRecursive(z, rec_t), Grammar.TypeRecursive(z', rec_t')) ->
    let fresh_z = AlphaLib.Atom.copy z' in
    let fresh_rec_t = Grammar.rename_typ (AlphaLib.Atom.Map.singleton z fresh_z) rec_t in
    let fresh_rec_t' = Grammar.rename_typ (AlphaLib.Atom.Map.singleton z' fresh_z) rec_t' in
    let context' = ContextType.add fresh_z fresh_rec_t context in
    let history_subtype, is_subtype =
      subtype_internal history context' fresh_rec_t fresh_rec_t'
    in
    DerivationTree.create_subtyping_node
      ~rule:"UN-REC"
      ~is_true:is_subtype
      ~env:context
      ~s
      ~t
      ~history:[history_subtype]
  (* UN-REC-I<:
     We compare a recursive type with another. We use VAR-UNPACK on the left side.
  *)
  | (Grammar.TypeRecursive(z, t'), t) ->
    (* We need to extend the context with the type of z because t' can use
       fields and types defined in z.
    *)
    let fresh_z = AlphaLib.Atom.copy z in
    let fresh_t' = Grammar.rename_typ (AlphaLib.Atom.Map.singleton z fresh_z) t' in
    let context' = ContextType.add fresh_z fresh_t' context in
    let history_subtype, is_subtype =
      subtype_internal history context' fresh_t' t
    in
    DerivationTree.create_subtyping_node
      ~rule:"UN-REC-I <:"
      ~is_true:is_subtype
      ~env:context
      ~s
      ~t
      ~history:[history_subtype]
  (* UN-<: REC-I
     We compare a recursive type with another. We use VAR-UNPACK on the right side.
  *)
  | (s, Grammar.TypeRecursive(z, t')) ->
    (* We need to extend the context with the type of z because t' can use
       fields and types defined in z.
    *)
    let fresh_z = AlphaLib.Atom.copy z in
    let fresh_t' = Grammar.rename_typ (AlphaLib.Atom.Map.singleton z fresh_z) t' in
    let context' = ContextType.add fresh_z fresh_t' context in
    let history_subtype, is_subtype =
      subtype_internal history context' s fresh_t'
    in
    DerivationTree.create_subtyping_node
      ~rule:"UN-<: REC-I"
      ~is_true:is_subtype
      ~env:context
      ~s
      ~t
      ~history:[history_subtype]
  (* Type projection on the left with another type. *)
  | (Grammar.TypeProjection(x, label), u') ->
    rule_sel SEL_SUB history context (x, label) u'
  (* <: SEL.
     SUB is allowed for lower bound. This rule unifies official <: SEL and SUB.
     Γ ⊦ x : { A : L .. U }
     =>
     Γ ⊦ L <: x.A
     becomes
     Γ ⊦ x : { A : L' .. U } ∧ Γ ⊦ L <: L'
     =>
     Γ ⊦ L <: x.A

     With [TypeUtils.greatest_lower_bound], the actual rule is
     Γ ⊦ x : T ∧ Γ ⊦ { A : L' .. U } <: T ∧ Γ ⊦ L <: L'
     =>
     Γ ⊦ L <: x.A
  *)
  (* Type projection on the right with another type. *)
  | (l, Grammar.TypeProjection(x, label)) ->
    rule_sel SUB_SEL history context (x, label) l
  (* ----- Beginning of DOT rules ----- *)
  (* The order is important. First, we check AND1 <: to eliminate the interseciton on the right.
     After that, we test case by cases when we have an intersection on the left.
     First, recursive types must be handled because it implies to use UNPACK and
     PACK.
  *)
  (* AND1 <:
     Γ ⊦ S <: T ∧ Γ ⊦ T ∧ U <: T
     =>
     Γ ⊦ S ∧ U <: T

     AND2 <:
     Γ ⊦ S <: U ∧ Γ ⊦ T ∧ U <: U
     =>
     Γ ⊦ T ∧ S <: U
  *)
  | (s, Grammar.TypeIntersection(t, u)) ->
    let left_history_subtype, left_is_subtype =
      subtype_internal history context s t
    in
    let right_history_subtype, right_is_subtype =
      subtype_internal history context s u
    in
    DerivationTree.create_subtyping_node
      ~rule:"AND <:"
      ~is_true:(left_is_subtype && right_is_subtype)
      ~env:context
      ~s
      ~t:(Grammar.TypeIntersection(t, u))
      ~history:[left_history_subtype ; right_history_subtype]
  (* UN-AND <: REC-REC
     We need this case for the following cases for examples:
     sig type t end INTER sig val f : Int end <: sig type t val f : Int end

     The answer to this question is yes. We can use AND, UNPACK two times and
     VAR-PACK on the intersection we got with the unpack.
  *)
  | (Grammar.TypeIntersection(Grammar.TypeRecursive(z1, s1), Grammar.TypeRecursive(z2, s2)), t) ->
    let fresh_z = AlphaLib.Atom.copy z1 in
    let fresh_s1 = Grammar.rename_typ (AlphaLib.Atom.Map.singleton z1 fresh_z) s1 in
    let fresh_s2 = Grammar.rename_typ (AlphaLib.Atom.Map.singleton z2 fresh_z) s2 in
    let fresh_s = Grammar.TypeRecursive(fresh_z, Grammar.TypeIntersection(fresh_s1, fresh_s2)) in
    let history, is_subtype =
      subtype_internal history context fresh_s t
    in
    DerivationTree.create_subtyping_node
      ~rule:"UN-AND <: REC-REC"
      ~is_true:is_subtype
      ~env:context
      ~s
      ~t:fresh_s
      ~history:[history]
  (* UN-AND <: REC-T
     We need this case for the following case:
     sig type t end INTER val f : Int <: sig type t val f : Int end

     The answer to this question is yes. We can use AND, UNPACK one time and
     VAR-PACK on the intersection we got with the unpack.
  *)
  | (Grammar.TypeIntersection(Grammar.TypeRecursive(z1, s1), s2), t) ->
    let fresh_s = Grammar.TypeRecursive(z1, Grammar.TypeIntersection(s1, s2)) in
    let history, is_subtype =
      subtype_internal history context fresh_s t
    in
    DerivationTree.create_subtyping_node
      ~rule:"UN-AND <: REC-T"
      ~is_true:is_subtype
      ~env:context
      ~s
      ~t:fresh_s
      ~history:[history]
  (* UN-AND <: T-REC
     We need this case for the following case:
     val f : Int INTER sig type t end <: sig type t val f : Int end

     The answer to this question is yes. We can use AND, UNPACK one time and
     VAR-PACK on the intersection we got with the unpack.
  *)
  | (Grammar.TypeIntersection(s1, Grammar.TypeRecursive(z2, s2)), t) ->
    let fresh_s = Grammar.TypeRecursive(z2, Grammar.TypeIntersection(s1, s2)) in
    let history, is_subtype =
      subtype_internal history context fresh_s t
    in
    DerivationTree.create_subtyping_node
      ~rule:"UN-AND <: T-REC"
      ~is_true:is_subtype
      ~env:context
      ~s
      ~t:fresh_s
      ~history:[history]

  (* <: AND
     Γ ⊦ S <: T ∧ Γ ⊦ S <: U
     =>
     Γ ⊦ S <: T ∧ U
  *)
  | (Grammar.TypeIntersection(s, u), t) ->
    let history_left, is_subtype_left =
      subtype_internal history context s t
    in
    if is_subtype_left
    then (
      DerivationTree.create_subtyping_node
        ~rule:"AND1<:"
        ~is_true:is_subtype_left
        ~env:context
        ~s:(Grammar.TypeIntersection(s, u))
        ~t
        ~history:[history_left]
    )
    else (
      let history_right, is_subtype_right =
        subtype_internal history context u t
      in
      DerivationTree.create_subtyping_node
        ~rule:"AND2<:"
        ~is_true:is_subtype_right
        ~env:context
        ~s:(Grammar.TypeIntersection(s, u))
        ~t
        ~history:[history_left ; history_right]
    )
  | _ ->
    DerivationTree.create_subtyping_node
      ~rule:"NO RULE"
      ~is_true:false
      ~env:context
      ~s
      ~t
      ~history:[]

let subtype ?(context = ContextType.empty ()) s t =
  subtype_internal [] context s t
(* subtype_internal (ContextType.empty ()) s t *)

let is_subtype ?(context = ContextType.empty ()) s t =
  let _, b = subtype ~context s t in
  b
