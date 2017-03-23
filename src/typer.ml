let rec subtype_internal context s t = match (s, t) with
  (* --- Lattice structure --- *)
  (* BOT *)
  | (Grammar.TypeBottom, _) -> true
  (* TOP *)
  | (_, Grammar.TypeTop) -> true
  (* AND11 and AND12 *)
  | (Grammar.TypeIntersection(t1, t2), t) ->
    (subtype_internal context t1 t) || (subtype_internal context t2 t)
  (* OR21 and OR22 *)
  | (t, Grammar.TypeUnion(t1, t2)) ->
    (subtype_internal context t t1) || (subtype_internal context t t2)
  (* AND1 *)
  | (t, Grammar.TypeIntersection(t1, t2)) ->
    (subtype_internal context t t1) && (subtype_internal context t t2)
  (* OR2 *)
  | (Grammar.TypeUnion(t1, t2), t) ->
    (subtype_internal context t1 t) && (subtype_internal context t2 t)
  (* --- Type and method members --- *)
  (* TYP *)
  | (Grammar.TypeTypeMember(method_label1, s1, u1),
     Grammar.TypeTypeMember(method_label2, s2, u2)
    ) ->
    (* We need to be sure we compare the same label *)
    (method_label1 == method_label2) &&
    (* Covariant for the lower bound *)
    (subtype_internal context s2 s1) &&
    (* Contravariant for the upper bound *)
    (subtype_internal context u1 u2)
  | (Grammar.TypeMethodMember(method_label1, s1, (n_x1, t1)),
     Grammar.TypeMethodMember(method_label2, s2, (n_x2, t2))
    ) ->
    (* When comparing t1 and t2, the nominal representation of the variable x
    can be different if we compare two different methods. When adding to the
    context, we must be sure that both variables are in the context. *)
    let context' = ContextType.add n_x1 s2 (ContextType.add n_x2 s2 context) in
    (method_label1 == method_label2) &&
    (subtype_internal context s2 s1) &&
    (subtype_internal context' t1 t2)
  (* --- Path selections --- *)
  (* SELX *)
  | (Grammar.TypePathDependent(x1, type_label1),
     Grammar.TypePathDependent(x2, type_label2)
    ) ->
    (* We need to compare type labels of the same variable *)
    (AlphaLib.Atom.equal x1 x2) &&
    (type_label1 == type_label2)
    (* TODO: SEL1 *)
    (* TODO: SEL2 *)
  (* --- Recursive self types *)
  (*
  | (Grammar.TypeRecursive(z1, l1), Grammar.TypeRecursive(z2, l2)) ->
  *)
  (* --- Transitivity --- *)
  (* TODO: SEL2 *)
  | (_, _) -> false

let subtype s t =
  subtype_internal (ContextType.empty ()) s t

