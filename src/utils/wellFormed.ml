(* Return [true] if [nominal_typ] is well formed. *)
let rec typ ?(use_subtyping=false) context nominal_typ = match nominal_typ with
  | Grammar.TypeTop | Grammar.TypeBottom -> true
  | Grammar.TypeDeclaration(_, s, t) ->
    (if use_subtyping then Subtype.is_subtype ~context s t else true) &&
    typ ~use_subtyping context s &&
    typ ~use_subtyping context t
  | Grammar.TypeProjection(x, a) ->
    let typ_of_x = ContextType.find x context in
    (* We can suppose x is well formed because it's checked in the case of a
       dependent function.
    let typ_of_x_is_well_formed =
      typ context typ_of_x
    in
    typ_of_x_is_well_formed &&
    *)
    Subtype.is_subtype
      ~context
      typ_of_x
      (Grammar.TypeDeclaration(a, Grammar.TypeBottom, Grammar.TypeTop))
  | Grammar.TypeDependentFunction(s, (x, t)) ->
    let context' = ContextType.add x s context in
    typ ~use_subtyping context s &&
    typ ~use_subtyping context' t
  | Grammar.TypeIntersection(t1, t2) ->
    typ ~use_subtyping context t1 &&
    typ ~use_subtyping context t2
  | Grammar.TypeRecursive(z, t) ->
    typ ~use_subtyping context t
  | Grammar.TypeFieldDeclaration(label, t) ->
    typ ~use_subtyping context t
