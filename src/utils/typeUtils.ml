module SetFieldDeclaration = Set.Make(String)

type direction =
  | Upper
  | Lower

let var_unpack x z t =
  (*
   REC-I
   Γ ⊦ x : μ(z : T^{z})
   =>
   Γ ⊦ x : T^{x}
  *)
  Grammar.rename_typ (AlphaLib.Atom.Map.singleton z x) t

let rec best_bound_of_recursive_type ~direction ~x ~label context t = match t with
  (* Bottom type : ⟂ *)
  (* The least upper bound is the type declaration { A : Bottom .. Bottom }.
  And there is no greatest lower bound in the form { A : L .. U } *)
  | Grammar.TypeBottom ->
    (match direction with
    | Upper -> Some Grammar.TypeBottom
    | Lower -> None)
  (* Top type : ⊤ *)
  (* The greatest lower bound is the type declaration { A : Top .. Top }.
  And there is no least upper bound in the form { A : L .. U } *)
  | Grammar.TypeTop ->
    (match direction with
    | Lower -> Some Grammar.TypeTop
    | Upper -> None)
  (* ∀(x : S) T --> (S, (x, T)) *)
  (* No comparable *)
  | Grammar.TypeDependentFunction(_) ->
    None
  (* { L : S..T } --> S (resp. T) if direction is upper (resp. lower) *)
  (* The type of the given variable is a type declaration. *)
  | Grammar.TypeDeclaration(l, s, t) ->
    if String.equal l label then
      match direction with
      | Lower -> Some s
      | Upper -> Some t
    else None
  (* x.L, a path selection. We must call recursively. *)
  | Grammar.TypeProjection(y, label_y) ->
    let type_of_y = ContextType.find y context in
    (* Recursive call to the algorithm. It is supposed to return the greatest
       lower bound (resp. the least upper bound) of x wrt the label given by
       [label_selected].
       [u'] is the best bound for the type of [x].
    *)
    let u' =
      best_bound_of_recursive_type
        ~direction
        ~x:y
        ~label:label_y
        context
        type_of_y
    in
    (match u' with
    | Some u' -> best_bound_of_recursive_type ~direction ~x ~label context u'
    | None -> None
    )
  (* ----- Beginning of DOT types ----- *)
  (* { z => T^{z} } *)
  | Grammar.TypeRecursive(z, t) ->
    (* First, we unpack the type of x *)
    let type_of_x = var_unpack x z t in
    (* And we do a recursive call to the algorithm. *)
    best_bound_of_recursive_type ~direction ~x ~label context type_of_x
    (*
    let t =
      best_bound_of_recursive_type ~direction ~x:z ~label context t
    in
    (match t with
     | None -> None
     | Some t -> Some (Grammar.rename_typ (AlphaLib.Atom.Map.singleton z x) t)
    )
    *)

  (* T ∧ T *)
  | Grammar.TypeIntersection(t1, t2) ->
    let best_bound_for_t1 =
      best_bound_of_recursive_type ~direction ~x ~label context t1
    in
    let best_bound_for_t2 =
      best_bound_of_recursive_type ~direction ~x ~label context t2
    in
    (match (best_bound_for_t1, best_bound_for_t2) with
     (* If both return None, it means we can not find a best bound, so we return None. *)
    | (None, None) -> None
    (* If one of them is None, it means we can find a U which only satisfies one of
    them, not both. We must return None because to return the found U, it must
    be OK for both as it's an intersection. *)
    | (Some t, None) | (None, Some t) -> Some t
    (* If a same field is defined two times, we take the last definition, i.e.
       in the type in the right.
       Normally, this case can not be produced because in the typing algorithm,
       we need to have different domains when doing an intersection.
    *)
    | (Some s, Some t) -> Some t)
  (* { a : T } *)
  | Grammar.TypeFieldDeclaration(a, t) ->
    if String.equal a label
    then Some t
    else None

let least_upper_bound_of_recursive_type ~x ~label context t =
  best_bound_of_recursive_type ~direction:Upper ~x ~label context t

let greatest_lower_bound_of_recursive_type ~x ~label context t =
  best_bound_of_recursive_type ~direction:Lower ~x ~label context t

let rec least_upper_bound_of_dependent_function context t = match t with
  | Grammar.TypeDependentFunction(s, (x, t)) ->
    Some (s, (x, t))
  | Grammar.TypeBottom -> Some (Grammar.TypeTop, ((AlphaLib.Atom.fresh "_"), Grammar.TypeBottom))
  | Grammar.TypeTop -> None
  | Grammar.TypeDeclaration(_) -> None
  (* Si on a L = x.A, on a x de la forme { A : L .. U }. On fait alors appel à
     least_upper_bound pour récupérer le plus petit U tel que L <: U et on
     applique de nouveau best_tuple_of_dependent_function sur U pour récupérer le
     plus U' tel que U' est de la forme ∀(x : S) T.
  *)
  | Grammar.TypeProjection(x, label) ->
    let type_of_x =
      ContextType.find x context
    in
    let least_upper_bound =
      least_upper_bound_of_recursive_type ~x ~label context type_of_x
    in
    (match least_upper_bound with
    | None -> None
    | Some u -> least_upper_bound_of_dependent_function context u
    )
  | _ -> None

let rec labels_of_recursive_type context recursive_type =
  match recursive_type with
  (* Top type : ⊤ *)
  | Grammar.TypeTop ->
    SetFieldDeclaration.empty
  (* Bottom type : ⟂ *)
  | Grammar.TypeBottom ->
    SetFieldDeclaration.empty
  (* { L : S..T } --> (L, S, T) *)
  | Grammar.TypeDeclaration(a, s, t) ->
    SetFieldDeclaration.singleton a
  (* x.L *)
  (* Need to proof it's correct *)
  | Grammar.TypeProjection(x, l) ->
    let type_of_x = ContextType.find x context in
    let u =
      least_upper_bound_of_recursive_type ~x ~label:l context type_of_x
    in
    (match u with
    | None -> SetFieldDeclaration.empty
    | Some u -> labels_of_recursive_type context u)
  (* ∀(x : S) T --> (S, (x, T)) *)
  | Grammar.TypeDependentFunction(_) ->
    SetFieldDeclaration.empty
  (* T ∧ T *)
  | Grammar.TypeIntersection(t1, t2) ->
    let labels_of_t1 =
      labels_of_recursive_type context t1
    in
    let labels_of_t2 =
      labels_of_recursive_type context t1
    in
    SetFieldDeclaration.union labels_of_t1 labels_of_t2
  (* { z => T^{z} } *)
  | Grammar.TypeRecursive(z, t) ->
    labels_of_recursive_type context t
  (* { a : T } *)
  | Grammar.TypeFieldDeclaration(a, t) ->
    SetFieldDeclaration.singleton a

let is_value t = match t with
  | Grammar.TermRecursiveRecord(_) | Grammar.TermAbstraction(_) -> true
  | _ -> false

let rec labels_of_declaration decl = match decl with
  | Grammar.TermTypeDeclaration(label, _) ->
    SetFieldDeclaration.singleton label
  | Grammar.TermFieldDeclaration(label, _) ->
    SetFieldDeclaration.singleton label
  | Grammar.TermAggregateDeclaration(d, d') ->
    SetFieldDeclaration.union
      (labels_of_declaration d)
      (labels_of_declaration d')

let domain term = match term with
  | Grammar.TermRecursiveRecord(t, (z, decl)) ->
    labels_of_declaration decl
  | _ -> SetFieldDeclaration.empty

(*
let rec field_type_of_record field term = match term with
  | Grammar.TermFieldDeclaration(a, t) ->
    if String.equal field a
    then Some t
    else None
  | Grammar.TermTypeDeclaration(a, t) ->
    if String.equal field a
    then Some t
    else None
  | Grammar.TermAggregateDeclaration(d1, d2) ->
    let field_d1 = field_type_of_record field d1 in
    let field_d2 = field_type_of_record field d2 in
    match field_d1, field_d2 with
    | Some t, None | None, Some t | (Some _), Some t -> Some t
    | None, None -> None

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
    let y_as_record = record_of_variable context y in
    let field_type_of_x = field_type_of_record a y_as_record in
    (match field_type_of_x with
    | None -> raise (Error.NotWellFormed(Grammar.TermVariable(x), type_of_x))
    | Some t -> t)
  | _ -> raise (Error.NotARecordOrUnboundField(Grammar.TermVariable(x)))

*)

let is_type_intersection = function
  | Grammar.TypeIntersection (_) -> true
  | _ -> false

let is_term_intersection = function
  | Grammar.TermAggregateDeclaration(_) -> true
  | _ -> false

let remove_top_from_intersection = function
  | Grammar.TypeIntersection(Grammar.TypeTop, t) -> t
  | Grammar.TypeIntersection(t, Grammar.TypeTop) -> t
  | t -> t

(** Simplify the type. *)
let rec simplify_type t = match t with
  | Grammar.TypeIntersection(Grammar.TypeBottom, t) ->
    Grammar.TypeBottom
  (* Top can be removed. *)
  | Grammar.TypeIntersection(Grammar.TypeTop, t) ->
    simplify_type t
  | Grammar.TypeIntersection(t, Grammar.TypeTop) ->
    simplify_type t
  | Grammar.TypeIntersection(Grammar.TypeRecursive(x, type_of_x), Grammar.TypeRecursive(z, type_of_z)) ->
    let fresh_z = AlphaLib.Atom.fresh "self" in
    let type_of_x = simplify_type type_of_x in
    let type_of_z = simplify_type type_of_z in
    let type_of_x =
      Grammar.rename_typ
        (AlphaLib.Atom.Map.singleton x fresh_z)
        type_of_x
    in
    let type_of_z =
      Grammar.rename_typ
        (AlphaLib.Atom.Map.singleton z fresh_z)
        type_of_z
    in
    Grammar.TypeIntersection(type_of_x, type_of_z)
  (* We have two incompatible types like function and recursive records..*)
  | Grammar.TypeIntersection(_, _) -> Grammar.TypeBottom
  (* It's not an intersection, we only return the given type. *)
  | t -> t
