module SetFieldDeclaration = Set.Make(String)

type direction =
  | Upper
  | Lower

let rec best_bound_of_recursive_type ~direction ~label context t = match t with
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
  (* { L : S..T } --> (L, S, T) *)
  (* The type of the given variable is a module. *)
  | Grammar.TypeDeclaration(l, s, t) ->
    if String.equal l label then
      match direction with
      | Lower -> Some s
      | Upper -> Some t
    else None
  (* x.L *)
  (* Else, it's a path selection type. *)
  | Grammar.TypeProjection(x, label_selected) ->
    let type_of_x = ContextType.find x context in
    (* Recursive call to the algorithm. It is supposed to return the greatest
       lower bound (resp. the least upper bound) of x wrt the label given by
       [label_selected].
       [u'] is the best bound for the type of [x].
    *)
    let u' =
      best_bound_of_recursive_type
        ~direction
        ~label:label_selected
        context
        type_of_x
    in
    (match u' with
    | Some u' -> best_bound_of_recursive_type ~direction ~label context u'
    | None -> None
    )
  (* ----- Beginning of DOT types ----- *)
  (* { z => T^{z} } *)
  | Grammar.TypeRecursive(z, t) ->
    best_bound_of_recursive_type ~direction ~label context t
  (* T ∧ T *)
  | Grammar.TypeIntersection(t1, t2) ->
    let best_bound_for_t1 =
      best_bound_of_recursive_type ~direction ~label context t1
    in
    let best_bound_for_t2 =
      best_bound_of_recursive_type ~direction ~label context t1
    in
    (match (best_bound_for_t1, best_bound_for_t2) with
    | (None, None) -> None
    | (Some t, None) | (None, Some t) -> Some t
    (* If a same field is defined two times, we take the last definition, i.e.
       in the type in the right *)
    | (Some _, Some t) -> Some t)
  (* { a : T } *)
  | Grammar.TypeFieldDeclaration(a, t) ->
    if String.equal a label
    then Some t
    else None

let least_upper_bound_of_recursive_type ~label context t =
  best_bound_of_recursive_type ~direction:Upper ~label context t

let greatest_lower_bound_of_recursive_type ~label context t =
  best_bound_of_recursive_type ~direction:Lower ~label context t

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
      least_upper_bound_of_recursive_type ~label context type_of_x
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
      least_upper_bound_of_recursive_type ~label:l context type_of_x
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
  | _ -> raise (Error.NotARecord(Grammar.TermVariable(x)))

*)

let is_type_intersection = function
  | Grammar.TypeIntersection (_) -> true
  | _ -> false

let is_term_intersection = function
  | Grammar.TermAggregateDeclaration(_) -> true
  | _ -> false
