exception Subtype of DerivationTree.subtyping_node DerivationTree.t * Grammar.nominal_typ * Grammar.nominal_typ

exception AvoidanceProblem of string * ContextType.context * AlphaLib.Atom.t * Grammar.nominal_typ

exception TypeMismatch of string * (Grammar.nominal_typ * Grammar.nominal_typ)

exception AggregationIntersectionNotEmpty of
    string * Grammar.nominal_decl * Grammar.nominal_decl

exception NotADependentFunction of Grammar.nominal_typ

exception MultipleDefinitionOfField of
    string * Grammar.nominal_typ * Grammar.nominal_typ

exception NotWellFormed of
    ContextType.context * Grammar.nominal_typ

exception NotARecord of Grammar.nominal_term

exception NotARecordOrUnboundField of
    AlphaLib.Atom.t * Grammar.nominal_typ * string

let raise_not_a_record term =
  raise (NotARecord term)

let raise_not_well_formed context typ =
  raise (NotWellFormed (context, typ))

let raise_subtype history s t =
  DerivationTree.print_subtyping_derivation_tree history;
  raise (Subtype (history, s, t))

let raise_aggregate_intersection_not_empty d1 d2 =
  raise (
    AggregationIntersectionNotEmpty(
      "When defining an aggregation, the domains must be disjoint.",
      d1,
      d2
    ))

let raise_avoidance_problem rule context atom s =
  raise (AvoidanceProblem(rule, context, atom, s))

let raise_type_mismatch term s t =
  raise
    (TypeMismatch (
        Printf.sprintf
          "ALL-E: %s must be a subtype of %s but it's of type %s."
          (Print.string_of_nominal_term term)
          (Print.string_of_raw_typ (Grammar.show_typ s))
          (Print.string_of_raw_typ (Grammar.show_typ t)),
        (s, t)
      )
    )

let raise_not_a_dependent_function typ =
  raise (NotADependentFunction(typ))

let raise_not_a_record_or_unbound_field x type_of_x a =
  raise (NotARecordOrUnboundField(x, type_of_x, a))

let print e = match e with
  | Subtype(history, s, t) ->
    Printf.printf
      "\x1b[32m%a\x1b[0m is not a subtype of \x1b[32m%a\x1b[0m\n"
      (Print.Pretty.nominal_typ ()) s
      (Print.Pretty.nominal_typ ()) t;
  | AvoidanceProblem(rule, context, atom, typ) ->
    Printf.printf
      "Avoidance Problem (rule %s): %s appears in %s.\n"
      rule
      (AlphaLib.Atom.show atom)
      (Print.string_of_nominal_typ typ)
  | ContextType.NotInEnvironment(key, context) ->
    Printf.printf
      "The key %s is not in context :\n%a\n"
      (ANSITerminal.sprintf [ANSITerminal.blue] "%s" (AlphaLib.Atom.show key))
      ContextType.Pretty.print context;
  | NotWellFormed(context, typ) ->
    Printf.printf
      "%a is not well formed.\n"
      (Print.Pretty.nominal_typ ()) typ;
  | NotADependentFunction(typ) ->
    Printf.printf
      "%a is not a dependent function.\n"
      (Print.Pretty.nominal_typ ()) typ
  | TypeMismatch(s, _) -> print_endline s
  | NotARecordOrUnboundField(var, type_of_var, field) ->
    Printf.printf
      "%s is not a field of the variable %s which is of type %s"
      field
      (AlphaLib.Atom.show var)
      (Print.string_of_nominal_typ type_of_var)
  | _ -> print_endline (Printexc.to_string e)
