exception Subtype of string * Grammar.nominal_typ * Grammar.nominal_typ

exception AvoidanceProblem of string * AlphaLib.Atom.t * Grammar.nominal_typ

exception TypeMismatch of string * (Grammar.nominal_typ * Grammar.nominal_typ)

exception AggregationIntersectionNotEmpty of
    string * Grammar.nominal_decl * Grammar.nominal_decl

exception NotADependentFunction of Grammar.nominal_typ

exception MultipleDefinitionOfField of
    string * Grammar.nominal_typ * Grammar.nominal_typ

exception NotWellFormed of
    ContextType.context * Grammar.nominal_typ

exception NotARecord of Grammar.nominal_term

exception NotARecordOrUnboundField of Grammar.nominal_term * string

let raise_not_a_record term =
  raise (NotARecord term)

let raise_not_well_formed context typ =
  raise (NotWellFormed (context, typ))

let raise_subtype s t =
  let str =
    Printf.sprintf
      "%s is not a subtype of %s"
      (Print.string_of_nominal_typ s)
      (Print.string_of_nominal_typ t)
  in
  raise (Subtype (str, s, t))

let raise_aggregate_intersection_not_empty d1 d2 =
  raise (
    AggregationIntersectionNotEmpty(
      "When defining an aggregation, the domains must be disjoint.",
      d1,
      d2
    ))

let raise_avoidance_problem atom s =
  raise (AvoidanceProblem(
    (Printf.sprintf
       "%s appears in %s."
       (AlphaLib.Atom.show atom)
       (Print.string_of_nominal_typ s)
    ),
    atom,
    s
  ))

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

let print e = match e with
  | Subtype(str, s, t) ->
    Printf.printf
      "\x1b[32m%a\x1b[0m is not a subtype of \x1b[32m%a\x1b[0m"
      (Print.Pretty.nominal_typ ()) s
      (Print.Pretty.nominal_typ ()) t;
  | AvoidanceProblem(str, atom, typ) ->
    print_endline str;
  | ContextType.NotInEnvironment(key, context) ->
    Printf.printf
      "The key %s is not in context :\n%a\n"
      (ANSITerminal.sprintf [ANSITerminal.blue] "%s" (AlphaLib.Atom.show key))
      ContextType.Pretty.print context;
  | NotWellFormed(context, typ) ->
    Printf.printf
      "%a is not well formed.\n"
      (Print.Pretty.nominal_typ ()) typ;
  | _ -> print_endline (Printexc.to_string e)
