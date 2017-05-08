(* ------------------------------------------------- *)
(* References for arguments *)
let file_name = ref ""
let current_file = ref ""
let eval_opt = ref ""
let verbose = ref false
let use_stdlib = ref false

let show_derivation_tree = ref false
let print_context_in_derivation_tree = ref true
let check_well_formed_ref = ref false
let pretty_print = ref false
(* ------------------------------------------------- *)

(* ------------------------------------------------- *)
(* Style using ANSITerminal *)
let error_style = [ANSITerminal.red]
let success_style = [ANSITerminal.green]
(* ------------------------------------------------- *)

(* ------------------------------------------------- *)
(* References used for environments *)
(* The environment to convert raw terms/types to nominal terms/types.
   Due to the design choice of the eval loop, we use a reference.
*)
let kit_import_env : AlphaLib.KitImport.env ref = ref AlphaLib.KitImport.empty

(* The environment for typing and subtyping algorithms.
   Due to the design choice of the eval loop, we use a reference.
*)
let typing_env = ref (ContextType.empty ())
(* ------------------------------------------------- *)

let reset_default_config () =
    show_derivation_tree := false;
    print_context_in_derivation_tree := true;
    check_well_formed_ref := false;
    pretty_print := false

let parse_annotation annotation = match annotation with
  | "show_derivation_tree" ->
    show_derivation_tree := true
  | "no_context" ->
    print_context_in_derivation_tree := false
  | "check_well_formed" ->
    check_well_formed_ref := true
  | "pretty_print" ->
    pretty_print := true
  | _ -> ()

let parse_annotation_list l =
  reset_default_config ();
  List.iter parse_annotation l
(* ------------------------------------------------- *)

(* ------------------------------------------------- *)
(* Printing functions *)
let print_info string =
  if !verbose then (ANSITerminal.print_string [ANSITerminal.cyan] string)

let print_term_color t =
  Printf.printf
    "\x1b[32m%a\x1b[0m\n"
    (Print.Pretty.nominal_term ()) t

let print_term_color_if_verbose t =
  if (!verbose) then print_term_color t

let print_variable_color x =
  ANSITerminal.printf
    [ANSITerminal.green]
    "%s"
    x

let print_variable_color_if_verbose x =
  if (!verbose) then print_variable_color x

let print_type_color t =
  Printf.printf
    "\x1b[36m%a\x1b[0m\n"
    (Print.Pretty.nominal_typ ()) t

let print_type_color_if_verbose t =
  if (!verbose) then print_type_color t

let print_endline_if_verbose s =
  if (!verbose) then print_endline s

let print_error lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.printf
    "Syntax error %d:%d\n"
    pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let print_typing_derivation_tree history =
  if (!show_derivation_tree)
  then (
    DerivationTree.print_typing_derivation_tree
      ~print_context:(!print_context_in_derivation_tree)
      history
  )

let print_subtyping_derivation_tree history =
  if (!show_derivation_tree)
  then (
    DerivationTree.print_subtyping_derivation_tree
      ~print_context:(!print_context_in_derivation_tree)
      history
  )

(** [print_is_subtyppe s t raw_is_subtype is_subtype] *)
let print_is_subtype s t raw_is_subtype is_subtype =
  ANSITerminal.printf
    (if raw_is_subtype = is_subtype then success_style else error_style)
    "%s - %s is%s a subtype of %s\n"
    (if raw_is_subtype = is_subtype then "✓" else "❌")
    (Print.string_of_nominal_typ s)
    (if raw_is_subtype then "" else " not")
    (Print.string_of_nominal_typ t);
  if raw_is_subtype <> is_subtype then exit(1)

(* ------------------------------------------------- *)
(* Utils *)
let check_well_formed context typ =
  if (!check_well_formed_ref)
  then CheckUtils.check_well_formedness context typ

let read_top_level_let x raw_term =
  (* Convert raw term/type to nominal term/type using the import environment. *)
  let nominal_term =
    Grammar.import_term
      (!kit_import_env)
      raw_term
  in
  let history, nominal_typ =
    Typer.type_of
      ~context:(!typing_env)
      nominal_term
  in
  let extended_kit_import_env, atom_x =
    AlphaLib.KitImport.extend
      (!kit_import_env)
      x
  in
  print_variable_color_if_verbose x;
  print_endline_if_verbose " : ";
  print_type_color_if_verbose nominal_typ;
  print_typing_derivation_tree history;
  print_endline_if_verbose "-----------------------";
  kit_import_env := extended_kit_import_env;
  typing_env := ContextType.add atom_x nominal_typ (!typing_env)
(* ------------------------------------------------- *)

(* ------------------------------------------------- *)
(* Functions for actions *)
let eval lexbuf = ()

(** Action to check the subtype algorithm (with or without REFL). It uses the
    syntax S <: T or S !<: T and automatically check if the answer is the same than
    we want.
*)
let check_subtype lexbuf =
  let (raw_is_subtype, raw_couple, annotation) = Parser.top_level_subtype Lexer.prog lexbuf in
  parse_annotation_list annotation;
  match raw_couple with
  | Grammar.CoupleTypes(raw_s, raw_t) ->
    let nominal_s = Grammar.import_typ (!kit_import_env) raw_s in
    let nominal_t = Grammar.import_typ (!kit_import_env) raw_t in
    check_well_formed (!typing_env) nominal_s;
    check_well_formed (!typing_env) nominal_t;
    let history, is_subtype =
      Subtype.subtype ~context:(!typing_env) nominal_s nominal_t
    in
    print_subtyping_derivation_tree history;
    print_is_subtype nominal_s nominal_t raw_is_subtype is_subtype;
    print_endline "-------------------------"
  | Grammar.CoupleTerms(raw_s, raw_t) ->
    let nominal_s = Grammar.import_term (!kit_import_env) raw_s in
    let nominal_t = Grammar.import_term (!kit_import_env) raw_t in
    let history_s, type_of_s = Typer.type_of ~context:(!typing_env) nominal_s in
    let history_t, type_of_t = Typer.type_of ~context:(!typing_env) nominal_t in
    print_typing_derivation_tree history_s;
    print_typing_derivation_tree history_t;
    check_well_formed (!typing_env) type_of_s;
    check_well_formed (!typing_env) type_of_t;
    let history, is_subtype =
      Subtype.subtype ~context:(!typing_env) type_of_s type_of_t
    in
    print_subtyping_derivation_tree history;
    print_is_subtype type_of_s type_of_t raw_is_subtype is_subtype;
    print_endline "-------------------------"
  | Grammar.TopLevelLetSubtype (var, raw_term) ->
    read_top_level_let var raw_term

let typing lexbuf =
  let raw_term, annotation = Parser.top_level_term Lexer.prog lexbuf in
  parse_annotation_list annotation;
  match raw_term with
  | Grammar.TopLevelLetTerm(x, raw_term) ->
    read_top_level_let x raw_term
  | Grammar.Term(raw_term) ->
    let nominal_term =
      Grammar.import_term
      (!kit_import_env)
      raw_term
    in
    let history, nominal_typ =
      Typer.type_of
        ~context:(!typing_env)
        nominal_term
    in
    check_well_formed (!typing_env) nominal_typ;
    print_typing_derivation_tree history;
    print_term_color nominal_term;
    print_type_color nominal_typ

let rec execute action lexbuf =
  try
    action lexbuf;
    execute action lexbuf
  with
  | End_of_file -> ()
  | Parser.Error ->
    print_endline (!current_file);
    print_error lexbuf;
    exit 1
  | _ as e ->
    let pos = lexbuf.Lexing.lex_curr_p in
    Printf.printf
      "In file %s %d:%d : \n  "
      (!current_file)
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1);
    Error.print (!show_derivation_tree) (!print_context_in_derivation_tree) e;
    exit 1

(* ------------------------------------------------- *)
(* Arguments *)
let args_list = [
  ("-f",
   Arg.Set_string file_name,
   "File to read"
  );
  ("-a",
   Arg.Symbol (Action.available, (fun s -> eval_opt := s)),
   "The action to do"
  );
  ("--use-stdlib",
   Arg.Set use_stdlib,
   "Use standard library."
  );
  ("-v",
   Arg.Set verbose,
   "Verbose mode"
  )
]

let () =
  Arg.parse args_list print_endline "An interpreter for RML implemented in OCaml"
(* ------------------------------------------------- *)

let stdlib_files = [
  "stdlib/unit.rml";
  "stdlib/bool.rml";
  "stdlib/condition.rml";
  "stdlib/int.rml";
  "stdlib/float.rml";
  "stdlib/char.rml";
  "stdlib/string.rml";
  "stdlib/option.rml";
  "stdlib/option_church.rml";
  "stdlib/sum_church.rml";
  "stdlib/comparable.rml";
  "stdlib/list.rml";
  "stdlib/list_with.rml";
  "stdlib/pervasives.rml";
  "stdlib/point.rml";
  "stdlib/pair.rml";
  "stdlib/pair_with.rml";
  "stdlib/map.rml";
  "stdlib/graph.rml";
]

let rec add_in_environment files = match files with
  | [] -> ()
  | head :: tail ->
    current_file := head;
    let channel = open_in head in
    let lexbuf = Lexing.from_channel channel in
    print_info (
      Printf.sprintf
        "  File: %s\n"
        head
    );
    execute typing lexbuf;
    close_in channel;
    add_in_environment tail

let () =
  let lexbuf = Lexing.from_channel (open_in (!file_name)) in
  if (!use_stdlib)
  then (
    print_info "Loading definitions from standard library.\n";
    add_in_environment stdlib_files;
    print_info "\nStandard library loaded.\n";
    print_info "-------------------------\n\n"
  );
  current_file := (!file_name);
  match (Action.t_of_string (!eval_opt)) with
  | Action.Eval -> execute eval lexbuf
  | Action.Subtype -> execute check_subtype lexbuf
  | Action.Typing -> execute typing lexbuf
