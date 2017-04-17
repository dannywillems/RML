(* ------------------------------------------------- *)
(* References for arguments *)
let file_name = ref ""
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
    (Print.string_of_raw_typ s)
    (if raw_is_subtype then "" else " not")
    (Print.string_of_raw_typ t);
  if raw_is_subtype <> is_subtype then exit(1)

let print_is_subtype_algorithms
    raw_s raw_t is_subtype_with_refl is_subtype_without_refl raw_is_subtype
  =
  Printf.printf
    "%s <: %s\n"
    (Print.string_of_raw_typ raw_s)
    (Print.string_of_raw_typ raw_t);
  ANSITerminal.printf
    (if is_subtype_without_refl = raw_is_subtype then success_style else error_style)
    "    %s %s\n"
    (if is_subtype_without_refl = raw_is_subtype then "✓" else "❌")
    "Without REFL";
  ANSITerminal.printf
    (if is_subtype_with_refl = raw_is_subtype then success_style else error_style)
    "    %s %s\n"
    (if is_subtype_with_refl = raw_is_subtype then "✓" else "❌")
    "With REFL"

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
  print_endline " : ";
  print_type_color_if_verbose nominal_typ;
  print_typing_derivation_tree history;
  print_endline_if_verbose "-----------------------";
  kit_import_env := extended_kit_import_env;
  typing_env := ContextType.add atom_x nominal_typ (!typing_env)
(* ------------------------------------------------- *)

(* ------------------------------------------------- *)
(* Functions for actions *)
let check_typing lexbuf = ()

let well_formed lexbuf = ()

let read_term_file lexbuf =
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
    print_term_color nominal_term;
    ()

let read_type_file lexbuf = ()

let eval lexbuf = ()

(** Action to check the subtype algorithm (with or without REFL). It uses the
    syntax S <: T or S !<: T and automatically check if the answer is the same than
    we want.
*)
let check_subtype ~with_refl lexbuf =
  let (raw_is_subtype, raw_couple, annotation) = Parser.top_level_subtype Lexer.prog lexbuf in
  parse_annotation_list annotation;
  match raw_couple with
  | Grammar.CoupleTypes(raw_s, raw_t) ->
    let nominal_s = Grammar.import_typ (!kit_import_env) raw_s in
    let nominal_t = Grammar.import_typ (!kit_import_env) raw_t in
    check_well_formed (!typing_env) nominal_s;
    check_well_formed (!typing_env) nominal_t;
    let history, is_subtype =
      Subtype.subtype ~with_refl ~context:(!typing_env) nominal_s nominal_t
    in
    print_subtyping_derivation_tree history;
    print_is_subtype raw_s raw_t raw_is_subtype is_subtype;
    print_endline "-------------------------"
  | Grammar.TopLevelLetSubtype (var, raw_term) ->
    read_top_level_let var raw_term

let check_subtype_algorithms lexbuf =
  let (raw_is_subtype, raw_couples, annotation) = Parser.top_level_subtype Lexer.prog lexbuf in
  parse_annotation_list annotation;
  match raw_couples with
  | Grammar.CoupleTypes(raw_s, raw_t) ->
    let nominal_s = Grammar.import_typ (!kit_import_env) raw_s in
    let nominal_t = Grammar.import_typ (!kit_import_env) raw_t in
    check_well_formed (!typing_env) nominal_s;
    check_well_formed (!typing_env) nominal_t;
    let history_with_refl, is_subtype_with_refl =
      Subtype.subtype ~with_refl:true ~context:(!typing_env) nominal_s nominal_t
    in
    let history_without_refl, is_subtype_without_refl =
      Subtype.subtype ~with_refl:false ~context:(!typing_env) nominal_s nominal_t
    in
    print_subtyping_derivation_tree history_with_refl;
    print_subtyping_derivation_tree history_without_refl;
    print_is_subtype_algorithms
      raw_s raw_t is_subtype_with_refl is_subtype_without_refl raw_is_subtype;
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
    print_error lexbuf;
    exit 1
  | _ as e ->
    let pos = lexbuf.Lexing.lex_curr_p in
    Printf.printf
      "In file %s %d:%d : "
      (!file_name)
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1);
    Error.print e;
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
  "stdlib/int.rml";
  "stdlib/float.rml";
  "stdlib/char.rml";
  "stdlib/string.rml";
  "stdlib/option.rml";
  "stdlib/list.rml";
  "stdlib/condition.rml";
  "stdlib/pervasives.rml";
  "stdlib/point.rml";
]

let rec add_in_environment files = match files with
  | [] -> ()
  | head :: tail ->
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
  match (Action.t_of_string (!eval_opt)) with
  | Action.Check_typing -> execute check_typing lexbuf
  | Action.WellFormed -> execute well_formed lexbuf
  | Action.Read_term -> execute read_term_file lexbuf
  | Action.Read_type -> execute read_type_file lexbuf
  | Action.Eval -> execute eval lexbuf
  | Action.Subtype -> execute (check_subtype ~with_refl:false) lexbuf
  | Action.Subtype_with_REFL -> execute (check_subtype ~with_refl:true) lexbuf
  | Action.Subtype_same_output ->
    ANSITerminal.printf
      [ANSITerminal.red]
      "ERROR: Without REFL algorithm not implemented.\n"
    (*
    execute check_subtype_algorithms lexbuf
    *)
  | Action.Typing -> execute typing lexbuf
