(* ------------------------------------------------- *)
(* References for arguments *)
let file_name = ref ""
let eval_opt = ref ""
let show_derivation_tree = ref false
let verbose = ref false
let use_stdlib = ref false
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

(* ------------------------------------------------- *)
(* Printing functions *)
let print_info string =
  if !verbose then (ANSITerminal.print_string [ANSITerminal.cyan] string)

let print_term_color t =
  Printf.printf
    "\x1b[32m%a\x1b[0m\n"
    (Print.Pretty.nominal_term ()) t

let print_type_color t =
  Printf.printf
    "\x1b[36m%a\x1b[0m\n"
    (Print.Pretty.nominal_typ ()) t

let print_error lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.printf
    "Syntax error %d:%d\n"
    pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let print_typing_derivation_tree history =
  if (!show_derivation_tree)
  then DerivationTree.print_typing_derivation_tree history

let print_subtyping_derivation_tree history =
  if (!show_derivation_tree)
  then DerivationTree.print_subtyping_derivation_tree history
(* ------------------------------------------------- *)

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
  print_term_color nominal_term;
  print_type_color nominal_typ;
  print_typing_derivation_tree history;
  print_endline "-----------------------";
  kit_import_env := extended_kit_import_env;
  typing_env := ContextType.add atom_x nominal_typ (!typing_env)

let check_typing lexbuf = ()

let well_formed lexbuf = ()

let read_term_file lexbuf =
  let raw_term = Parser.top_level_term Lexer.prog lexbuf in
  match raw_term with
  | Grammar.TopLevelLet(x, raw_term) ->
    read_top_level_let x raw_term
  | Grammar.TopLevelTerm(raw_term) ->
    let nominal_term =
      Grammar.import_term
      (!kit_import_env)
      raw_term
    in
    print_term_color nominal_term;
    ()

let read_type_file lexbuf = ()

let eval lexbuf = ()

let check_subtype lexbuf = ()

let typing lexbuf =
  let raw_term = Parser.top_level_term Lexer.prog lexbuf in
  match raw_term with
  | Grammar.TopLevelLet(x, raw_term) ->
    read_top_level_let x raw_term
  | Grammar.TopLevelTerm(raw_term) ->
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
    print_typing_derivation_tree history;
    Printf.printf
      "%s\n%s\n"
      (Print.string_of_nominal_term nominal_term)
      (Print.string_of_nominal_typ nominal_typ);
    ()

let rec execute action lexbuf =
  try
    action lexbuf;
    execute action lexbuf
  with
  | End_of_file -> ()
  | Parser.Error ->
    print_error lexbuf;
    exit 1
  | Error.Subtype(_) | Error.AvoidanceProblem(_) as e ->
    Error.print e;
    execute action lexbuf

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
  ("--show-derivation-tree",
   Arg.Set show_derivation_tree,
   "Show derivation tree"
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
  Arg.parse args_list print_endline "An interpreter for DSub implemented in OCaml"
(* ------------------------------------------------- *)

let stdlib_files = []

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
  | Action.Subtype -> execute check_subtype lexbuf
  | Action.Typing -> execute typing lexbuf
