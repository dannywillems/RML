exception SubtypeError of string * Grammar.nominal_typ * Grammar.nominal_typ

exception AvoidanceProblem of string * AlphaLib.Atom.t * Grammar.nominal_typ

exception TypeMismatch of string * (Grammar.nominal_typ * Grammar.nominal_typ)

let print e =
  let string_of_e = match e with
  | SubtypeError (str, _, _) -> str
  | AvoidanceProblem (str, _, _) -> str
  | e -> Printexc.to_string e
  in
  ANSITerminal.printf
    [ANSITerminal.red]
    "Error while evaluating: %s\n"
    string_of_e
