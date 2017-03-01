let rec eval_file f =
  try
    let raw_term = Parser.top_level Lexer.prog f in
    let nominal_term = Grammar.import_term AlphaLib.KitImport.empty raw_term in
    print_endline "Raw term";
    Print.raw_term raw_term;
    print_endline "\nNominal term";
    Print.raw_term (Grammar.show_term nominal_term);
    print_endline "";
    eval_file f
  with End_of_file -> ()

let () =
  let argc = Array.length Sys.argv in
  if argc > 1
  then
    let f = open_in (Array.get Sys.argv 1) in
    (eval_file (Lexing.from_channel f); close_in f)
  else
    (print_endline "You must give a file"; exit 1)
