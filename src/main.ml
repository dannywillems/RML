open Grammar

let term1 : Grammar.raw_term =
  Grammar.TermVar "x"

let type1 : Grammar.raw_typ =
  Grammar.TypeUnion (Grammar.TypeBottom, Grammar.TypeTop)

let type2 : Grammar.raw_typ =
  Grammar.TypeRecursive (
    "z",
    Grammar.TypeIntersection (
      Grammar.TypeIntersection (
        Grammar.TypeTypeMember ("T2",
                             Grammar.TypePathDependent ("z", "T"),
                             Grammar.TypePathDependent ("z", "T2")
                           ),
        Grammar.TypeTypeMember ("T",
                             Grammar.TypePathDependent ("z", "T2"),
                             Grammar.TypePathDependent ("z", "T")
                           )
      ),
      Grammar.TypeMethodMember ("he",
                                Grammar.TypePathDependent ("z", "T"),
                                ("x", Grammar.TypePathDependent ("z", "T2"))
                               )
    )
  )

let () =
  let n_type1 = Grammar.import_typ AlphaLib.KitImport.empty type1 in
  let n_type2 = Grammar.import_typ AlphaLib.KitImport.empty type2 in
  Printf.printf "%a\n" AlphaLib.Atom.Set.print (Grammar.ba_typ n_type2);
  Printf.printf "%d\n" (Grammar.size_typ n_type1);
  Printf.printf "%d\n" (Grammar.size_typ n_type2)
