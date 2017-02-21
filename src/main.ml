let term1 : Grammar.raw_term =
  Grammar.TermVar "x"

let type1 : Grammar.raw_typ =
  Grammar.TypeUnion (Grammar.TypeBottom, Grammar.TypeTop)

let type2 : Grammar.raw_typ =
  Grammar.TypeRecursiveRecord (
    "z",
    Grammar.TypeIntersection (
      Grammar.TypeIntersection (
        Grammar.TypeMember ("T2",
                             Grammar.TypePathDependent ("z", "T"),
                             Grammar.TypePathDependent ("z", "T2")
                           ),
        Grammar.TypeMember ("T",
                             Grammar.TypePathDependent ("z", "T2"),
                             Grammar.TypePathDependent ("z", "T")
                           )
      ),
      Grammar.TypeMethodMember ("x",
                                Grammar.TypePathDependent ("z", "T"),
                                Grammar.TypePathDependent ("z", "T2")
                               )
    )
  )

let () =
  Print.print_raw_term term1;
  Print.print_raw_typ type1;
  Print.print_raw_typ type2
