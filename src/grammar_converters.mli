exception SameFieldLabels

(* val nominal_term_of_raw_term : raw_term -> nominal_term *)

val raw_typ_of_typ : ('a -> string) -> 'a Grammar.typ -> Grammar.raw_typ

val raw_term_of_term : ('a -> string) -> 'a Grammar.term -> Grammar.raw_term

val raw_term_of_nominal_term : Grammar.nominal_term -> Grammar.raw_term

val raw_typ_of_nominal_typ : Grammar.nominal_typ -> Grammar.raw_typ
