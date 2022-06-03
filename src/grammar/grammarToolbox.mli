val ba_raw_term : Grammar.nominal_term -> string list

val fa_raw_term : Grammar.nominal_term -> string list

val string_of_term_for_fresh_variable : Grammar.raw_term -> string

val is_raw_variable : Grammar.raw_term -> bool

val extract_variable : Grammar.raw_term -> string option
