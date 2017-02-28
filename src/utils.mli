val field_labels_of_object :
  'a Grammar.obj list -> string list * string list

(** Check if all given field labels are disjoint *)
val has_distinct_field_labels :
  string list -> bool
