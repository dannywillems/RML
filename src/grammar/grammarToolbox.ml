let ba_raw_term nominal_term =
  let ba_term = Grammar.ba_term nominal_term in
  List.rev (
    AlphaLib.Atom.Set.fold
      (fun elt l -> (AlphaLib.Atom.hint elt) :: l)
      ba_term
      []
  )

let fa_raw_term nominal_term =
  let fa_term = Grammar.fa_term nominal_term in
  List.rev (
    AlphaLib.Atom.Set.fold
      (fun elt l -> (AlphaLib.Atom.hint elt) :: l)
      fa_term
      []
  )

let remove_type_ascription t =
  match t with
  | Grammar.TermAscription(t, _) -> t
  | Grammar.TermRecursiveRecord(_, z, d) ->
    Grammar.TermRecursiveRecordUntyped(z, d)
  | _ -> t
