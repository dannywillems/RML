module TermVariable = String

module ContextMap = Map.Make(TermVariable)

type context = Grammar.nominal_typ ContextMap.t

