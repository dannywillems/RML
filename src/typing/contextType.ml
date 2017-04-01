(*
  The context contains a list a couple (x, T) where x is a term variable and T a
  type.
*)
module TermVariable = AlphaLib.Atom

(* The identifier ContextModule is used to avoid to change the name if we change
  the representation of a context.
*)
module ContextModule = Map.Make(TermVariable)

type key = AlphaLib.Atom.t

type t = Grammar.nominal_typ

(* The type of a context *)
type context = t ContextModule.t

exception NotInEnvironment of key * context

(* ------------------------------------------------- *)
(*
  The usual operations on contexts like create an empty one, add, check if it's
  empty.
*)

let empty () =
  ContextModule.empty

let add x n context =
  ContextModule.add x n context

let is_empty context =
  ContextModule.is_empty context

let find x context =
  try
    ContextModule.find x context
  with
  | Not_found -> raise (NotInEnvironment (x, context))

let string_of_context context =
  let s = ref "" in
  ContextModule.iter
    (fun k v ->
       s := (!s) ^ (AlphaLib.Atom.show k) ^ " : " ^ (Print.string_of_raw_typ (Grammar.show_typ v)) ^ ", "
    )
    context;
  (!s ^ "∅")

module Style = struct
  let string_of_context style context =
    ANSITerminal.sprintf
      style
      "%s"
      (string_of_context context)
end
(* ------------------------------------------------- *)
