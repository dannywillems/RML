open AlphaLib
open BindingForms

(* ------------------------------------------------------ *)
(* Types of termes and types of the DOT calculus *)

type method_label = string[@opaque]

and type_label = string[@opaque]

(* Terms *)
and ('bn, 'fn) term =
  (* Variable *)
  | TermVar of 'fn
  (* {x => t^{x}} *)
  | TermRecursiveRecord of ('bn, ('bn, 'fn) decl list) abs
  (* t.m(t) *)
  | TermMethodApp of ('bn, 'fn) term * method_label * ('bn, 'fn) term
  (* | TermMethodApp of 'fn * method_label * ('bn, 'fn) term *)

(* Objects.
   TODO: Replace list with a HashMap for efficiency.
*)
and ('bn, 'fn) decl =
  (* L = T *)
  | DeclarationType of type_label * ('bn, 'fn) typ
  (* m(x : S) = t *)
  | DeclarationMethod of method_label * ('bn, 'fn) typ * ('bn, ('bn, 'fn) term) abs

(* Types *)
and ('bn, 'fn) typ =
  (* Top type : ⊤ *)
  | TypeTop
  (* Bottom type : ⟂ *)
  | TypeBottom
  (* T ∧ T *)
  | TypeIntersection of ('bn, 'fn) typ * ('bn, 'fn) typ
  (* T ∨ T *)
  | TypeUnion of ('bn, 'fn) typ * ('bn, 'fn) typ
  (* { z => T^{z} } *)
  | TypeRecursive of ('bn, ('bn, 'fn) typ) abs
  (* L : S..T *)
  | TypeTypeMember of type_label * ('bn, 'fn) typ * ('bn, 'fn) typ
  (* m(x : S) : T *)
  | TypeMethodMember of method_label * ('bn, 'fn) typ * ('bn, ('bn, 'fn) typ) abs
  (* x.L *)
  | TypePathDependent of 'fn * type_label


[@@deriving
  visitors {
    variety = "iter";
    ancestors = ["BindingForms.iter"]
  },
  visitors {
    variety = "map";
    ancestors = ["BindingForms.map"]
  },
  visitors {
    variety = "iter2";
    ancestors = ["BindingForms.iter2"]
  }
]

type raw_term = (string, string) term

type raw_typ = (string, string) typ

type nominal_term = (string, string) term

type nominal_typ = (AlphaLib.Atom.t, AlphaLib.Atom.t) typ

#include "AlphaLibMacros.cppo.ml"
__ALL
ALL(term)
ALL(typ)
