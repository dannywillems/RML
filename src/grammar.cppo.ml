open AlphaLib
open BindingForms

(* ------------------------------------------------------ *)
(* Types of termes and types of the DOT calculus *)

type field_label = string[@opaque]

and type_label = string[@opaque]

and ('bn, 'fn) term =
  (* x *)
  | TermVariable of 'fn
  (* λ(x : S) t --> (S, (x, t)) *)
  | TermAbstraction of
      ('bn, 'fn) typ * ('bn, ('bn, 'fn) term) abs
  (* x y *)
  | TermVarApplication of 'fn * 'fn
  (* let x = t in u --> (t, (x, u))*)
  | TermLet of ('bn, 'fn) term * ('bn, ('bn, 'fn) term) abs

  (* ----- Unofficial terms ----- *)
  (* t : T *)
  | TermAscription of ('bn, 'fn) term * ('bn, 'fn) typ
  (* Add the unimplemented term allows to define terms without given their implementation. Useful for testing. *)
  | TermUnimplemented

  (* ----- Beginning of DOT terms ----- *)
  (* {x => t^{x}} *)
  | TermRecursiveRecord of ('bn, ('bn, 'fn) decl) abs
  (* x.a *)
  | TermFieldSelection of 'fn * string

(* Objects. Type tag becomes DeclarationType (instead of TermTypeTag) *)
and ('bn, 'fn) decl =
  (* L = T *)
  | DeclarationType of type_label * ('bn, 'fn) typ
  (* a = t *)
  | DeclarationField of field_label * ('bn, 'fn) term
  (* d ∧ d' *)
  | DeclarationAggregate of ('bn, 'fn) decl * ('bn, 'fn) decl

and ('bn, 'fn) typ =
  (* Top type : ⊤ *)
  | TypeTop
  (* Bottom type : ⟂ *)
  | TypeBottom
  (* { L : S..T } --> (L, S, T) *)
  | TypeDeclaration of type_label * ('bn, 'fn) typ * ('bn, 'fn) typ
  (* x.L *)
  | TypeProjection of 'fn * type_label
  (* ∀(x : S) T --> (S, (x, T)) *)
  | TypeDependentFunction of
      ('bn, 'fn) typ *
      ('bn, ('bn, 'fn) typ) abs
  (* ----- Beginning of DOT types ----- *)
  (* T ∧ T *)
  | TypeIntersection of ('bn, 'fn) typ * ('bn, 'fn) typ
  (* { z => T^{z} } *)
  | TypeRecursive of ('bn, ('bn, 'fn) typ) abs
  (* { a : T } *)
  | TypeFieldDeclaration of field_label * ('bn, 'fn) typ

and ('bn, 'fn) top_level =
  | TopLevelLet of 'fn * ('bn, 'fn) term
  | TopLevelTerm of ('bn, 'fn) term

[@@deriving
  visitors {
    variety = "map" ;
    ancestors = ["BindingForms.map"]
  },
  visitors {
    variety = "iter" ;
    ancestors = ["BindingForms.iter"]
  },
  visitors {
    variety = "iter2" ;
    ancestors = ["BindingForms.iter2"]
  },
  visitors {
    variety = "endo" ;
    ancestors = ["BindingForms.endo"]
  }
]


type raw_term = (string, string) term
type raw_typ = (string, string) typ
type raw_decl = (string, string) decl
type raw_top_level = (string, string) top_level

type nominal_term = (Atom.t, Atom.t) term
type nominal_typ = (Atom.t, Atom.t) typ
type nominal_decl = (Atom.t, Atom.t) decl
type nominal_top_level = (Atom.t, Atom.t) top_level

#include "AlphaLibMacros.cppo.ml"

__ALL
ALL(term)
ALL(decl)
ALL(typ)
ALL(top_level)
