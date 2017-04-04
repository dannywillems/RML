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
  (* ν(x : T) d *)
  | TermRecursiveRecord of ('bn, 'fn) typ * ('bn, ('bn, 'fn) decl) abs
  (* Unofficiql -- ν(x) d *)
  | TermRecursiveRecordUntyped of ('bn, ('bn, 'fn) decl) abs
  (* x.a *)
  | TermFieldSelection of 'fn * string

(* Objects. Type tag becomes DeclarationType (instead of TermTypeTag) *)
and ('bn, 'fn) decl =
  (* L = T *)
  | TermTypeDeclaration of type_label * ('bn, 'fn) typ
  (* a = t *)
  | TermFieldDeclaration of field_label * ('bn, 'fn) term
  (* d ∧ d' *)
  | TermAggregateDeclaration of ('bn, 'fn) decl * ('bn, 'fn) decl

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

(* --------------------------------------------------------------- *)
(* ----- Allow to use the syntax [let x : T = t] on the top level. *)
(* The top level let bindings are separated from the terms. A top level term is
   either a usual term or a top level term.
*)
type ('bn, 'fn) top_level_term =
  | Term of ('bn, 'fn) term
  (* let x : T = t -> Top level definition. Must never appear in a term *)
  | TopLevelLetTerm of 'fn * ('bn, 'fn) term

(* We also allow to use top level let expressions in a file using only types
   (for example sub-typing or well formed algorithms).
*)
type ('bn, 'fn) top_level_typ =
  | Type of ('bn, 'fn) typ
  (* let x : T = t -> Top level definition. Must never appear in a term *)
  | TopLevelLetType of 'fn * ('bn, 'fn) term

type ('bn, 'fn) top_level_subtype =
  | CoupleTypes of ('bn, 'fn) typ * ('bn, 'fn) typ
  | TopLevelLetSubtype of 'fn * ('bn, 'fn) term

type raw_top_level_subtype = (string, string) top_level_subtype
type raw_top_level_typ = (string, string) top_level_typ
type raw_top_level_term = (string, string) top_level_term
(* --------------------------------------------------------------- *)

#include "AlphaLibMacros.cppo.ml"

__ALL
ALL(term)
ALL(decl)
ALL(typ)
ALL(top_level)
