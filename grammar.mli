open AlphaLib
open BindingForms

(* ------------------------------------------------------ *)
(* Types of termes and types of the DOT calculus *)

type method_label = string

type type_label = string

(* Terms *)
type ('bn, 'fn) term =
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
and ('bn, 'fn) term_decl =
  (* L = T *)
  | DeclarationType of type_label * 'a typ
  (* m(x : S) = t *)
  | DeclarationMethod of method_name * ('bn, 'fn) typ * ('bn, ('bn, 'fn) term) abs

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
  | TypeRecursive of 'bn * (('bn, 'fn) typ) list
  (* L : S..T *)
  | TypeTypeMember of type_label * ('bn, 'fn) typ * ('bn, 'fn) typ
  (* m(x : S) : T *)
  | TypeMethodMember of method_label * ('bn, 'fn) typ * ('bn, ('bn, 'fn) typ) abs
  (* x.L *)
  | TypePathDependent of 'fn * type_label
[@@deriving visitors {
    variety = "map";
  }]
(* ------------------------------------------------------ *)

(*
(* ------------------------------------------------------ *)
(* Some concrete representation of terms. *)

(* A raw term (resp. typ) is a term (resp. typ) where term (resp. type)
   variables are represented by string. No check is done if there are conflicts between variable names.
*)
type raw_term =
  string term

type raw_typ =
  string typ

(* A nominal term (resp. typ) represents a term (resp typ) where all variables
  are unique.
*)
type nominal_term =
  Nominal.t term

type nominal_typ =
  Nominal.t typ
*)
