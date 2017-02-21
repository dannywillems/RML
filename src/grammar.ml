(** The terms.
    The type parameters are respectively the type of bounded variables name
    (which can be a string, a int, etc) and the type of free variables name.

    If a type is needed in a term (like for the method assignment), the type
    parameters for the types are the same.
*)

type ('bn, 'term) abs =
  ('bn, 'term) AlphaLib.BindingForms.abs

(* The type for label *)
type 'fn typ_label = 'fn

(* The type for term variable *)
type 'fn termvar = 'fn

(* Terms *)
type ('bn, 'fn) term =
  (* Variable *)
  | TermVar of 'fn termvar
  (* {x => t^{x}} *)
  | TermRecursiveRecord of 'fn termvar * ('bn, 'fn) obj
  (* t.m(t) *)
  | TermMethodApp of ('bn, 'fn) term * ('bn, 'fn) term
  (* L = T *)
  | TermTypeAssignment of 'bn typ_label * ('bn, 'fn) typ
  (* m(x : S) = t *)
  | TermMethodAssignment of ('bn termvar, ('bn, 'fn) term) abs * ('bn, 'fn) typ

(* Objects.
   TODO: Replace list with a HashMap for efficiency.
*)
and ('bn, 'fn) obj = ('bn, 'fn) term list

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
  | TypeRecursiveRecord of 'fn termvar * ('bn, 'fn) typ
  (* L : S..T *)
  | TypeMember of 'fn typ_label * ('bn, 'fn) typ * ('bn, 'fn) typ
  (* m(x : S) : T *)
  | TypeMethodMember of 'bn termvar * ('bn, 'fn) typ * ('bn, 'fn) typ
  (* x.L *)
  | TypePathDependent of 'bn termvar * 'bn typ_label

[@@deriving
  visitors {
    variety = "iter";
    ancestors = ["AlphaLib.BindingForms.iter"]
  },
  visitors {
    variety = "map";
    ancestors = [
      "AlphaLib.BindingForms.map"
    ]
  },
  visitors {
    variety = "endo";
    ancestors = ["AlphaLib.BindingForms.endo"]
  },
  visitors {
    variety = "reduce";
    ancestors = ["AlphaLib.BindingForms.reduce"]
  },
  visitors {
    variety = "iter2";
    ancestors = ["AlphaLib.BindingForms.iter2"]
  }
]

type nominal_term =
  (AlphaLib.Atom.t, AlphaLib.Atom.t) term

type raw_term =
  (string, string) term

type norminal_typ =
  (AlphaLib.Atom.t, AlphaLib.Atom.t) typ

type raw_typ =
  (string, string) typ
