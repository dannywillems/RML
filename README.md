RML - ML modules and functors as first-class citizens.
===

RML mixes ML records and ML modules in a single type called recursive records,
given the particularity that ML modules and ML functors are first-class citizens
i.e. can be considered like any other terms. RML is the combination of the
simple lambda-calculus with subtyping and parametric polymorphism, recursive
type, record and dependent type.

**The implementation is entirely written in OCaml and is focused on typing and subtyping algorithms, not evaluation. For this reason, for example, a term `Unimplemented` of type `Bottom` is defined and often used to leave out the implementation, the meaning of the term. Only types are important. RML is simply a proof-of-concept which shows how ML modules can be manipulated as first-class citizens in OCaml.**

If you want to contribute,
see [GitHub issues](https://github.com/dannywillems/RML/issues).

Any suggestion, advice, bug report, test, etc is appreciated.

## Expressions syntax

Every expression ends with two semicolons.
Here the list of examples. Lines with `>` are the output of the typing algorithm. New expressions are separated by a new empty line.

Examples:
```OCaml
(* File test/MISC/expression.rml.
   [let x = 42;;] is the expression and the two following lines are the output
   of the typing algorithm.
*)
let x = 42;;
> x : 
Int.t

let f = fun(x : Int.t) -> x;;
> f : 
∀(x : Int.t) Int.t

let f = fun(y : Int.t) -> let x = y in x;;
> f :
∀(x : Int.t) Int.t
```

#### Comments

Comments can be defined with `(* *)` like in ML.

```OCaml
(* File test/MISC/comment.rml *)
(* This is a comment *)
(* You can also have comments in comments (* like this and (* like this *) *) *)
let x = (* It's also allowed in expressions *) 42;;
> x :
Int.t
```

#### Let binding

You can define top level let binding with the expression `let var = expr`.

Examples:
```OCaml
(* File test/MISC/let_binding.rml *)
let x = 42;;
> x : 
Int.t

let f = fun(x : Int.t) -> x;;
> f : 
∀(x : Int.t) Int.t

let f = fun(y : Int.t) -> let x = y in x;;
> f : 
∀(y : Int.t) Int.t
```

#### Local bindings

As in ML, you can define local bindings with the syntax `let ... = ... in ...`.

Examples:
```OCaml
(* File test/MISC/local_binding.rml *)
let x = 42 in x;;
> let  x = 42 : Int.t in
x :
Int.t

let f = fun(x : Int.t) -> x in f 42;;
> let  f = fun (x : Int.t) -> x in
let  'ASC_UN-INT_ = 42 : Int.t in
f 'ASC_UN-INT_ :
Int.t

let x = 42 in let y = x in y;;
> let  x = 42 : Int.t in
let  y = x in
y :
Int.t
```

The second example shows how syntactic sugars are implemented and how RML
desugars it. The low level calculus which RML is based on doesn't accept terms
in applications but only variables. RML comes with syntactic sugars for terms in
applications. A simple local let binding is done with an unique variable name.
No local binding is done for variable due to the avoidance problem when we have
a variable for a module (see below).

**You need to be careful about the avoidance problem when using local bindings.**

```OCaml
(* File test/MISC/local_binding_error.rml *)
let module M = struct
  type t = Int.t
  let x : self.t = 42
end in M.x;;
> In file test/MISC/local_binding_error.rml 5:13 : 
  Avoidance Problem (rule LET): M/436 appears in M/436.t.
```
is a good example of the avoidance problem because `M.x` is of type `M.t` and
the entire expression is of type `M.t` but the module `M` doesn't exist outside
the let binding, so it's not well-typed.

#### Functions.

Parameters in the functions are annotated with the type. The keyword `fun` is used and a
simple right arrow `->` is used to define the body of the function.
A function can be curryfied and take multiple parameters by using a comma. But, it
remains a syntactic sugar for functions returning functions like in many
functional programming languages.

RML also focuses on dependent types and dependent functions. It means parameters
can use the parameters defined before.

```OCaml
(* File test/MISC/function.rml *)
let f = fun(x : Int.t) -> x;;
> f : 
∀(x : Int.t) Int.t

let g = fun(x : Int.t, y : String.t) -> y;;
> g : 
∀(x : Int.t) ∀(y : String.t) String.t

let h = fun(x : Int.t, f : Int.t -> Int.t) -> f x;;
> h : 
∀(x : Int.t) ∀(f : Int.t -> Int.t) Int.t

(* Is equivalent to *)
let h = fun(x : Int.t) -> fun(f : Int.t -> Int.t) -> f x;;
> h : 
∀(x : Int.t) ∀(f : Int.t -> Int.t) Int.t

(* Here an example of a dependent function. the two last parameters depend on
   the first parameter which is a module.
*)
let h' = fun(m : sig type t val add : self.t -> self.t -> Int.t end, x : m.t, y : m.t) -> m.add x y;;
> ∀(m : self => sig
type t = ⟂ .. ⊤
  val add : self.t -> self.t -> Int.t
end) ∀(x : m.t) ∀(y : m.t) Int.t
```

#### Modules and functors.

The most important thing in RML is modules (and in the same time functors) are
first class citizens. It means you can manipulate modules and functors like any
other values. You have a good example in the last example about functions.

A module is a term containing a list of declarations which can be recursive (i.e. declarations can be mutually dependent) through a variable surrounding by the keywords `struct` and `end`.
A declaration is a type declaration (using `type`) or a field declaration (`let`).

```OCaml
(* File test/MISC/modules_functors.rml *)
(* Define a simple module with a top level let binding. The module name begins
   with an uppercase letter and can contain number and underscore.
*)
let module Point2D = struct
  type t = { x : Int.t ; y : Int.t }
  let add = fun(p1 : self.t, p2 : self.t) ->
    let x' = Int.plus p1.x p2.x in
    let y' = Int.plus p1.y p2.y in
    { x = x' ; y = y' }
end;;
> Point2D :
self => sig
type t = 'self => sig
  val x : Int.t
    val y : Int.t
  end .. 'self => sig
  val x : Int.t
    val y : Int.t
  end
  val add : ∀(p : self.t) ∀(p : self.t) 'self => sig
  val x : Int.t
    val y : Int.t
  end
end

(* In the previous example, the internal variable used to refer to another field
    is `self` which is the default variable name. Another name can be used as in
    the following example.
*)
(* Instead of self, the variable point is used. *)
let module Point2D = struct(point)
  type t = { x : Int.t ; y : Int.t }
  let add = fun(p1 : point.t, p2 : point.t) ->
    let x' = Int.plus p1.x p2.x in
    let y' = Int.plus p1.y p2.y in
    { x = x' ; y = y' }
end;;
> Point2D : 
point => sig
type t = 'self => sig
  val x : Int.t
    val y : Int.t
  end .. 'self => sig
  val x : Int.t
    val y : Int.t
  end
  val add : ∀(p : point.t) ∀(p : point.t) 'self => sig
  val x : Int.t
    val y : Int.t
  end
end

(* You can also use lowercase identifier and don't use the keyword module. The
   let module syntax is there to differentiate modules from other terms which
   are common in OCaml.
*)

let point2D = struct(point)
  type t = { x : Int.t ; y : Int.t }
  let add = fun(p1 : point.t, p2 : point.t) ->
    let x' = Int.plus p1.x p2.x in
    let y' = Int.plus p1.y p2.y in
    { x = x' ; y = y' }
end;;
> point2D : 
point => sig
type t = 'self => sig
  val x : Int.t
    val y : Int.t
  end .. 'self => sig
  val x : Int.t
    val y : Int.t
  end
  val add : ∀(p : point.t) ∀(p : point.t) 'self => sig
  val x : Int.t
    val y : Int.t
  end
end

(* You can also annotated a module with a signature.
   It's the equivalent to the following OCaml code.
   module Point2D_with_sig : sig
     type t
     val add : t -> t -> t
   end = struct
     type t = { x : int ; y : int }
     let add x y = { x = p1.x + p2.x ; y = p1.y + p2.y }
   end
*)
let point2D_with_sig = sig
  type t
  val add : self.t -> self.t -> self.t
end : struct
  type t = { x : Int.t ; y : Int.t }
  let add = fun(p1 : self.t, p2 : self.t) ->
    let x' = Int.plus p1.x p2.x in
    let y' = Int.plus p1.y p2.y in
    { x = x' ; y = y' }
end;;
> point2D_with_sig : 
self => sig
type t = ⟂ .. ⊤
  val add : self.t -> self.t -> self.t
end
```

**NB: When you annotate the module with a signature, the typing algorithm is naive: it only takes the signature and doesn't perform any verification on the content of the module. The reason is the current inference algorithm on modules is very simple and will fail on some examples. For example, the following code is accepted.**
```OCaml
(* Even if the body doesn't define a field add, it's allowed by the compiler. *)
let point2D_with_sig_error = sig
  type t
  val add : self.t -> self.t -> self.t
end : struct
  type t = { x : Int.t ; y : Int.t }
end;;
> self => sig
type t = ⟂ .. ⊤
  val add : self.t -> self.t -> self.t
end
```

Functors use the same syntax than functions. The following example transforms
our Point2D module defining points using only integers in a polymorphic points
type.
As in OCaml, we use the convention functors begin with the uppercase identifier `Make`.
```OCaml
(* File test/MISC/modules_functors.rml *)
(* We create a functor. The first argument of MakePoint2D is a module with a
   type t and a function plus which takes two elements of type t and returns the
   same type.
*)
let module MakePoint2D = fun(typ : sig type t val plus : self.t -> self.t -> self.t end) -> struct(point)
  type t = { x : typ.t ; y : typ.t }
  let add = fun(p1 : point.t, p2 : point.t) ->
    let x' = typ.plus p1.x p2.x in
    let y' = typ.plus p1.y p2.y in
    { x = x' ; y = y' }
end;;
> MakePoint2D : 
∀(typ : self => sig
type t = ⟂ .. ⊤
  val plus : self.t -> self.t -> self.t
end) point => sig
type t = 'self => sig
  val x : typ.t
    val y : typ.t
  end .. 'self => sig
  val x : typ.t
    val y : typ.t
  end
  val add : ∀(p : point.t) ∀(p : point.t) 'self => sig
  val x : typ.t
    val y : typ.t
  end
end

(* And we can apply a module with the signature to the functor. Int is a good
   example.
*)
let module Point2DInt = MakePoint2D Int;;
> Point2DInt : 
point => sig
type t = 'self => sig
  val x : Int.t
    val y : Int.t
  end .. 'self => sig
  val x : Int.t
    val y : Int.t
  end
  val add : ∀(p : point.t) ∀(p : point.t) 'self => sig
  val x : Int.t
    val y : Int.t
  end
end
```

#### Field selection.

You can access to fields and types of a module with the syntax `M.f` where `M` is the identifier of the module and `f` the field.
```OCaml
(* File test/MISC/field_selection.rml *)
(* We build a module for a list of integers. List is a predefined functor
   representing lists.
*)
let module ListInt = List Int;;
> ListInt : 
list => sig
type t = self => sig
  val head : Unit.t -> Int.t
    val tail : Unit.t -> list.t
    val size : Int.t
    val is_empty : Bool.t
  end .. self => sig
  val head : Unit.t -> Int.t
    val tail : Unit.t -> list.t
    val size : Int.t
    val is_empty : Bool.t
  end
  val empty : list.t
  val cons : Int.t -> list.t -> list.t
end

(* We can use the field `cons` which add an element at the beginning to a list,
   here the empty list, given by ListInt.empty.
)
let l_42 = ListInt.cons 42 ListInt.empty;;
> l_42 :
ListInt.t

(* And get the head of the list *)
l_42.head ();;
> let  'F-SEL_l_42_head = l_.head in
let  'F-SEL_Unit_unit = Unit.unit in
'F-SEL_l_42_head 'F-SEL_Unit_unit :
Int.t

(* Get the size *)
l_42.size;;
> l_.size :
Int.t
```

#### Records.

A record is a list of couples `(id, terms)` and the syntax is the same than in OCaml.

Records and modules have the same internal representation
(`Grammar.TermRecursiveRecord` or `Grammar.TermRecursiveRecordUntyped`).

*Implementation details:*
Records are modules which can not contain fields referring to the module itself.
To forbid it, the variable which binds internally to the module is `'self` . As
quotes in the beginning of a variable name are not allowed in the lexer, we are
sure fields in the records doesn't refer to other fields.


```OCaml
(* File test/MISC/record.rml *)
let x = { a = 42 ; x = 52 };;
> x :
'self => sig
val a : Int.t
  val x : Int.t
end

let f = fun(x : {a : Int.t ; b : Int.t}) -> Int.succ x.a;;
> f : 
∀(x : 'self => sig
val a : Int.t
  val b : Int.t
end) ∀(typ : self => sig type t = ⟂ .. ⊤ end) ∀(s : typ.t -> typ.t) ∀(z : typ.t) typ.t

let g = fun(x : {a : Int.t}) -> x.a in f { a = 42 ; b = 52 };;
> let  g = fun (x : 'self => sig val a : Int.t end) -> x.a in
let 
'UN-REC_'self_AGG_F-DECL_a_F-DECL_b
=
'self => struct 
  let a = 42 : Int.t
    let b = 52 : Int.t
  end in
f 'UN-REC_'self_AGG_F-DECL_a_F-DECL_b
∀(typ : self => sig type t = ⟂ .. ⊤ end) ∀(s : typ.t -> typ.t) ∀(z : typ.t) typ.t
```

#### Syntactic sugar.

RML is based on DOT (for Dependent Object Types) calculus, initially developed
as a basis for the Scala language. This version on RML is based on the syntax,
semantics and typing/subtyping rules given
in
[Wadler Fest 2016](https://www.cs.purdue.edu/homes/rompf/papers/amin-wf16.pdf).

The syntax is very low level and not very convenient for writing real programs.
For example, you can't
- define function with multiple arguments.
- only apply one variable (not terms) to functions (which must be a variable).
 
This implies RML has a syntax which supports it.

For example, you can use terms in function applications and use multiple
arugments in applications. Let bindings are created in the generated AST.

## Types

#### Int (stdlib/int.rml)

Any integer can be used as usual. For example, `42`, `1764`, `0`, ...
A part is defined with the Church encoding and another with `Unimplemented`. The
type of integers is `Int.t`. The module for integers is `Int`.

#### Unit (stdlib/unit.rml)

The unit term is defined in the module `Unit`. The
type of `unit` is `Unit.t`. The term `unit` is `Unit.unit`. A syntactic sugar
`()` is also available.

```OCaml
let example_unit = ();;
let example_unit_no_sugar = Unit.unit;;
```

#### Boolean (stdlib/bool.rml)

Booleans are defined with the Church encoding. The
two values `true` and `false` are respectively `Bool.true` and `Bool.false`.

#### Char (stdlib/char.rml), String (stdlib/string.rml), Float (stdlib/float.rml)

Functions on these types are not implemented. It's just there to be able to use
it in some functions and do some tests.
No syntactic sugar or real syntax is given to create values of these types.

#### Pair (stdlib/pair.rml)

Pairs are built like a functor taking two types. No syntactic sugars are
provided for this.

```OCaml
(* Pair implementation *)
let module Pair = fun(left_typ : sig type t end,
                      right_typ : sig type t end) -> struct(pair)
  type t = sig
    val fst : Unit.t -> left_typ.t
    val snd : Unit.t -> right_typ.t
  end
  let init : left_typ.t -> right_typ.t -> pair.t =
    fun(f : left_typ.t, s : right_typ.t) -> struct
       let fst = fun(u : Unit.t) -> f
       let snd = fun(u : Unit.t) -> s
     end
end;;
> Pair : 
∀(left_typ : self => sig type t = ⟂ .. ⊤ end) ∀(right_typ : self => sig
type t = ⟂ .. ⊤
end) pair => sig
type t = self => sig
  val fst : Unit.t -> left_typ.t
    val snd : Unit.t -> right_typ.t
  end .. self => sig
  val fst : Unit.t -> left_typ.t
    val snd : Unit.t -> right_typ.t
  end
  val init : left_typ.t -> right_typ.t -> pair.t
end
```

Another implementation without functor is defined in `stdlib/pair_with.rml` in the module `PairWith`. Here the definitation and an example.

```
(* File stdlib/pair_with.rml *)
(* Pair implementation without functor. *)
let module PairWith = struct(pair_mod)
  type pair = sig
    type fst_typ
    type snd_typ
    val fst : self.fst_typ
    val snd : self.snd_typ
  end
  let init =
    fun(
      fst_typ : sig type t end,
      snd_typ : sig type t end,
      fst : fst_typ.t,
      snd : snd_typ.t) ->
    struct
      type fst_typ = fst_typ.t
      type snd_typ = snd_typ.t
      let fst = fst
      let snd = snd
    end
end;;

(* File test/typing/pair_with.rml *)
let int_42_42 = PairWith.init Int Int 42 42;;
int_42_42.fst;;
int_42_42.snd;;
```
#### Intersection and sugar `with`.

Intersection can also be defined with the keyword `&`. The intersection can be
used with any type. For example `{ x : Int.t} & {y : Int.t}`.

Like OCaml, the keyword `with` followed by a type declaration can also be used.

```
(* File stdlib/list_with.rml *)
(* Polymorphic list implementation based on WF 2016. *)
let module List = struct(sci)
  type list = sig
    type t
    val is_empty : Bool.t
    val head : self.t
    val tail : sci.list with type t <: self.t
  end
  let nil : sci.list with type t = Nothing = struct
    type t = Nothing
    let is_empty = Bool.true
    let head = Unimplemented
    let tail = Unimplemented
  end
  let cons = fun(typ : sig type t end, head : typ.t, tail : sci.list with type t <: typ.t) ->
  struct
    type t = typ.t
    let is_empty = Bool.false
    let head = head
    let tail = tail
  end
end;;

(* The following line doesn't terminate because it's an infinite derivation tree. *)'
let l_42 = List.cons Int 42 List.nil;;
```

#### List (stdlib/list.rml)

Lists are polymorphic as in a previous example. The module `List` is a
functor taking a module containing a type `t`.
No syntactic sugar is provided for the moment.

See `test/typing/list.rml` for good examples.

Another implementation, based
on
[Wadler Fest 2016](https://www.cs.purdue.edu/homes/rompf/papers/amin-wf16.pdf)
is also provided in `stdlib/list_with.rml` but the algorithm doesn't
terminate due to infinite derivation tree.

#### Option (stdlib/option_church.rml)

Options are defined with the Church encoding and are defined by the functor
`Option` which needs a module containing a type `t` for the `some` term.
You can define a `some` with `YourOption.some value` and a `none` with
`YourOption.none`.

```OCaml
(* file test/typing/option.rml *)
let module optionint = option int;;
> opt => sig
type t = ∀(b : self => sig type t = ⟂ .. ⊤ end) Int.t -> b.t -> b.t -> b.t .. ∀(b : self => sig
  type t = ⟂ .. ⊤
  end) Int.t -> b.t -> b.t -> b.t
  val some : Int.t -> opt.t
  val none : opt.t
  val match : ∀(returned_type : self => sig type t = ⟂ .. ⊤ end) ∀(option : opt.t) ∀(is_some : Int.t -> returned_type.t) ∀(is_none : returned_type.t) returned_type.t
end

let some_42 = optionint.some 42;;
> some_42 : 
OptionInt.t

let none_int = optionint.none;;
> none_int : 
OptionInt.t

let f = fun(opt : optionint.t) ->
  optionint.match
    (* the type of the match is in the given module in the type t *)
    int
    (* the option value *)
    opt
    (* some case *)
    (fun(x : int.t) -> int.plus x 42)
    (* none case *)
    (42);;
> f : 
∀(opt : OptionInt.t) Int.t

f some_42;;
> f some_ :
Int.t

f none_int;;
> f none_int :
Int.t

(* As match is a function, you can define patterm matching with default values
   for some cases. in ml, it's a warning about non-exhaustive pattern mathing.
*)
let f_with_no_none_case = fun(opt : optionint.t) ->
  optionint.match
    (* the type of the match is in the given module in the type t *)
    int
    (* the option value *)
    opt
    (* some case *)
    (fun(x : int.t) -> int.plus x 42);;
    (* none case is not used. *)
> f_with_no_none_case : 
∀(opt : OptionInt.t) ∀(is_none : Int.t) Int.t

f_with_no_none_case none_int (int.plus 42 42);;
> let 
'LET_'F-SEL_Int_plus44_F-SEL_Int_plus_LET_'ASC_UN-INT_4245_ASC_UN-INT_42_LET_'ASC_UN-INT_4246_ASC_UN-INT_42_LET_'VAR_'ASC_UN-INT_424547_APP_'F-SEL_Int_plus44_'ASC_UN-INT_4245_APP_'VAR_'ASC_UN-INT_424547_'ASC_UN-INT_
=
let  'F-SEL_Int_plus = Int.plus in
  let  'ASC_UN-INT_ = 42 : Int.t in
  let  'ASC_UN-INT_ = 42 : Int.t in
  let  'VAR_'ASC_UN-INT_ = 'F-SEL_Int_plus 'ASC_UN-INT_ in
  'VAR_'ASC_UN-INT_ 'ASC_UN-INT_ in
let  'VAR_none_int = f_with_no_none_case none_int in
'VAR_none_int 'LET_'F-SEL_Int_plus44_F-SEL_Int_plus_LET_'ASC_UN-INT_4245_ASC_UN-INT_42_LET_'ASC_UN-INT_4246_ASC_UN-INT_42_LET_'VAR_'ASC_UN-INT_424547_APP_'F-SEL_Int_plus44_'ASC_UN-INT_4245_APP_'VAR_'ASC_UN-INT_424547_'ASC_UN-INT_ :
Int.t
```

#### Sum

Very close to `Option`. It also uses the Church encoding. Sum are polymorphic so
as previous types, the module `Sum` is actually a functor with two types. Only
two values can be defined (`left` and `right`) and a match can be done with
`match` field. It takes the module containing the type `t` which must be
returned by the pattern matching, the sum value, and two functions, one in the
left case, one in the right case which parameter type depends on the branches
and the returned type is the returned typed of the pattern matching.

```
(* File /test/typing/sum.rml *)
(* We crate a Sum type for Int and Float. The left value will be Int and rhe
   right Float.
   It's like
   type t = Left of int | Right of float
*)
let module SumIntFloat = Sum Int Float;;
> sum => sig
type t = ∀(b : self => sig type t = ⟂ .. ⊤ end) Int.t -> b.t -> Float.t -> b.t -> b.t .. ∀(b : self => sig
  type t = ⟂ .. ⊤
  end) Int.t -> b.t -> Float.t -> b.t -> b.t
  val left : Int.t -> sum.t
  val right : Float.t -> sum.t
  val match : ∀(return_type : self => sig type t = ⟂ .. ⊤ end) ∀(s : sum.t) ∀(is_left : Int.t -> return_type.t) ∀(is_right : Float.t -> return_type.t) return_type.t
end

(* We create a value of type Sum.t by using the variant Left. *)
let sum_42_int = SumIntFloat.left 42;;
> sum_42_int : 
SumIntFloat.t

(* And we create a value of type Sum.t by using the variant Right. *)
let sum_42_float = SumIntFloat.right (float_of_int 42);;
> sum_42_float : 
SumIntFloat.t

(* We can do a pattern matching on a value of type SumintFloat.t like this. *)
let f = fun(sum : SumIntFloat.t) ->
  SumIntFloat.match
    Int
    sum
    (* Left case. *)
    (fun(x : Int.t) -> x)
    (* Right case. *)
    (fun(x : Float.t) -> Float.int_of_float x);;
> f : 
∀(sum : SumIntFloat.t) Int.t

f sum_42_float;;
> f sum_42_float :
Int.t

f sum_42_int;;
> f sum_42_int :
Int.t

(* We can also change the type of the patterm matching. *)
let module IntList = List Int;;
> IntList : 
list => sig
type t = self => sig
  val head : Unit.t -> Int.t
    val tail : Unit.t -> list.t
    val size : Int.t
    val is_empty : Bool.t
  end .. self => sig
  val head : Unit.t -> Int.t
    val tail : Unit.t -> list.t
    val size : Int.t
    val is_empty : Bool.t
  end
  val empty : list.t
  val cons : Int.t -> list.t -> list.t
end

let f' = fun(sum : SumIntFloat.t) ->
  SumIntFloat.match
    IntList
    sum
    (fun(x : Int.t) -> IntList.cons x IntList.empty)
    (fun(x : Float.t) ->
       let x = Float.int_of_float x in
       IntList.cons x IntList.empty)
    ;;
> f' : 
∀(sum : SumIntFloat.t) IntList.t

f' sum_42_float;;
> f' sum_42_float
IntList.t

f' sum_42_int;;
> f' sum_42_int
IntList.t
```

#### Module type/signature.

The type of module is also called the signature.

Some syntactic sugar are provided:
```OCaml
(* For type declarations in modules. *)
type t --> type t = Nothing .. Any
type t :> Int.t --> type t = Int.t .. Any
type t <: Int.t --> type t = Nothing .. Int.t
type t = Int.t --> type t = Int.t .. Int.t

(* When no variable for the internal use in the module signature, `self` is
   automatically used.
*)
sig
  type t
  val add : self.t -> Int.t
end --> sig(self)
  type t = Nothing .. Any
  val add self.t -> Int.t
end
```

## Compilation.

To compile, use
```
make
```

It produces an executable `rml`.

## How to use?

Use `./rml --help` for the complete documentation.

The produced executable has two parameters: the **file** and the **action**.

**Actions** are algorithms you want to execute/test.
Possible actions are:
- use the typing algorithm on terms (`typing`).
- use the subtyping algorithm on types (`subtype`).

For example, you can try the subtyping algorithm on the file `test/subtype/stdlib.dsubml` by using:
```
./rml -f test/typing/option.dsubml -a typing --use-stdlib
```

## Annotations

Like in OCaml with PPX, you can add some annotations to activate or desactivate some options.
For example, you can add `[@show_derivation_tree]` to the end of an expression
(before `;;`) to ask to show the typing or sub-typing derivation tree (i.e. to
see how the algorithms infer the type or decide if the types are sub-type).
You can ask multiple annotations by using a comma.

The list of available annotations are:
- `show_derivation_tree` to show the derivation tree. (not activated by default)
- `no_context` will deactivate the print of the environment (i.e. pre-defined
  values and types) when printing the derivation tree.
- `check_well_formed` checks at runtime if types (inferred from terms) are well
  formed. Note that the well-formed algorithm is always called on internal types
  during sub-typing and typing algorithms.

#### Examples

Print the derivation tree without the context.
```OCaml
let y = sig
  type t = Nothing
  val x : Nothing
  val f : self.t -> self.t
end : struct
  type t = Nothing
  let x = Unimplemented
  let f = fun (x : self.t) -> x
end [@show_derivation_tree, no_context];;
```

Print the derivation tree with the context.
```OCaml
let identity = fun (x : Int.t) -> x [@show_derivation_tree] ;;
```
