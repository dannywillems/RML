RML - ML modules as ML records with subtyping.
===

RML mixes ML Records and ML modules in a single type called recursive records
with dependent types. RML is simple lambda-calculus with subtyping and
parametric polymorphism, recursive type, record and dependent type.

Everything is written in OCaml.

**The implementation is focused on typing and subtyping algorithms, not evaluation. For this reason, for example, a term `Unimplemented` of type `Bottom` is defined and often used to leave out the implementation, the meaning of the term. Only types are important.**

## Expressions syntax

Every expression ends with two semicolons.

Examples:
```OCaml
let x = 42;;
let f = fun(x : Int.t) -> x;;
let f = fun(y : Int.t) -> let x = y in x;;
```

#### Comments

Comments can be defined with `(* *)` like in ML.

```OCaml
(* File test/MISC/comment.rml *)
(* This is a comment *)
(* You can also have comments in comments (* like this and (* like this *) *) *)
let x = (* It's also allowed in expressions *) 42;;
```

#### Let binding

You can define top level let binding with the expression `let var = expr`.

Examples:
```OCaml
(* File test/MISC/let_binding.rml *)
let x = 42;;
let f = fun(x : Int.t) -> x;;
let f = fun(y : Int.t) -> let x = y in x;;
```

#### Local bindings

As in ML, you can define local bindings with the syntax `let ... = ... in ...`.

Examples:
```OCaml
(* File test/MISC/local_binding.rml *)
let x = 42 in x;;
let f = fun(x : Int.t) -> x in f 42;;
let x = 42 in let y = x in y;;
```

**You need to be careful about the avoidance problem when using local bindings.**

```OCaml
let module M = struct
  type t = Int.t
  let x : self.t = 42
end in M.x;;
```
is a good example of the avoidance problem because `M.x` is of type `M.t` and
the entire expression is of type `M.t` but the module `M` doesn't exist outside
the let binding, so it's not well-typed.

#### Functions.

Parameters in the functions are annotated with the type. The keyword `fun` is used and a
simple right arrow `->` is used to define the body of the function. Thanks to
currying, a function can take multiple parameters by using a comma. But, it
remains a syntactic sugar for functions returning functions like in many
functional programming languages.

RML also focuses on dependent types and dependent function. It means parameters
can use the parameters defined before.

```OCaml
(* File test/MISC/function.rml *)
let f = fun(x : Int.t) -> x;;
let g = fun(x : Int.t, y : String.t) -> y;;
let h = fun(x : Int.t, f : Int.t -> Int.t) -> f x;;
(* Is equivalent to *)
let h = fun(x : Int.t) -> fun(f : Int.t -> Int.t) -> f x;;
(* Here an example of a dependent function. the two last parameters depend on the first parameter which is a module *)
let h' = fun(m : sig type t val add : self.t -> self.t -> Int.t end, x : m.t, y : m.t) -> m.add x y;;
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
    let x' = Int.add p1.x p2.x in
    let y' = Int.add p1.y p2.y in
    { x = x' ; y = y' }
end;;

(* In the previous example, the internal variable used to refer to another field
   is `self` which is the default variable name. Another name can be used as in the
   following example.
*)
(* Instead of self, the variable point is used. *)
let module Point2D = struct(point)
  type t = { x : Int.t ; y : Int.t }
  let add = fun(p1 : point.t, p2 : point.t) ->
    let x' = Int.plus p1.x p2.x in
    let y' = Int.plus p1.y p2.y in
    { x = x' ; y = y' }
end;;

(* You can also use lowercase identifier and don't use the keyword module. The
   let module syntax is there to differentiate modules from other terms which are
   common in OCaml. *)

let point2D = struct(point)
  type t = { x : Int.t ; y : Int.t }
  let add = fun(p1 : point.t, p2 : point.t) ->
    let x' = Int.plus p1.x p2.x in
    let y' = Int.plus p1.y p2.y in
    { x = x' ; y = y' }
end;;

(* You can also annotated a module with it's signature. *)
let point2D = sig
  type t = { x : Int.t ; y : Int.t}
  val add : self.t -> self.t -> self.t = struct(point)
  type t = { x : Int.t ; y : Int.t }
  let add = fun(p1 : point.t, p2 : point.t) ->
    let x' = Int.plus p1.x p2.x in
    let y' = Int.plus p1.y p2.y in
    { x = x' ; y = y' }
end;;


```

Functors use the same syntax than functions. The following example transforms
our Point2D module defining points using only integers in a polymorphic points
type.
As in OCaml, we use the convention functors begin with the uppercase identifier `Make`.
```OCaml
(* File test/MISC/modules_functors.rml *)
let module MakePoint2D = fun(typ : sig type t val plus : self.t -> self.t -> self.t end) -> struct(point)
  type t = { x : typ.t ; y : typ.t }
  let add = fun(p1 : point.t, p2 : point.t) ->
    let x' = typ.plus p1.x p2.x in
    let y' = typ.plus p1.y p2.y in
    { x = x' ; y = y' }
end;;

let module Point2DInt = MakePoint2D Int;;
```

#### Field selection.

You can access to fields and types of a module with the syntax `M.f` where `M` is the identifier of the module and `f` the field.
```
(* File test/MISC/field_selection.rml *)
(* We build a module for a list of integers. List is a predefined functor
   representing lists.
*)
let module ListInt = List Int;;

(* We can use the field `cons` which add an element at the beginning to a list,
   here the empty list, given by ListInt.empty.
)
let l_42 = ListInt.cons 42 ListInt.empty;;
(* And get the head of the list *)
l_42.head ();;

(* Get the size *)
l_42.size;;
```

#### Records.

A record is a list of couples `(id, terms)` and the syntax is the same than in OCaml.

Records and modules have the same internal representation
(`Grammar.TermRecursiveRecord` or `Grammar.TermRecursiveRecordUntyped`).

```OCaml
(* File test/MISC/record.rml *)
let x = { a = 42 ; x = 52 };;
let f = fun(x : {a : Int.t ; b : Int.t}) -> Int.succ x.a;;
let g = fun(x : {a : Int.t}) -> x.a in f { a = 42 ; b = 52 };;
```

*Implementation details:*
Records are modules which can not contain fields referring to the module itself.
To forbid it, the variable which binds internally to the module is `'self` . As
quotes in the beginning of a variable name are not allowed in the lexer, we are
sure fields in the records doesn't refer to other fields.

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
```

#### List (stdlib/list.rml)

Lists are polymorphic as in a previous example. The module `List` is a
functor taking a module containing a type `t`.
No syntactic sugar is provided for the moment.

See `test/typing/list.rml` for good examples.

#### Option (stdlib/option_church.rml)

Options are defined with the Church encoding and are defined by the functor
`Option` which needs a module containing a type `t` for the `some` term.
You can define a `some` with `YourOption.some value` and a `none` with
`YourOption.none`.

```OCaml
(* file test/typing/option.rml *)
let module optionint = option int;;
let some_42 = optionint.some 42;;
let none_int = optionint.none;;

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

f some_42;;
f none_int;;

(* as match is a function, you can define patterm matching with default values
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

f_with_no_none_case none_int (int.plus 42 42);;
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
let module SumIntFloat = Sum Int Float;;
let sum_42_int = SumIntFloat.left 42;;
let sum_42_float = SumIntFloat.right (float_of_int 42);;

let f = fun(sum : SumIntFloat.t) ->
  SumIntFloat.match
    Int
    sum
    (fun(x : Int.t) -> x)
    (fun(x : Float.t) -> Float.int_of_float x);;
f sum_42_float;;
f sum_42_int;;

let module IntList = List Int;;
let f' = fun(sum : SumIntFloat.t) ->
  SumIntFloat.match
    IntList
    sum
    (fun(x : Int.t) -> IntList.cons x IntList.empty)
    (fun(x : Float.t) ->
       let x = Float.int_of_float x in
       IntList.cons x IntList.empty)
    ;;
f' sum_42_float;;
f' sum_42_int;;
```

#### Basic types.

RML comes
- `Unit`.
- `Int`.
- `Unit`.
- `Unit`.
- `Unit`.

#### Module type/signature.

The type of module is also called the signature.

Some syntactic sugar are provided:
```OCaml
(* For type declarations in modules. *)
type t --> type t = Nothing .. Any
type t :> Int.t --> type t = Int.t .. Any
type t <: Int.t --> type t = Nothing .. Int.t
type t = Int.t --> type t = Int.t .. Int.t

(* When no variable for the internal use in the module signature, `self` is automatically used. *)
sig type t val add : self.t -> Int.t end --> sig(self) type t = Nothing .. Any val add self.t -> Int.t end
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
./rml -f test/subtype/stdlib.dsubml -a subtype
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
