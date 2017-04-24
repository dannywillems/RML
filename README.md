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
```

Functors used the same syntax than functions. The following examples transforms our Point2D module defining points using only integers in a polymorphic points type.
As in OCaml, we use the convention functors begin with the uppercase identifier `Make`.
```OCaml
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

*Implementation detail:*
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
arugments applications. Let bindings are created in the generated AST.

## Types

#### Int

Any integer can be used as usual. For example, `42`, `1764`, `0`, ...
The `Int` module is defined in the file `stdlib/int.rml`. A part is defined with
the Church encoding and another with `Unimplemented`.
The type of intergers is `Int.t`.

#### Unit

The unit term is defined in the module `Unit` in the file `stdlib/unit.rml`. The
type of `unit` is `Unit.t`. The term `unit` is `Unit.unit`. A syntactic sugar
`()` is also available.

```OCaml
let example_unit = ();;
let example_unit_no_sugar = Unit.unit;;
```

#### Boolean

Booleans are defined with the Church encoding in the file `stdlib/bool.rml`. The two values `true` and `false` are

#### Char, String, Float

#### Pair

#### List

Polymorphic lists are defined in the file ``

#### Option


#### Sum

Very close to `Option`. It also uses the Church encoding.

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
- `check_well_formed` checks at runtime if types are well formed.

#### Examples

Print the derivation tree without the context.
```OCaml
let y = sig
  type T = Nothing
  val x : Nothing
  val f : self.T -> self.T
end : struct
  type T = Nothing
  let x = Unimplemented
  let f = fun (x : self.T) -> x
end [@show_derivation_tree, no_context];;
```

Print the derivation tree.
```OCaml
let identity = fun (x : self.T) -> x [@show_derivation_tree] ;;
```
