RML - ML modules as ML records with subtyping.
===

**WIP: Don't trust lines below.**

RML mixes ML Records and ML modules in a single type called recursive records
with dependent types. RML is simple lambda-calculus with subtyping and
parametric polymorphism, recursive type, record and dependent type.

Everything is written in OCaml.

## Examples

```
module String = { type t = Nothing }
```

To compile, use
```
make
```

It produces an executable `main.native`.

## How to use?

The produced executable has two parameters: the **file** and the **action**.

**Actions** are algorithms you want to execute/test.
Possible actions are:
- read a file containing a list of terms, print the red raw term and the corresponding nominal term (`read_term`).
- read a file containing a list of types, print the red raw type and the corresponding nominal type (`read_type`).
- evaluate a list of terms (`eval`).
- use the typing algorithm on terms (`typing`).
- use the subtyping algorithm on types (`subtype`).
- use the subtyping algorithm on types without REFL rules (`subtype_without_REFL`).
- check if each subtyping algorithm outputs the same result
  (`subtype_same_output`).
- type a term and print the type (`typing`)
- check if a term is well typed with the typing algorithm (`check_typing`).

For example, you can try the subtyping algorithm on the file `test/subtype_simple.dsubml` by using:
```
./main.native -f test/subtype_simple.dsubml -a subtype
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
