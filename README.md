RML - ML modules as ML modules with subtyping.
===

RML mixes ML Records and ML modules in a single type called recursive records
with dependent types. RML is simple lambda-calculus with subtyping and
parametric polymorphism, recursive type, record and dependent type.

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

A verbose mode is available for some action to see the derivation tree. Use `--show-derivation-tree` to activate this mode.
