%{
  exception No_argument

  let rec currying args return = match args with
    | [] -> raise No_argument
    | [(x, s)] -> Grammar.TermAbstraction(s, (x, return))
    | head :: tail ->
       let x, s = head in
       Grammar.TermAbstraction(s, (x, currying tail return))

  (*
  let rec sugar_list l = match l with
    | [] -> Grammar.TermAscription(
                Grammar.TermUnimplemented,
                Grammar.TypeProjection("list", "empty")
              )
    | head :: tail ->
       Grammar.Grammar.TermFieldSelection("list", "cons")
         )
   *)
%}

%token COLON
%token COMMA
%token DOT
%token EQUAL

%token LEFT_BRACKET
%token RIGHT_BRACKET

%token LEFT_PARENT
%token RIGHT_PARENT

%token LEFT_SQUARE_BRACKET
%token RIGHT_SQUARE_BRACKET
%token AT

%token SEMICOLON
%token DOUBLE_RIGHT_ARROW
%token TYPE_TOP
%token TYPE_BOTTOM

%token SUBTYPE
%token SUPERTYPE
%token SIG
%token END
%token STRUCT
%token TYPE
%token VAL

%token <string> ID
%token <string> ID_CAPITALIZE

%token <int> INTEGER
%token PLUS
%token MINUS
%token TIMES

%token IF
%token THEN
%token ELSE

%token ABSTRACTION
%token FUN
%token FORALL

%token LET
%token IN

%token ARROW_RIGHT

%token UNIMPLEMENTED_TERM

%token EOF

(* Use for tests *)
%token NOT_SUBTYPE

%start <Grammar.raw_top_level_term * Grammar.ppx_annotation list> top_level_term
%start <bool * Grammar.raw_top_level_subtype * Grammar.ppx_annotation list> top_level_subtype
%%

(* ------------------------------------------ *)
(* Entry points *)

(* Entry point for typing algorithm *)
top_level_term:
| t = top_level_term_content ;
  SEMICOLON ;
  SEMICOLON { (t, []) }
| t = top_level_term_content ;
  annotation = rule_annotation ;
  SEMICOLON ;
  SEMICOLON { (t, annotation) }
| EOF { raise End_of_file }

top_level_term_content:
| t = rule_term { Grammar.Term(t) }
| content = top_level_let {
                let x, t = content in
                Grammar.TopLevelLetTerm(x, t)
              }

(* Entry point for subtyping algorithm *)
top_level_subtype:
| content = top_level_subtype_content ;
  SEMICOLON ;
  SEMICOLON {
      let b, term = content in
      (b, term, [])
    }
| content = top_level_subtype_content ;
  annotation = rule_annotation ;
  SEMICOLON ;
  SEMICOLON {
      let b, term = content in (b, term, annotation)
    }
| EOF { raise End_of_file }

(* Content of a subtype statement. It can be a top level let declaration or a
   statement in the form S <: T or S !<: T. The first is used to say S is a subtype
   of T and the second is the opposite.
 *)
top_level_subtype_content:
| content = top_level_let {
                  let x, t = content in
                  (false, Grammar.TopLevelLetSubtype(x, t))
                }
| s = rule_type ;
  SUBTYPE ;
  t = rule_type {
          (true, Grammar.CoupleTypes(s, t))
        }
| s = rule_type ;
  NOT_SUBTYPE ;
  t = rule_type {
          (false, Grammar.CoupleTypes(s, t))
        }

(* A top level let declaration. It can be in the form let x = t
   or let x : T = t
*)
top_level_let:
| LET ;
  x = ID ;
  EQUAL ;
  t = rule_term { x, t }
| LET ;
  x = ID ;
  COLON ;
  typ = rule_type ;
  EQUAL ;
  term = rule_term { x, Grammar.TermAscription(term, typ) }
(* ------------------------------------------ *)

(* Rules for terms *)
rule_term:
| t = rule_term_without_parent { t }
| t = rule_term_with_parent { t }

(* Terms which doesn't need parenthesis *)
rule_term_without_parent:
(* x *)
| id = ID { Grammar.TermVariable (id) }
(* Field selection: x.a *)
| x = ID ;
  DOT ;
  a = ID {
          Grammar.TermFieldSelection(x, a)
        }
(* Sugar which doesn't need parenthesis. *)
| t = rule_sugar_term_without_parent { t }
(* Nested parenthesis *)
| LEFT_PARENT ;
  t = rule_term_without_parent ;
  RIGHT_PARENT { t }
(* ----- Unofficial terms ----- *)
(* Unimplemented *)
| UNIMPLEMENTED_TERM { Grammar.TermUnimplemented }

rule_term_with_parent:
(* Applications *)
| app = rule_application { app }
(* let x = t in u --> (t, (x, u))*)
| LET ;
  x = ID ;
  EQUAL ;
  t = rule_term ;
  IN ;
  u = rule_term {
          Grammar.TermLet(t,(x, u))
        }
(* Values *)
| v = rule_value { v }
(* ----- Unofficial terms ----- *)
(* t : T *)
| term = rule_term ;
  COLON ;
  typ = rule_type {
            Grammar.TermAscription(term, typ)
          }

rule_application:
(* x y *)
| x = ID ; y = ID {
                   (*
                   let x = Grammar.TermVariable(x) in
                   let y = Grammar.TermVariable(y) in
                   *)
                   Grammar.TermVarApplication (x, y)
                 }

(*
| t = rule_term ; u = rule_term {
      Grammar.TermLet(
          t,
          ("f",
           Grammar.TermLet(
             u,
             ("g",
              Grammar.TermVarApplication("f", "g")
             )
           )
          )
        )
    }
*)
(* f x y z z' ... *)
(* TODO *)


rule_module:
(* { x : T => d } *)
| LEFT_BRACKET ;
  x = ID ;
  COLON ;
  t = rule_type ;
  DOUBLE_RIGHT_ARROW ;
  d = rule_decl ;
  RIGHT_BRACKET {
      Grammar.TermRecursiveRecord(t, (x, d))
    }
(* x : T => struct type A = ... ; let a = ... end *)
| x = ID ;
  COLON ;
  t = rule_type ;
  DOUBLE_RIGHT_ARROW ;
  STRUCT ;
  d = rule_decl ;
  END {
      Grammar.TermRecursiveRecord(t, (x, d))
    }

(* T : struct type A = ... ; let a = ... end *)
| t = rule_type ;
  COLON ;
  STRUCT ;
  d = rule_decl ;
  END {
      Grammar.TermRecursiveRecord(t, ("self", d))
    }

(* struct(x) type A = ... ; let a = ... end *)
| STRUCT ;
  LEFT_PARENT ;
  x = ID ;
  RIGHT_PARENT ;
  d = rule_decl ;
  END {
      Grammar.TermRecursiveRecordUntyped(x, d)
    }
(* struct
     type A = ...
     let a = ...
  end
*)
| STRUCT ;
  d = rule_decl ;
  END {
      Grammar.TermRecursiveRecordUntyped("self", d)
    }
(* { x => d } *)
| LEFT_BRACKET ;
  x = ID ;
  DOUBLE_RIGHT_ARROW ;
  d = rule_decl ;
  RIGHT_BRACKET {
      Grammar.TermRecursiveRecordUntyped(x, d)
    }

(* Rules for values *)
rule_value:
| t = rule_module { t }
| t = rule_abstraction { t }

(* Abstractions *)
rule_abstraction:
(* λ(x : S) t *)
| ABSTRACTION ;
  LEFT_PARENT ;
  args = rule_arguments_list
  RIGHT_PARENT ;
  t = rule_term {
          currying args t
        }
(* fun (x : T) -> t *)
| FUN ;
  LEFT_PARENT ;
  args = rule_arguments_list ;
  RIGHT_PARENT ;
  ARROW_RIGHT ;
  t = rule_term {
          currying args t
        }

(* A rule to parse the list of arguments of a function. Curryfication is used; *)
rule_arguments_list:
| c = rule_arguments_content {
          [c]
        }
| head = rule_arguments_content
  COMMA ;
  tail = rule_arguments_list { head :: tail }

rule_arguments_content:
| x = ID ;
  COLON ;
  t = rule_type { x, t }

(* Term declaration in a structure *)
rule_decl:
(* type L = T *)
| TYPE ;
  type_label = ID_CAPITALIZE ;
  EQUAL ;
  typ = rule_type {
            Grammar.TermTypeDeclaration(type_label, typ)
          }
(* let a = t *)
| LET ;
  field_label = ID ;
  EQUAL ;
  t = rule_term {
          Grammar.TermFieldDeclaration(field_label, t)
        }
(* let a : T = t *)
| LET ;
  field_label = ID ;
  COLON ;
  typ = rule_type ;
  EQUAL ;
  t = rule_term {
          Grammar.TermFieldDeclaration(field_label, Grammar.TermAscription(t, typ))
        }
(* d ∧ d' *)
| d1 = rule_decl ;
  d2 = rule_decl {
           Grammar.TermAggregateDeclaration(d1, d2)
         }

(* Rule for sugar terms like if .. then .. else .. or unit ( () ) *)
(* How can we exactly define pre defined terms ?
  This solution doesn't work very well because if the variable unit is
  redefined, it will use this definition, and not from the standard library.
*)
rule_sugar_term:
| t = rule_sugar_term_without_parent { t }
(* | t = rule_sugar_term_with_parent { t } *)

rule_sugar_term_without_parent:
(* Unit *)
| LEFT_PARENT ;
  RIGHT_PARENT {
      Grammar.TermAscription(
          Grammar.TermUnimplemented,
          Grammar.TypeProjection("unit", "T")
        )
    }
(* For integers *)
| n = INTEGER {
          Grammar.TermAscription(
              Grammar.TermInteger(n),
              Grammar.TypeProjection("int", "T")
            )
        }
| LEFT_PARENT ;
  t = rule_sugar_term_without_parent ;
  RIGHT_PARENT { t }

(*
(* For lists *)
| l = rule_sugar_term_list { l }

(* Rule to build a list with the sugar *)
rule_sugar_term_list:
| LEFT_SQUARE_BRACKET ;
  l = rule_sugar_term_list_content ;
  RIGHT_SQUARE_BRACKET {
    }
*)

rule_sugar_term_infix:
| m = INTEGER ;
  PLUS ;
  n = INTEGER {
          let m = Grammar.TermAscription(
                      Grammar.TermInteger(m),
                      Grammar.TypeProjection("int", "T")
                    )
          in
          let n = Grammar.TermAscription(
                      Grammar.TermInteger(n),
                      Grammar.TypeProjection("int", "T")
                    )
          in
          Grammar.TermVarApplication(
              Grammar.TermVarApplication(
                  Grammar.TermFieldSelection("int", "plus"),
                  m
                ),
              n
            )
        }

(*
rule_sugar_term_with_parent:
| IF ;
  cond = rule_term ;
  THEN ;
  if_true = rule_term ;
  ELSE ;
  if_false = rule_term {
                 condition.
               }
*)

(* ------------------------------- *)
(* Types *)
rule_type:
| TYPE_BOTTOM { Grammar.TypeBottom }
| TYPE_TOP { Grammar.TypeTop }
(* T ∧ T *)
| t1 = rule_type ;
  t2 = rule_type {
           Grammar.TypeIntersection(t1, t2)
         }

(* { z => T^{z} } *)
| LEFT_BRACKET ;
  var = ID ;
  DOUBLE_RIGHT_ARROW ;
  typ = rule_type_declaration;
  RIGHT_BRACKET {
      Grammar.TypeRecursive(var, typ)
    }
(* sig T^{self} end *)
| SIG ;
  typ = rule_type_declaration;
  END {
      Grammar.TypeRecursive("self", typ)
    }
(* sig(x) T^{x} end *)
| SIG ;
  LEFT_PARENT ;
  var = ID ;
  RIGHT_PARENT ;
  typ = rule_type_declaration ;
  END {
      Grammar.TypeRecursive(var, typ)
    }
(* x.L *)
| var = ID ;
  DOT ;
  type_label = ID_CAPITALIZE {
                   Grammar.TypeProjection(var, type_label)
                 }
(* S -> T --> ∀(_ : S) T *)
| s = rule_type ;
  ARROW_RIGHT ;
  t = rule_type {
          Grammar.TypeDependentFunction(s, ("_", t))
        }
(* ∀(x : S) T --> (S, (x, T)) *)
| FORALL ;
  LEFT_PARENT ;
  x = ID ;
  COLON ;
  s = rule_type ;
  RIGHT_PARENT ;
  t = rule_type {
          Grammar.TypeDependentFunction(s, (x, t))
        }
(* (T) *)
| LEFT_PARENT ;
  t = rule_type ;
  RIGHT_PARENT { t }

(* The content of a signature *)
rule_type_declaration:
(* type L : S..T --> (L, S, T) *)
| TYPE ;
  type_label = ID_CAPITALIZE ;
  COLON ;
  s = rule_type ;
  DOT ;
  DOT ;
  t = rule_type {
          Grammar.TypeDeclaration(type_label, s, t)
        }
(* type L <: T --> (L, Bottom, T) *)
| TYPE ;
  type_label = ID_CAPITALIZE ;
  SUBTYPE ;
  t = rule_type {
          Grammar.TypeDeclaration(type_label, Grammar.TypeBottom, t)
        }
(* type L :> S --> (L, S, Any) *)
| TYPE ;
  type_label = ID_CAPITALIZE ;
  SUPERTYPE ;
  s = rule_type {
          Grammar.TypeDeclaration(type_label, s, Grammar.TypeTop)
        }
(* type L = S --> (L, S, S) *)
| TYPE ;
  type_label = ID_CAPITALIZE ;
  EQUAL ;
  s = rule_type {
          Grammar.TypeDeclaration(type_label, s, s)
        }
(* type L --> (L, Nothing, Any) *)
| TYPE ;
  type_label = ID_CAPITALIZE {
                   Grammar.TypeDeclaration(
                       type_label,
                       Grammar.TypeBottom,
                       Grammar.TypeTop
                     )
                 }
(* val a : T *)
| VAL ;
  field_label = ID ;
  COLON ;
  t = rule_type {
          Grammar.TypeFieldDeclaration(field_label, t)
        }
(* T ∧ T *)
| t1 = rule_type_declaration ;
  t2 = rule_type_declaration {
           Grammar.TypeIntersection(t1, t2)
         }

(* Rule for annotations, like PPX in OCaml *)
rule_annotation:
| LEFT_SQUARE_BRACKET ;
  AT ;
  content = rule_annotation_list ;
  RIGHT_SQUARE_BRACKET {
      content
    }

rule_annotation_list:
| c = rule_annotation_content { [c] }
| c1 = rule_annotation_content ;
  COMMA ;
  c2 = rule_annotation_list {
           c1 :: c2
         }

rule_annotation_content:
| content = ID { content }
