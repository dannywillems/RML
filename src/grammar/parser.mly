%token COLON
%token DOT
%token EQUAL

%token LEFT_BRACKET
%token RIGHT_BRACKET

%token LEFT_PARENT
%token RIGHT_PARENT

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

%start <Grammar.raw_top_level> top_level_term
%start <bool * Grammar.raw_top_level_subtype> top_level_subtype
%%

(* ------------------------------------------ *)
(* Entry points *)
top_level_term:
| t = rule_term ; SEMICOLON ; SEMICOLON { Grammar.TopLevelTerm(t) }
| LET ;
  x = ID ;
  EQUAL ;
  t = rule_term ;
  SEMICOLON ;
  SEMICOLON {
      Grammar.TopLevelLet(x, t)
    }
| EOF { raise End_of_file }

(* *)
top_level_subtype:
| LET ;
  x = ID ;
  EQUAL ;
  t = rule_term ;
  SEMICOLON ;
  SEMICOLON { (false, Grammar.TopLevelLetSubtype(x, t)) }
| s = rule_type ;
  SUBTYPE ;
  t = rule_type ;
  SEMICOLON ;
  SEMICOLON { (true, Grammar.CoupleTypes(s, t)) }
| s = rule_type ;
  NOT_SUBTYPE ;
  t = rule_type ;
  SEMICOLON ;
  SEMICOLON { (false, Grammar.CoupleTypes(s, t)) }
| EOF { raise End_of_file }

(* ------------------------------------------ *)

rule_term:
(* x *)
| id = ID { Grammar.TermVariable ( id ) }
(* x y *)
| x = ID ; y = ID { Grammar.TermVarApplication (x, y) }
(* let x = t in u --> (t, (x, u))*)
| LET ;
  x = ID ;
  EQUAL ;
  t = rule_term ;
  IN ;
  u = rule_term {
          Grammar.TermLet(t,(x, u))
        }
(* (t) *)
| LEFT_PARENT ;
  t = rule_term ;
  RIGHT_PARENT { t }
(* v *)
| v = rule_value { v }

(* ----- Unofficial terms ----- *)
(* t : T *)
| term = rule_term ;
  COLON ;
  typ = rule_type {
            Grammar.TermAscription(term, typ)
          }
(* Unimplemented *)
| UNIMPLEMENTED_TERM { Grammar.TermUnimplemented }

(* ----- Beginning of DOT terms ----- *)
(* x.a *)
| x = ID ;
  DOT ;
  a = ID {
          Grammar.TermFieldSelection(x, a)
        }

rule_value:
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

(* x => struct type A = ... ; let a = ... end *)
| x = ID ;
  DOUBLE_RIGHT_ARROW ;
  STRUCT ;
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

(* λ(x : S) t *)
| ABSTRACTION ;
  LEFT_PARENT ;
  id = ID ;
  COLON ;
  typ = rule_type ;
  RIGHT_PARENT ;
  t = rule_term {
          Grammar.TermAbstraction(typ, (id, t))
        }
(* fun (x : T) -> t *)
| FUN ;
  LEFT_PARENT ;
  x = ID ;
  COLON ;
  typ = rule_type ;
  RIGHT_PARENT ;
  ARROW_RIGHT ;
  t = rule_term {
          Grammar.TermAbstraction(typ, (x, t))
        }

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
(* d ∧ d' *)
| d1 = rule_decl ;
  d2 = rule_decl {
           Grammar.TermAggregateDeclaration(d1, d2)
         }

(* ------------------------------- *)
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
(* x => sig T^{x} end *)
| var = ID ;
  DOUBLE_RIGHT_ARROW ;
  SIG ;
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
