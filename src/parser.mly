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

%token INTERSECTION

%token <string> ID
%token <string> ID_CAPITALIZE

%token ABSTRACTION
%token FORALL

%token LET
%token IN

%token ARROW_RIGHT

%token UNIMPLEMENTED_TERM

%token EOF

%start <Grammar.raw_top_level> top_level_term
%%

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
| LEFT_PARENT ;
  t = rule_term ;
  RIGHT_PARENT { t }
| v = rule_value { v }

(* ----- Unofficial terms ----- *)
| term = rule_term ;
  COLON ;
  typ = rule_type {
            Grammar.TermAscription(term, typ)
          }
| UNIMPLEMENTED_TERM { Grammar.TermUnimplemented }

(* ----- Beginning of DOT terms ----- *)
(* x.a *)
| x = ID ;
  DOT ;
  a = ID {
          Grammar.TermFieldSelection(x, a)
        }

rule_value:
(* {x => t^{x}} *)
| LEFT_BRACKET ;
  id = ID ;
  DOUBLE_RIGHT_ARROW ;
  decl = rule_decl ;
  RIGHT_BRACKET {
      Grammar.TermRecursiveRecord(id, decl)
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

rule_decl:
(* L = T *)
| type_label = ID_CAPITALIZE ;
  EQUAL ;
  typ = rule_type {
            Grammar.DeclarationType(type_label, typ)
          }
(* a = t *)
| field_label = ID ;
  EQUAL ;
  t = rule_term {
          Grammar.DeclarationField(field_label, t)
        }
(* d ∧ d' *)
| d1 = rule_decl ;
  SEMICOLON ;
  d2 = rule_decl {
           Grammar.DeclarationAggregate(d1, d2)
         }

rule_type:
(* Top type : ⊤ *)
| TYPE_TOP { Grammar.TypeTop }
(* Bottom type : ⟂ *)
| TYPE_BOTTOM { Grammar.TypeBottom }
(* T ∧ T *)
| t1 = rule_type ;
  INTERSECTION ;
  t2 = rule_type {
           Grammar.TypeIntersection(t1, t2)
         }
(* { z => T^{z} } *)
| LEFT_BRACKET ;
  var = ID ;
  DOUBLE_RIGHT_ARROW ;
  typ = rule_type ;
  RIGHT_BRACKET {
      Grammar.TypeRecursive(var, typ)
    }
(* { L : S..T } --> (L, S, T) *)
| type_label = ID_CAPITALIZE ;
  COLON ;
  s = rule_type ;
  DOT ;
  DOT ;
  t = rule_type {
          Grammar.TypeDeclaration(type_label, s, t)
        }
(* { a : T } *)
| field_label = ID ;
  COLON ;
  t = rule_type {
          Grammar.TypeFieldDeclaration(field_label, t)
        }
(* x.L *)
| var = ID ;
  DOT ;
  type_label = ID_CAPITALIZE {
                   Grammar.TypeProjection(var, type_label)
                 }
| LEFT_PARENT ;
  t = rule_type ;
  RIGHT_PARENT {
      t }
| t = rule_type_forall { t }

rule_type_forall:
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
| LEFT_PARENT ;
  t = rule_type_forall
  RIGHT_PARENT { t }
