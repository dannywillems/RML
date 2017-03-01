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
%token TYPE_INTERSECTION
%token TYPE_UNION
%token <string> ID
%token <string> ID_CAPITALIZE
%token EOF

%start <Grammar.raw_term> top_level
%%

top_level:
| t = rule_term ; SEMICOLON ; SEMICOLON { t }
| EOF { raise End_of_file }

rule_term:
| id = ID { Grammar.TermVar ( id ) }
| LEFT_BRACKET ;
  id = ID ;
  DOUBLE_RIGHT_ARROW ;
  decl_list = rule_decl_list ;
  RIGHT_BRACKET {
      Grammar.TermRecursiveRecord(id, decl_list)
    }
| t1 = rule_term ;
  DOT ;
  method_label = ID ;
  t2 = rule_term {
           Grammar.TermMethodApp(t1, method_label, t2)
         }

rule_decl_list:
(* Empty record *)
| { [ ] }
(* With this line, the last semicolon can be revmoved. *)
| decl = rule_decl { [decl] }
| decl_head = rule_decl ;
  SEMICOLON ;
  decl_tail = rule_decl_list {
      decl_head :: decl_tail
    }

rule_decl:
| type_label = ID_CAPITALIZE ;
  EQUAL ;
  typ = rule_type {
            Grammar.DeclarationType(type_label, typ)
          }
| method_label = ID ;
  LEFT_PARENT ;
  var = ID ;
  COLON ;
  s = rule_type ;
  RIGHT_PARENT ;
  EQUAL ;
  t = rule_term {
          Grammar.DeclarationMethod(method_label, s, (var, t))
        }

rule_type:
| TYPE_TOP { Grammar.TypeTop }
| TYPE_BOTTOM { Grammar.TypeBottom }
| t1 = rule_type ;
  TYPE_INTERSECTION ;
  t2 = rule_type {
           Grammar.TypeIntersection(t1, t2)
         }
| t1 = rule_type ;
  TYPE_UNION ;
  t2 = rule_type {
           Grammar.TypeUnion(t1, t2)
         }
| LEFT_BRACKET ;
  var = ID ;
  DOUBLE_RIGHT_ARROW ;
  typ = rule_type ;
  RIGHT_BRACKET {
      Grammar.TypeRecursive(var, typ)
    }
| type_label = ID_CAPITALIZE ;
  COLON ;
  s = rule_type ;
  DOT ;
  DOT ;
  t = rule_type {
          Grammar.TypeTypeMember(type_label, s, t)
        }
| method_label = ID ;
  LEFT_PARENT ;
  var = ID ;
  COLON ;
  s = rule_type ;
  RIGHT_PARENT ;
  COLON ;
  t = rule_type {
          Grammar.TypeMethodMember(method_label, s, (var, t))
        }
| var = ID ;
  DOT ;
  type_label = ID_CAPITALIZE {
                   Grammar.TypePathDependent(var, type_label)
                 }
