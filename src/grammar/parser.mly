%{
  exception No_argument

  (*
    [4 ; 5 ; 6] ->
    let 'x = 4 in
    let 'y = 5 in
    let 'z = 6 in
    List.const 'x (List.cons 'y (List.cons 'z List.empty))
  let rec sugar_list l = match l with
    | [] -> Grammar.TermFieldSelection("List", "empty")
    | head :: tail ->
       Grammar.Grammar.TermFieldSelection("List", "cons")
         )
  *)
  let make_integer n =
    Grammar.TermAscription(
        Grammar.TermInteger(n),
        Grammar.TypeProjection("Int", "t")
      )

  let make_string s =
    Grammar.TermAscription(
        Grammar.TermInteger(s),
        Grammar.TypeProjection("String", "t")
      )

  let make_unit () =
    Grammar.TermAscription(
        Grammar.TermUnimplemented,
        Grammar.TypeProjection("Unit", "t")
      )

  let current_integer = ref 0

  let fresh_variable term =
    let fresh_name_term =
      GrammarToolbox.string_of_term_for_fresh_variable term
    in
    let name = "'" ^ fresh_name_term ^ (string_of_int (!current_integer)) in
    incr current_integer;
    name

  let rec currying args return = match args with
    | [] -> raise No_argument
    | [(x, s)] -> Grammar.TermAbstraction(s, (x, return))
    | head :: tail ->
       let x, s = head in
       Grammar.TermAbstraction(s, (x, currying tail return))
  (* f x y z -->
     let f_x = f x in
     let f_x_y = f_x y in
     f_x_y z
  *)
  let rec currying_app_term f args = match args with
    | [] -> failwith "No arguments"
    | [head] -> Grammar.TermVarApplication(f, head)
    | head :: tail ->
       let x = fresh_variable (Grammar.TermVariable(head)) in
       let s = Grammar.TermVarApplication(f, head) in
       let t = currying_app_term x tail in
       Grammar.TermLet(s, (x, t))

  (*
     f t1 t2 t3 -->
     let v_t1 = t1 in
     let v_t2 = t2 in
     let v_t3 = t3 in
     let f_t1 = f v_t1 in
     let f_t1_t2 = f_t1 v_t2 in
     f_t1_t2 v_t3
  *)
  let rec let_bindings_of_terms t terms_with_variables =
    match terms_with_variables with
    | [] -> t
    | head :: tail ->
       let x, s = head in
       if GrammarToolbox.is_raw_variable s
       then let_bindings_of_terms t tail
       else
         let t = let_bindings_of_terms t tail in
         Grammar.TermLet(s, (x, t))

  let rec currying_app_with_terms f terms =
    let terms_with_variables =
      List.map
        (fun t ->
          match t with
          | Grammar.TermVariable(x) -> x, t
          | _ -> fresh_variable t, t
        )
        terms
    in
    let variables = List.map fst terms_with_variables in
    let terms_with_variables =
      List.filter
        (fun (x, t) -> not (GrammarToolbox.is_raw_variable t))
        terms_with_variables
    in
    let t = currying_app_term f variables in
    let_bindings_of_terms t terms_with_variables
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
%token MODULE

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
| content = rule_let_binding {
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
| content = rule_let_binding {
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

(* ------------------------------------------ *)

(* A let binding. It can be in the form let x = t
   or let x : T = t
   Let binding to modules can be declared with a uppercase letter (but it's not
   mandatory).
*)
rule_let_binding:
(* let x = t *)
| LET ;
  x = ID ;
  EQUAL ;
  t = rule_term { x, t }
(* let x : T = t *)
| LET ;
  x = ID ;
  COLON ;
  typ = rule_type ;
  EQUAL ;
  term = rule_term { x, Grammar.TermAscription(term, typ) }
(* let M = t *)
| LET ;
  MODULE ;
  x = ID_CAPITALIZE ;
  EQUAL ;
  m = rule_module {
          x, m
        }
(* let M : T = t *)
| LET ;
  MODULE ;
  x = ID_CAPITALIZE ;
  COLON ;
  typ = rule_type ;
  EQUAL ;
  m = rule_module {
          x, Grammar.TermAscription(m, typ)
        }

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
(* Field selection: M.a *)
| x = ID_CAPITALIZE ;
  DOT ;
  a = ID {
          Grammar.TermFieldSelection(x, a)
        }
(* Sugar which doesn't need parenthesis. *)
| t = rule_sugar_term_without_parent { t }
| t = rule_record { t }
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
(* Let binding for module or simple terms *)
| let_binding = rule_term_let_binding { let_binding }
(* Values *)
| v = rule_value { v }
(* ----- Unofficial terms ----- *)
(* t : T *)
| term = rule_term ;
  COLON ;
  typ = rule_type {
            Grammar.TermAscription(term, typ)
          }

rule_term_let_binding:
(* let x = t in u --> (t, (x, u))*)
| let_binding = rule_let_binding ;
  IN ;
  u = rule_term {
          let x, t = let_binding in
          Grammar.TermLet(t,(x, u))
        }

rule_application:
(* f x y z *)
| f = ID ; args = rule_application_arguments {
                    currying_app_with_terms f args
                    }

(* term y --> let variable = term in variable y *)
| term = rule_term_for_application ;
  args = rule_application_arguments {
          let f_term = fresh_variable term in
          let s = currying_app_with_terms f_term args in
          Grammar.TermLet(term, (f_term, s))
        }

rule_application_arguments:
| t = rule_term_for_application { [t] }
| t = rule_term_for_application ;
  tail = rule_application_arguments {
             t :: tail
           }
(* Term which can be used for application. *)
rule_term_for_application:
| t = rule_term_without_parent { t }
| LEFT_PARENT ; t = rule_term_with_parent ; RIGHT_PARENT { t }

rule_record:
| LEFT_BRACKET ;
  d = rule_record_content ;
  RIGHT_BRACKET {
      Grammar.TermRecursiveRecordUntyped("'self", d)
    }

(* Content of a record *)
rule_record_content:
(* field = term, like in ML *)
| field = ID ;
  EQUAL ;
  t = rule_term {
          Grammar.TermFieldDeclaration(field, t)
        }
(* Fields in a record separated by a semicolon *)
| d1 = rule_record_content ;
  SEMICOLON ;
  d2 = rule_record_content {
           Grammar.TermAggregateDeclaration(d1, d2)
         }

rule_module:
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
| t = rule_decl_field { t }
| t = rule_decl_type { t }
(* d ∧ d' *)
| d1 = rule_decl ;
  d2 = rule_decl {
           Grammar.TermAggregateDeclaration(d1, d2)
         }

rule_decl_type:
(* type t = T *)
| TYPE ;
  type_label = ID ;
  EQUAL ;
  typ = rule_type {
            Grammar.TermTypeDeclaration(type_label, typ)
          }

rule_decl_field:
(* let a = t *)
| let_binding = rule_let_binding {
          let (x, t) = let_binding in
          Grammar.TermFieldDeclaration(x, t)
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
      make_unit ()
    }
(* For integers *)
| n = INTEGER {
          make_integer n
        }
| LEFT_PARENT ;
  t = rule_sugar_term_without_parent ;
  RIGHT_PARENT { t }
(* For lists
| l = rule_sugar_term_list { l }
*)

(* Rule to build a list with the sugar *)
(*
rule_sugar_term_list:
| LEFT_SQUARE_BRACKET ;
  l = rule_sugar_term_list_content ;
  {
    }

rule_sugar_term_list_content:
| RIGHT_SQUARE_BRACKET { [Grammar.TermFieldSelection("List", "empty")] }
| t = rule_term_for_application ;
  SEMICOLON ;
  tail = rule_sugar_term_list_content {
             let cons = Grammar.TermFieldSelection("List", "cons")
             t :: tail
           }
*)
rule_sugar_term_infix:
| m = INTEGER ;
  PLUS ;
  n = INTEGER {
          let m = make_integer m in
          let n = make_integer n in
          Grammar.TermVarApplication(
              Grammar.TermVarApplication(
                  Grammar.TermFieldSelection("Int", "plus"),
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
(* x.t -> Lowercase must be used for records, not modules. *)
| var = ID ;
  DOT ;
  type_label = ID {
                   Grammar.TypeProjection(var, type_label)
                 }
(* M.t -> must be used for modules *)
| var = ID_CAPITALIZE ;
  DOT ;
  type_label = ID {
                   Grammar.TypeProjection(var, type_label)
                 }
(* Record *)
| t = rule_type_record { t }
(* Modules *)
| t = rule_type_module_signature { t }
(* Abstraction *)
| t = rule_type_abstraction { t }
(* (T) *)
| LEFT_PARENT ;
  t = rule_type ;
  RIGHT_PARENT { t }

rule_type_module_signature:
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

rule_type_abstraction:
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

(* A record is a module without type and without recursive variable.
   The id "'self" is used to avoid to define recursive records because ' is
   forbidden an identifier.
 *)
rule_type_record:
| LEFT_BRACKET ;
  typ = rule_type_record_declaration_field;
  RIGHT_BRACKET {
      Grammar.TypeRecursive("'self", typ)
    }

rule_type_record_declaration_field:
| field_label = ID ;
  COLON ;
  t = rule_type {
          Grammar.TypeFieldDeclaration(field_label, t)
        }
| d1 = rule_type_record_declaration_field ;
  SEMICOLON ;
  d2 = rule_type_record_declaration_field {
           Grammar.TypeIntersection(d1, d2)
         }
(* ----------------------------------------------------------------- *)
(* The content of a signature *)
rule_type_declaration:
| t = rule_type_declaration_type { t }
| t = rule_type_declaration_field { t }
(* T ∧ T *)
| t1 = rule_type_declaration ;
  t2 = rule_type_declaration {
           Grammar.TypeIntersection(t1, t2)
         }

rule_type_declaration_type:
(* type t : S..T --> (L, S, T) *)
| TYPE ;
  type_label = ID ;
  COLON ;
  s = rule_type ;
  DOT ;
  DOT ;
  t = rule_type {
          Grammar.TypeDeclaration(type_label, s, t)
        }
(* type t <: T --> (L, Bottom, T) *)
| TYPE ;
  type_label = ID ;
  SUBTYPE ;
  t = rule_type {
          Grammar.TypeDeclaration(type_label, Grammar.TypeBottom, t)
        }
(* type t :> S --> (L, S, Any) *)
| TYPE ;
  type_label = ID ;
  SUPERTYPE ;
  s = rule_type {
          Grammar.TypeDeclaration(type_label, s, Grammar.TypeTop)
        }
(* type t = S --> (L, S, S) *)
| TYPE ;
  type_label = ID ;
  EQUAL ;
  s = rule_type {
          Grammar.TypeDeclaration(type_label, s, s)
        }
(* type t --> (L, Nothing, Any) *)
| TYPE ;
  type_label = ID {
                   Grammar.TypeDeclaration(
                       type_label,
                       Grammar.TypeBottom,
                       Grammar.TypeTop
                     )
                 }

rule_type_declaration_field:
(* val a : T *)
| VAL ;
  field_label = ID ;
  COLON ;
  t = rule_type {
          Grammar.TypeFieldDeclaration(field_label, t)
        }
(* ----------------------------------------------------------------- *)

(* ----------------------------------------------------------------- *)
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
(* ----------------------------------------------------------------- *)
