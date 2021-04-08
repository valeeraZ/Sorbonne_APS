%{
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == S-expression Syntaxe ML                                              == *)
(* == Fichier: parser.mly                                                  == *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

%}
  
%token <int> NUM
%token <string> IDENT
%token LPAR RPAR 

%type <Ast.sexpr> sexpr
%type <Ast.sexpr list> sexprs

%start sexpr             /* the entry point */

%%

  sexpr:
    NUM                       { ASTNum($1) }
  | IDENT                     { ASTId($1) }
  | LPAR sexpr sexprs RPAR    { ASTApp($2, $3) }
  ;
  sexprs :
    sexpr       { [$1] }
  | sexpr sexprs { $1::$2 }
  ;

