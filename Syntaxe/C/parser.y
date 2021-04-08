/* ========================================================================== */
/* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == */
/* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == */
/* == Analyse des programmes et sémantiques                                == */
/* ========================================================================== */
/* == S-expressions Syntaxe C                                              == */
/* == Fichier: parser.y                                                    == */
/* ==  Grammaire et génération terme Prolog                                == */
/* == Nota: prend son entrée sur stdin                                     == */
/* ========================================================================== */

%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <math.h>

#include "ast.h"
#include "prologTerm.h"

int yylex (void);
int yyerror (char *);

 Sexpr theExpr;
 
%}

%token<num>  NUM
%token<str>  IDENT
%token       LPAR RPAR

%union {
  int num;
  char* str;
  Sexpr expr;
  Sexprs exprs;
}

%type<expr> expr
%type<exprs> exprs
%type<expr> line

%start line
%%

line: expr    { theExpr = $1; }
  ;

expr:
  NUM                       { $$ = newASTNum($1); }
| IDENT                     { $$ = newASTId($1); }
| LPAR expr exprs RPAR      { $$ = newASTApp($2,$3); }
;
exprs:
  expr       { $$ = addSexpr($1,NULL); }
| expr exprs { $$ = addSexpr($1,$2); }
%%

int yyerror(char *s) {
  printf("error: %s\n",s);
  return 1;
}

int main(int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "usage: ./prologTerm <fichier.aps>\n");
    return 1;
  }
  FILE* infile = freopen(argv[1], "r", stdin);
  yyparse();
  fclose(infile);
  printSexpr(theExpr);
  printf(".\n");
  return 0;
}

