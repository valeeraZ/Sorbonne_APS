/* ========================================================================== */
/* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == */
/* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == */
/* == Analyse des programmes et sémantiques                                == */
/* ========================================================================== */
/* == S-expressions Syntaxe C                                              == */
/* == Fichier: ast.c                                                       == */
/* ==  Arbre de syntaxe abstraite                                          == */
/* ========================================================================== */
#include <stdlib.h>
#include <stdio.h>
#include "ast.h"

Sexpr newASTNum(int v) {
  Sexpr r = mallocSexpr;
  r->tag = ASTNum;
  r->content.num = v;
  return r;
}

Sexpr newASTId(char* v) {
  Sexpr r = mallocSexpr;
  r->tag = ASTId;
  r->content.id = v;
  return r;
}

Sexpr newASTApp(Sexpr e, Sexprs es) {
  Sexpr r = mallocSexpr;
  r->tag = ASTApp;
  r->content.app.fun = e;
  r->content.app.args = es;
  return r;
}

Sexprs addSexpr(Sexpr e, Sexprs es) {
  Sexprs r = mallocSexprs;
  r->head = e;
  r->tail = es;
  return r;
}
