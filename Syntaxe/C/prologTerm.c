/* ========================================================================== */
/* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == */
/* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == */
/* == Analyse des programmes et sémantiques                                == */
/* ========================================================================== */
/* == S-expression Syntaxe C                                               == */
/* == Fichier: prologTerm.c                                                == */
/* == Génération du terme prolog                                           == */
/* ========================================================================== */

#include <stdio.h>
#include "ast.h"
#include "prologTerm.h"

void printNum(int n) {
  printf("num(%d)",n);
}

void printId(char* x) {
  printf("id(%s)",x);
}

void printSexpr(Sexpr e) {
  switch(tagOf(e)) {
  case ASTNum : printNum(getNum(e)); break;
  case ASTId : printId(getId(e)); break;
  case ASTApp : {
    printf("app(");
    printSexpr(getFun(e));
    printf(",[");
    printSexprs(getArgs(e));
    printf("])");
    break;
  }
  }
}

void printSexprs(Sexprs es) {
  if (es) {
    while (es->tail) {
      printSexpr(es->head);
      printf(",");
      es = es->tail;
    };
    printSexpr(es->head);
  }
}

