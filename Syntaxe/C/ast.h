/* ========================================================================== */
/* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == */
/* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == */
/* == Analyse des programmes et sémantiques                                == */
/* ========================================================================== */
/* == S-expressions Syntaxe C                                              == */
/* == Fichier: ast.h                                                       == */
/* ==  Arbre de syntaxe abstraite                                          == */
/* ========================================================================== */

typedef struct _sexpr *Sexpr;
typedef struct _sexprs *Sexprs;
typedef enum _tag Tag;

enum _tag {
  ASTNum, ASTId, ASTApp 
};

struct _sexpr {
  Tag tag;
  union {
    int num;
    char* id;
    struct {
      Sexpr fun;
      Sexprs args;
    } app;
  } content;
};

struct _sexprs {
  Sexpr head;
  Sexprs tail;
};

Sexpr newASTNum(int n);
Sexpr newASTId(char* x);
Sexpr newASTApp(Sexpr e, Sexprs es);

Sexprs addSexpr(Sexpr e, Sexprs es);

#define mallocSexpr malloc(sizeof(struct _sexpr))
#define mallocSexprs malloc(sizeof(struct _sexprs))
#define tagOf(r) r->tag
#define getNum(r) r->content.num
#define getId(r) r->content.id
#define getFun(r) r->content.app.fun
#define getArgs(r) r->content.app.args


