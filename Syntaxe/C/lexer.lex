/* ========================================================================== */
/* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == */
/* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == */
/* == Analyse des programmes et sémantiques                                == */
/* ========================================================================== */
/* == S-expressions Syntaxe C                                              == */
/* == Fichier: lexer.lex                                                   == */
/* == Lexique                                                              == */
/* ========================================================================== */

%{

#include <stdlib.h>

#include "ast.h"  
#include "y.tab.h"


%}

nls "\n"|"\r"|"\r\n"
nums "-"?[0-9]+
idents [a-zA-Z][a-zA-Z0-9]*
%%

[ \t\n\r]  { /* on ignore */ }
"\r\n"     { /* on ignore aussi */ }


"("   { return(LPAR); }
")"   { return(RPAR); }

{nums}    {
            yylval.num=atoi(yytext);
            return(NUM);
          }

{idents}  {
            yylval.str=strdup(yytext);
            return(IDENT);
          }

