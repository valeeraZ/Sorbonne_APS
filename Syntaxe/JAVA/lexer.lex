/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright (C) 2000 Gerwin Klein <lsf@jflex.de>                          *
 * All rights reserved.                                                    *
 *                                                                         *
 * Thanks to Larry Bell and Bob Jamison for suggestions and comments.      *
 *                                                                         *
 * License: BSD                                                            *
 *                                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* ========================================================================== */
/* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == */
/* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == */
/* == Analyse des programmes et sémantiques                                == */
/* ========================================================================== */
/* == S-expressions Syntaxe JAVA                                           == */
/* == Fichier: lexer.lex                                                   == */
/* ==  Lexique                                                             == */
/* ========================================================================== */

%%

%byaccj

%{
  private Parser yyparser;

  public Yylex(java.io.Reader r, Parser yyparser) {
    this(r);
    this.yyparser = yyparser;
  }
%}

nums = -?[0-9]+
ident = [a-z][a-zA-Z0-9]*
nls  = \n | \r | \r\n

%%

/* parenthesis */
"("  { return Parser.LPAR; }
")"  { return Parser.RPAR; }

/* newline */
{nls}   { return 0; } //{ return Parser.NL; }

/* float */
{nums}  { yyparser.yylval = new ParserVal(Integer.parseInt(yytext()));
         return Parser.NUM; }

{ident} { yyparser.yylval = new ParserVal(yytext());
  return Parser.IDENT;
}

/* whitespace */
[ \t]+ { }

\b     { System.err.println("Sorry, backspace doesn't work"); }

/* error fallback */
[^]    { System.err.println("Error: unexpected character '"+yytext()+"'"); return -1; }
