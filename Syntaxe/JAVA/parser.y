/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright (C) 2001 Gerwin Klein <lsf@jflex.de>                          *
 * All rights reserved.                                                    *
 *                                                                         *
 * This is a modified version of the example from                          *
 *   http://www.lincom-asg.com/~rjamison/byacc/                            *
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
/* == APS Syntaxe JAVA                                                     == */
/* == Fichier: parser.y                                                    == */
/* ==  Grammaire                                                           == */
/* ========================================================================== */

%{

  import java.io.*;
  import java.util.ArrayList;

%}
      
%token NL                    /* newline  */
%token <ival> NUM            /* a number */
%token <sval> IDENT          /* an identifier */
%token LPAR RPAR             /* parethesis */

%type <obj> line
%type <obj> sexpr
%type <obj> sexprs

%start line      
%%


line:  sexpr   { prog=(Ast)$1; $$=$1; }
;
      
sexpr:
  NUM                      { $$ = new AstNum($1); }
| IDENT                    { $$ = new AstId($1); }
| LPAR sexpr sexprs RPAR    { $$ = new AstApp((Ast)$2,(ArrayList<Ast>)$3); }
;
sexprs:
  sexpr                   { ArrayList<Ast> r = new ArrayList<Ast>();
                            r.add((Ast)$1);
			    $$ = r; }
| sexprs sexpr            { ((ArrayList<Ast>)$1).add((Ast)$2); $$ = $1; }
;
%%

  public Ast prog;
  
  private Yylex lexer;


  private int yylex () {
    int yyl_return = -1;
    try {
      yylval = new ParserVal(0);
      yyl_return = lexer.yylex();
    }
    catch (IOException e) {
      System.err.println("IO error :"+e);
    }
    return yyl_return;
  }


  public void yyerror (String error) {
    System.err.println ("Error: " + error);
  }


  public Parser(Reader r) {
    lexer = new Yylex(r, this);
  }
