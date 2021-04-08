# ==============================================================================
# === UPMC/master/info/4I506 -- Janvier 2016/2017/2018                        ==
# === SU/FSI/master/info/MU4IN503 -- Janvier 2020                             ==
# === Analyse des programmes et sémantiques                                   ==
# ==============================================================================
# === APS Syntaxe JAVA                                                        ==
# === Fichier: Makefile                                                       ==
# ==============================================================================

LEX_J  = jflex 
YACC_J = ~/tmp/yacc.macosx -J
JAVAC = javac

prologTerm: parser PrologTerm.java
	$(JAVAC) PrologTerm.java

parser: parser.y lexer.lex
	$(LEX_J) lexer.lex
	$(YACC_J) parser.y

clean:
	rm -f Parser*.java
	rm -f Yylex.java
	rm -f *.class
	rm -f *~
