# ==============================================================================
# === UPMC/master/info/4I506 -- Janvier 2016/2017/2018                        ==
# === SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                        ==
# === Analyse des programmes et sémantiques                                   ==
# ==============================================================================
# === S-expressions Syntaxe C                                                 ==
# === Fichier: Makefile                                                       ==
# ==============================================================================

LEX_C = flex
YACC_C = yacc
GCC = gcc

prologTerm: parser ast.h ast.c prologTerm.c
	$(GCC) -c ast.c
	$(GCC) -c prologTerm.c
	$(GCC) -o prologTerm ast.o prologTerm.o lex.yy.o y.tab.o -lm -ll

parser: lexer.lex parser.y
	$(YACC_C) -d parser.y
	$(LEX_C) lexer.lex
	$(GCC) -c lex.yy.c
	$(GCC) -c y.tab.c

clean:
	rm -f lex.yy.*
	rm -f y.tab.*
	rm -f *.o
	rm -f *~
	rm -f prologTerm



