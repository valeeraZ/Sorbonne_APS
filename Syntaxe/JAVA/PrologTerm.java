/* ========================================================================== */
/* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == */
/* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == */
/* == Analyse des programmes et sémantiques                                == */
/* ========================================================================== */
/* == S-expression Syntaxe JAVA                                            == */
/* == Fichier: PrologTerm.java                                             == */
/* == Génération terme prolog                                              == */
/* ========================================================================== */
import java.io.*;

class PrologTerm {
    
    public static void main(String args[]) throws IOException {
	
	Parser yyparser;
	Ast prog;
	
	yyparser = new Parser(new InputStreamReader(new FileInputStream(args[0])));
	yyparser.yyparse();
	prog = (Ast) yyparser.yyval.obj;

	if (prog != null)
	    System.out.println((prog.toPrologString())+".\n");
	else
	    System.out.println("Null");
    }

}
