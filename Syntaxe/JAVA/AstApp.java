/* ========================================================================== */
/* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == */
/* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == */
/* == Analyse des programmes et sémantiques                                == */
/* ========================================================================== */
/* == APS Syntaxe JAVA                                                     == */
/* == Fichier: AstApp.java                                                 == */
/* == Arbre de syntaxe abstraite (applications)                            == */
/* ========================================================================== */

import java.util.ArrayList;

public class AstApp implements Ast {

    Ast fun;
    ArrayList<Ast> args;
    
    AstApp(Ast e, ArrayList<Ast> es) {
	this.fun = e;
	this.args = es;
    }
    
    public String toPrologString() {
	String r = "";
	r = "app("+fun.toPrologString()+",[";
	for(int i=0; i < args.size()-1; i++)
	    r += args.get(i).toPrologString()+",";
	r += args.get(args.size()-1).toPrologString();
	r += "])";
	return r;
    }

}
