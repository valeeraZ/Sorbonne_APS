/* ========================================================================== */
/* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == */
/* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == */
/* == Analyse des programmes et sémantiques                                == */
/* ========================================================================== */
/* == S-expressions Syntaxe JAVA                                           == */
/* == Fichier: AstId.java                                                  == */
/* == Arbre de syntaxe abstraite (identificateurs)                         == */
/* ========================================================================== */

public class AstId implements Ast {
	
	String name;
	
	AstId(String x) {
		name = x;
	}

	@Override
	public String toPrologString() {
		return "id("+name+")";
	}

}
