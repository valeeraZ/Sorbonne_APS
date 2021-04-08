/* ========================================================================== */
/* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == */
/* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == */
/* == Analyse des programmes et sémantiques                                == */
/* ========================================================================== */
/* == S-expressions Syntaxe JAVA                                           == */
/* == Fichier: AstNum.java                                                 == */
/* == Arbre de syntaxe abstraite (constantes numériques)                   == */
/* ========================================================================== */

public class AstNum implements Ast {
	
	Integer val;
	
	AstNum(Integer n) {
		val = n;
	}

	@Override
	public String toPrologString() {
		return ("num("+val+")");
	}

}
