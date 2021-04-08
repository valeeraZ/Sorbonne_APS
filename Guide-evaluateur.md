*** Etape 0 : chaine d'interpretation ***

- s'assurer qu'on sait écrire, dans le langage choisi, une chaine d'interprétation qui:
    - prend en entrée un programme écrit en APS,
    - lance l'analyse syntaxique du programme et produit un AST,
    - projette l'AST en Prolog, appelle le typeur et récupère le résultat de typage (et arrête la chaîne si nécessaire),
    - lance l'évaluateur.

*** Etape 1 : langage d'expressions d'ordre 0 ***

- définir un type pour les *valeurs* (qui dans un premier temps, correspondent uniquement aux valeurs immédiates)
- écrire une fonction d'évaluation d'expression récursive de type `AST -> Valeur` qui transforme des expressions du type `3 + (2 * 2)` (entiers et booléens composés avec des primitives) en leur valeur.

*** Etape 2 : langage d'expressions avec identifiants ***

- définir un type pour les *contextes d'évaluation*, qui associent des valeurs aux identifiants
- modifier la fonction d'évaluation d'expressions pour qu'elle soit de type `Contexte -> AST -> Valeur` et qu'elle puisse évaluer `3 + (2 * x)` dans un contexte dans lequel `x` est associé à `42`
- écrire une fonction d'évaluation de programmes qui évalue un programme APS0 sans fonction (uniquement des définitions de constantes et des échos), en faisant en sorte que les définitions **ajoutent de l'information** à un contexte initialement vide.

*** Etape 3 : fonctions simples ***

- ajouter à la définition du type des valeurs un constructeur pour les *fermetures*. Sans utiliser les lambda du langage de l'évaluateur, il suffit d'un triplet (environnement, liste des paramètres formels, corps de la fonction) pour stocker les informations dont on a besoin.
- modifier la fonction d'évaluation de programmes pour prendre en compte les définitions de fonctions.
- modifier la fonction d'évaluation d'expression pour prendre en compte l'appel de fonction.
- écrire des exemples qui vérifie qu'on a bien une *liaison statique*

*** Etape 4 : fonctions récursives ***

- ajouter à la définition du type des valeurs un constructeur pour les *fermetures récursives*. 
- prendre en compte la définition et l'appel a des fonctions récursives.