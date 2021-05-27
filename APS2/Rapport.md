# Rapport
*Wenzhuo Zhao, 3971004*
L'implémentation de l'APS est faite jusqu'à l'APS2 qui passe des dizaines tests de pour chaque version antérieure. L'évaluateur, lexer, parser et génération du terme prolog sont réalisés avec OCaml et la partie typage est faite sans doute avec prolog.  
La gestion de mémoire et de contexte est la plus importante partie dans ce projet. Au lieu d'utiliser les primitives comme `Array` dans OCaml, j'ai choisi la structure `liste` qui fait le développement plus facile même si pour ceux qui n'ont pas appris OCaml.
