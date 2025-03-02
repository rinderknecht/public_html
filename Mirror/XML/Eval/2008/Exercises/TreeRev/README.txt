En présentant ce sujet, il convient de proposer le petit example d'un
arbre dont les noeuds contiennent des entiers. Montrer comment l'arbre
résultant est obtenu, niveau par niveau. Puis suggérer la construction
d'un patron «flip» faisant appel au patron «rev», niveau par niveau
jusqu'à ce qu'on rencontre un problème. Souligner alors le choix qui
se pose: soit rebrousser chemin, soit fuir en avançant. Dans le second
cas de figure, cela se traduit par la création d'un nouveau patron
dont l'action consiste à appeler récursivement «flip» sur les enfants
d'une séquence de sections, de telle sorte que les parents restent
dans le même ordre. Ainsi, le fait que les parents ont déjà été
inversés par «rev» ne pose plus problème. Dans le premier cas de
figure, on se rend compte qu'il ne faut pas faire appel à «rev» mais
entrelacer la définition de «rev» avec les actions spécifiques à
«flip». Ainsi, on se rend compte que le schéma de calcul est isomorphe
à ceux des patrons «count» et «sum»: un appel récursif sur les frères
et un autre sur les enfants, puis traitement spécifique (dans la
situation présente, il s'agit de reconstruire une copie superficielle
de la première section, contenant l'appel récursif sur ses enfants).
