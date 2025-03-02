type carte = Carte of ordinaire | Joker
and ordinaire = couleur * figure
and couleur = Coeur | Carreau | Pique | Trefle
and figure = As | Roi | Reine | Valet | Simple of int
;;

fun (x,y) -> match (x,y) with  (Carte z, z) -> true
