type permutation = int array

(* Engendre une permutation aléatoire des entiers de l'intervalle
   [0..n). *)

val permutation: int -> permutation

(* Détermine si une permutation est accessible au sens du taquin. *)

val accessible: int -> permutation -> bool

