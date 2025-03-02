type permutation = int array

(* Engendre une permutation al�atoire des entiers de l'intervalle
   [0..n). *)

val permutation: int -> permutation

(* D�termine si une permutation est accessible au sens du taquin. *)

val accessible: int -> permutation -> bool

