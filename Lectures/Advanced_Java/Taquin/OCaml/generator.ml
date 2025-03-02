open Log
open Permutations

(* Construction du graphe implicite. *)

let n = IO.n()

module G = GrapheTaquin.Make(struct
  let n = n
end)

(* Génération d'une permutation. *)

let sigma =
  permutation (n * n)

let _ =
  log2 "%s: l'état engendré semble %saccessible.\n"
    Sys.argv.(0)
    (if accessible n sigma then "" else "in")

(* Affichage de la permutation. *)

let _ =
  try
    G.print (G.pack sigma)
  with G.Pack msg ->
    die1 "Erreur: %s.\n" msg

