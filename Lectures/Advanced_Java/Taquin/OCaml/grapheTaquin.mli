(* Ce module propose une pr�sentation implicite du graphe
   correspondant au jeu de taquin. *)

module Make (N : sig

  (* Dimension du jeu de taquin, typiquement 3, 4 ou 5. *)

  val n: int

end) : sig

  (* Nombre de cases du taquin, �gal au carr� de n. *)
  val size: int

  (* Repr�sentation interne des noeuds. *)
  type node

  (* Co�t des ar�tes. *)
  type cost = int

  (* �tiquettes des ar�tes. *)
  type label = int
  val up: label
  val down: label
  val left: label
  val right: label

  (* Traduction d'un �tat, repr�sent� sous forme de
     permutation d'ordre [size], vers une repr�sentation
     interne. *)

  exception Pack of string

  val pack: int array -> node

  (* Affichage d'un �tat. *)
  val print: node -> unit

  (* �tat id�al. *)
  val is_goal: node -> bool

  (* G�n�ration des �tats accessibles � partir d'un
     �tat donn�. Le co�t de chaque ar�te est 1. *)
  val successors: node -> (label -> cost -> node -> unit) -> unit

  (* D�termine si une liste de noeuds repr�sente bien un chemin
     de l'�tat initial fourni (et pr�sent en t�te de liste) vers
     un �tat but. *)

  exception EtatsNonVoisins of node * node
  exception EtatFinalNonBut of node

  val chemin: node list -> unit

  (* Estimation de la distance entre l'�tat fourni et l'�tat id�al.
     L'heuristique utilis�e est la distance de Manhattan. *)
  val estimate: node -> cost

  (* G�n�ration des �tats accessibles � partir d'un
     �tat donn�, avec �valuation incr�mentale de la
     distance de Manhattan. *)
  val successors_estimate: node -> cost -> (label -> cost -> node -> cost -> unit) -> unit

end

