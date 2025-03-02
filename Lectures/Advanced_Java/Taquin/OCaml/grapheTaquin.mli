(* Ce module propose une présentation implicite du graphe
   correspondant au jeu de taquin. *)

module Make (N : sig

  (* Dimension du jeu de taquin, typiquement 3, 4 ou 5. *)

  val n: int

end) : sig

  (* Nombre de cases du taquin, égal au carré de n. *)
  val size: int

  (* Représentation interne des noeuds. *)
  type node

  (* Coût des arêtes. *)
  type cost = int

  (* Étiquettes des arêtes. *)
  type label = int
  val up: label
  val down: label
  val left: label
  val right: label

  (* Traduction d'un état, représenté sous forme de
     permutation d'ordre [size], vers une représentation
     interne. *)

  exception Pack of string

  val pack: int array -> node

  (* Affichage d'un état. *)
  val print: node -> unit

  (* État idéal. *)
  val is_goal: node -> bool

  (* Génération des états accessibles à partir d'un
     état donné. Le coût de chaque arête est 1. *)
  val successors: node -> (label -> cost -> node -> unit) -> unit

  (* Détermine si une liste de noeuds représente bien un chemin
     de l'état initial fourni (et présent en tête de liste) vers
     un état but. *)

  exception EtatsNonVoisins of node * node
  exception EtatFinalNonBut of node

  val chemin: node list -> unit

  (* Estimation de la distance entre l'état fourni et l'état idéal.
     L'heuristique utilisée est la distance de Manhattan. *)
  val estimate: node -> cost

  (* Génération des états accessibles à partir d'un
     état donné, avec évaluation incrémentale de la
     distance de Manhattan. *)
  val successors_estimate: node -> cost -> (label -> cost -> node -> cost -> unit) -> unit

end

