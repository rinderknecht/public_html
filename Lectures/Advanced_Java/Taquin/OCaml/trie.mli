module Make (K : sig

  (* Nombre de symboles de l'alphabet. Les symboles sont 0..(k-1). *)
  val k: int

end) : sig

  (* Une repr�sentation des ensembles de mots. On suppose que deux
     mots distincts dans ne peuvent �tre sous-cha�nes l'un de
     l'autre. *)

  type symbol = int

  type word = symbol list

  type wordset

  (* L'ensemble vide. *)

  val empty: wordset

  (* Insertion. La fonction d�truit l'ensemble fournit et en produit
     un nouveau. *)

  val insert: word -> wordset -> wordset

  (* Affichage du trie au format dot. *)

  val print: wordset -> unit

  (* Compilation d'un ensemble de mots vers un automate fini
     d�terministe qui le reconna�t. *)

  type automaton = int array

  val compile: wordset -> automaton

  (* Interpr�tation de l'automate. La fonction de transition
     produit [None] si le mot lu jusqu'ici a �t� reconnu comme
     appartenant � l'ensemble, sinon elle produit un nouvel
     �tat. *)

  type state = int

  val start: state

  val transition: automaton -> state -> symbol -> state option

  val interprete: automaton -> word -> state option

end
