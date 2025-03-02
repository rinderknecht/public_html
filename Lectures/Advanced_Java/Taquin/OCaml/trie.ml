module Make (K : sig

  (* Nombre de symboles de l'alphabet. Les symboles sont 0..(k-1). *)
  val k: int

end) = struct

  open K

  type symbol = int

  type word = symbol list

  (* Une représentation simple pour des tries de degré k. Les états portent
     des numéros d'identification entiers pour faciliter ensuite la 
     construction de l'automate. Les noeuds internes portent un tableau
     de leurs k successeurs. Les noeuds finaux ne sont jamais internes,
     car l'analyse stoppera de toute façon dès que l'on aura reconnu un
     mot. *)

  type trie =
    | Empty                    (* Trie vide. Ne reconnaît aucun mot. *)
    | Final of int             (* État final. Accepte le mot vide. *)
    | Node of int * trie array (* État interne. N'accepte pas le mot vide. *)

  (* Le point de vue de l'utilisateur. On empaquète un trie et l'état de
     l'allocateur d'entiers. *)

  type wordset =
    trie * int

  (* L'ensemble vide. *)

  let empty = (Empty, 0)

  (* Insertion. La fonction détruit l'ensemble fournit et en produit
     un nouveau. *)

  let insert word (trie, numero) =

    let compteur =
      ref numero in

    let alloue () =
      let numero = !compteur in
      compteur := numero + 1;
      numero
    in

    let rec insert word trie =
      match word, trie with
      | [], Empty ->
	  Final (alloue())
      | [], Node _
      | _, Final _ ->
	  (* On ne devrait jamais insérer un mot qui est sous-chaîne
	     d'un mot déjà inséré. *)
	  assert false
      | _ :: _, Empty ->
	  insert word (Node (alloue(), Array.create k Empty))
      | letter :: word, Node (_, sons) ->
	  sons.(letter) <- insert word sons.(letter);
	  trie
    in

    let trie = insert word trie in
    trie, !compteur

  (* Identité d'un noeud. *)

  let identity = function
    | Empty ->
	assert false
    | Final id
    | Node (id, _) ->
	id

  (* Affichage du trie au format dot. *)

  open Printf

  let print (trie, _) =
    printf "digraph G {\n";
    let rec visit pere lettre = function
      | Empty ->
	  ()
      | Final id ->
	  printf "n%d [ label=\"%d\" ] ;\n" id id;
	  if pere <> -1 then
	    printf "n%d -> n%d [ label=\"%d\" ] ;\n" pere id lettre;
      | Node (id, sons) as node ->
	  printf "n%d [ label=\"%d\" ] ;\n" id id;
	  if pere <> -1 then
	    printf "n%d -> n%d [ label=\"%d\" ] ;\n" pere id lettre;
	  Array.iteri (visit id) sons
    in
    visit (-1) (-1) trie;
    printf "}\n"

  (* Transformation d'un trie en automate fini déterministe.
     L'algorithme est semblable à celui d'Aho et Corasick.
     L'arbre est transformé en place de façon à ce que toutes
     les transitions soient définies. Aucun noeud nouveau n'est
     ajouté, uniquement des boucles de retour en arrière. *)

  let transform racine =

    (* Calcul de la fonction d'échec, au sens d'Aho et Corasick.
       Pour chaque état, cette fonction indique quelle epsilon-
       transition effectuer lorsque qu'aucune transition explicite
       n'existe pour le caractère courant. *)

    (* La table [failure] représente la fonction d'échec. Elle ne
       contient que des noeuds internes [Node], car [Empty] ne
       représente pas un noeud et un noeud [Final] ne peut être dans
       l'image de la fonction [failure] du fait qu'un mot reconnu ne
       peut être sous-chaîne stricte d'un mot reconnu.

       L'image d'un noeud par [failure] est soit la racine soit un
       noeud pour lequel [failure] est également déjà définie; de
       plus, il est toujours situé à une profondeur strictement
       inférieure vis-à-vis du noeud fourni. *)

    let failure =
      Hashtbl.create 10001 in

    (* [follow] accepte un noeud interne [node], pour lequel [failure] a
       déjà été calculée ou bien qui est la racine, et situé à une
       profondeur p telle que [failure] a été déjà été calculée pour
       tous les noeuds (excepté la racine) jusqu'à la profondeur p+1; un
       symbole [letter]; et renvoie le noeud successeur de [node] selon le
       symbole [letter], en se basant sur la fonction [failure] si ce
       successeur n'est pas défini. Ce noeud résultat est toujours un
       noeud interne pour lequel [failure] a déjà été calculée, et situé
       à une profondeur accrue au plus de 1 vis-à-vis du noeud
       fourni. *)

    let rec follow node letter =
      match node with
      | Empty
      | Final _ ->
	  assert false
      | Node (id, sons) ->
	  match sons.(letter) with
	  | Empty ->
	      (* Ce noeud a un successeur non défini, donc ce n'est
		 pas la racine, donc sa fonction [failure] a déjà été
		 calculée. *)
	      follow (Hashtbl.find failure id) letter
	  | Final _ ->
	      (* Un mot reconnu est sous-chaîne d'un autre mot reconnu. *)
	      assert false
	  | Node _ as son ->
	      (* Le successeur est défini. *)
	      son
    in

    (* Queue pour parcours en largeur de l'arbre. Cette queue ne
       contiendra que des noeuds internes pour lesquels [failure]
       a déjà été calculée. *)

    let queue =
      Queue.create() in

    match racine with
    | Empty 
    | Final _ ->
	(* Le trie est vide ou accepte la chaîne vide. Ça devrait
	   être rare en pratique. Je n'implémente pas. *)
	assert false
    | Node (id_racine, sons_racine) ->

	(* Initialisation du parcours en largeur. On insère dans la
	   queue tous les noeuds internes à profondeur 1, et on leur
	   attribue la racine comme valeur de la fonction d'échec.
	   On complète également le noeud racine pour que tous ses
	   successeurs soient définis, en créant des self-loops.
	   Cela garantit que l'on ne tentera jamais d'évaluer la
	   fonction [failure] pour la racine. *)

	Array.iteri (fun letter son ->
	  match son with
	  | Empty ->
	      sons_racine.(letter) <- racine;
	  | Final _ ->
	      ()
	  | Node (id, _) ->
	      Queue.add son queue;
	      Hashtbl.add failure id racine
	) sons_racine;

	(* Parcours. *)

	try
	  while true do
	    match Queue.take queue with
	    | Empty | Final _ ->
		assert false
	    | Node (id, sons) ->

		(* Nous avons pris un noeud interne, pour lequel [failure] a déjà
		   été calculée, dans la queue. Soit [fail] son image. *)

		let fail = Hashtbl.find failure id in

		Array.iteri (fun letter son ->
		  match son with
		  | Empty
		  | Final _ ->
		      ()
		  | Node (id_son, _) ->

		      (* Nous avons trouvé un successeur qui est lui-même
			 un noeud interne. Nous l'ajoutons à la queue,
			 pour continuer le parcours, et nous calculons
			 son image dans la fonction [failure]. *)

		      Queue.add son queue;
		      Hashtbl.add failure id_son (follow fail letter)
		) sons
	  done
	with Queue.Empty ->

	  (* Nouvel et dernier parcours en largeur de l'arbre, pour
	     calculer la fonction de transition de l'automate
	     déterministe. La queue ne contiendra cette fois que des
	     noeuds internes pour lesquels la fonction de transition
	     a déjà été calculée. *)

	  (* Initialisation du parcours en largeur. On insère dans la
	     queue tous les noeuds internes à profondeur 1. *)

	  Array.iter (fun son ->
	    match son with
	    | Empty ->
		assert false
	    | Final _ ->
		()
	    | Node (id, _) ->
		if id <> id_racine then
		  Queue.add son queue
	  ) sons_racine;

	  try
	    while true do
	      let node = Queue.take queue in
	      match node with
	      | Empty
	      | Final _ ->
		  assert false
	      | Node (id, sons) ->

		  (* Nous avons pris un noeud interne, pour lequel la
		     fonction de transition a déjà été calculée, dans la
		     queue. *)

		  Array.iteri (fun letter son ->
		    match son with
		    | Final _ ->
			()
		    | Node _ ->
			Queue.add son queue
		    | Empty ->

			(* Nous avons trouvé une transition non encore
			   définie. Nous la définissons en composant une
			   transition par [failure], ce qui nous donne
			   un noeud moins profond, et une transition
			   standard -- déjà calculée -- issue de ce
			   noeud. *)

			sons.(letter) <-
			  match Hashtbl.find failure id with
			  | Empty
			  | Final _ ->
			      assert false
			  | Node (_, sons) ->
			      sons.(letter)

		  ) sons
	    done
	  with Queue.Empty ->

	    (* C'est terminé! *)

	    ()

  (* Itération sur un arbre complété. *)

  let iter f arbre =

    let vu =
      Hashtbl.create 10001 in

    let nouveau id =
      try
	Hashtbl.find vu id;
	false
      with Not_found ->
	Hashtbl.add vu id ();
	true
    in

    let rec traverse node =
      match node with
      | Empty ->
	  assert false
      | Final id ->
	  if nouveau id then
	    f id node
      | Node (id, sons) ->
	  if nouveau id then begin
	    f id node;
	    Array.iter traverse sons
	  end
    in

    traverse arbre

  (* Transformation de l'arbre complété en un tableau. *)

  let tableau (arbre, taille) =

    let tableau =
      Array.create ((k + 1) * taille) 0 in

    iter (fun id node ->
      match node with
      | Empty ->
	  assert false
      | Final _ ->
	  tableau.((k + 1) * id + k) <- 1
      | Node (_, sons) ->
	  Array.iteri (fun letter son ->
	    match son with
	    | Empty ->
		assert false
	    | Final id_son
	    | Node (id_son, _) ->
		tableau.((k + 1) * id + letter) <- id_son
	  ) sons
    ) arbre;

    tableau

  (* Compilation d'un arbre en en automate. *)

  type automaton = int array
 
  let compile (trie, taille) =
    transform trie;
    tableau (trie, taille)

  (* Interprétation de l'automate. *)

  type state = int

  let start = 0

  let transition tableau etat lettre =
    (* Transition. *)
    let etat = tableau.((k + 1) * etat + lettre) in
    (* Test d'acceptation. *)
    if tableau.((k + 1) * etat + k) = 1 then
      None
    else
      Some etat

  let rec interprete tableau mot =
    let rec loop etat mot =
      match mot with
      | [] ->
	  Some etat
      | lettre :: mot ->
	  match transition tableau etat lettre with
	  | None ->
	      None
	  | Some etat ->
	      loop etat mot
    in
    loop start mot

end

