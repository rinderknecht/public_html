open Printf
open Log

module Make (N : sig

  (* Dimension du jeu de taquin. *)

  val n: int

end) = struct

  open N

  (* Nombre de carrés. *)

  let size = n * n

  (* Représentation interne des noeuds. Le tableau donne, pour chaque
     case du jeu, le numéro de la pièce qui s'y trouve. *)

  type node = int array (* de taille [size] *)

  (* Coût des arêtes. *)

  type cost = int

  (* Étiquettes des arêtes. *)

  type label = int
 
  let up = 0
  let down = 1
  let left = 2
  let right = 3

  (* Recherche de la position du trou dans un tableau. *)

  let blank tableau : int =
    let rec loop j =
      if tableau.(j) = 0 then
	j
      else
	loop (j + 1)
    in
    loop 0

  (* Conversion d'un état externe vers un état interne. Implémentée
     comme l'identité, mais avec vérification que le tableau fourni
     représente un état valide. *)

  exception Pack of string

  let fail1 format x1 =
    raise (Pack (sprintf format x1))

  let fail2 format x1 x2 =
    raise (Pack (sprintf format x1 x2))

  let pack tableau =
    let l = Array.length tableau in
    if l <> size then
      fail2 "l'état est formé de %d entiers au lieu de %d" l size;
    let count = Array.create size 0 in
    for j = 0 to size - 1 do
      let tile = tableau.(j) in
      if (tile < 0) or (tile >= size) then
	fail1 "Erreur: le numéro %d est invalide.\n" tile;
      count.(tile) <- count.(tile) + 1
    done;
    for j = 0 to size - 1 do
      if count.(j) = 0 then
	fail1 "Erreur: la tuile %d n'apparaît pas dans l'état proposé.\n" j;
      if count.(j) > 1 then
	fail1 "Erreur: la tuile %d apparaît plusieurs fois dans l'état proposé.\n" j;
    done;
    tableau

  (* Affichage d'un état. *)

  let print tableau =
    for j = 0 to size - 1 do
      printf "%d " tableau.(j)
    done;
    printf "\n"

  (* État idéal. *)

  let goal =
    let tableau = Array.create size 0 in
    for j = 1 to size - 1 do
      tableau.(j) <- j
    done;
    tableau

  let is_goal i =
    i = goal

  (* Pré-calcul de la distance de Manhattan. Pour chaque tuile
     et pour chaque position, on calcule la distance entre la
     position idéale de cette tuile et la position fournie. *)

  let manhattan =
    Array.make (size * size) 0

  let _ =
    for tile = 1 to size - 1 do
      for position = 0 to size - 1 do
	manhattan.(size * tile + position) <-
	  abs(tile mod n - position mod n) +
	  abs(tile / n - position / n)
      done
    done

  let manhattan_estimate tableau =
    let rec loop j distance =
      let tile = tableau.(j) in
      let distance =
	if tile = 0 then
	  distance
	else
	  distance + manhattan.(size * tile + j)
      in
      let j = j + 1 in
      if j < size then
	loop j distance
      else
	distance
    in
    loop 0 0

  (* Linear Conflicts heuristics. *)

  (* Adds a tile to a list of tiles annotated with their conflict
     counts. *)
  let add tile1 row =
    let num1 = ref 0 in
    let row = List.map (fun (tile2, num2) ->
      if tile1 > tile2 then begin
	incr num1;
	(tile2, num2 + 1)
      end
      else
	(tile2, num2)
    ) row in
    (tile1, !num1) :: row

  (* Returns the tile that has the greatest conflict count, as
     well as its conflict count. *)
  let rec maxima maxi maxv = function
    | [] ->
	maxi, maxv
    | (i, v) :: rest ->
	if v > maxv then
	  maxima i v rest
	else
	  maxima maxi maxv rest

  (* Takes a list of tiles; annotates each tile with its conflict
     count; removes the tile that has the greatest conflict count;
     and starts over; and returns the total number of tiles that
     had to be moved, times 2. *)
  let rec lc accu row =
    let i, v = maxima (-1) (-1) (List.fold_right add row []) in
    if v > 0 then
      lc (accu + 2) (List.filter (fun x -> x <> i) row)
    else
      accu

  (* Encoding a list of integers into an integer. The encoding
     does keep track of leading zeros in the list, but we do not
     care about them because they do not contribute to conflicts
     anyway. *)

  let rec encode accu = function
    | [] ->
	accu
    | digit :: rest ->
	encode (n * accu + digit) rest

  let rec decode accu = function
    | 0 ->
	accu
    | x ->
	decode ((x mod n) :: accu) (x / n)

  (* Precomputing conflicts. *)

  let rec npower k =
    if k = 0 then 1 else n * npower (k-1)

  let limit =
    npower n

  let lctable =
    Array.create limit 0

  let _ =
    for i = 0 to limit - 1 do
      lctable.(i) <- lc 0 (decode [] i)
    done

  (* Turns a state row into an (encoded) list of tiles. Keeps only
     physical tiles that are in their goal row. *)
  let rec row2row accu state i j =
    if j = n then
      accu
    else
      let position = n * i + j in
      let tile = state.(position) in
      let accu =
	if (tile <> 0) && (tile / n = i) then
	  n * accu + tile mod n
	else
	  accu
      in
      row2row accu state i (j + 1)

  (* Turns a state column into an (encoded) list of tiles. Keeps only
     physical tiles that are in their goal column. *)
  let rec column2row accu state i j =
    if i = n then
      accu
    else
      let position = n * i + j in
      let tile = state.(position) in
      let accu =
	if (tile <> 0) && (tile mod n = j) then
	  n * accu + tile / n
	else
	  accu
      in
      column2row accu state (i + 1) j

  (* Linear conflicts heuristic for a given column or row. *)

  let conflicts_row state i =
    lctable.(row2row 0 state i 0)

  let conflicts_column state j =
    lctable.(column2row 0 state 0 j)

  (* Returns the linear conflicts heuristic for a state. This
     can be added to the Manhanttan distance. *)
  let linear_conflicts_estimate state =
    let num = ref 0 in
    for i = 0 to n - 1 do
      num := !num + conflicts_row state i + conflicts_column state i
    done;
    !num

  (* Total estimate. *)

  let estimate state =
    manhattan_estimate state + linear_conflicts_estimate state

  (* Génération des états accessibles à partir d'un
     état donné. *)

  let successors tableau f =

    (* Déterminer la position du trou. *)

    let blank = blank tableau in

    (* Déterminer les états voisins. *)

    let move label blank' =
      let tableau' = Array.copy tableau in
      tableau'.(blank) <- tableau.(blank');
      tableau'.(blank') <- 0;
      f label 1 tableau' in

    if (blank > n - 1) then
      (* Le trou n'est pas sur la ligne du haut. Il peut monter. *)
      move up (blank - n);

    if (blank mod n > 0) then
      (* Le trou n'est pas sur la ligne de gauche. Il peut aller à gauche. *)
      move left (blank - 1);

    if (blank mod n < n - 1) then
      (* Le trou n'est pas sur la ligne de droite. Il peut aller à droite. *)
      move right (blank + 1);

    if (blank < size - n) then
      (* Le trou n'est pas sur la ligne du bas. Il peut descendre. *)
      move down (blank + n)

  (* Génération des états accessibles à partir d'un état donné,
     avec évaluation incrémentale de l'estimation de coût. *)

  let successors_estimate tableau estimate f =

    (* Déterminer la position du trou. *)

    let blank = blank tableau in

    (* Déterminer les états voisins. *)

    let move label blank' =
      let tile = tableau.(blank') in
      let tableau' = Array.copy tableau in
      tableau'.(blank) <- tile;
      tableau'.(blank') <- 0;
      let row = blank / n
      and column = blank mod n
      and row' = blank' / n
      and column' = blank' mod n in
      let estimate' =
	estimate
 	  - manhattan.(size * tile + blank') 
	  + manhattan.(size * tile + blank)
      in
      let estimate' =
	if row = row' then
	  estimate'
            - conflicts_column tableau column
            - conflicts_column tableau column'
	    + conflicts_column tableau' column
	    + conflicts_column tableau' column'
	else
	  estimate'
            - conflicts_row tableau row
            - conflicts_row tableau row'
	    + conflicts_row tableau' row
	    + conflicts_row tableau' row'
      in
      f label 1 tableau' estimate' in

    if (blank > n - 1) then
      (* Le trou n'est pas sur la ligne du haut. Il peut monter. *)
      move up (blank - n);

    if (blank mod n > 0) then
      (* Le trou n'est pas sur la ligne de gauche. Il peut aller à gauche. *)
      move left (blank - 1);

    if (blank mod n < n - 1) then
      (* Le trou n'est pas sur la ligne de droite. Il peut aller à droite. *)
      move right (blank + 1);

    if (blank < size - n) then
      (* Le trou n'est pas sur la ligne du bas. Il peut descendre. *)
      move down (blank + n)

  (* Détermine si deux états sont voisins. Code pas spécialement
     efficace, mais simple. *)

  exception Voisins
  exception EtatsNonVoisins of node * node

  let voisins tableau1 tableau2 =
    try
      successors tableau1 (fun _ _ voisin ->
	if voisin = tableau2 then
	  raise Voisins
      );
      raise (EtatsNonVoisins (tableau1, tableau2))
    with Voisins ->
      ()

  (* Détermine si une liste de noeuds représente bien un chemin
     de l'état initial fourni (non présent dans la liste) vers
     un état but. *)

  exception EtatFinalNonBut of node

  let rec chemin precedent = function
    | [] ->
	if not (is_goal precedent) then
	  raise (EtatFinalNonBut precedent)
    | etat :: suite ->
	voisins precedent etat;
	chemin etat suite

  (* Détermine si une liste de noeuds représente bien un chemin
     de l'état initial fourni (et présent en tête de liste) vers
     un état but. *)

  let chemin = function
    | [] ->
	assert false
    | initial :: suite ->
	chemin initial suite

end
