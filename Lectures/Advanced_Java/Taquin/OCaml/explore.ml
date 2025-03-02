open Log

(* Construction du graphe implicite. *)

module G = struct

  include GrapheTaquin.Make(struct
    let n = 7
  end)

  (* Construction d'un état initial avec trou au centre. *)

  let start =
    let tableau = Array.create size 0 in
    for j = 1 to size - 1 do
      tableau.(j) <- j
    done;
    tableau.(0) <- tableau.(24);
    tableau.(24) <- 0;
    pack tableau

end

(* Affichage d'un symbole et d'un chemin. *)

let affiche_symbole = function
  | 2 -> "L "
  | 3 -> "R "
  | 0 -> "U "
  | 1 -> "D "
  | _ -> assert false

let affiche_chemin chemin =
  List.fold_left (fun accu symbole ->
    accu ^ (affiche_symbole symbole)
  ) "" chemin

(* Calcul de l'étendue d'un chemin. Ce sont les
   coordonnées maximales atteintes au cours du
   parcours dans les quatre directions. *)

let extent path =

  let minx = ref 0
  and maxx = ref 0
  and miny = ref 0
  and maxy = ref 0 in

  let move (x, y) symbol =
    let x, y =
      match symbol with
      | 2 (* left *)  -> x-1, y
      | 3 (* right *) -> x+1, y
      | 0 (* up *)    -> x, y+1
      | 1 (* down *)  -> x, y-1
      | _             -> assert false
    in
    minx := min x !minx;
    maxx := max x !maxx;
    miny := min y !miny;
    maxy := max y !maxy;
    x, y
  in

  let _ = List.fold_left move (0, 0) path in
  !minx, !maxx, !miny, !maxy

(* Comparaison de deux étendues. *)

type comparison = No | Equal | Yes

let printc = function
  | No ->
      "No"
  | Equal ->
      "Equal"
  | Yes ->
      "Yes"

let less i1 i2 =
  if i1 = i2 then
    Equal
  else if i1 < i2 then
    Yes
  else
    No

let et c1 c2 =
  match c1, c2 with
  | No, _
  | _, No ->
      No
  | Equal, Equal ->
      Equal
  | Equal, Yes
  | Yes, Equal
  | Yes, Yes ->
      Yes

let better_extent path1 path2 =
  let (minx1, maxx1, miny1, maxy1) = extent path1 in
  let (minx2, maxx2, miny2, maxy2) = extent path2 in
  et (et (less minx2 minx1) (less maxx1 maxx2))
     (et (less miny2 miny1) (less maxy1 maxy2))

(* Comparaison de deux chemins. En supposant que ces deux chemins
   mènent toujours au même noeud à partir d'une même position de
   départ, le premier est meilleur que le second s'il est plus court
   et si son étendue est inférieure. En cas d'égalité selon ces deux
   critères, le meilleur est déterminé par l'ordre
   lexicographique. *)

let better path1 path2 =
  let c1 = less (List.length path1) (List.length path2)
  and c2 = better_extent path1 path2 in

  match et c1 c2 with
  | No ->
      false
  | Equal ->
      path1 < path2
  | Yes ->
      true

(* Parcours en largeur d'abord, avec borne sur la longueur des chemins
   explorés et automate d'élagage. *)

let duplicates = ref []

module Trie = Trie.Make (struct let k = 4 end)

let parcours automate initial borne =

  let explored =
    ref 0 in

  let queue =
    Queue.create () in

  let table =
    Hashtbl.create 10001 in

  Queue.add (G.start, 0, [], initial) queue;
  Hashtbl.add table G.start [ [] ];

  try
    while true do
      let node, depth, path, state = Queue.take queue in
      incr explored;
      let depth = depth + 1 in
      G.successors node (fun label cost son ->
	(* Tentons d'effectuer une transition. *)
	match automate state label with
	| None ->
	    (* Refus. *)
	    ()
	| Some state ->
	    (* Chemins représentés avec le coup le plus récent en tête. *)
	    let path = label :: path in
	    try
	      let paths = Hashtbl.find table son in
	      Hashtbl.replace table son (path :: paths)
	    with Not_found ->
	      Hashtbl.add table son [ path ];
	      if depth < borne then
		Queue.add (son, depth, path, state) queue 
      )
    done;
    assert false
  with Queue.Empty ->

    log2 "Parcours à la profondeur %d terminé -- exploré %d noeuds.\n" borne !explored;

    (* Parcours terminé, reconnaissance des chemins doublons. *)

    let doublons =
      ref 0 in

    Hashtbl.iter (fun _ paths ->
      if List.length paths > 1 then begin

	(* TEMPORARY *)
	log1 "Nombre de chemins menant à ce noeud: %d.\n" (List.length paths);
	List.iter (fun path ->
	  log1 "%s\n" (affiche_chemin path);
        ) paths;

	List.iter (fun path ->
	  if List.exists (fun p -> better p path) paths then begin
	    duplicates := (List.rev path) :: !duplicates;
	    incr doublons
	  end
	) paths

      end

    ) table;

    log1 "%d nouveaux doublons découverts.\n" !doublons;

    (* Construction de l'automate reconnaissant ces chemins. *)

    let mots = List.fold_left (fun mots mot ->
      Trie.insert mot mots
    ) Trie.empty !duplicates in

    let automate = Trie.compile mots in

    log1 "Construit un automate à %d états.\n" (Array.length automate / 5);

    automate

(* Sauvegarde de l'automate dans un fichier externe. *)

let sauvegarde automate n =
  let filename = Printf.sprintf "automate.%02d" n in
  let canal = open_out_bin filename in
  output_value canal automate;
  close_out canal;
  log1 "Automate sauvegardé dans le fichier %s.\n" filename

(* Parcours successifs à profondeur accrue. *)

let n = IO.n()

let rec boucle automate initial borne =
  let automate = parcours automate initial borne in
  sauvegarde automate borne;
  let borne = borne + 1 in
  if borne <= n then
    boucle (Trie.transition automate) Trie.start borne
  else
    automate

(* Amorçage avec un automate trivial. *)

let _ =
  boucle
    (fun state label -> Some 0)
    0
    2
