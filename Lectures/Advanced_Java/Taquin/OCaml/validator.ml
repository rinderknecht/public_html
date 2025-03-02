open Log
open Printf

(* Construction du graphe implicite. *)

let n = IO.n()

module G = GrapheTaquin.Make(struct
  let n = n
end)

(* Analyse d'un résumé (en provenance d'un fichier de référence). *)

let analyse_resume resume =
  if resume = "FAILURE" then
    None
  else if (Str.string_match (Str.regexp "OK \\([0-9]+\\)") resume 0) then
    Some (int_of_string (Str.matched_group 1 resume))
  else
    assert false

(* Analyse de la sortie du programme taquin. *)

let analyse_sortie reussites etat_initial resume_attendu sortie =
  match List.filter (fun line -> line <> "") (Str.split (Str.regexp "\n") sortie) with
  | [] ->
      "*** échec *** n'a rien affiché"
  | resume :: etats ->
      if resume_attendu <> resume then
	sprintf "*** échec *** a affiché %s au lieu de %s" resume resume_attendu
      else
	match analyse_resume resume with
	| None ->
	    incr reussites;
	    "réussi"
	| Some longueur_attendue ->
	    let longueur = List.length etats - 1 in
	    if longueur_attendue <> longueur then
	      sprintf "*** échec *** a affiché un chemin de longueur %d au lieu de %d" longueur longueur_attendue
	    else
	      try
		let chemin = List.map (fun etat ->
		  G.pack (IO.convertit G.size etat)
                ) etats in
		G.chemin chemin;
		if List.hd chemin = etat_initial then begin
		  incr reussites;
		  "réussi"
		end
		else
		  "*** échec *** n'a pas respecté l'état initial"
	      with
	      |	IO.Ligne (size_attendu, size) ->
		  sprintf "*** échec *** a affiché un état formé de %d entiers au lieu de %d" size size_attendu
	      |	IO.PasUnEntier x ->
		  sprintf "*** échec *** a affiché un état incompréhensible: \"%s\" n'est pas un entier" x
	      |	G.Pack msg ->
		  sprintf "*** échec *** a affiché un état incompréhensible: %s" msg
	      |	G.EtatsNonVoisins (etat1, etat2) ->
		  "*** échec *** est passé d'un état à un état non voisin"
	      |	G.EtatFinalNonBut etat ->
		  "*** échec *** n'a pas respecté l'état final"

(* Ouverture des fichiers tests. *)

let master_directory =
  "../rendus/depot_sources"

let test_one_student (directory, executable) =
  let temps = ref 0. in
  let reussites = ref 0 in
  let tests = ref 0 in
  List.iter (fun test ->
    log1 "Test: %s\n" test;
    let etat_initial = G.pack (IO.convertit G.size (IO.ligne test)) in
    let resume_attendu = IO.ligne (test ^ ".ref") in
    let debut = Unix.time() in
    let command = sprintf "cat %s | (cd %s/%s; %s)" test master_directory directory executable in
    log1 "Lancement: %s\n" command;
    let sortie = IO.lance command in
    let fin = Unix.time() in
    log1 "Temps écoulé: %.f secondes.\n" (fin -. debut);
    temps := !temps +. fin -. debut;
    incr tests;
    let resultat = analyse_sortie reussites etat_initial resume_attendu sortie in
    log1 "Résultat: %s\n" resultat
  ) (IO.ls "test/*.[0-9][0-9][0-9]");
  log1 "Temps total écoulé: %.f secondes.\n" !temps;
  log2 "Réussites: %d/%d\n" !reussites !tests

(* Itération sur tous les étudiants. *)

let () =
  List.iter test_one_student [
(*
    "../../code/", "./taquin 4"; (* le mien *)
    "simon.regnard/archive/src", "java Lancement";
    "audrey.dallamaggiore/archive/", "./tourne";
    "karina.cucchi/archive/src", "java Test"; 
    "sebastien.deprez/archive/src", "java Test";
    "karl.neuberger/archive", "./tourne";
    "adrien.ball/archive", "./tourne"
    "thomas.humeau", "./tourne"
    "francois.sevaistre/archive/src", "java test"
    "william.lucas/archive", "timeout 60 java Test"
    "vincent.flament", "java Calcul"
    "thomas.zaepffel/archive/src", "timeout 60 java Test"
    "aurelien.nober", "timeout 60 java Test"
    "william.lucas.bugfix/archive", "java Test"
*)
    "../../code/", "./taquin 4"; (* le mien *)
  ]

