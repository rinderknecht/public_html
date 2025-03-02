open Log

(* Détermination du paramètre [n], fourni sur la ligne
   de commande. *)

let n () =
  if Array.length Sys.argv <> 2 then
    die1 "Usage: %s <entier>\n" Sys.argv.(0);
  int_of_string Sys.argv.(1)

(* Conversion d'une chaîne en entier. *)

exception PasUnEntier of string

let s2i x =
  try
    int_of_string x
  with Failure _ ->
    raise (PasUnEntier x)

(* Conversion d'une ligne d'entiers séparés par des blancs en un
   tableau. *)

exception Ligne of int (* attendu *) * int (* fourni *)

let convertit size line =
  let tableau =
    Array.of_list (List.map s2i (Str.split (Str.regexp "[ \t]+") line))
  in
  let l =
    Array.length tableau
  in
  if l <> size then
    raise (Ligne (size, l))
  else
    tableau

(* Lecture sur l'entrée standard d'une ligne d'entiers séparés par des
   blancs. *)

let read size =
  try
    convertit size (input_line stdin)
  with Ligne (size, l) ->
    die2 "Erreur: vous avez fourni %d entiers, j'en attends %d.\n" l size

(* Lecture d'un fichier d'une ligne. *)

let ligne filename =
  let canal = open_in filename in
  let line = input_line canal in
  close_in canal;
  line

(* Lecture sur un canal jusqu'à épuisement. Version inefficace mais
   simple. Paramétré par la fonction de fermeture du canal. *)

let epuise canal ferme =
  let buffer = Buffer.create 16384 in
  try
    let rec loop () =
      Buffer.add_char buffer (input_char canal);
      loop()
    in
    loop()
  with End_of_file ->
    let _ = ferme canal in
    Buffer.contents buffer

(* Lancement d'un processus externe et récupération de sa sortie
   standard. *)

let lance commande =
  epuise (Unix.open_process_in commande) Unix.close_process_in

(* Lancement d'un processus externe, envoi de données sur son entrée
   standard, puis récupération de sa sortie standard. *)

let lance_et_parle commande donnees =
  let lecture, ecriture = Unix.open_process commande in
  output_string ecriture donnees;
  close_out ecriture;
  epuise lecture (fun _ -> Unix.close_process (lecture, ecriture))

(* Commande ls. *)

let ls motif =
  Str.split (Str.regexp "\n") (lance ("/bin/ls " ^ motif))

