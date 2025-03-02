open Printf
open Log

(* Construction du graphe implicite. *)

module G = struct

  include GrapheTaquin.Make(struct
    let n = IO.n()
  end)

  let start =
    try
      pack (IO.read size)
    with Pack msg ->
      die1 "Erreur: %s.\n" msg

end

(* Chargement d'un automate précompilé. *)

let chargement n =
  let filename = Printf.sprintf "automate.%02d" n in
  let canal = open_in_bin filename in
  let automate : int array = input_value canal in
  close_in canal;
  log1 "Automate lu dans le fichier %s.\n" filename;
  automate

(* Numéro de l'automate à charger. *)

let autonum = 13

(* Programme principal. *)

let _ =

  try

    let module Machine = struct
      include Trie.Make(struct let k = 4 end)
      let transition = transition (chargement autonum)
    end in
    let module Search = Idastar.Make(G)(Machine) in
    let path = Search.path() in
    printf "OK %d\n" (List.length path - 1);
    List.iter G.print path

  with Not_found ->
    printf "FAILURE\n"

let _ =
  log1 "Mémoire maximum utilisée: %dK.\n" ((Gc.stat()).Gc.top_heap_words / 256)

