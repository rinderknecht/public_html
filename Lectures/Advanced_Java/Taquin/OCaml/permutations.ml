type permutation = int array

let _ = Random.self_init()

(* Engendre une permutation aléatoire des entiers de l'intervalle
   [0..n), représentée par un tableau. *)

let permutation n =
  let sigma = Array.create n 0 in
  for i = 1 to n-1 do
    sigma.(i) <- i
  done;
  for i = 0 to n-2 do
    let j = i + Random.int (n - i) in
    let temp = sigma.(i) in
    sigma.(i) <- sigma.(j);
    sigma.(j) <- temp
  done;
  sigma

(* Permutation identité. *)

let id n =
  let id = Array.create n 0 in
  for i = 1 to n - 1 do
    id.(i) <- i
  done;
  id

(* Compose deux permutations. *)

let compose sigma1 sigma2 =
  let n = Array.length sigma1 in
  assert (n = Array.length sigma2);
  let sigma = Array.create n 0 in
  for i = 0 to n - 1 do
    sigma.(i) <- sigma1.(sigma2.(i))
  done;
  sigma

(* Inverse d'une permutation. *)

let inverse sigma =
  let n = Array.length sigma in
  let sigma' = Array.create n 0 in
  for i = 0 to n - 1 do
    sigma'.(sigma.(i)) <- i
  done;
  sigma'

(* Calcule le nombre d'inversions d'une permutation. *)

let inversions sigma =
  let n = Array.length sigma in
  let inversions = ref 0 in
  for i = 1 to n - 1 do
    for j = 0 to i - 1 do
      if sigma.(j) > sigma.(i) then
	incr inversions
    done
  done;
  !inversions

(* Supprime le zéro d'une permutation, et renvoie donc
   une permutation de n-1 éléments. *)

let supprime_zero sigma =
  let n = Array.length sigma in
  let n' = n - 1 in
  let sigma' = Array.create n' 0 in
  let rec loop i i' =
    if i = n then
      ()
    else if sigma.(i) = 0 then
      loop (i+1) i'
    else begin
      sigma'.(i') <- sigma.(i) - 1;
      loop (i+1) (i'+1)
    end
  in
  loop 0 0;
  sigma'

(* Permutation serpent. Constitue un chemin hamiltonien
   à travers le taquin. *)

let serpent n =
  let serpent = Array.create (n*n) 0 in
  let compteur = ref 0 in
  for i = 0 to n - 1 do
    if i mod 2 = 0 then
      for j = 0 to n - 1 do
	serpent.(!compteur) <- n*i + j;
	incr compteur
      done
    else
      for j = n - 1 downto 0 do
	serpent.(!compteur) <- n*i + j;
	incr compteur
      done
  done;
  serpent

(* Détermine si une permutation est accessible au sens du taquin. *)

let accessible n sigma =
  let size = n * n in
  assert (Array.length sigma = size);
  let serpent =
    serpent n in
  let polarite sigma =
    (inversions (supprime_zero (compose sigma serpent))) mod 2 in
  polarite (id size) = polarite sigma

