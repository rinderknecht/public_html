(* Corrigé des exercices de programmation du chapitre 3 *)

(* Le code donné ci-dessous est très (trop) fidèle à la description donnée
   dans les notes de cours: il utilise une représentation inefficace des
   substitutions, effectue l'occur-check de façon coûteuse, calcule l'ensemble
   des variables libres dans l'environnement de façon naïve, etc. On peut
   implémenter l'inférence de types pour ML de façon beaucoup plus efficace,
   mais la correspondance avec les règles de typage est alors plus difficile
   à justifier. Ce pourrait être le sujet d'un cours plus avancé. *)

(********** Exercice 3.1: propagation des types pour ML monomorphe *********)

(* L'algèbre de types (comme au chapitre 1) *)

type exprtype =
    Type_base of string
  | Type_var of string
  | Type_flèche of exprtype * exprtype
  | Type_produit of exprtype * exprtype

(* Type des opérateurs (monomorphes) *)

let type_opérateur op =
  match op with
    "+" | "-" | "*" | "/" ->
      Type_flèche(Type_produit(Type_base "int", Type_base "int"),
                  Type_base "int")
  | "fix" ->
      let int_f_int = Type_flèche(Type_base "int", Type_base "int") in
      Type_flèche(Type_flèche(int_f_int, int_f_int), int_f_int)
  | _ ->
      assert false (* opérateur non supporté *)

module Exercice31 = struct

(* La syntaxe abstraite des expressions mini-ML avec annotation de type
   sur les paramètres des "fun" *)

type expression =
    Var of string
  | Const of int
  | Op of string
  | Fun of string * exprtype * expression
  | App of expression * expression
  | Paire of expression * expression
  | Fst of expression
  | Snd of expression
  | Let of string * expression * expression

(* La fonction de propagation de types.

   Noter que dans le code ci-dessous, si on trouve une variable alors qu'on attend
   une flèche ou un produit, on signale une erreur. On ne fait donc aucune forme
   d'inférence. En fait, les variables de types sont traitées ici exactement comme
   des types de base inconnus. *)

exception Erreur_de_type

let rec type_de env a =
  match a with
    Var x -> List.assoc x env
  | Const c -> Type_base "int"
  | Op op -> type_opérateur op
  | Fun(x, tau, b) -> Type_flèche(tau, type_de ((x, tau) :: env) b)
  | App(b, c) ->
      begin match type_de env b with
        Type_flèche(ty_arg, ty_rés) ->
          if ty_arg = type_de env c then ty_rés else raise Erreur_de_type
      | _ -> raise Erreur_de_type
      end
  | Paire(b, c) -> Type_produit(type_de env b, type_de env c)
  | Fst b ->
      begin match type_de env b with
      |  Type_produit (t1, _) -> t1
      |  _ -> raise Erreur_de_type
      end
  | Snd b ->
      begin match type_de env b with
      |  Type_produit (_, t2) -> t2
      |  _ -> raise Erreur_de_type
      end
  | Let(x, b, c) ->
      let ty_b = type_de env b in
      type_de ((x, ty_b) :: env) c

end (* Exercice31 *)

(****************** Exercice 3.2: construction de C(a) *********************)

(* La syntaxe abstraite des expressions mini-ML (comme au chapitre 1) *)

type expression =
    Var of string
  | Const of int
  | Op of string
  | Fun of string * expression
  | App of expression * expression
  | Paire of expression * expression
  | Fst of expression
  | Snd of expression
  | Let of string * expression * expression

(* Génération de nouvelles variables de types a1, a2, a3, ... *)

let compteur_de_variables = ref 0

let nouvelle_variable _ =
  incr compteur_de_variables;
  "a" ^ string_of_int !compteur_de_variables

let nouveau _ =
  Type_var (nouvelle_variable ())

(* Associer de nouvelles variables de type à chaque identificateur et
   à chaque sous-expression d'une expression *)

type identificateur_typé =
  { id : string; type_id: exprtype }

type expression_typée = 
  { descr: expression_typée_descr; type_expr: exprtype }

and expression_typée_descr =
    TVar of identificateur_typé
  | TConst of int
  | TOp of string
  | TFun of identificateur_typé * expression_typée
  | TApp of expression_typée * expression_typée
  | TPaire of expression_typée * expression_typée
  | TFst of expression_typée
  | TSnd of expression_typée
  | TLet of identificateur_typé * expression_typée * expression_typée

let rec annotation env expr =
  let descr =
    match expr with
      Var x -> TVar(List.assoc x env)
    | Const c -> TConst c
    | Op op -> TOp op
    | Fun(x, a) ->
        let tx = {id = x; type_id = nouveau()} in
        TFun(tx, annotation ((x, tx) :: env) a)
    | App(a, b) ->
        TApp(annotation env a, annotation env b)
    | Paire(a, b) ->
        TPaire(annotation env a, annotation env b)
    | Fst a ->
  TFst (annotation env a)
    | Snd a ->
  TSnd (annotation env a)
    | Let(x, a, b) ->
        let tx = {id = x; type_id = nouveau()} in
        TLet(tx, annotation env a, annotation ((x, tx) :: env) b)
  in
    { descr = descr; type_expr = nouveau() }

(* Génération de l'ensemble d'équations C(a) pour une expression annotée a *)

let rec équations a =
  match a.descr with
    TVar id -> [a.type_expr, id.type_id]
  | TConst c -> [a.type_expr, Type_base "int"]
  | TOp op -> [a.type_expr, type_opérateur op]
  | TFun(id, b) ->
      (a.type_expr, Type_flèche(id.type_id, b.type_expr)) :: équations b
  | TApp(b, c) ->
      (b.type_expr, Type_flèche(c.type_expr, a.type_expr)) ::
      équations b @ équations c
  | TPaire(b, c) ->
      (a.type_expr, Type_produit(b.type_expr, c.type_expr)) ::
      équations b @ équations c
  | TFst b ->
      (b.type_expr, Type_produit(a.type_expr, nouveau())) ::
      équations b
  | TSnd b ->
      (b.type_expr, Type_produit(nouveau(), a.type_expr)) ::
      équations b
  | TLet(x, b, c) ->
      (a.type_expr, c.type_expr) ::
      (x.type_id, b.type_expr) ::
      équations b @ équations c

(******************** Exercice 3.3: unification **************************)

(* Représentation des substitutions par des fonctions des types dans les
   types *)

(* La substitution élémentaire [alpha_i <- tau_i] *)

let subst alpha_tau_list =
  let rec do_subst tau =
    match tau with
      Type_var alpha ->
        begin try
          List.assoc alpha alpha_tau_list
        with Not_found ->
          tau
        end
    | Type_base b -> tau
    | Type_flèche(tau1, tau2) -> Type_flèche(do_subst tau1, do_subst tau2)
    | Type_produit(tau1, tau2) -> Type_produit(do_subst tau1, do_subst tau2)
  in
    do_subst

(* La composition de substitutions et la substitution identité *)

let compose subst1 subst2 = fun typ -> subst1(subst2(typ))

let identité = fun typ -> typ

(* Appliquer une substitution à un ensemble d'équations entre types
   (une liste de paires de types) *)

let subst_equations sigma c =
  List.map (fun (tau1, tau2) -> (sigma tau1, sigma tau2)) c

(* Test d'occurrence d'une variable dans un type *)

let rec occurrence alpha tau =
  match tau with
    Type_base b -> false
  | Type_var beta -> alpha = beta
  | Type_flèche(tau1, tau2) -> occurrence alpha tau1 || occurrence alpha tau2
  | Type_produit(tau1, tau2) -> occurrence alpha tau1 || occurrence alpha tau2

(* Le calcul de l'unificateur principal *)

exception Non_unifiable

let rec mgu = function
    [] -> identité
  | (Type_var alpha, Type_var beta) :: c when alpha = beta -> mgu c
  | (Type_var alpha, tau) :: c when not(occurrence alpha tau) ->
      let s = subst [alpha, tau] in 
      compose (mgu(subst_equations s c)) s
  | (tau, Type_var alpha) :: c when not(occurrence alpha tau) ->
      let s = subst [alpha, tau] in 
      compose (mgu(subst_equations s c)) s
  | (Type_base b1, Type_base b2) :: c when b1 = b2 -> mgu c
  | (Type_flèche(tau1, tau2), Type_flèche(tau1', tau2')) :: c ->
      mgu((tau1, tau1') :: (tau2, tau2') :: c)
  | (Type_produit(tau1, tau2), Type_produit(tau1', tau2')) :: c ->
      mgu((tau1, tau1') :: (tau2, tau2') :: c)
  | _ ->
      raise Non_unifiable

(******************* Exercice 3.4: l'algorithme I **************************)

type résultat = Err | Type of exprtype

let inférence_monomorphe a =
  try
    let a' = annotation [] a in
    let phi = mgu(équations a') in
    Type(phi(a'.type_expr))
  with Non_unifiable ->
    Err

(******************** Exercice 3.5: l'algorithme W ***********************)

(* Les schémas de types (comme au chapitre 1) *)

type schéma = { quantif: string list; corps: exprtype }

(* Prendre une instance générique d'un schéma *)

let instance_générique sigma =
  (* On génère les variables nouvelles beta1... betaN *)
  let vars_inst =
    List.map nouveau sigma.quantif in
  (* On construit la substitution alphai <- betai *)
  let s = subst (List.combine sigma.quantif vars_inst) in
  (* On l'applique au corps du schéma *)
  s sigma.corps

(* Calcul des variables libres -- comme au chapitre 1 *)

let rec union l1 l2 =
  match l1 with
    [] -> l2
  | hd :: tl -> union tl (if List.mem hd l2 then l2 else hd :: l2)

let rec variables_libres t =
  match t with
    Type_base b -> []
  | Type_var v -> [v]
  | Type_flèche(t1, t2) -> union (variables_libres t1) (variables_libres t2)
  | Type_produit(t1, t2) -> union (variables_libres t1) (variables_libres t2)

let rec différence l1 l2 =
  match l1 with
    [] -> []
  | hd :: tl ->
      if List.mem hd l2 then différence tl l2 else hd :: différence tl l2

let variables_libres_schéma s =
  différence (variables_libres s.corps) s.quantif

let rec variables_libres_env env =
  match env with
    [] -> []
  | (ident, schéma) :: reste ->
      union (variables_libres_schéma schéma) (variables_libres_env reste)

(* Généralisation d'un type en un schéma *)

let généralisation typ env =
  { quantif = différence (variables_libres typ) (variables_libres_env env);
    corps = typ }

(* Appliquer une substitution à un schéma *)
(* Pour éviter les problèmes de capture de variables liées, on renomme
   toutes les variables liées dans le schéma en de nouvelles variables *)

let subst_schéma phi sigma =
  (* On génère de nouvelles variables beta1... betaN *)
  let quantif' = List.map nouvelle_variable sigma.quantif in
  (* On construit la substitution [alphai <- betai] o phi *)
  let psi =
    compose (subst (List.map2 (fun alpha beta -> (alpha, Type_var beta))
                              sigma.quantif quantif'))
            phi in
  (* On renvoie le schéma V betai. psi(tau) *)
  { quantif = quantif'; corps = psi sigma.corps }

(* Appliquer une substitution à un environnement *)

let subst_env phi env =
  List.map (fun (ident, schéma) -> (ident, subst_schéma phi schéma)) env

(* Type des opérateurs (polymorphes) *)

let type_opérateur op =
  match op with
    "+" | "-" | "*" | "/" ->
      { quantif = [];
        corps = Type_flèche(Type_produit(Type_base "int", Type_base "int"),
                            Type_base "int") }
  | "fix" ->
      let alpha = nouvelle_variable() in
      let alpha_f_alpha = Type_flèche(Type_var alpha, Type_var alpha) in
      { quantif = [alpha];
        corps = Type_flèche(Type_flèche(alpha_f_alpha, alpha_f_alpha), alpha_f_alpha) }
  | _ ->
      assert false (* opérateur non supporté *)

(* L'algorithme W *)

exception Erreur_de_typage

let inférence a =

  (* La variable globale phi. *)

  let phi = ref identité in

  (* L'algorithme récursif W. *)

  let rec w env a =
    match a with
      Var x ->
  begin try
    instance_générique (List.assoc x env)
  with Not_found ->
    (* la variable x n'est pas dans le domaine de env *)
    raise Erreur_de_typage
  end
    | Const n ->
  Type_base "int"
    | Op op ->
  instance_générique (type_opérateur op)
    | Fun(x, a1) ->
  let alpha = nouveau() in
  let schéma_alpha = {quantif = []; corps = alpha} in
  let tau1 = w ((x, schéma_alpha) :: env) a1 in
  Type_flèche(alpha, tau1)
    | App(a1, a2) ->
  let tau1 = w env a1 in
  let tau2 = w env a2 in
  let alpha = nouveau() in
  phi := compose (mgu [!phi tau1, Type_flèche(!phi tau2, alpha)]) !phi;
  alpha
    | Paire(a1, a2) ->
  let tau1 = w env a1 in
  let tau2 = w env a2 in
  Type_produit (tau1, tau2)
    | Fst a ->
  let tau = w env a in
  let alpha1, alpha2 = nouveau(), nouveau() in
  phi := compose (mgu [!phi tau, Type_produit (alpha1, alpha2)]) !phi;
  alpha1
    | Snd a ->
  let tau = w env a in
  let alpha1, alpha2 = nouveau(), nouveau() in
  phi := compose (mgu [!phi tau, Type_produit (alpha1, alpha2)]) !phi;
  alpha2
    | Let(x, a1, a2) ->
  let tau1 = w env a1 in
  let sigma = généralisation (!phi tau1) (subst_env !phi env) in
  w ((x, sigma) :: env) a2

  (* On démarre avec l'environnement vide. À la sortie, on applique la substitution courante au type renvoyé par W,
     de façon à obtenir un type qui a un sens. *)
  in
  !phi (w [] a)

(* Pour tester:
inférence (App(Op "+", Paire(Const 1, Const 2)));;
inférence (Let("x", Op "+", App(Var "x", Paire(Const 1, Const 2))));;
inférence (Let("id", Fun("x", Var "x"),
                  Paire(App(Var "id", Const 1), App(Var "id", Var "id"))));;
inférence (Fun("x", Paire(Snd (Var "x"), Fst (Var "x"))));;
inférence (Fun("f", App(Var "f", Var "f")));;
*)
