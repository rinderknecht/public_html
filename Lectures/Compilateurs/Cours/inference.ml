(* Corrig� des exercices de programmation du chapitre 3 *)

(* Le code donn� ci-dessous est tr�s (trop) fid�le � la description donn�e
   dans les notes de cours: il utilise une repr�sentation inefficace des
   substitutions, effectue l'occur-check de fa�on co�teuse, calcule l'ensemble
   des variables libres dans l'environnement de fa�on na�ve, etc. On peut
   impl�menter l'inf�rence de types pour ML de fa�on beaucoup plus efficace,
   mais la correspondance avec les r�gles de typage est alors plus difficile
   � justifier. Ce pourrait �tre le sujet d'un cours plus avanc�. *)

(********** Exercice 3.1: propagation des types pour ML monomorphe *********)

(* L'alg�bre de types (comme au chapitre 1) *)

type exprtype =
    Type_base of string
  | Type_var of string
  | Type_fl�che of exprtype * exprtype
  | Type_produit of exprtype * exprtype

(* Type des op�rateurs (monomorphes) *)

let type_op�rateur op =
  match op with
    "+" | "-" | "*" | "/" ->
      Type_fl�che(Type_produit(Type_base "int", Type_base "int"),
                  Type_base "int")
  | "fix" ->
      let int_f_int = Type_fl�che(Type_base "int", Type_base "int") in
      Type_fl�che(Type_fl�che(int_f_int, int_f_int), int_f_int)
  | _ ->
      assert false (* op�rateur non support� *)

module Exercice31 = struct

(* La syntaxe abstraite des expressions mini-ML avec annotation de type
   sur les param�tres des "fun" *)

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
   une fl�che ou un produit, on signale une erreur. On ne fait donc aucune forme
   d'inf�rence. En fait, les variables de types sont trait�es ici exactement comme
   des types de base inconnus. *)

exception Erreur_de_type

let rec type_de env a =
  match a with
    Var x -> List.assoc x env
  | Const c -> Type_base "int"
  | Op op -> type_op�rateur op
  | Fun(x, tau, b) -> Type_fl�che(tau, type_de ((x, tau) :: env) b)
  | App(b, c) ->
      begin match type_de env b with
        Type_fl�che(ty_arg, ty_r�s) ->
          if ty_arg = type_de env c then ty_r�s else raise Erreur_de_type
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

(* G�n�ration de nouvelles variables de types a1, a2, a3, ... *)

let compteur_de_variables = ref 0

let nouvelle_variable _ =
  incr compteur_de_variables;
  "a" ^ string_of_int !compteur_de_variables

let nouveau _ =
  Type_var (nouvelle_variable ())

(* Associer de nouvelles variables de type � chaque identificateur et
   � chaque sous-expression d'une expression *)

type identificateur_typ� =
  { id : string; type_id: exprtype }

type expression_typ�e = 
  { descr: expression_typ�e_descr; type_expr: exprtype }

and expression_typ�e_descr =
    TVar of identificateur_typ�
  | TConst of int
  | TOp of string
  | TFun of identificateur_typ� * expression_typ�e
  | TApp of expression_typ�e * expression_typ�e
  | TPaire of expression_typ�e * expression_typ�e
  | TFst of expression_typ�e
  | TSnd of expression_typ�e
  | TLet of identificateur_typ� * expression_typ�e * expression_typ�e

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

(* G�n�ration de l'ensemble d'�quations C(a) pour une expression annot�e a *)

let rec �quations a =
  match a.descr with
    TVar id -> [a.type_expr, id.type_id]
  | TConst c -> [a.type_expr, Type_base "int"]
  | TOp op -> [a.type_expr, type_op�rateur op]
  | TFun(id, b) ->
      (a.type_expr, Type_fl�che(id.type_id, b.type_expr)) :: �quations b
  | TApp(b, c) ->
      (b.type_expr, Type_fl�che(c.type_expr, a.type_expr)) ::
      �quations b @ �quations c
  | TPaire(b, c) ->
      (a.type_expr, Type_produit(b.type_expr, c.type_expr)) ::
      �quations b @ �quations c
  | TFst b ->
      (b.type_expr, Type_produit(a.type_expr, nouveau())) ::
      �quations b
  | TSnd b ->
      (b.type_expr, Type_produit(nouveau(), a.type_expr)) ::
      �quations b
  | TLet(x, b, c) ->
      (a.type_expr, c.type_expr) ::
      (x.type_id, b.type_expr) ::
      �quations b @ �quations c

(******************** Exercice 3.3: unification **************************)

(* Repr�sentation des substitutions par des fonctions des types dans les
   types *)

(* La substitution �l�mentaire [alpha_i <- tau_i] *)

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
    | Type_fl�che(tau1, tau2) -> Type_fl�che(do_subst tau1, do_subst tau2)
    | Type_produit(tau1, tau2) -> Type_produit(do_subst tau1, do_subst tau2)
  in
    do_subst

(* La composition de substitutions et la substitution identit� *)

let compose subst1 subst2 = fun typ -> subst1(subst2(typ))

let identit� = fun typ -> typ

(* Appliquer une substitution � un ensemble d'�quations entre types
   (une liste de paires de types) *)

let subst_equations sigma c =
  List.map (fun (tau1, tau2) -> (sigma tau1, sigma tau2)) c

(* Test d'occurrence d'une variable dans un type *)

let rec occurrence alpha tau =
  match tau with
    Type_base b -> false
  | Type_var beta -> alpha = beta
  | Type_fl�che(tau1, tau2) -> occurrence alpha tau1 || occurrence alpha tau2
  | Type_produit(tau1, tau2) -> occurrence alpha tau1 || occurrence alpha tau2

(* Le calcul de l'unificateur principal *)

exception Non_unifiable

let rec mgu = function
    [] -> identit�
  | (Type_var alpha, Type_var beta) :: c when alpha = beta -> mgu c
  | (Type_var alpha, tau) :: c when not(occurrence alpha tau) ->
      let s = subst [alpha, tau] in 
      compose (mgu(subst_equations s c)) s
  | (tau, Type_var alpha) :: c when not(occurrence alpha tau) ->
      let s = subst [alpha, tau] in 
      compose (mgu(subst_equations s c)) s
  | (Type_base b1, Type_base b2) :: c when b1 = b2 -> mgu c
  | (Type_fl�che(tau1, tau2), Type_fl�che(tau1', tau2')) :: c ->
      mgu((tau1, tau1') :: (tau2, tau2') :: c)
  | (Type_produit(tau1, tau2), Type_produit(tau1', tau2')) :: c ->
      mgu((tau1, tau1') :: (tau2, tau2') :: c)
  | _ ->
      raise Non_unifiable

(******************* Exercice 3.4: l'algorithme I **************************)

type r�sultat = Err | Type of exprtype

let inf�rence_monomorphe a =
  try
    let a' = annotation [] a in
    let phi = mgu(�quations a') in
    Type(phi(a'.type_expr))
  with Non_unifiable ->
    Err

(******************** Exercice 3.5: l'algorithme W ***********************)

(* Les sch�mas de types (comme au chapitre 1) *)

type sch�ma = { quantif: string list; corps: exprtype }

(* Prendre une instance g�n�rique d'un sch�ma *)

let instance_g�n�rique sigma =
  (* On g�n�re les variables nouvelles beta1... betaN *)
  let vars_inst =
    List.map nouveau sigma.quantif in
  (* On construit la substitution alphai <- betai *)
  let s = subst (List.combine sigma.quantif vars_inst) in
  (* On l'applique au corps du sch�ma *)
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
  | Type_fl�che(t1, t2) -> union (variables_libres t1) (variables_libres t2)
  | Type_produit(t1, t2) -> union (variables_libres t1) (variables_libres t2)

let rec diff�rence l1 l2 =
  match l1 with
    [] -> []
  | hd :: tl ->
      if List.mem hd l2 then diff�rence tl l2 else hd :: diff�rence tl l2

let variables_libres_sch�ma s =
  diff�rence (variables_libres s.corps) s.quantif

let rec variables_libres_env env =
  match env with
    [] -> []
  | (ident, sch�ma) :: reste ->
      union (variables_libres_sch�ma sch�ma) (variables_libres_env reste)

(* G�n�ralisation d'un type en un sch�ma *)

let g�n�ralisation typ env =
  { quantif = diff�rence (variables_libres typ) (variables_libres_env env);
    corps = typ }

(* Appliquer une substitution � un sch�ma *)
(* Pour �viter les probl�mes de capture de variables li�es, on renomme
   toutes les variables li�es dans le sch�ma en de nouvelles variables *)

let subst_sch�ma phi sigma =
  (* On g�n�re de nouvelles variables beta1... betaN *)
  let quantif' = List.map nouvelle_variable sigma.quantif in
  (* On construit la substitution [alphai <- betai] o phi *)
  let psi =
    compose (subst (List.map2 (fun alpha beta -> (alpha, Type_var beta))
                              sigma.quantif quantif'))
            phi in
  (* On renvoie le sch�ma V betai. psi(tau) *)
  { quantif = quantif'; corps = psi sigma.corps }

(* Appliquer une substitution � un environnement *)

let subst_env phi env =
  List.map (fun (ident, sch�ma) -> (ident, subst_sch�ma phi sch�ma)) env

(* Type des op�rateurs (polymorphes) *)

let type_op�rateur op =
  match op with
    "+" | "-" | "*" | "/" ->
      { quantif = [];
        corps = Type_fl�che(Type_produit(Type_base "int", Type_base "int"),
                            Type_base "int") }
  | "fix" ->
      let alpha = nouvelle_variable() in
      let alpha_f_alpha = Type_fl�che(Type_var alpha, Type_var alpha) in
      { quantif = [alpha];
        corps = Type_fl�che(Type_fl�che(alpha_f_alpha, alpha_f_alpha), alpha_f_alpha) }
  | _ ->
      assert false (* op�rateur non support� *)

(* L'algorithme W *)

exception Erreur_de_typage

let inf�rence a =

  (* La variable globale phi. *)

  let phi = ref identit� in

  (* L'algorithme r�cursif W. *)

  let rec w env a =
    match a with
      Var x ->
  begin try
    instance_g�n�rique (List.assoc x env)
  with Not_found ->
    (* la variable x n'est pas dans le domaine de env *)
    raise Erreur_de_typage
  end
    | Const n ->
  Type_base "int"
    | Op op ->
  instance_g�n�rique (type_op�rateur op)
    | Fun(x, a1) ->
  let alpha = nouveau() in
  let sch�ma_alpha = {quantif = []; corps = alpha} in
  let tau1 = w ((x, sch�ma_alpha) :: env) a1 in
  Type_fl�che(alpha, tau1)
    | App(a1, a2) ->
  let tau1 = w env a1 in
  let tau2 = w env a2 in
  let alpha = nouveau() in
  phi := compose (mgu [!phi tau1, Type_fl�che(!phi tau2, alpha)]) !phi;
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
  let sigma = g�n�ralisation (!phi tau1) (subst_env !phi env) in
  w ((x, sigma) :: env) a2

  (* On d�marre avec l'environnement vide. � la sortie, on applique la substitution courante au type renvoy� par W,
     de fa�on � obtenir un type qui a un sens. *)
  in
  !phi (w [] a)

(* Pour tester:
inf�rence (App(Op "+", Paire(Const 1, Const 2)));;
inf�rence (Let("x", Op "+", App(Var "x", Paire(Const 1, Const 2))));;
inf�rence (Let("id", Fun("x", Var "x"),
                  Paire(App(Var "id", Const 1), App(Var "id", Var "id"))));;
inf�rence (Fun("x", Paire(Snd (Var "x"), Fst (Var "x"))));;
inf�rence (Fun("f", App(Var "f", Var "f")));;
*)
