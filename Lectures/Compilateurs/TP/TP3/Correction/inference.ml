open Ast


(* Expressions de types *)

type variable = string

type type_expr =
  TEint 
| TEfun of type_expr * type_expr
| TEvar of variable
| TEunit


(* Associer de nouvelles variables de type à chaque variable et à
   chaque sous-expression d'une expression *)

type typed_var =
  { var: variable; type_var: type_expr }

(* La syntaxe abstraite des expressions est *)

type typed_expr = 
  { desc: typed_expr_desc; type_expr: type_expr }

and typed_expr_desc =
  TConst of int
| TBinOp of bin_op * typed_expr * typed_expr
| TVar of typed_var
| TLet of typed_var * typed_expr * typed_expr
| TLetRec of typed_var * typed_expr * typed_expr
| TFun of typed_var * typed_expr
| TApp of typed_expr * typed_expr
| TIf of bexpr * typed_expr * typed_expr
| TIfz of typed_expr * typed_expr * typed_expr
| TAssign of typed_var * typed_expr
| TU

(* Production d'une variable de type fraîche *)

let fresh_type_var =
  let cnt = ref 0
in fun () -> incr cnt; TEvar ("a" ^ string_of_int !cnt)

type type_binding = variable * type_expr
type typing_env = type_binding list

let rec annotate (env: typing_env) expr =
  let desc =
    match expr with
      Const n -> 
        TConst n
    | BinOp (op, e1, e2) ->
        TBinOp (op, annotate env e1, annotate env e2)
    | Var x -> (* Suppose que les termes sont clos. *)
        TVar {var = x; type_var = List.assoc x env}
    | Let (x, e1, e2) ->
        let ftv = fresh_type_var() in
          let tx = {var = x; type_var = ftv}
        in TLet (tx, annotate env e1, annotate ((x,ftv)::env) e2)
    | LetRec (f, e1, e2) ->
        let ftv = fresh_type_var() in
          let env' = (f,ftv)::env in
          let tf = {var = f; type_var = ftv}
        in TLet (tf, annotate env' e1, annotate env' e2)
    | Fun (x, e) ->
        let ftv = fresh_type_var() in
          let tx = {var = x; type_var = ftv}
        in TFun (tx, annotate ((x,ftv)::env) e)
    | App (e1, e2) ->
        TApp (annotate env e1, annotate env e2)
    | If (b, e1, e2) ->
        TIf (b, annotate env e1, annotate env e2)
    | Ifz (e1, e2, e3) ->
        TIfz (annotate env e1, annotate env e2, annotate env e3)
    | Assign (x, e) ->
        let tx = {var = x; type_var = List.assoc x env}
        in TAssign (tx, annotate env e)
    | U -> TU
in {desc = desc; type_expr = fresh_type_var()}

let rec string_of_type = function
  TEint -> 
    "int"
| TEfun (tau1, tau2) -> 
    "(" ^ string_of_type tau1 ^ " -> " ^ string_of_type tau2 ^ ")"
| TEvar alpha ->
    alpha
| TEunit -> 
    "unit"

let rec string_of_typed_expr a =
  let desc = 
    match a.desc with
      TConst n ->
      string_of_int n ^ ":int"
    | TBinOp (op, a1, a2) ->
        string_of_typed_expr a1 ^ 
        begin match op with
          Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
        end ^
        string_of_typed_expr a2
    | TVar x ->
        x.var ^ ":" ^ string_of_type(x.type_var)
    | TLet (x, a1, a2) ->
        "let " ^ x.var ^ ":" ^ string_of_type x.type_var ^ " = " ^ 
        string_of_typed_expr a1 ^ " in " ^
        string_of_typed_expr a2
    | TLetRec (x, a1, a2) ->
        "let rec " ^ x.var ^ ":" ^ string_of_type x.type_var ^ 
        " = " ^ string_of_typed_expr a1 ^ " in " ^
        string_of_typed_expr a2
    | TFun (x, a1) ->
        "fun " ^ x.var ^ ":" ^ string_of_type x.type_var ^ " -> " ^ 
        string_of_typed_expr a1
    | TApp (a1, a2) ->
        string_of_typed_expr a1 ^ " " ^ string_of_typed_expr a2
    | TIf (_, a1, a2) ->
        "if <bool> then " ^ string_of_typed_expr a1 ^ " else " ^
        string_of_typed_expr a2
    | TIfz (a1, a2, a3) ->
        "ifz "  ^ string_of_typed_expr a1 ^ " then " ^
        string_of_typed_expr a2 ^ " else " ^ string_of_typed_expr a3
    | TAssign (x, a1) ->
        x.var ^ ":" ^ string_of_type x.type_var ^ " := " ^
        string_of_typed_expr a1
    | TU -> 
        "():unit"
in "(" ^ desc ^ " : " ^ string_of_type a.type_expr ^ ")"


(* Génération de l'ensemble d'équations C(a) pour une expression annotée a *)

type equation = type_expr * type_expr

let rec make_eq (a: typed_expr) : equation list =
  match a.desc with
    TConst n -> 
      [(a.type_expr, TEint)]
  | TBinOp (op, a1, a2) ->
      (a.type_expr, TEint) :: (a1.type_expr, TEint) ::
      (a2.type_expr, TEint) :: make_eq a1 @ make_eq a2
  | TVar x ->
      [(a.type_expr, x.type_var)]
  | TLet (x, a1, a2) | TLetRec (x, a1, a2) ->
      (x.type_var, a1.type_expr) :: (a.type_expr, a2.type_expr)
      :: make_eq a1 @ make_eq a2
  | TFun (x, a1) ->
      (a.type_expr, TEfun (x.type_var, a1.type_expr)) :: make_eq a1
  | TApp (a1, a2) ->
      (a1.type_expr, TEfun (a2.type_expr, a.type_expr)) 
      :: make_eq a1 @ make_eq a2
  | TIf (_, a1, a2) ->
      (a.type_expr, a1.type_expr) :: (a1.type_expr, a2.type_expr) 
      :: make_eq a1 @ make_eq a2
  | TIfz (a1, a2, a3) ->
      (a.type_expr, a2.type_expr) :: (a2.type_expr, a3.type_expr) 
      :: (a1.type_expr, TEint) :: make_eq a1 @ make_eq a2 @ make_eq a3
  | TAssign (x, a1) ->
      (a.type_expr, TEunit) :: (x.type_var, a1.type_expr) :: make_eq a1
  | TU -> 
      [(a.type_expr, TEunit)]


(******************** Exercice 3.3: unification **************************)

(* Représentation des substitutions par des fonctions des types dans les
   types *)

type substitution = type_expr -> type_expr

(* La substitution élémentaire tau [alpha <- tau'] *)

let make_subst ((alpha,tau'): type_binding) =
  let rec mk_sub_aux tau =
  match tau with
    TEint | TEunit -> 
      tau
  | TEvar beta -> 
      if beta = alpha then tau' else tau
  | TEfun (tau1, tau2) -> 
      TEfun (mk_sub_aux tau1, mk_sub_aux tau2)
in (mk_sub_aux: substitution)

(* La composition de substitutions *)

let compose phi1 phi2 = fun tau -> phi1(phi2(tau))


(* Appliquer une substitution à un ensemble d'équations entre types
   (une liste de paires de types) *)

let subst_eq (phi: substitution) (eq: equation list) : equation list =
  List.map (fun (tau1, tau2) -> (phi(tau1), phi(tau2))) eq


(* Test d'occurrence d'une variable dans un type *)

let rec occurrence alpha tau =
  match tau with
    TEint | TEunit ->
      false
  | TEvar beta -> 
      alpha = beta
  | TEfun (tau1, tau2) -> 
      occurrence alpha tau1 || occurrence alpha tau2


(* Le calcul de l'unificateur principal *)

exception NonUnifiable

let rec (mgu: equation list -> substitution) = function
  [] -> 
    (fun tau -> tau)
| (TEvar alpha, TEvar beta)::c when alpha = beta -> 
    mgu(c)
| (TEvar alpha, tau)::c when not(occurrence alpha tau) ->
    let phi = make_subst (alpha, tau)
    in compose (mgu (subst_eq phi c)) phi
| (tau, TEvar alpha) :: c when not(occurrence alpha tau) ->
    let phi = make_subst (alpha, tau) 
    in compose (mgu (subst_eq phi c)) phi
| (TEint, TEint)::c ->
    mgu(c)
| (TEfun(tau1, tau2), TEfun(tau1', tau2'))::c ->
    mgu ((tau1, tau1')::(tau2, tau2')::c)
| _ ->
    raise NonUnifiable


let monomorphic_inference e =
  let a = annotate [] e in
  let phi = mgu (make_eq a) 
in phi (a.type_expr)


(*
let monomorphic_inference e =
  let a = annotate [] e in
  let () = prerr_endline (string_of_typed_expr a) 
in TEint
*)


(* Pour tester:
inférence (App(Op "+", Paire(Const 1, Const 2)));;
inférence (Let("x", Op "+", App(Var "x", Paire(Const 1, Const 2))));;
inférence (Let("id", Fun("x", Var "x"),
                  Paire(App(Var "id", Const 1), App(Var "id", Var "id"))));;
inférence (Fun("x", Paire(Snd (Var "x"), Fst (Var "x"))));;
inférence (Fun("f", App(Var "f", Var "f")));;
*)

