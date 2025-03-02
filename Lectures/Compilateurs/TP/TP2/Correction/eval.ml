(* Nous donnons ici le contenu du module [Eval] (fichier
  \textsf{eval.ml}). Il contient plusieurs sous-modules correspondant
  chacun à une calculette avec différen\-tes constructions qui sont
  ajoutées au fur et à mesure des questions. *)

open Ast

(* \subsection{Sous-module
   [Eval.WithFunAppLetRec]}\label{Eval.WithFunAppLetRec}
*)

(* Le sous-module [Eval.WithFunAppLetRec] reprend le module
  [Eval.WithCond] \emph{du TP précédent} et l'étend avec les
  abstractions, les applications et les fonctions récursives
  natives. Il répond aux questions~\ref{q2} et~\ref{q3}. L'ajout se
  réduit à un cas supplémentaire dans le filtre de la fonction
  [eval].
*)

module WithFunAppLetRec = struct

  type value = 
    Int of int 
  | Clos of string * expr * environnement

  and environnement = string -> value

  let empty_env = fun x -> raise Not_found
  let extend env (x,v) = fun y -> if x = y then v else env y

  type error = 
    DivByZero
  | FreeVar of string
  | Typing of string

  exception Err of error

  let rec eval_bool b = match b with
    True -> true
  | False -> false
  | And (b1,b2) -> (eval_bool b1) && (eval_bool b2)
  | Or (b1,b2) -> (eval_bool b1) || (eval_bool b2)
  | Not b -> not (eval_bool b)

  let rec eval env e = match e with
    Const n -> Int (n)
  | BinOp (op,e1,e2) ->
      let v1 = eval env e1 and v2 = eval env e2
      in begin match (v1,v2) with
           (Clos _,_) | (_, Clos _) ->
             raise (Err (Typing "Operands must be integers"))
         | (Int n1, Int n2) ->
             Int (
               match op with
                 Add -> n1 + n2 | Sub -> n1 - n2 | Mult -> n1 * n2
               | Div -> 
                   if n2 = 0 then raise (Err DivByZero) else n1/n2
             )
         end
  | Var x -> begin try env x with 
               Not_found -> raise (Err (FreeVar x))
             end
  | Let (x,e1,e2) -> 
      let v1 = eval env e1 in eval (extend env (x,v1)) e2
  | LetRec (x,e1,e2) ->
      let rec env' = fun x -> extend env (x, v1 ()) x
      and v1 = fun () -> eval env' e1
      in eval env' e2
  | Ifz (e1,e2,e3) ->
      begin match eval env e1 with
        Int (0) -> eval env e2
      | Int _ -> eval env e3
      | Clos _ -> 
          raise (Err (Typing "\"ifz\" condition must be integer"))
      end
  | If (b,e1,e2) -> if eval_bool b then eval env e1 else eval env e2
  | Fun (x,e) -> Clos (x,e,env)
  | App (e1,e2) ->
      begin match eval env e1 with
        Clos (x,e0,env0) ->
          let v2 = eval env e2
          in eval (extend env0 (x,v2)) e0
      | _ -> raise (Err (Typing "Integers cannot be applied"))
      end
  | _ -> failwith "Construction non traitée"
end


module WithAssign = struct

  type address = int
  type variable = string
  type environnement = variable -> address

  type memory = address -> value

  and value =
    Int of int
  | Clos of string * expr * environnement
  | Unit

  let empty_env = fun x -> raise Not_found
  let extend env (x,v) = fun y -> if x = y then v else env y

  let empty_mem = empty_env

  let get_new_addr =
    let counter = ref 0
  in fun () -> (incr counter; !counter)

  type error =
    DivByZero
  | FreeVar of string
  | Typing of string

  exception Err of error

  let rec eval_bool b = match b with
    True -> true
  | False -> false
  | And (b1,b2) -> (eval_bool b1) && (eval_bool b2)
  | Or (b1,b2) -> (eval_bool b1) || (eval_bool b2)
  | Not b -> not (eval_bool b)

  let rec eval env mem e = match e with
    Const n -> (Int (n), mem)
  | BinOp (op,e1,e2) ->
      let (v1, mem1) = eval env mem e1 in
        let (v2, mem2) = eval env mem1 e2
      in begin match (v1,v2) with
           (Int n1, Int n2) ->
            let n =
              match op with
                Add -> n1 + n2 | Sub -> n1 - n2 | Mult -> n1 * n2
              | Div ->
                  if n2 = 0 then raise (Err DivByZero) else n1/n2
            in (Int n, mem2)
         | _ -> raise (Err (Typing "Operands must be integers"))
         end
  | Var x -> begin try (mem (env x), mem) with
               Not_found -> raise (Err (FreeVar x))
             end
  | Let (x,e1,e2) ->
      let (v1, mem1) = eval env mem e1
      and a = get_new_addr ()
      in eval (extend env (x,a)) (extend mem1 (a,v1)) e2
  | LetRec (x,e1,e2) ->
      let a =  get_new_addr () in
        let  env' = extend env (x,a) in
        let rec mem' = fun x -> extend mem (a, v1()) x
        and v1 = fun () -> fst (eval env' mem' e1) in
        let mem1 = snd (eval env' mem' e1)
      in eval env' (mem1) e2
  | Ifz (e1,e2,e3) ->
      begin match eval env mem e1 with
        (Int (0), mem1) -> eval env mem1 e2
      | (Int _, mem1) -> eval env mem1 e3
      | _ ->
          raise (Err (Typing "\"ifz\" condition must be integer"))
      end
  | If (b,e1,e2) ->
      if eval_bool b then eval env mem e1 else eval env mem e2
  | Fun (x,e) -> (Clos (x,e,env), mem)
  | App (e1,e2) ->
      begin match eval env mem e1 with
        (Clos (x0,e0,env0), mem1) ->
          let (v2, mem2) = eval env mem1 e2
          and a = get_new_addr ()
          in eval (extend env0 (x0,a)) (extend mem2 (a,v2)) e0
      | _ -> raise (Err (Typing "Integers cannot be applied"))
      end
  | U -> (Unit, mem)
  | Assign (x,e) ->
      let (v, mem') = eval env mem e
      in begin try
            (Unit, extend mem' (env x, v))
         with Not_found ->
           raise (Err (FreeVar x))
         end
end
