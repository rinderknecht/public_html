open Ast;;

(* Il faut réécrire et compléter le code qui suit. *)

let extend env (x,v) = fun y -> if x = y then v else env x
;;

let rec eval env expr = match expr with
  _ -> "ok"
;;
