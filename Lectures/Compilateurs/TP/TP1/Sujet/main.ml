open Eval;;

let empty_env = fun x -> raise Not_found
;;

let main () =
  let () = 
    print_endline "Saisissez une expression + RET + CTRL-D:";
    flush stdout in
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.expression Lexer.token lexbuf in
  let () = print_string "=> " in

(* A partir d'ici il faut r��crire et compl�ter le code qui suit de
  fa�on � ce que si l'�valuation d�clenche une exception, il faut
  afficher le message d'erreur associ� (puis finir l'ex�cution). Si le
  r�sultat est un entier affichez-le, sinon affichez simplement
  "<fun>".  
*)

  let result = eval empty_env ast
in print_endline result
;;

Printexc.print main ()
;;
