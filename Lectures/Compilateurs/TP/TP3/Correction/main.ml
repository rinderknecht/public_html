module WithFunAppLetRec = struct

  open Eval.WithFunAppLetRec

  let calc ast =
    try
      begin match eval empty_env ast with
        Int n -> print_int n
      | Clos _ -> print_string "<fun>"
      end;
      print_newline(); 
      exit 0
    with Err DivByZero ->
           print_endline "Error: division by zero."; exit 1
       | Err (FreeVar x) ->
           print_endline ("Error: free variable " ^ x ^ "."); exit 1
       | Err (Typing m) ->
           print_endline ("Typing error: " ^ m ^ "."); exit 1
end


module WithAssign = struct

  open Eval.WithAssign

  let calc ast =
    try
      let v = eval empty_env empty_mem ast in
        let () = print_string "The value is " 
      in begin match v with
        (Int n, _) -> print_int n
      | (Unit, _) -> print_string "()"
      | (Clos _, _) -> print_string "<fun>"
      end;
      print_newline();
      exit 0
    with Err DivByZero ->
           print_endline "Error: division by zero."; exit 1
       | Err (FreeVar x) ->
           print_endline ("Error: free variable " ^ x ^ "."); exit 1
       | Err (Typing m) ->
           print_endline ("Typing error: " ^ m ^ "."); exit 1
end

module WithTyping = struct

  open Eval.WithAssign
  open Inference

  let calc ast =
    try
      let free_vars = Ast.fv (ast) 
      in if Ast.StringSet.is_empty free_vars
         then 
           let inf_type = monomorphic_inference ast in
             let pr_type = string_of_type inf_type in
             let () = print_endline ("The type is " ^ pr_type) in
             let () = print_string "=> "
           in WithAssign.calc ast
         else 
           print_string "Free variables: ";
           Ast.StringSet.iter (fun x -> print_string (x ^ " ")) free_vars;
           print_newline(); 
           exit 0
    with NonUnifiable ->
           print_endline "Error: Ill-typed phrase."; exit 1

end

let main () =
  let () = 
    print_endline "Saisissez une expression + RET + CTRL-D:";
    flush stdout in
  let lexbuf = Lexing.from_channel stdin in
  let ast = 
    try Parser.expression Lexer.token lexbuf with 
      Lexer.Illegal_char s ->
        print_endline ("Lexing error: Illegal character " ^ s ^ "."); 
        exit 1 
    | Lexer.Open_comment ->
        print_endline "Lexing error: Comment not terminated.";
        exit 1 in
  let () = print_string "=> " 
in WithTyping.calc (ast)


let _ = Printexc.print main ()
