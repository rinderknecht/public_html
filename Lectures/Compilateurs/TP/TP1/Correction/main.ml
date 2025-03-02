
(* Le module [Main] est le pilote de la calculette. C'est ce module
  \emph{uniquement} qui interagit avec l'usager et, en particulier,
  filtre les erreurs des autres modules (ici [Eval]). Il est composé
  de plusieurs sous-modules correspondant chacun à un des sous-modules
  de [Eval] de même nom. Par exemple [Main.Initial] correspond à (en
  l'occurrence \emph{utilise}) [Eval.Initial].  *)

(*
  \subsection{Sous-module [Main.Initial]}\label{Main.Initial}

  Le sous-module [Main.Initial] correspond au cas initial avec les
  quatre opérations arithmétiques de base ainsi que les constantes. Il
  répond donc à la question~\ref{q1} en complétant le module
  [Eval.Initial] (cf. section~\ref{Eval.Initial}). La seule erreur qui
  puisse provenir de [Eval.Initial] est [DivByZero].

*)


module Initial = struct

  open Eval.Initial

  let calc ast =
    try 
      print_int (eval ast); print_newline(); exit 0
    with DivByZero -> 
           print_endline "Error: division by zero."; exit 1
end


(*
  \subsection{Sous-module [Main.WithLet]}\label{Main.WithLet}

  Le sous-module [Main.WithLet] correspond au module [Eval.WithLet]
  (cf. section~\ref{Eval.WithLet}). En complétant ce dernier, il
  répond ainsi à la question~\ref{q2}. Les variables et la liaison
  locale sont possibles, donc il faut filtrer en plus le cas des
  variables libres.

*)

module WithLet = struct

  open Eval.WithLet
 
  let calc ast =
    try
      print_int (eval empty_env ast); print_newline(); exit 0
    with Err DivByZero -> 
           print_endline "Error: division by zero."; exit 1
       | Err (FreeVar x) ->
           print_endline ("Error: free variable " ^ x); exit 1
end

(*
  \subsection{Sous-module [Main.WithCondInt]}\label{Main.WithCondInt}

  Le sous-module [Main.WithCondInt] correspond à [Eval.WithCondInt]
  (cf. section~\ref{Eval.WithCondInt}). En complétant ce dernier, il
  répond ainsi à la question~\ref{q3}. Du point de vue du pilotage,
  rien ne change: les erreurs possibles sont toujours les mêmes
  qu'avec [Main.WithLet]. *)

module WithCondInt = struct

  open Eval.WithCondInt
 
  let calc ast =
    try
      print_int (eval empty_env ast); print_newline(); exit 0
    with Err DivByZero -> 
           print_endline "Error: division by zero."; exit 1
       | Err (FreeVar x) ->
           print_endline ("Error: free variable " ^ x); exit 1
end

(*
  \subsection{Sous-module [Main.WithCond]}\label{Main.WithCond}

  Le sous-module [Main.WithCond] correspond à [Eval.WithCond]
  (cf. section~\ref{Eval.WithCond}). En complétant ce dernier, il
  répond ainsi à la question~\ref{q4}.

*)

module WithCond = struct

  open Eval.WithCond

  let calc ast =
    try
      print_int (eval empty_env ast); print_newline(); exit 0
    with Err DivByZero -> 
           print_endline "Error: division by zero."; exit 1
       | Err (FreeVar x) ->
           print_endline ("Error: free variable " ^ x); exit 1
end

(*
  \subsection{Sous-module [Main.WithEnvList]}\label{Main.WithEnvList}

  Le sous-module (Main.WithEnvList] correspond à [Eval.WithEnvList]
  (cf. section~\ref{Eval.WithEnvList}). En complétant ce dernier, il
  répond ainsi à la question~\ref{q5}. Nous devons néanmoins réécrire
  [Main.WithEnvList.calc] car le codage de l'environ\-ne\-ment vide à
  changé (c'est [[]] maintenant).

*)

module WithEnvList = struct

  open Eval.WithEnvList

  let calc ast =
    try
      print_int (eval [] ast); print_newline(); exit 0
    with Err DivByZero ->
           print_endline "Error: division by zero."; exit 1
       | Err (FreeVar x) ->
           print_endline ("Error: free variable " ^ x); exit 1
end

(* La fonction [main] est celle qui est appelée après le lancement du
  programme. Pour tester en boîte blanche (c.-à-d. en modifiant le
  code) il faut alors modifier les appels à [calc] et recompiler. Par
  exemple, nous avons ici l'appel à [WithEnvList.calc], que l'on peut
  remplacer par un appel à [WithCond.calc] etc. Cette méthode n'est
  pas bonne car, en général, un test en boîte grise ou noire est
  préférable. Pour cela il faudrait proposer,par exemple, à l'usager
  des options sur la ligne de commande de lancement de la calculette.
  *)

let main () =
  let () = 
    print_endline "Saisissez une expression + RET + CTRL-D:";
    flush stdout in
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.expression Lexer.token lexbuf in
  let () = print_string "=> " 
in WithEnvList.calc (ast)


(* Cette liaison globale est le point d'entrée du programme (il s'agit
  d'évaluer l'appel à [main]). Son intérêt est de demander à
  l'environnement d'exécution de tracer toutes les exceptions qui
  pourraient remonter de l'appel à [main].  *)

let _ = Printexc.print main ()


