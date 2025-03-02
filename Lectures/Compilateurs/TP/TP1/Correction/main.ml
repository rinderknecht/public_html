
(* Le module [Main] est le pilote de la calculette. C'est ce module
  \emph{uniquement} qui interagit avec l'usager et, en particulier,
  filtre les erreurs des autres modules (ici [Eval]). Il est compos�
  de plusieurs sous-modules correspondant chacun � un des sous-modules
  de [Eval] de m�me nom. Par exemple [Main.Initial] correspond � (en
  l'occurrence \emph{utilise}) [Eval.Initial].  *)

(*
  \subsection{Sous-module [Main.Initial]}\label{Main.Initial}

  Le sous-module [Main.Initial] correspond au cas initial avec les
  quatre op�rations arithm�tiques de base ainsi que les constantes. Il
  r�pond donc � la question~\ref{q1} en compl�tant le module
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
  (cf. section~\ref{Eval.WithLet}). En compl�tant ce dernier, il
  r�pond ainsi � la question~\ref{q2}. Les variables et la liaison
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

  Le sous-module [Main.WithCondInt] correspond � [Eval.WithCondInt]
  (cf. section~\ref{Eval.WithCondInt}). En compl�tant ce dernier, il
  r�pond ainsi � la question~\ref{q3}. Du point de vue du pilotage,
  rien ne change: les erreurs possibles sont toujours les m�mes
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

  Le sous-module [Main.WithCond] correspond � [Eval.WithCond]
  (cf. section~\ref{Eval.WithCond}). En compl�tant ce dernier, il
  r�pond ainsi � la question~\ref{q4}.

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

  Le sous-module (Main.WithEnvList] correspond � [Eval.WithEnvList]
  (cf. section~\ref{Eval.WithEnvList}). En compl�tant ce dernier, il
  r�pond ainsi � la question~\ref{q5}. Nous devons n�anmoins r��crire
  [Main.WithEnvList.calc] car le codage de l'environ\-ne\-ment vide �
  chang� (c'est [[]] maintenant).

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

(* La fonction [main] est celle qui est appel�e apr�s le lancement du
  programme. Pour tester en bo�te blanche (c.-�-d. en modifiant le
  code) il faut alors modifier les appels � [calc] et recompiler. Par
  exemple, nous avons ici l'appel � [WithEnvList.calc], que l'on peut
  remplacer par un appel � [WithCond.calc] etc. Cette m�thode n'est
  pas bonne car, en g�n�ral, un test en bo�te grise ou noire est
  pr�f�rable. Pour cela il faudrait proposer,par exemple, � l'usager
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


(* Cette liaison globale est le point d'entr�e du programme (il s'agit
  d'�valuer l'appel � [main]). Son int�r�t est de demander �
  l'environnement d'ex�cution de tracer toutes les exceptions qui
  pourraient remonter de l'appel � [main].  *)

let _ = Printexc.print main ()


