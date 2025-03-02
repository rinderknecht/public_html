
(* Nous donnons ici le contenu du module [Eval] (fichier
  \textsf{eval.ml}). Il contient plusieurs sous-modules correspondant
  chacun à une calculette avec différen\-tes constructions qui sont
  ajoutées au fur et à mesure des questions. *)

open Ast

(*
\subsection{Sous-module [Eval.Initial]}\label{Eval.Initial}
*)

(* Le sous-module [Eval.Initial] regroupe les définitions de type et
  de valeurs correspondant à la calculette avec constantes et
  opérateurs arithmétiques. Il répond à la question~\ref{q1}. *)

module Initial = struct
  type value = int

  (* L'exception [DivByZero] sera déclanchée lorsqu'une division par
     zéro sera anticipée par la calculette.  *)

  exception DivByZero

  (* Le dernier cas du filtre de la fonction [eval] est nécessaire
     car la syntaxe abstraite, c'est-à-dire le type [Ast.expr],
     inclut tous les constructeurs pour réaliser l'ensemble du
     travail pratique.  *)

  let rec eval e = match e with
    Const n -> n
  | BinOp (op,e1,e2) ->
      let v1 = eval e1 and v2 = eval e2
      in begin match op with
           Add -> v1 + v2 | Sub -> v1 - v2 | Mult -> v1 * v2
         | Div -> if v2 = 0 then raise DivByZero else v1/v2
         end
  | _ -> failwith "Construction non traitée"
end (*c module Initial *)

(*
  \subsection{Sous-module [Eval.WithLet]}\label{Eval.WithLet}
*)

(* Le module [Eval.WithLet] reprend le module [Eval.Initial] et
  l'étend avec les variables et la liaison locale. Il répond à la
  question~\ref{q2}. *)

module WithLet = struct
  type value = int

  (* Les environnements $\rho$ sont codés de façon fonctionnelle. *)

  type environnement = string -> value

  (* La fonction [empty_env] implante l'environnement vide.  *)

  let empty_env = fun x -> raise Not_found

  (* La fonction [extend] implante l'extension des environnements par
    une nouvelle liaison (il s'agit de l'opérateur $\oplus$). Ainsi
    l'implantation de $\rho \oplus x \mapsto v$ est [extend env
    (x,v)]. *)

  let extend env (x,v) = fun y -> if x = y then v else env y

  (* Il y a maintenant deux types d'erreurs: la division par zéro et
    les variables libres (c'est-à-dire non introduites par un
    \textsf{let}). Il est alors utile de regrouper ces erreurs sous un
    même type. Ainsi, l'exception [Err (FreeVar x)] est déclanchée
    quand la variable [x] est libre dans l'expression. Par exemple:
    \texttt{let a = 1 in b} (ici la variable libre est \texttt{b}).
    *)

  type error = 
    DivByZero
  | FreeVar of string

  exception Err of error

  (* La fonction [eval] prend ici un argument supplémentaire:
    l'environ\-nement dans lequel l'expression doit être évaluée.  *)

  let rec eval env e = match e with
    Const n -> n
  | BinOp (op,e1,e2) ->
      let v1 = eval env e1 and v2 = eval env e2
      in begin match op with
           Add -> v1 + v2 | Sub -> v1 - v2 | Mult -> v1 * v2
         | Div -> if v2 = 0 then raise (Err DivByZero) else v1/v2
         end
  | Var x -> begin try env x with 
               Not_found -> raise (Err (FreeVar x))
             end
  | Let (x,e1,e2) -> 
      let v1 = eval env e1 in eval (extend env (x,v1)) e2
  | _ -> failwith "Construction non traitée"
end (*c module WithLet*)

(*
  \subsection{Sous-module [Eval.WithCondInt]}\label{Eval.WithCondInt}
*)

(* Le module [Eval.WithCondInt] reprend et étend le module
  [Eval.WithLet] avec une construction conditionnelle discriminant les
  branches avec un entier (à zéro ou non), dans le style du langage
  C. Il répond à la question~\ref{q3}. On nous donne:

  \begin{itemize}

    \ibullet \textbf{Syntaxe concrète}
  {\small 
   \begin{verbatim}
Expression ::= ...
  | "ifz" Expression "then" Expression "else" Expression
   \end{verbatim}
}

  \vspace*{-15pt}

  \ibullet \textbf{Exemple} \qquad \texttt{1+2*(ifz 0 then 3
  else 4)}

  \ibullet \textbf{Syntaxe abstraite}

  \kwd{type} \type{expr} \equal{} \texttt{...} \vbar{} \cst{Ifz}
  \kwd{of} \type{expr} \texttt{*} \type{expr} \texttt{*} \type{expr}

\end{itemize}
  *)

module WithCondInt = struct
  type value = int
  type environnement = string -> value

  let empty_env = fun x -> raise Not_found
  let extend env (x,v) = fun y -> if x = y then v else env (y)

  type error = 
    DivByZero
  | FreeVar of string

  exception Err of error

  (* 
  \noindent Une sémantique opérationnelle pour notre conditionnelle est:

  \begin{mathpar}
  \inferlabel{if-then}
    {\eval{\rho}{e_1}{\cst{Int} \, (n)}\\
     n \neq 0\\
     \eval{\rho}{e_2}{v_2}}
    {\eval{\rho}{\cst{Ifz} \, (e_1,e_2,e_3)}{v_2}}
  \and
  \inferlabel{if-else}
    {\eval{\rho}{e_1}{\cst{Int} \, (0)}\\
     \eval{\rho}{e_3}{v_3}}
    {\eval{\rho}{\cst{Ifz} \, (e_1,e_2,e_3)}{v_3}}
  \end{mathpar}
*)

(* L'implantation par un nouveau cas dans le filtre de la fonction
  [eval] est immédiat.  *)

  let rec eval env e = match e with
    Const n -> n
  | BinOp (op,e1,e2) ->
      let v1 = eval env e1 and v2 = eval env e2
      in begin match op with
           Add -> v1 + v2 | Sub -> v1 - v2 | Mult -> v1 * v2
         | Div -> if v2 = 0 then raise (Err DivByZero) else v1/v2
         end
  | Var x -> begin try env x with 
               Not_found -> raise (Err (FreeVar x))
             end
  | Let (x,e1,e2) -> 
      let v1 = eval env e1 in eval (extend env (x,v1)) e2
  | Ifz (e1,e2,e3) ->
      if eval env e1 = 0 then eval env e3 else eval env e2
  | _ -> failwith "Construction non traitée"
end (*c module WithCondInt *)

(*
  \subsection{Sous-module [Eval.WithCond]}\label{Eval.WithCond}
*)

(* Le module [Eval.WithCond] reprend le module [Eval.WithCondInt] et
  l'étend avec une conditionnelle qui discrimine ses branches à l'aide
  d'une expression booléenne (comme en O'Caml, par exemple). Il répond
  à la question~\ref{q4}. Nous devons ajouter au langage de la
  calculette les expressions booléennes.  Pour faciliter le travail,
  cela est déja fait au niveau de la syntaxe concrète et abstraite --
  cf. \textsf{lexer.mll}, \textsf{parser.mly} et \textsf{ast.ml}. Dans
  ce dernier, la syntaxe abstraite contient la définition des
  booléens.  *)

module WithCond = struct
  type value = int
  type environnement = string -> value

  let empty_env = fun x -> raise Not_found
  let extend env (x,v) = fun y -> if x = y then v else env y

  type error = 
    DivByZero
  | FreeVar of string

  exception Err of error

(*
  Voici les syntaxes reconnues par défaut par la calculette:

  \begin{itemize}

    \ibullet \textbf{Syntaxe concrète}
  {\small 
   \begin{verbatim}
Boolean ::= true | false 
      | Boolean "and" Boolean 
      | Boolean "or" Boolean
      | "not" "(" Boolean ")"
Expression ::=  ... 
      | "if" Boolean "then" Expression "else" Expression
   \end{verbatim}
}

  \vspace*{-6pt}

  \ibullet \textbf{Exemple} \qquad \texttt{1+2*(if true then 3
  else 4)}

  \ibullet \textbf{Syntaxe abstraite}
  
  \begin{tabbing}
    \kwd{type} \type{boolean} \= \equal{} \= \cst{True} \vbar{}
    \cst{False} \vbar{} \cst{Not} \kwd{of} \type{boolean}\\
    \> \vbar \> \cst{And} \kwd{of} \type{boolean} \(\times\)
    \type{boolean}\\
    \> \vbar \> \cst{Or} \kwd{of} \type{boolean}
    \(\times\) \type{boolean}\\ 
    \kwd{type} \type{expr} \equal{} \texttt{...} \vbar{} \cst{If}
    \kwd{of} \type{boolean} \(\times\) \type{expr} \(\times\)
    \type{expr}
  \end{tabbing}

  \end{itemize}

  On remarque que la solution présentée ici ne permet pas qu'un
  booléen soit une expression, donc le type des valeurs restera
  inchangé dans la sémantique, mais il faudra une seconde relation
  d'évaluation dédiée aux booléens. Donc, en théorie, il faudrait
  choisir une autre notation que $\eval{\rho}{e}{v}$, mais en pratique
  c'est commode de reprendre la même, à ceci près que les expressions
  booléennes n'ont pas besoin d'un environnement pour s'évaluer car
  nous n'avons pas permis au \texttt{let ... in ...} de lier des
  booléens. Donc nous noterons la relation d'évaluation des booléens
  $\evalb{b}{\overline{b}}$, où $b$ est une valeur O'Caml de type
  [Ast.bexpr] et $\overline{b}$ est de type \type{bool}.

  \begin{mathpar}
  \inferlabel{if-then}
    {\evalb{b}{\kwd{true}}\\
     \eval{\rho}{e_2}{v_2}}
    {\eval{\rho}{\cst{If} \, (b,e_2,e_3)}{v_2}}
  \and
  \inferlabel{if-else}
    {\evalb{b}{\kwd{false}}\\
     \eval{\rho}{e_3}{v_3}}
    {\eval{\rho}{\cst{If} \, (b,e_2,e_3)}{v_3}}
  \end{mathpar}

  L'implantation de cette sémantique est directe (elle s'appuie
  inductivement sur la syntaxe abstraite). 

  Remarque: le code qui suit a été enjolivé par un outil (nommé
  \textsf{ocamlweb}) qui produit $\land$ au lieu de \textsf{\&\&},
  $\lor$ au lieu de \textsf{||} et $\lnot$ au lieu de \textsf{not}.
  *)
  
  let rec eval_bool b = match b with
    True -> true
  | False -> false
  | And (b1,b2) -> (eval_bool b1) && (eval_bool b2)
  | Or (b1,b2) -> (eval_bool b1) || (eval_bool b2)
  | Not b -> not (eval_bool b)

(* Maintenant l'implantation par un nouveau cas dans le filtre de la
  fonction [eval] est immédiat.  *)

  let rec eval env e = match e with
    Const n -> n
  | BinOp (op,e1,e2) ->
      let v1 = eval env e1 and v2 = eval env e2
      in begin match op with
           Add -> v1 + v2 | Sub -> v1 - v2 | Mult -> v1 * v2
         | Div -> if v2 = 0 then raise (Err DivByZero) else v1/v2
         end
  | Var x -> begin try env x with 
               Not_found -> raise (Err (FreeVar x))
             end
  | Let (x,e1,e2) -> 
      let v1 = eval env e1 in eval (extend env (x,v1)) e2
  | Ifz (e1,e2,e3) ->
      if eval env e1 = 0 then eval env e3 else eval env e2
  | If (b,e1,e2) ->
      if eval_bool b then eval env e1 else eval env e2
  | _ -> failwith "Construction non traitée"
end (*c module WithCond *)

(*
  \subsection{Sous-module [Eval.WithEnvList]}\label{Eval.WithEnvList}
*)

(* Le module [Eval.WithEnvList] reprend le module [Eval.WithCond] en
  remplaçant le codage fonctionnel de l'environnement par un codage à
  l'aide d'une structure de donnée nommée \emph{liste
  d'association}. Il répond à la question~\ref{q5}. Une liste
  d'association est une liste de paires qui modélisent les
  liaisons. Ainsi, $\rho(x)$ est implanté non plus par [env x] ([env]
  est une fonction O'Caml) mais par [List.assoc x env] ([env] est une
  liste d'association). La raison est essentiellement l'efficacité de
  la recherche d'une liaison, bien qu'il serait encore plus efficace
  d'utiliser une autre structure de donnée, comme une table de
  hachage, si le nombre de variable est élevé en moyenne par
  programme. *)

module WithEnvList = struct
  type value = int

  (* Les environnements $\rho$ sont codés à l'aide d'une liste
     d'association. L'envi\-ron\-nement vide est alors simplement codé
     par [[]] (plus besoin de [empty_env]). *)

  type environnement = (string * value) list

  type error = 
    DivByZero
  | FreeVar of string

  exception Err of error

  (* Au lieu de coder $\rho(x)$ par [env x] (plus un [try with], on
    fait [lookup x env].  *)

  let lookup x env =
    try List.assoc x env with
      Not_found -> raise (Err (FreeVar x))

  (* L'ajout d'une nouvelle liaison devient l'ajout d'une paire an
    tête de liste: $\rho \oplus x \mapsto v$ est codé par [(x,v)::env]
    au lieu de [extend env (x,v)] avec un codage fonctionnel. On n'a
    donc plus besoin de la fonction [extend].  *)

  let rec eval_bool b = match b with
    True -> true
  | False -> false
  | And (b1,b2) -> (eval_bool b1) && (eval_bool b2)
  | Or (b1,b2) -> (eval_bool b1) || (eval_bool b2)
  | Not b -> not (eval_bool b)

  (* Dans la définition de la fonction [eval], le motif [Var x] change
     et appelle [lookup x env] dorénavant, et le cas de la liaison
     locale est plus simple. *)

  let rec eval env e = match e with
    Const n -> n
  | BinOp (op,e1,e2) ->
      let v1 = eval env e1 and v2 = eval env e2
      in begin match op with
           Add -> v1 + v2 | Sub -> v1 - v2 | Mult -> v1 * v2
         | Div -> if v2 = 0 then raise (Err DivByZero) else v1/v2
         end
  | Ifz (e1,e2,e3) ->
      if eval env e1 = 0 then eval env e3 else eval env e2
  | If (b,e1,e2) ->
      if eval_bool b then eval env e1 else eval env e2
  | Var x -> lookup x env
  | Let (s,e1,e2) -> let v1 = eval env e1 in eval ((s,v1)::env) e2
  | _ -> failwith "Construction non traitée"
end (*c module WithEnvList*)

