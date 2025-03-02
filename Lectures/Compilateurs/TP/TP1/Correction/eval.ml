
(* Nous donnons ici le contenu du module [Eval] (fichier
  \textsf{eval.ml}). Il contient plusieurs sous-modules correspondant
  chacun � une calculette avec diff�ren\-tes constructions qui sont
  ajout�es au fur et � mesure des questions. *)

open Ast

(*
\subsection{Sous-module [Eval.Initial]}\label{Eval.Initial}
*)

(* Le sous-module [Eval.Initial] regroupe les d�finitions de type et
  de valeurs correspondant � la calculette avec constantes et
  op�rateurs arithm�tiques. Il r�pond � la question~\ref{q1}. *)

module Initial = struct
  type value = int

  (* L'exception [DivByZero] sera d�clanch�e lorsqu'une division par
     z�ro sera anticip�e par la calculette.  *)

  exception DivByZero

  (* Le dernier cas du filtre de la fonction [eval] est n�cessaire
     car la syntaxe abstraite, c'est-�-dire le type [Ast.expr],
     inclut tous les constructeurs pour r�aliser l'ensemble du
     travail pratique.  *)

  let rec eval e = match e with
    Const n -> n
  | BinOp (op,e1,e2) ->
      let v1 = eval e1 and v2 = eval e2
      in begin match op with
           Add -> v1 + v2 | Sub -> v1 - v2 | Mult -> v1 * v2
         | Div -> if v2 = 0 then raise DivByZero else v1/v2
         end
  | _ -> failwith "Construction non trait�e"
end (*c module Initial *)

(*
  \subsection{Sous-module [Eval.WithLet]}\label{Eval.WithLet}
*)

(* Le module [Eval.WithLet] reprend le module [Eval.Initial] et
  l'�tend avec les variables et la liaison locale. Il r�pond � la
  question~\ref{q2}. *)

module WithLet = struct
  type value = int

  (* Les environnements $\rho$ sont cod�s de fa�on fonctionnelle. *)

  type environnement = string -> value

  (* La fonction [empty_env] implante l'environnement vide.  *)

  let empty_env = fun x -> raise Not_found

  (* La fonction [extend] implante l'extension des environnements par
    une nouvelle liaison (il s'agit de l'op�rateur $\oplus$). Ainsi
    l'implantation de $\rho \oplus x \mapsto v$ est [extend env
    (x,v)]. *)

  let extend env (x,v) = fun y -> if x = y then v else env y

  (* Il y a maintenant deux types d'erreurs: la division par z�ro et
    les variables libres (c'est-�-dire non introduites par un
    \textsf{let}). Il est alors utile de regrouper ces erreurs sous un
    m�me type. Ainsi, l'exception [Err (FreeVar x)] est d�clanch�e
    quand la variable [x] est libre dans l'expression. Par exemple:
    \texttt{let a = 1 in b} (ici la variable libre est \texttt{b}).
    *)

  type error = 
    DivByZero
  | FreeVar of string

  exception Err of error

  (* La fonction [eval] prend ici un argument suppl�mentaire:
    l'environ\-nement dans lequel l'expression doit �tre �valu�e.  *)

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
  | _ -> failwith "Construction non trait�e"
end (*c module WithLet*)

(*
  \subsection{Sous-module [Eval.WithCondInt]}\label{Eval.WithCondInt}
*)

(* Le module [Eval.WithCondInt] reprend et �tend le module
  [Eval.WithLet] avec une construction conditionnelle discriminant les
  branches avec un entier (� z�ro ou non), dans le style du langage
  C. Il r�pond � la question~\ref{q3}. On nous donne:

  \begin{itemize}

    \ibullet \textbf{Syntaxe concr�te}
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
  \noindent Une s�mantique op�rationnelle pour notre conditionnelle est:

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
  [eval] est imm�diat.  *)

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
  | _ -> failwith "Construction non trait�e"
end (*c module WithCondInt *)

(*
  \subsection{Sous-module [Eval.WithCond]}\label{Eval.WithCond}
*)

(* Le module [Eval.WithCond] reprend le module [Eval.WithCondInt] et
  l'�tend avec une conditionnelle qui discrimine ses branches � l'aide
  d'une expression bool�enne (comme en O'Caml, par exemple). Il r�pond
  � la question~\ref{q4}. Nous devons ajouter au langage de la
  calculette les expressions bool�ennes.  Pour faciliter le travail,
  cela est d�ja fait au niveau de la syntaxe concr�te et abstraite --
  cf. \textsf{lexer.mll}, \textsf{parser.mly} et \textsf{ast.ml}. Dans
  ce dernier, la syntaxe abstraite contient la d�finition des
  bool�ens.  *)

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
  Voici les syntaxes reconnues par d�faut par la calculette:

  \begin{itemize}

    \ibullet \textbf{Syntaxe concr�te}
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

  On remarque que la solution pr�sent�e ici ne permet pas qu'un
  bool�en soit une expression, donc le type des valeurs restera
  inchang� dans la s�mantique, mais il faudra une seconde relation
  d'�valuation d�di�e aux bool�ens. Donc, en th�orie, il faudrait
  choisir une autre notation que $\eval{\rho}{e}{v}$, mais en pratique
  c'est commode de reprendre la m�me, � ceci pr�s que les expressions
  bool�ennes n'ont pas besoin d'un environnement pour s'�valuer car
  nous n'avons pas permis au \texttt{let ... in ...} de lier des
  bool�ens. Donc nous noterons la relation d'�valuation des bool�ens
  $\evalb{b}{\overline{b}}$, o� $b$ est une valeur O'Caml de type
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

  L'implantation de cette s�mantique est directe (elle s'appuie
  inductivement sur la syntaxe abstraite). 

  Remarque: le code qui suit a �t� enjoliv� par un outil (nomm�
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
  fonction [eval] est imm�diat.  *)

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
  | _ -> failwith "Construction non trait�e"
end (*c module WithCond *)

(*
  \subsection{Sous-module [Eval.WithEnvList]}\label{Eval.WithEnvList}
*)

(* Le module [Eval.WithEnvList] reprend le module [Eval.WithCond] en
  rempla�ant le codage fonctionnel de l'environnement par un codage �
  l'aide d'une structure de donn�e nomm�e \emph{liste
  d'association}. Il r�pond � la question~\ref{q5}. Une liste
  d'association est une liste de paires qui mod�lisent les
  liaisons. Ainsi, $\rho(x)$ est implant� non plus par [env x] ([env]
  est une fonction O'Caml) mais par [List.assoc x env] ([env] est une
  liste d'association). La raison est essentiellement l'efficacit� de
  la recherche d'une liaison, bien qu'il serait encore plus efficace
  d'utiliser une autre structure de donn�e, comme une table de
  hachage, si le nombre de variable est �lev� en moyenne par
  programme. *)

module WithEnvList = struct
  type value = int

  (* Les environnements $\rho$ sont cod�s � l'aide d'une liste
     d'association. L'envi\-ron\-nement vide est alors simplement cod�
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
    t�te de liste: $\rho \oplus x \mapsto v$ est cod� par [(x,v)::env]
    au lieu de [extend env (x,v)] avec un codage fonctionnel. On n'a
    donc plus besoin de la fonction [extend].  *)

  let rec eval_bool b = match b with
    True -> true
  | False -> false
  | And (b1,b2) -> (eval_bool b1) && (eval_bool b2)
  | Or (b1,b2) -> (eval_bool b1) || (eval_bool b2)
  | Not b -> not (eval_bool b)

  (* Dans la d�finition de la fonction [eval], le motif [Var x] change
     et appelle [lookup x env] dor�navant, et le cas de la liaison
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
  | _ -> failwith "Construction non trait�e"
end (*c module WithEnvList*)

