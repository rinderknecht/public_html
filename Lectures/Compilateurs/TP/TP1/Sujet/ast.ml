
(* La syntaxe abstraite des expressions booléennes est *)

type bexpr = 
  And of bexpr * bexpr
| Or of bexpr * bexpr
| Not of bexpr
| True
| False

(* La syntaxe abstraite des expressions est *)

type expr =
  Const of int
| BinOp of bin_op * expr * expr
| Var of string
| Let of string * expr * expr
| LetRec of string * expr * expr
| Fun of string * expr
| App of expr * expr
| If of bexpr * expr * expr
| Ifz of expr * expr * expr
| Assign of string * expr
| U

and bin_op = Add | Sub | Mult | Div


(* L'expression O'Caml [fv e] représente les variables libres
 (\emph{free variables}) de l'expression [e]. La fonction O'Caml [fv]
 implante la fonction mathématique ${\cal L}$ définie par
\begin{align*}
{\cal L} (\cst{Const} \,\, \_) & = \varnothing\\
{\cal L} (\cst{Var} \,\, x) & = \{x\}\\
{\cal L} (\cst{BinOp} \, (\_,e_1,e_2)) & = {\cal L}(e_1) \cup {\cal
 L}(e_2)\\ 
{\cal L} (\cst{Let} \, (x,e_1,e_2)) & = {\cal L}(e_1) \cup ({\cal
 L}(e_2) \backslash \{x\})\\
{\cal L} (\cst{LetRec} \, (x,e_1,e_2)) & = ({\cal L}(e_1) \cup
   {\cal L}(e_2)) \backslash \{x\}\\
{\cal L} (\cst{Fun} \, (x, e)) & = {\cal L} (e) \backslash \{x\}\\
{\cal L} (\cst{App} \, (e_1,e_2)) & = {\cal L} (e_1) \cup {\cal L}
 (e_2)\\
{\cal L} (\cst{If} \, (\_,e_1,e_2)) & = {\cal L} (e_1) \cup {\cal L}
 (e_2)\\
{\cal L} (\cst{Ifz} \, (e_0,e_1,e_2)) & = {\cal L} (e_0) \cup {\cal L} (e_1) \cup {\cal L} (e_2)
\end{align*}
*)

module StringSet = 
  Set.Make (struct type t = string let compare = compare end)

open StringSet

let rec fv e = match e with
  Const _ -> empty
| BinOp (_,e1,e2) -> union (fv e1) (fv e2)
| Var x -> singleton x
| Let (x,e1,e2) 
| LetRec (x,e1,e2) -> union (fv e1) (remove x (fv e2))
| Fun (x,e) -> remove x (fv e)
| App (e1,e2) -> union (fv e1) (fv e2)
| If (_,e1,e2) -> union (fv e1) (fv e2)
| Ifz (e0,e1,e2) -> union (fv e0) (union (fv e1) (fv e2))
| Assign (x,e) -> fv e
| U -> empty


