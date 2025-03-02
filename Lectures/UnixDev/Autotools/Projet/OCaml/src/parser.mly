%{
  open Ast;;
%}

%token <string> IDENT
%token <int> INT
%token TRUE FALSE 

%token PLUS MINUS SLASH TIMES ARROW LPAR RPAR EQUAL
%token ASSIGN UNIT

%token LET REC IN FUN 
%token IFZ IF THEN ELSE
%token AND OR NOT

%token EOF

%start expression
%type <Ast.expr> expression

%%

expression: gexpr EOF   { $1 }
;

gexpr: 
  expr1                              { $1 }
| LET IDENT EQUAL gexpr IN gexpr     { Let ($2,$4,$6) }
| LET REC IDENT EQUAL gexpr IN gexpr { LetRec ($3,$5,$7) }
| IF bexpr THEN gexpr ELSE gexpr     { If ($2,$4,$6) }
| IFZ gexpr THEN gexpr ELSE gexpr    { Ifz ($2,$4,$6) }
| FUN IDENT ARROW gexpr              { Fun ($2,$4) }
| IDENT ASSIGN gexpr                 { Assign ($1,$3) }
;

expr1:
  expr1 PLUS expr2      { BinOp (Add,$1,$3) }
| expr1 MINUS expr2     { BinOp (Sub,$1,$3) }
| expr2                 { $1 }
;

expr2:
  expr2 TIMES expr3      { BinOp (Mult,$1,$3) }
| expr2 SLASH expr3       { BinOp (Div,$1,$3) }
| expr3                 { $1 }
;

expr3:
  expr3 expr4           { App ($1,$2) }
| expr4                 { $1 }
;

expr4:
  INT                   { Const ($1) }
| IDENT                 { Var ($1) }
| UNIT                  { U }
| LPAR gexpr RPAR       { $2 }
;

bexpr:
  bexpr1 OR bexpr1 { Or ($1,$3) }
| bexpr1           { $1 }
;

bexpr1:
  bexpr2 AND bexpr2 { And ($1,$3) }
| bexpr2            { $1 }
;

bexpr2:
  TRUE      { True }
| FALSE     { False}
| NOT LPAR bexpr RPAR { Not ($3) }
;
