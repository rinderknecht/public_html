%{
%}

/* Token definitions */

%token ZERO
%token <int> PNZ
%token <string> VAR
%token PLUS
%token MINUS
%token EQUAL
%token EOF

%start system
%type <Ast.system> system

%%

system:
  equation subsystem EOF { $1::$2 }
;

subsystem:
  equation subsystem { $1::$2 }
|                    { [] }
;

equation:
  combination EQUAL pos { ($1, $3) }
;

term:
  pnz VAR      { ($2, $1) }
| sign pnz VAR { ($3, $1($2)) }
;

combination:
  term             { [$1] }
| term combination { $1::$2 }
;

pnz:
  PNZ { $1 }
|     { 1 }
;

pos:
  sign PNZ  { $1($2) }
| PNZ       { $1 }
| ZERO      { 0 }
;

sign:
  PLUS  { fun x -> x  }
| MINUS { fun x -> -x }
;
