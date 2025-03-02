{
  open Parser;;
  exception Error of string
}

let ident = ['a'-'z'] ['_' 'A'-'Z' 'a'-'z' '0'-'9']*

rule token = parse
  [' ' '\n' '\t' '\r'] { token (lexbuf) }
| "let"                { LET }
| "rec"                { REC }
| "="                  { EQUAL }
| "in"                 { IN }
| "if"                 { IF }
| "true"               { TRUE }
| "false"              { FALSE }
| "and"                { AND }
| "or"                 { OR }
| "not"                { NOT }
| "ifz"                { IFZ }
| "then"               { THEN }
| "else"               { ELSE }
| "fun"                { FUN }
| "->"                 { ARROW }
| "("                  { LPAR }
| ")"                  { RPAR }
| "()"                 { UNIT }
| "+"                  { PLUS }
| "-"                  { MINUS }
| "/"                  { SLASH }
| "*"                  { TIMES }
| ":="                 { ASSIGN }
| ident                { IDENT (Lexing.lexeme lexbuf) }
| ['0'-'9']+   
    { let lexeme = Lexing.lexeme lexbuf in
        let n = float_of_string lexeme in
        let max_bits = int_of_float (ceil (log n /. log 2.0))
      in if max_bits > 32
         then raise (Error ("Integer constant too large: " ^ lexeme))
         else INT (int_of_float n)
    }
| eof                  { EOF }
| "//" [^ '\n']* '\n'? { token lexbuf }
| _  { raise (Error ("Illegal character " ^ Lexing.lexeme lexbuf)) }

