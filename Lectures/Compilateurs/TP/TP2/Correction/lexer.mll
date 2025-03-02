{ 
  open Parser

  exception Illegal_char of string
  exception Open_comment

  let keywords = [("let",LET); ("rec",REC); ("in",IN); ("if",IF);
    ("true",TRUE); ("false",FALSE); ("and",AND); ("or",OR);
    ("not",NOT); ("ifz",IFZ); ("then",THEN); ("else",ELSE); ("fun",FUN)
  ]

  let kwd_tbl : (string, token) Hashtbl.t = 
    Hashtbl.create (List.length keywords)

  let () = 
    List.iter
      (fun (str,tok) -> Hashtbl.add kwd_tbl str tok) keywords
}

let ident = ['a'-'z'] (['_' 'A'-'Z' 'a'-'z' '0'-'9'])*

rule token = parse
  [' ' '\n' '\t' '\r']  { token lexbuf }
| "="                   { EQUAL }
| "->"                  { ARROW }
| "("                   { LPAR }
| ")"                   { RPAR }
| "()"                  { UNIT }
| "+"                   { PLUS }
| "-"                   { MINUS }
| "/"                   { SLASH }
| "*"                   { TIMES }
| ":="                  { ASSIGN }
| ident                 { let str = Lexing.lexeme lexbuf
                          in try Hashtbl.find kwd_tbl str with
                               Not_found -> IDENT str }
| ['0'-'9']+            { INT (int_of_string (Lexing.lexeme lexbuf)) }
| "//" [^ '\n']* '\n'?  { token lexbuf }
| "/*"                  { in_comment lexbuf 1 }
| eof                   { EOF }
| _                     { raise (Illegal_char (Lexing.lexeme lexbuf)) }
and in_comment = parse
  "*/"   { fun depth -> if depth = 1 then token lexbuf 
                        else in_comment lexbuf (depth-1) }
| "/*"   { fun depth -> in_comment lexbuf (depth+1) }
| _      { in_comment lexbuf }
| eof    { raise Open_comment }
