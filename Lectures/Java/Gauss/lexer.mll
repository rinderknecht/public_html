{}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let alpha = digit | letter
let ident = letter (('_')* alpha)*
let pos_non_zero = ['1'-'9'] ['0'-'9']*

rule token = parse
  [' ' '\013' '\009' '\010' '\012']+
           { token lexbuf }
| pos_non_zero 
           { let n = Lexing.lexeme (lexbuf) 
             in Parser.PNZ (int_of_string n) }
| '0'      { Parser.ZERO }
| ident    { let id =  Lexing.lexeme (lexbuf) in Parser.VAR (id) }
| '+'      { Parser.PLUS }
| '-'      { Parser.MINUS }
| '='      { Parser.EQUAL }
| eof      { Parser.EOF }

{
let string_of_token = function
  Parser.ZERO -> "0\n"
| Parser.PNZ n -> " " ^ string_of_int n
| Parser.VAR s -> s
| Parser.PLUS  -> " +"
| Parser.MINUS -> " -"
| Parser.EQUAL -> " ="
| Parser.EOF   -> " <EOF>\n"

 
let trace_token lex_buf =
  let t = token lex_buf
in print_string (string_of_token (t)); t
    
let trace filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let rec aux lex_buf =
    match trace_token lex_buf with
      Parser.EOF -> close_in ic
    | _ -> aux lex_buf
 in aux lexbuf
}
