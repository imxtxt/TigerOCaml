{
  open Parser
  exception Lexer_error of string
  let strBuf = ref ""
}

let blank = [' ' '\t' '\r' '\n']
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
  | eof     { EOF }
  | blank+  { token lexbuf }

  | ","  { COMMA }
  | ":"  { COLON }
  | ";"  { SEMICOLON }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "["  { LBRACK }
  | "]"  { RBRACK }
  | "{"  { LBRACE }
  | "}"  { RBRACE }
  | "."  { DOT }
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { TIMES }
  | "/"  { DIVIDE }
  | "="  { EQ }
  | "<>" { NEQ }
  | "<"  { LT }
  | "<=" { LE }
  | ">"  { GT }
  | ">=" { GE }
  | "&"  { AND }
  | "|"  { OR }
  | ":=" { ASSIGN }

  | "for"      { FOR }
  | "while"    { WHILE }
  | "to"       { TO }
  | "break"    { BREAK }
  | "let"      { LET }
  | "in"       { IN }
  | "end"      { END }
  | "function" { FUNCTION }
  | "var"      { VAR }
  | "type"     { TYPE }
  | "array"    { ARRAY }
  | "if"       { IF }
  | "then"     { THEN }
  | "else"     { ELSE }
  | "do"       { DO }
  | "of"       { OF }
  | "nil"      { NIL }

  | '"'        { string lexbuf }

and string = parse
  | '"'  { 
            let t = !strBuf in 
            strBuf := "";
            STRING (t)
         }  
  | eof  { raise (Lexer_error (Printf.sprintf "String is not terminated")) }
  | _    { strBuf := !strBuf ^ (Lexing.lexeme lexbuf); string lexbuf }
