%{
  open Ast
%}

%token EOF

%token <string> ID
%token <string> STRING
%token <int>    INT

%token COMMA 
%token COLON 
%token SEMICOLON 
%token LPAREN
%token RPAREN
%token LBRACK 
%token RBRACK 
%token LBRACE
%token RBRACE 
%token DOT 
%token PLUS 
%token MINUS 
%token TIMES 
%token DIVIDE 
%token EQ 
%token NEQ 
%token LT 
%token LE 
%token GT 
%token GE
%token AND 
%token OR 
%token ASSIGN
%token ARRAY 
%token IF 
%token THEN 
%token ELSE 
%token WHILE 
%token FOR 
%token TO 
%token DO 
%token LET 
%token IN 
%token END 
%token OF 
%token BREAK 
%token NIL
%token FUNCTION 
%token VAR
%token TYPE

%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ LT LE GE GT 
%left PLUS MINUS
%left TIMES DIVIDE


%start prog
%type <Ast.exp> prog
%%

prog: 
  | e=exp EOF { e }

exp:
  | v=var                                                    { Var v }
  | NIL                                                      { NIL }
  | i=INT                                                    { Int i }
  | s=STRING                                                 { Str s }
  | id=ID LPAREN l=separated_list(COMMA ,exp) RPAREN         { Call (id, l) }
  | l=exp b=bop r=exp                                        { 
                                                               match b with
                                                               | And -> If (l, r, Int 0)
                                                               | Or  -> If (l, Int 1, r)
                                                               | _   -> Bop (b, l, r) 
                                                             }

  | i=ID LBRACE l=separated_list(SEMICOLON, refield) RBRACE  { Record (i, l) }
  | LPAREN l=separated_list(SEMICOLON, exp) RPAREN           { l }
  | v=var ASSIGN e=exp                                       { Assign (v, e) }
  | IF c=exp THEN t=exp                                      { If (c, t, None) }
  | IF c=exp THEN t=exp ELSE e=exp                           { If (c, t, Some e)}
  | WHILE c=exp DO e=exp                                     { While (c, e)}
  | FOR i=ID ASSIGN l=exp TO h=exp DO b=exp                  { For (i, l, h, b)}
  | BREAK                                                    { Break }
  | LET d=list(dec) IN l=separated_list(SEMICOLON, exp) END  { Let (d, Seq l)}
  | i=ID LBRACK len=exp RBRACK OF init=exp                   { Array (i, len, init) }

refield:
  | i=ID EQ e=exp  { (i, e) }

%inline bop:
  | PLUS   { Plus }
  | MINUS  { Minus }
  | TIMES  { Times }
  | DIVIDE { Divide }
  | EQ     { Eq }
  | NEQ    { Neq }
  | LT     { Lt }
  | LE     { LE }
  | GT     { Gt }
  | GE     { Ge }
  | AND    { And }
  | OR     { Or }

ty:
  | i=ID                                          { NamedTy i }
  | LBRACE l=separated_list(COMMA, rfield) RBRACE { l }
  | ARRAY OF i=ID                                 { ArrayTy i}

rfield:
  | i=ID COLON t=ID { (i, t)}

dec:
  | TYPE i=ID EQ t=ty                     { TypeDecl (i, t) }
  | VAR i=ID ASSIGN e=exp                 { VarDecl (i, None, e) }
  | VAR i=ID COLON t=ID ASSIGN e=exp      { VarDecl (i, Some t, e) }
  | FUNCTION i=ID LPAREN l=separated_list(COMMA, rfield) RPAREN EQ e=exp 
                                          { { name = i; param = l; result = None; body = e} }
  | FUNCTION i=ID LPAREN l=separated_list(COMMA, rfield) RPAREN COLON r=ID EQ e=exp 
                                          { { name = i; param = l; result = Some r; body = e} }

var:
  | i=ID                      { Simple i }
  | v=var DOT i=ID            { Field (v, i) }
  | v=var LBRACK e=exp RBRACK { ArrSelect (v, e) }