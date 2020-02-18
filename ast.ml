type bop =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge

type var = 
  | Simp of string
  | Field of var * string
  | ArrSelt of var * exp

and exp = 
  | Var of var
  | Nil
  | Int of int
  | Str of string
  | Call of string * exp list
  | Bop of bop * exp * exp
  | Record of string * (string * exp) list
  | Seq of exp list
  | Assign of var * exp
  | If of exp * exp * exp option
  | While of exp * exp
  | ForExp of string * exp * exp * exp
  | BreakExp
  | LetExp of dec list * exp
  | ArrayExp of string * exp * exp

and dec = 
  | FunDecl of func list
  | VarDecl of string * string * exp
  | TypeDecl of string * ty

and ty =
  | NamedTy of string
  | RecordTy of (string * string) list
  | ArrayTy of string

and func = {
  name: string; 
  param: (string * string) list;
  result: string;
  body: exp
}
