type bop =
  | Plus
  | Minus
  | Times
  | Divide
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge

  (* Not used *)
  | And
  | Or

type var = 
  | Simple of string
  | Field of var * string
  | ArrSelect of var * exp

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
  | For of string * exp * exp * exp
  | Break
  | Let of dec list * exp
  | Array of string * exp * exp

and dec = 
  | FunDecl of func list
  | VarDecl of string * string option * exp
  | TypeDecl of string * ty

and ty =
  | NamedTy of string
  | RecordTy of (string * string) list
  | ArrayTy of string

and func = {
  name: string; 
  param: (string * string) list;
  result: string option;
  body: exp
}
