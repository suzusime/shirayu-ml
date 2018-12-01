(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | Or | And

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp (* if e1 then e2 else e3*)
  | LetExp of id * exp * exp (* let id = e1 in e2 *)
  | LetRecExp of id * id * exp * exp (* let rec i1 i2 = e1 in e2 *)
  | FunExp of id * exp
  | AppExp of exp * exp

type program =
    Exp of exp
  | Decl of id * exp (* let id = e;; *)
  | RecDecl of id * id * exp (* let rec i1 = fun i2 -> e;; *)
