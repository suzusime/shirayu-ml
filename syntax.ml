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

type program =
    Exp of exp
  | Decl of id * exp (* let id = e;; *)
