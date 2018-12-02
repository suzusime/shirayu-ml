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

type tyvar = int

type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty

let rec pp_ty = function
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyVar v -> print_string ("type variable " ^ (string_of_int v))
  | TyFun (t1, t2) -> (match t1 with
        TyFun _ ->
        print_string "("; pp_ty t1; print_string ") -> "; pp_ty t2
      | _ -> pp_ty t1; print_string " -> "; pp_ty t2)

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1; v
  in body

let rec freevar_ty ty = match ty with
    TyVar v -> MySet.singleton v
  | TyFun (t1, t2) ->
    let s1 = freevar_ty t1 in
    let s2 = freevar_ty t2 in
    MySet.union s1 s2
  | _ -> MySet.empty

