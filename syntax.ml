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

(* update tyvar dict and return tyvar alphabet *)
let get_tyvar_alph dict v =
  let accord x =
    let (ty, num) = x in ty = v
  in
  if List.exists accord dict then
    let (ty, num) = List.find accord dict in
    (num, dict)
  else
    let num = List.length dict in
    let new_dict = (v, num) :: dict in
    (num, new_dict)

let rec pp_ty t dict = match t with
    TyInt -> print_string "int"; dict
  | TyBool -> print_string "bool"; dict
  | TyVar v ->
    let (num, newdict) = get_tyvar_alph dict v in
    print_string ("type variable " ^ (string_of_int num)); newdict
  | TyFun (t1, t2) -> (match t1 with
        TyFun _ ->
        print_string "(";
        let newdict1 = pp_ty t1 dict in
        print_string ") -> ";
        pp_ty t2 newdict1
      | _ ->
        let newdict1 = pp_ty t1 dict in
        print_string " -> ";
        pp_ty t2 newdict1)

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

