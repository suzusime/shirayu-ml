open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

let ty_prim op ty1 ty2 = match op with
    Plus ->
    (match ty1, ty2 with
       TyInt, TyInt -> TyInt
     | _ -> err ("Argument must be of integer: +"))
  | Mult ->
    (match ty1, ty2 with
       TyInt, TyInt -> TyInt
     | _ -> err ("Argument must be of integer: *"))
  | Lt ->
    (match ty1, ty2 with
       TyInt, TyInt -> TyBool
     | _ -> err ("Argument must be of integer: <"))
  | Or ->
    (match ty1, ty2 with
       TyBool, TyBool -> TyBool
     | _ -> err ("Argument must be of boolean: ||"))
  | And ->
    (match ty1, ty2 with
       TyBool, TyBool -> TyBool
     | _ -> err ("Argument must be of boolean: &&"))

let rec ty_exp tyenv = function
    Var x ->
    (try
       Environment.lookup x tyenv
     with
       Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
    let tyarg1 = ty_exp tyenv exp1 in
    let tyarg2 = ty_exp tyenv exp2 in
    ty_prim op tyarg1 tyarg2
  | IfExp (test, exp1, exp2) ->
    let tytest = ty_exp tyenv test in
    let tye1 = ty_exp tyenv exp1 in
    let tye2 = ty_exp tyenv exp2 in
    (match tytest with
       TyBool ->
       if tye1 = tye2 then tye1 else err ("Types of results must agree: if")
     | _ -> err ("Test expression must have type boolean: if")
    )
  | LetExp (id, exp1, exp2) ->
    let tye1 = ty_exp tyenv exp1 in
    let newtyenv = Environment.extend id tye1 tyenv in
    let tye2 = ty_exp newtyenv exp2 in
    tye2
  | _ -> err ("Not Implemented!")

let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  | _ -> err ("Not Implemented!")

