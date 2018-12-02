open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

(* type substitution *)
type subst = (tyvar * ty) list

(* subst -> ty ->ty *)
let subst_type s ty =
  (* ty -> tyvar*ty -> ty *)
  let rec f t r =
    let (v_from, type_to) = r in
    match t with
      TyVar v -> if v=v_from then type_to else TyVar v
    | TyFun (t1, t2) -> TyFun ((f t1 r), (f t2 r))
    | t -> t
  in
  List.fold_left f ty s

(* (ty * ty) list -> subst *)
let rec unify (pairs:(ty*ty) list) = match pairs with
    [] -> []
  | (t1, t2) :: rest ->
    if t1 = t2 then
      unify rest
    else match (t1, t2) with
        (TyFun (t11, t12), TyFun (t21, t22)) ->
        unify ((t11, t21)::(t12, t22)::rest)
      | (TyVar v, t) | (t, TyVar v)->
        let vars = freevar_ty t in
        if MySet.member v vars then
          err ("t can't contain alpha (ref. ex.4.3.4)")
        else
          let s = [(v, t)] in
          (* new constraint *)
          let newcon =
            let subst_pair (t1, t2) =
              ((subst_type s t1), (subst_type s t2))
            in
            List.map subst_pair rest
          in
          let unified = unify newcon in
          s :: unified
      | _ -> err ("Unify failed")


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
    Exp e -> (ty_exp tyenv e), tyenv
  | Decl (id, e) ->
    let ty = ty_exp tyenv e in
    let newtyenv = Environment.extend id ty tyenv in
    ty, newtyenv
  | _ -> err ("Not Implemented!")

