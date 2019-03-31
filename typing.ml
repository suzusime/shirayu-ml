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

(* subst -> (ty * ty) list *)
let eqs_of_subst s =
  List.map (fun (v, t) -> ((TyVar v), t)) s

(* subst -> (ty * ty) list -> (ty * ty) list *)
let subst_eqs s eqs =
  let subst_pair (t1, t2) =
    ((subst_type s t1), (subst_type s t2))
  in
  List.map subst_pair eqs

(* (ty * ty) list -> subst *)
let rec unify pairs = match pairs with
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
          let newcon = subst_eqs s rest in
          let unified = unify newcon in
          s @ unified
      | _ -> err ("Unify failed")

(* binOp -> ty -> ty -> ((ty * ty) list * ty) *)
let ty_prim op ty1 ty2 = match op with
    Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | And -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)

(* ty t -> exp -> ((tyvar * ty) list * ty) *)
let rec ty_exp tyenv = function
    Var x ->
    (try
       ([], Environment.lookup x tyenv)
     with
       Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp tyenv exp2 in
    let (eqs3, ty) = ty_prim op ty1 ty2 in
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
    let s3 = unify eqs in
    (s3, subst_type s3 ty)
  | IfExp (test, exp1, exp2) ->
    let (st, tyt) = ty_exp tyenv test in
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp tyenv exp2 in
    let eqs = (eqs_of_subst st) @ (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(tyt, TyBool); (ty1, ty2)] in
    let s3 = unify eqs in
    (s3, subst_type s3 ty1)
  | LetExp (id, exp1, exp2) ->
    let (s1, ty1) = ty_exp tyenv exp1 in
    let newtyenv = Environment.extend id ty1 tyenv in
    let (s2, ty2) = ty_exp newtyenv exp2 in
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in
    let s3 = unify eqs in
    (s3, subst_type s3 ty2)
  | FunExp (id, exp) ->
    let domty = TyVar (fresh_tyvar ()) in
    let s, ranty =
      ty_exp (Environment.extend id domty tyenv) exp
    in
    (s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) ->
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp tyenv exp2 in
    let ranty = TyVar (fresh_tyvar ()) in
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(ty1, TyFun (ty2, ranty))] in
    let s3 = unify eqs in
    (s3, subst_type s3 ranty)
  | LetRecExp (f, x, exp1, exp2) ->
    let domty = TyVar (fresh_tyvar ()) in
    let dummy_ty_f = TyVar (fresh_tyvar ()) in
    let tyenv_added_by_dummy_f = Environment.extend f dummy_ty_f tyenv in
    let (sf, ty_f) = ty_exp tyenv_added_by_dummy_f (FunExp (x, exp1)) in
    let tyenv_added_by_f = Environment.extend f ty_f tyenv_added_by_dummy_f in
    let (s1, ranty) = ty_exp tyenv_added_by_f exp2 in
    let newtyenv = Environment.extend x domty tyenv_added_by_f in
    let (s2, ty2) = ty_exp newtyenv exp1 in
    let eqs = (eqs_of_subst sf) @ (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(ty_f, dummy_ty_f)] in
    let s3 = unify eqs in
    (s3, subst_type s3 ranty)
  | _ -> err ("Not Implemented!")

let ty_decl tyenv = function
    Exp e -> (ty_exp tyenv e), tyenv
  | Decl (id, e) ->
    let (s, ty) = ty_exp tyenv e in
    let newtyenv = Environment.extend id ty tyenv in
    (s, ty), newtyenv
  | RecDecl (f, x, exp) ->
    let domty = TyVar (fresh_tyvar ()) in
    let dumm_ty_f = TyVar (fresh_tyvar ()) in
    let tyenv_added_by_dummy_f = Environment.extend f dumm_ty_f tyenv in
    let (sf, ty_f) = ty_exp tyenv_added_by_dummy_f (FunExp (x, exp)) in
    let tyenv_added_by_f = Environment.extend f ty_f tyenv_added_by_dummy_f in
    let newtyenv = Environment.extend x domty tyenv_added_by_f in
    let (s2, ty2) = ty_exp newtyenv exp in
    let eqs = (eqs_of_subst sf) @ (eqs_of_subst s2) @ [(ty_f, dumm_ty_f); (ty_f, TyFun(domty, ty2))] in
    let s = unify eqs in
    (s,  subst_type s ty_f), tyenv_added_by_f
  | _ -> err ("Not Implemented!")

