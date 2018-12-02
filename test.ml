open Syntax
open Typing

let test_subst_type_1 =
  print_endline "[Test] subst_type 1 :";
  let alpha = fresh_tyvar () in
  let t = subst_type [(alpha, TyInt)] (TyFun (TyVar alpha, TyBool)) in
  pp_ty t; print_newline ()

let test_subst_type_2 =
  print_endline "[Test] subst_type 2 :";
  let alpha = fresh_tyvar () in
  let beta = fresh_tyvar () in
  let t = subst_type [(beta, (TyFun (TyVar alpha, TyInt))); (alpha, TyBool)] (TyVar beta) in
  pp_ty t; print_newline ()
