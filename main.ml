open Syntax
open Eval
open Typing

let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  try
    (* execute type checking *)
    let (ty, newtyenv) = ty_decl tyenv decl in
    (* evaluate value *)
    let (id, newenv, v) = eval_decl env decl in
    (* print *)
    Printf.printf "val %s : " id;
    pp_ty ty;
    print_string " = ";
    pp_val v;
    print_newline();
    read_eval_print newenv newtyenv
  with
    Typing.Error e ->
    print_endline ("[Type Error] " ^ e);
    read_eval_print env tyenv
  | Eval.Error e ->
    print_endline ("[Eval Error] " ^ e);
    read_eval_print env tyenv

let initial_env =
  Environment.empty
  |> Environment.extend "i" (IntV 1)
  |> Environment.extend "ii" (IntV 2)
  |> Environment.extend "iii" (IntV 3)
  |> Environment.extend "iv" (IntV 4)
  |> Environment.extend "v" (IntV 5)
  |> Environment.extend "x" (IntV 10)

let initial_tyenv =
  Environment.empty
  |> Environment.extend "i" TyInt
  |> Environment.extend "ii" TyInt
  |> Environment.extend "iii" TyInt
  |> Environment.extend "iv" TyInt
  |> Environment.extend "v" TyInt
  |> Environment.extend "x" TyInt

let _ = read_eval_print initial_env initial_tyenv

