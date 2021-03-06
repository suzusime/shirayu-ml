open Syntax

(* define of "value" *)
type exval =
  | IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t ref
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV _ -> "<function>"

let pp_val v = print_string (string_of_exval v)

(* apply primitive calculation *)
let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | Or, BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | Or, _, _ -> err ("Both arguments must be boolean: ||")
  | And, BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | And, _, _ -> err ("Both arguments must be boolean: &&")

(* evaluate expression *)
let rec eval_exp env = function
    Var x ->
    (try Environment.lookup x env with
       Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) ->
    let arg1 = eval_exp env exp1 in
    let arg2 = eval_exp env exp2 in
    apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
    let test = eval_exp env exp1 in
    (match test with
       BoolV true -> eval_exp env exp2
     | BoolV false -> eval_exp env exp3
     | _ -> err ("Test expression must be boolean: if"))
  | LetExp (id, exp1, exp2) ->
    (* evaluate exp1 in present environment *)
    let value = eval_exp env exp1 in
    (* evaluate exp2 in new environment *)
    let newenv = Environment.extend id value env in
    eval_exp newenv exp2
  | LetRecExp (id, para, exp1, exp2 ) ->
    (* make ref of a dummy environment *)
    let dummyenv = ref Environment.empty in
    (* make a closure and extend env *)
    let newenv = Environment.extend id (ProcV (para, exp1, dummyenv)) env in
    (* backpatch *)
    dummyenv := newenv;
    eval_exp newenv exp2
  | FunExp (id, exp) -> ProcV (id, exp, ref env)
  | AppExp (exp1, exp2) ->
    let funval = eval_exp env exp1 in
    let arg = eval_exp env exp2 in
    (match funval with
       ProcV (id, body, env') ->
       (* extend the environment in the closure with parameters *)
       let newenv = Environment.extend id arg !env' in
       eval_exp newenv body
     | _ -> err ("Non-function value is applied"))

(* evaluate declaration *)
let eval_decl env = function
    Exp e ->
    let v = eval_exp env e in
    ("-", env, v)
  | Decl (id, e) ->
    let v = eval_exp env e in
    let newenv = Environment.extend id v env in
    (id, newenv, v)
  | RecDecl (id, para, exp) ->
    let dummyenv = ref Environment.empty in
    let v = ProcV (para, exp, dummyenv) in
    let newenv = Environment.extend id v env in
    dummyenv := newenv;
    (id, newenv, v)


