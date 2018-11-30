%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT
%token IF THEN ELSE TRUE FALSE

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
  e=Expr SEMISEMI { Exp e }

(* expression *)
Expr :
    e=IfExpr { e }
  | e=LTExpr { e }

(* less than expression *)
LTExpr :
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

(* plus expression *)
PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

(* multiplication expression *)
MExpr :
    l=MExpr MULT r=AExpr { BinOp (Mult, l, r) }
  | e=AExpr { e }

(* a expression *)
AExpr :
    i=INTV { ILit i }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | i=ID { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
  IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }
