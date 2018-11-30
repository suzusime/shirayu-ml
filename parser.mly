%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) }

(* expression *)
Expr :
    e=IfExpr { e }
  | e=LetExpr { e }
  | e=ORExpr { e }

(* let expression *)
LetExpr :
  LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }

(* or expression *)
ORExpr :
    l=ANDExpr OR r=ANDExpr { BinOp (Or, l, r) }
  | e=ANDExpr { e }

(* and expression *)
ANDExpr :
    l=LTExpr AND r=LTExpr { BinOp (And, l, r) }
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
