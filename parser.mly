%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ
%token RARROW FUN

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
  | e=FunExpr { e }

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
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) }
  | e=AppExpr { e }

(* apply function *)
AppExpr :
    e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e=AExpr { e }

(* a expression *)
AExpr :
    i=INTV { ILit i }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | i=ID { Var i }
  | LPAREN e=Expr RPAREN { e }

(* if expression *)
IfExpr :
  IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

(* fuction expression *)
FunExpr :
   FUN l=nonempty_list(ID) RARROW e=Expr
   { let f x e = FunExp(x, e) in List.fold_right f l e }
