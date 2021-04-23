

%%
(* required declarations *)

%name Ass2

%term
  EOF | TERM | CONST of string
  | NOT | AND | OR | XOR | EQUALS | IMPLIES
  | IF | THEN | ELSE | FI | LPAREN | RPAREN  | ID of string
  | PLUS | MINUS | TIMES | NEGATE | LESSTHAN | GREATERTHAN
  | LET | IN | END | EQ | NUM of int | FUN | Fn | COLON | ARROW | ASSIGN | INT | BOOL

%nonterm formula of AST.exp | program of AST.program | statement of AST.statement | DECL of AST.decl | TYPE of AST.Type

%pos int 

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%left NEGATE
%left TIMES
%left PLUS MINUS
%right NOT
%left AND OR XOR EQUALS LESSTHAN GREATERTHAN
%right IMPLIES
%right IF THEN ELSE
%nonassoc EQ INT BOOL FUN Fn COLON ARROW ASSIGN 

(* %right *)
(* %nonassoc*)
%start program

%verbose

%%
program: statement (AST.Program(statement))

statement: formula statement(AST.Statement(formula,statement))
	|(AST.EndOfFile)

DECL: ID EQ formula(AST.ValDecl(AST.VarExp(ID),formula))

TYPE: INT (AST.INT)
	| BOOL (AST.BOOL)
	
formula: IF formula THEN formula ELSE formula FI(AST.IfElseThen(formula1,formula2,formula3))
	|formula IMPLIES formula (AST.BinExp(AST.Implies,formula1,formula2))
	|formula AND formula (AST.BinExp(AST.And,formula1,formula2))
	|formula OR formula (AST.BinExp(AST.OR,formula1,formula2))
	|formula XOR formula (AST.BinExp(AST.Xor,formula1,formula2))
	|formula EQUALS formula (AST.BinExp(AST.Equals,formula1,formula2))
	|NOT formula (AST.UnaryExp(AST.Not,formula))
	|LPAREN formula RPAREN (formula)
	|CONST (AST.Const(CONST))
	|ID (AST.VarExp(ID))
	|TERM (AST.EndOfStatement)
	|LET DECL IN formula END(AST.LetExp(DECL,formula))
	|formula LESSTHAN formula (AST.BinExp(AST.LESSTHAN,formula1,formula2))
	|formula GREATERTHAN formula (AST.BinExp(AST.GREATERTHAN,formula1,formula2))
	|formula PLUS formula (AST.BinExp(AST.Plus,formula1,formula2))
	|formula MINUS formula (AST.BinExp(AST.Minus,formula1,formula2))
	|NEGATE formula (AST.UnaryExp(AST.Negate,formula))
	|formula TIMES formula (AST.BinExp(AST.Times,formula1,formula2))
	|NUM (AST.NumExp(NUM))
	|LPAREN ID formula RPAREN (AST.AppExp(ID,formula))
	|Fn LPAREN ID COLON TYPE RPAREN COLON TYPE ASSIGN formula(AST.Fn(ID,TYPE1,TYPE2,formula))
	|FUN ID LPAREN ID COLON TYPE ARROW TYPE RPAREN COLON TYPE ASSIGN formula (AST.Fun(ID1,ID2,AST.Arrow(TYPE1,TYPE2),TYPE3,formula))
