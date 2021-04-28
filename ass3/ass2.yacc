

%%
(* required declarations *)

%name Ass2

%term
  EOF | TERM | CONST of string
  | NOT | AND | OR | XOR | EQUALS | IMPLIES
  | IF | THEN | ELSE | FI | LPAREN | RPAREN  | ID of string
  | PLUS | MINUS | TIMES | NEGATE | LESSTHAN | GREATERTHAN
  | LET | IN | END | EQ | NUM of int | FUN | Fn | COLON | ARROW | ASSIGN | INT | BOOL

%nonterm expression of AST.exp | formula of AST.formula | program of AST.statement| statement of AST.statement | DECL of AST.decl | TYPE of AST.Type | function of AST.function 

%pos int 

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%right IF THEN ELSE
%right IMPLIES
%left AND OR XOR EQUALS LESSTHAN GREATERTHAN
%right NOT NEGATE
%right ARROW 
%left PLUS MINUS 
%left TIMES 
%nonassoc EQ INT BOOL FUN Fn COLON ASSIGN

(* %right *)
(* %nonassoc*)
%start program

%verbose

%%
program: statement (statement)

statement: formula statement(AST.Statement(formula,statement))
		| (AST.EOS)

formula: function (AST.Function(function))
		| expression (AST.Expression(expression))

DECL: ID EQ formula(AST.ValDecl(ID,formula))

TYPE: TYPE ARROW TYPE (AST.Arrow(TYPE1,TYPE2))
	| INT (AST.INT)
	| BOOL (AST.BOOL)

function: FUN ID LPAREN ID COLON TYPE RPAREN COLON TYPE ASSIGN expression (AST.Fun(ID1,ID2,TYPE1,TYPE2,expression))
		| Fn LPAREN ID COLON TYPE RPAREN COLON TYPE ASSIGN expression (AST.Fn(ID,TYPE1,TYPE2,expression))
	
expression: IF expression THEN expression ELSE expression FI(AST.IfElseThen(expression1,expression2,expression3))
	|expression IMPLIES expression (AST.BinExp(AST.Implies,expression1,expression2))
	|expression AND expression (AST.BinExp(AST.And,expression1,expression2))
	|expression OR expression (AST.BinExp(AST.OR,expression1,expression2))
	|expression XOR expression (AST.BinExp(AST.Xor,expression1,expression2))
	|expression EQUALS expression (AST.BinExp(AST.Equals,expression1,expression2))
	|NOT expression (AST.UnaryExp(AST.Not,expression))
	|LPAREN expression RPAREN (expression)
	|CONST (AST.Const(CONST))
	|ID (AST.VarExp(ID))
	|TERM (AST.TERM)
	|LET DECL IN expression END(AST.LetExp(DECL,expression))
	|expression LESSTHAN expression (AST.BinExp(AST.LESSTHAN,expression1,expression2))
	|expression GREATERTHAN expression (AST.BinExp(AST.GREATERTHAN,expression1,expression2))
	|expression PLUS expression (AST.BinExp(AST.Plus,expression1,expression2))
	|expression MINUS expression (AST.BinExp(AST.Minus,expression1,expression2))
	|NEGATE expression (AST.UnaryExp(AST.Negate,expression))
	|expression TIMES expression (AST.BinExp(AST.Times,expression1,expression2))
	|NUM (AST.NumExp(NUM))
	|LPAREN ID expression RPAREN (AST.AppExp(ID,expression))
	