

%%
(* required declarations *)

%name Ass2

%term
  EOF | TERM | CONST of string
  | NOT | AND | OR | XOR | EQUALS | IMPLIES
  | IF | THEN | ELSE | FI | LPAREN | RPAREN  | ID of string
  | PLUS | MINUS | TIMES | NEGATE | LESSTHAN | GREATERTHAN
  | LET | IN | END | EQ | NUM of int | FUN | Fn | COLON | ARROW | ASSIGN | INT | BOOL

%nonterm expression of AST.exp | program of AST.statement| statement of AST.statement | DECL of AST.decl | TYPE of AST.Type | function of AST.function

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
program: statement (statement)

statement: function statement(AST.FunctionStatement(function,statement))
		| expression statement(AST.ExpressionStatement(expression,statement))
		| (AST.EndOfFile)

DECL: ID EQ function(AST.ValDeclFunc(ID,function))
	| ID EQ expression(AST.ValDeclExp(ID,expression))

TYPE: INT (AST.INT)
	| BOOL (AST.BOOL)

function: FUN ID LPAREN ID COLON TYPE ARROW TYPE RPAREN COLON TYPE ASSIGN expression (AST.Fun(ID1,ID2,AST.Arrow(TYPE1,TYPE2),TYPE3,expression))
		| Fn LPAREN ID COLON TYPE RPAREN COLON TYPE ASSIGN expression(AST.Fn(ID,TYPE1,TYPE2,expression))

	
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
	|LET DECL IN expression END(AST.LetExp(DECL,expression))
	|expression LESSTHAN expression (AST.BinExp(AST.LESSTHAN,expression1,expression2))
	|expression GREATERTHAN expression (AST.BinExp(AST.GREATERTHAN,expression1,expression2))
	|expression PLUS expression (AST.BinExp(AST.Plus,expression1,expression2))
	|expression MINUS expression (AST.BinExp(AST.Minus,expression1,expression2))
	|NEGATE expression (AST.UnaryExp(AST.Negate,expression))
	|expression TIMES expression (AST.BinExp(AST.Times,expression1,expression2))
	|NUM (AST.NumExp(NUM))
	|LPAREN expression expression RPAREN (AST.AppExp(expression1,expression2))
	