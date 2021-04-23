

%%
(* required declarations *)

%name Ass2

%term
  EOF | TERM | CONST of string
  | NOT | AND | OR | XOR | EQUALS | IMPLIES 
  | IF | THEN | ELSE | LPAREN | RPAREN  | ID of string

%nonterm formula of AST.exp | program of AST.program | statement of AST.statement

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%right NOT
%left AND OR XOR EQUALS
%right IMPLIES
%right IF THEN ELSE

(* %right *)
(* %nonassoc*)
%start program

%verbose

%%
program: statement (AST.Program(statement))

statement: formula TERM statement(AST.Statement(formula,AST.EOS,statement))
	|(AST.EOF)
	
formula: IF formula THEN formula ELSE formula (AST.IfElseThen(formula1,formula2,formula3))
	|formula IMPLIES formula (AST.BinExp(AST.Implies,formula1,formula2))
	|formula AND formula (AST.BinExp(AST.And,formula1,formula2)) 
	|formula OR formula (AST.BinExp(AST.OR,formula1,formula2))
	|formula XOR formula (AST.BinExp(AST.Xor,formula1,formula2))
	|formula EQUALS formula (AST.BinExp(AST.Equals,formula1,formula2))
	|NOT formula (AST.UnaryExp(AST.Not,formula))
	|LPAREN formula RPAREN (AST.BracketExp(AST.LeftParen,formula1,AST.RightParen))
	|CONST (AST.Const(CONST))
	|ID (AST.ID(ID))



