structure AST =
struct
type id  = string

datatype binop = Implies | And | OR | Xor | Equals | LESSTHAN | GREATERTHAN | Plus | Minus |Times
datatype uniop = Not | Negate
datatype Term = EOS
datatype Type = INT | BOOL
datatype arrow = Arrow of Type*Type
datatype Paren = LeftParen | RightParen
datatype program = Program of statement
and statement = Statement of exp*statement | EndOfFile
and decl = ValDecl of exp*exp
and  exp = Id of string
| IfElseThen of exp * exp * exp
| BinExp of binop*exp*exp
| UnaryExp of uniop*exp
| BracketExp of Paren*exp*Paren
| Const of string
| VarExp of id
| StringExp of string
| LetExp of decl*exp
| NumExp of int
| AppExp of id*exp
| EndOfStatement
| Fn of id*Type*Type*exp
| Fun of id*id*arrow*Type*exp

end
				
