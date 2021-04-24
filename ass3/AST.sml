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
and statement = FunctionStatement of function*statement | ExpressionStatement of exp*statement | EndOfFile
and decl = ValDeclExp of string*exp | ValDeclFunc of string*function
and function = Fn of string*Type*Type*exp | Fun of string*string*arrow*Type*exp
and  exp = IfElseThen of exp * exp * exp
| BinExp of binop*exp*exp
| UnaryExp of uniop*exp
| BracketExp of Paren*exp*Paren
| Const of string
| VarExp of id
| StringExp of string
| LetExp of decl*exp
| NumExp of int
| AppExp of exp*exp
| EndOfStatement

end
				
