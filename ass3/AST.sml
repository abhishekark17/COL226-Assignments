structure AST =
struct
type id  = string
datatype binop = Implies | And | OR | Xor | Equals
datatype uniop = Not 
datatype Term = EOS
datatype Paren = LeftParen | RightParen
datatype program = Program of statement
and statement = Statement of exp*Term*statement | EOF
and  exp = Num of int
| Id of string
| IfElseThen of exp * exp * exp
| BinExp of binop*exp*exp
| UnaryExp of uniop*exp
| BracketExp of Paren*exp*Paren
| Const of string
| ID of id

end
				
