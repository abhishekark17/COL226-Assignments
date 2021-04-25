structure AST =
struct
type id  = string

datatype binop = Implies | And | OR | Xor | Equals | LESSTHAN | GREATERTHAN | Plus | Minus |Times
datatype uniop = Not | Negate
datatype Type = INT | BOOL | Arrow of Type*Type
(* and arrow = Arrow of Type*Type *)
(* datatype Paren = LeftParen | RightParen *)
datatype program = Program of statement
and formula = Expression of exp | Function of function
and statement = Statement of formula*statement | EOS
and decl = ValDecl of id*formula
and function = Fn of id*Type*Type*formula | Fun of id*id*Type*Type*formula
and  exp = IfElseThen of exp * exp * exp
| BinExp of binop*exp*exp
| UnaryExp of uniop*exp
(* | BracketExp of Paren*exp*Paren *)
| Const of string
| VarExp of id
| LetExp of decl*exp
| NumExp of int
| AppExp of exp*exp
| EndOfStatement



datatype value = IntVal of int
                (* | StringVal of string *)
	            | BoolVal of bool
                | funcVal of id*Type*Type*formula
                (* | statementList of (value) list *)
				
type environment = (id * value) list

type envtype = (id*Type) list

type statementList = (value) list

fun envAdd (var:id, v:value, env:environment) = 
    (var,v)::env

fun envAddType (var:id, v:Type, env:envtype) =
    (var,v)::env

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise Fail "Environment lookup error"	

fun envLookupType (var:id, env:envtype) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise Fail "Environment lookup error"
end
				
