functor Ass2LrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Ass2_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct



end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\003\000\016\000\004\000\015\000\005\000\027\000\006\000\026\000\
\\007\000\025\000\008\000\024\000\009\000\023\000\010\000\014\000\
\\014\000\013\000\015\000\051\000\016\000\012\000\017\000\022\000\
\\018\000\021\000\019\000\020\000\020\000\011\000\021\000\019\000\
\\022\000\018\000\023\000\010\000\027\000\009\000\000\000\
\\001\000\003\000\016\000\004\000\015\000\010\000\014\000\014\000\013\000\
\\016\000\012\000\020\000\011\000\023\000\010\000\027\000\009\000\000\000\
\\001\000\003\000\016\000\004\000\015\000\010\000\014\000\014\000\013\000\
\\016\000\012\000\020\000\011\000\023\000\010\000\027\000\009\000\
\\028\000\008\000\029\000\007\000\000\000\
\\001\000\005\000\027\000\006\000\026\000\007\000\025\000\008\000\024\000\
\\009\000\023\000\011\000\052\000\017\000\022\000\018\000\021\000\
\\019\000\020\000\021\000\019\000\022\000\018\000\000\000\
\\001\000\005\000\027\000\006\000\026\000\007\000\025\000\008\000\024\000\
\\009\000\023\000\012\000\064\000\017\000\022\000\018\000\021\000\
\\019\000\020\000\021\000\019\000\022\000\018\000\000\000\
\\001\000\005\000\027\000\006\000\026\000\007\000\025\000\008\000\024\000\
\\009\000\023\000\013\000\072\000\017\000\022\000\018\000\021\000\
\\019\000\020\000\021\000\019\000\022\000\018\000\000\000\
\\001\000\005\000\027\000\006\000\026\000\007\000\025\000\008\000\024\000\
\\009\000\023\000\015\000\057\000\017\000\022\000\018\000\021\000\
\\019\000\020\000\021\000\019\000\022\000\018\000\000\000\
\\001\000\005\000\027\000\006\000\026\000\007\000\025\000\008\000\024\000\
\\009\000\023\000\017\000\022\000\018\000\021\000\019\000\020\000\
\\021\000\019\000\022\000\018\000\025\000\063\000\000\000\
\\001\000\014\000\028\000\000\000\
\\001\000\014\000\047\000\000\000\
\\001\000\015\000\066\000\031\000\065\000\000\000\
\\001\000\015\000\071\000\031\000\065\000\000\000\
\\001\000\016\000\029\000\000\000\
\\001\000\016\000\031\000\000\000\
\\001\000\016\000\046\000\000\000\
\\001\000\016\000\054\000\000\000\
\\001\000\024\000\048\000\000\000\
\\001\000\026\000\049\000\000\000\
\\001\000\030\000\053\000\000\000\
\\001\000\030\000\062\000\000\000\
\\001\000\030\000\070\000\000\000\
\\001\000\030\000\074\000\000\000\
\\001\000\031\000\065\000\032\000\075\000\000\000\
\\001\000\031\000\065\000\032\000\078\000\000\000\
\\001\000\033\000\061\000\034\000\060\000\000\000\
\\081\000\000\000\
\\082\000\000\000\
\\083\000\003\000\016\000\004\000\015\000\010\000\014\000\014\000\013\000\
\\016\000\012\000\020\000\011\000\023\000\010\000\027\000\009\000\
\\028\000\008\000\029\000\007\000\000\000\
\\084\000\000\000\
\\085\000\005\000\027\000\006\000\026\000\007\000\025\000\008\000\024\000\
\\009\000\023\000\017\000\022\000\018\000\021\000\019\000\020\000\
\\021\000\019\000\022\000\018\000\000\000\
\\086\000\000\000\
\\087\000\031\000\065\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\009\000\023\000\000\000\
\\094\000\009\000\023\000\000\000\
\\095\000\009\000\023\000\000\000\
\\096\000\009\000\023\000\000\000\
\\097\000\009\000\023\000\000\000\
\\098\000\005\000\027\000\006\000\026\000\007\000\025\000\008\000\024\000\
\\009\000\023\000\021\000\019\000\022\000\018\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\009\000\023\000\000\000\
\\104\000\009\000\023\000\000\000\
\\105\000\005\000\027\000\006\000\026\000\007\000\025\000\008\000\024\000\
\\009\000\023\000\021\000\019\000\022\000\018\000\000\000\
\\106\000\005\000\027\000\006\000\026\000\007\000\025\000\008\000\024\000\
\\009\000\023\000\021\000\019\000\022\000\018\000\000\000\
\\107\000\005\000\027\000\006\000\026\000\007\000\025\000\008\000\024\000\
\\009\000\023\000\017\000\022\000\018\000\021\000\019\000\020\000\
\\021\000\019\000\022\000\018\000\000\000\
\\108\000\005\000\027\000\006\000\026\000\007\000\025\000\008\000\024\000\
\\009\000\023\000\017\000\022\000\018\000\021\000\021\000\019\000\
\\022\000\018\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\"
val actionRowNumbers =
"\028\000\029\000\026\000\028\000\
\\030\000\009\000\013\000\054\000\
\\014\000\002\000\046\000\002\000\
\\002\000\002\000\045\000\027\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\015\000\010\000\
\\017\000\018\000\052\000\001\000\
\\004\000\043\000\049\000\048\000\
\\053\000\051\000\050\000\038\000\
\\042\000\041\000\040\000\039\000\
\\019\000\016\000\002\000\003\000\
\\007\000\044\000\002\000\025\000\
\\020\000\008\000\031\000\055\000\
\\005\000\011\000\034\000\033\000\
\\025\000\047\000\002\000\025\000\
\\021\000\012\000\006\000\032\000\
\\025\000\022\000\037\000\023\000\
\\025\000\003\000\024\000\036\000\
\\003\000\035\000\000\000"
val gotoT =
"\
\\001\000\004\000\002\000\003\000\003\000\078\000\004\000\002\000\
\\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\004\000\002\000\003\000\004\000\015\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\028\000\000\000\
\\001\000\030\000\000\000\
\\000\000\
\\001\000\031\000\000\000\
\\001\000\032\000\000\000\
\\001\000\033\000\000\000\
\\000\000\
\\000\000\
\\001\000\034\000\000\000\
\\001\000\035\000\000\000\
\\001\000\036\000\000\000\
\\001\000\037\000\000\000\
\\001\000\038\000\000\000\
\\001\000\039\000\000\000\
\\001\000\040\000\000\000\
\\001\000\041\000\000\000\
\\001\000\042\000\000\000\
\\001\000\043\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\048\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\053\000\000\000\
\\001\000\004\000\002\000\054\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\056\000\000\000\
\\006\000\057\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\065\000\000\000\
\\000\000\
\\001\000\066\000\000\000\
\\006\000\067\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\071\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\074\000\000\000\
\\001\000\004\000\002\000\075\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\004\000\002\000\077\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 79
val numrules = 30
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUM of unit ->  (int) | ID of unit ->  (string)
 | CONST of unit ->  (string) | function of unit ->  (AST.function)
 | TYPE of unit ->  (AST.Type) | DECL of unit ->  (AST.decl)
 | statement of unit ->  (AST.statement)
 | program of unit ->  (AST.statement)
 | formula of unit ->  (AST.formula)
 | expression of unit ->  (AST.exp)
end
type svalue = MlyValue.svalue
type result = AST.statement
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "TERM"
  | (T 2) => "CONST"
  | (T 3) => "NOT"
  | (T 4) => "AND"
  | (T 5) => "OR"
  | (T 6) => "XOR"
  | (T 7) => "EQUALS"
  | (T 8) => "IMPLIES"
  | (T 9) => "IF"
  | (T 10) => "THEN"
  | (T 11) => "ELSE"
  | (T 12) => "FI"
  | (T 13) => "LPAREN"
  | (T 14) => "RPAREN"
  | (T 15) => "ID"
  | (T 16) => "PLUS"
  | (T 17) => "MINUS"
  | (T 18) => "TIMES"
  | (T 19) => "NEGATE"
  | (T 20) => "LESSTHAN"
  | (T 21) => "GREATERTHAN"
  | (T 22) => "LET"
  | (T 23) => "IN"
  | (T 24) => "END"
  | (T 25) => "EQ"
  | (T 26) => "NUM"
  | (T 27) => "FUN"
  | (T 28) => "Fn"
  | (T 29) => "COLON"
  | (T 30) => "ARROW"
  | (T 31) => "ASSIGN"
  | (T 32) => "INT"
  | (T 33) => "BOOL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27)
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11)
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ 
(T 3) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.statement statement1, statement1left, 
statement1right)) :: rest671)) => let val  result = MlyValue.program
 (fn _ => let val  (statement as statement1) = statement1 ()
 in (statement)
end)
 in ( LrTable.NT 2, ( result, statement1left, statement1right), 
rest671)
end
|  ( 1, ( ( _, ( MlyValue.statement statement1, _, statement1right))
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.statement (fn _ => let val  (formula
 as formula1) = formula1 ()
 val  (statement as statement1) = statement1 ()
 in (AST.Statement(formula,statement))
end)
 in ( LrTable.NT 3, ( result, formula1left, statement1right), rest671)

end
|  ( 2, ( rest671)) => let val  result = MlyValue.statement (fn _ => (
AST.EOS))
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.function function1, function1left, 
function1right)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  (function as function1) = function1 ()
 in (AST.Function(function))
end)
 in ( LrTable.NT 1, ( result, function1left, function1right), rest671)

end
|  ( 4, ( ( _, ( MlyValue.expression expression1, expression1left, 
expression1right)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  (expression as expression1) = expression1 ()
 in (AST.Expression(expression))
end)
 in ( LrTable.NT 1, ( result, expression1left, expression1right), 
rest671)
end
|  ( 5, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: _ ::
 ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result
 = MlyValue.DECL (fn _ => let val  (ID as ID1) = ID1 ()
 val  (formula as formula1) = formula1 ()
 in (AST.ValDecl(ID,formula))
end)
 in ( LrTable.NT 4, ( result, ID1left, formula1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.TYPE TYPE2, _, TYPE2right)) :: _ :: ( _, ( 
MlyValue.TYPE TYPE1, TYPE1left, _)) :: rest671)) => let val  result = 
MlyValue.TYPE (fn _ => let val  TYPE1 = TYPE1 ()
 val  TYPE2 = TYPE2 ()
 in (AST.Arrow(TYPE1,TYPE2))
end)
 in ( LrTable.NT 5, ( result, TYPE1left, TYPE2right), rest671)
end
|  ( 7, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.TYPE (fn _ => (AST.INT))
 in ( LrTable.NT 5, ( result, INT1left, INT1right), rest671)
end
|  ( 8, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.TYPE (fn _ => (AST.BOOL))
 in ( LrTable.NT 5, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: _ ::
 ( _, ( MlyValue.TYPE TYPE2, _, _)) :: _ :: _ :: ( _, ( MlyValue.TYPE 
TYPE1, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, FUN1left, _)) :: rest671)) => let
 val  result = MlyValue.function (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  TYPE1 = TYPE1 ()
 val  TYPE2 = TYPE2 ()
 val  (formula as formula1) = formula1 ()
 in (AST.Fun(ID1,ID2,TYPE1,TYPE2,formula))
end)
 in ( LrTable.NT 6, ( result, FUN1left, formula1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: _
 :: ( _, ( MlyValue.TYPE TYPE2, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.TYPE TYPE1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) ::
 _ :: ( _, ( _, Fn1left, _)) :: rest671)) => let val  result = 
MlyValue.function (fn _ => let val  (ID as ID1) = ID1 ()
 val  TYPE1 = TYPE1 ()
 val  TYPE2 = TYPE2 ()
 val  (formula as formula1) = formula1 ()
 in (AST.Fn(ID,TYPE1,TYPE2,formula))
end)
 in ( LrTable.NT 6, ( result, Fn1left, formula1right), rest671)
end
|  ( 11, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.expression 
expression3, _, _)) :: _ :: ( _, ( MlyValue.expression expression2, _,
 _)) :: _ :: ( _, ( MlyValue.expression expression1, _, _)) :: ( _, (
 _, IF1left, _)) :: rest671)) => let val  result = MlyValue.expression
 (fn _ => let val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 val  expression3 = expression3 ()
 in (AST.IfElseThen(expression1,expression2,expression3))
end)
 in ( LrTable.NT 0, ( result, IF1left, FI1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (AST.BinExp(AST.Implies,expression1,expression2))
end)
 in ( LrTable.NT 0, ( result, expression1left, expression2right), 
rest671)
end
|  ( 13, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (AST.BinExp(AST.And,expression1,expression2))
end)
 in ( LrTable.NT 0, ( result, expression1left, expression2right), 
rest671)
end
|  ( 14, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (AST.BinExp(AST.OR,expression1,expression2))
end)
 in ( LrTable.NT 0, ( result, expression1left, expression2right), 
rest671)
end
|  ( 15, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (AST.BinExp(AST.Xor,expression1,expression2))
end)
 in ( LrTable.NT 0, ( result, expression1left, expression2right), 
rest671)
end
|  ( 16, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (AST.BinExp(AST.Equals,expression1,expression2))
end)
 in ( LrTable.NT 0, ( result, expression1left, expression2right), 
rest671)
end
|  ( 17, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (expression as expression1) = 
expression1 ()
 in (AST.UnaryExp(AST.Not,expression))
end)
 in ( LrTable.NT 0, ( result, NOT1left, expression1right), rest671)

end
|  ( 18, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.expression (fn _ => let val  (expression as 
expression1) = expression1 ()
 in (expression)
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.expression (fn _ => let val  (
CONST as CONST1) = CONST1 ()
 in (AST.Const(CONST))
end)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.expression (fn _ => let val  (ID as ID1) =
 ID1 ()
 in (AST.VarExp(ID))
end)
 in ( LrTable.NT 0, ( result, ID1left, ID1right), rest671)
end
|  ( 21, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: _ :: ( _, ( MlyValue.DECL DECL1, _, _)) :: ( _,
 ( _, LET1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (DECL as DECL1) = DECL1 ()
 val  (expression as expression1) = expression1 ()
 in (AST.LetExp(DECL,expression))
end)
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (AST.BinExp(AST.LESSTHAN,expression1,expression2))
end)
 in ( LrTable.NT 0, ( result, expression1left, expression2right), 
rest671)
end
|  ( 23, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (AST.BinExp(AST.GREATERTHAN,expression1,expression2))
end)
 in ( LrTable.NT 0, ( result, expression1left, expression2right), 
rest671)
end
|  ( 24, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (AST.BinExp(AST.Plus,expression1,expression2))
end)
 in ( LrTable.NT 0, ( result, expression1left, expression2right), 
rest671)
end
|  ( 25, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (AST.BinExp(AST.Minus,expression1,expression2))
end)
 in ( LrTable.NT 0, ( result, expression1left, expression2right), 
rest671)
end
|  ( 26, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, NEGATE1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (expression as expression1) = 
expression1 ()
 in (AST.UnaryExp(AST.Negate,expression))
end)
 in ( LrTable.NT 0, ( result, NEGATE1left, expression1right), rest671)

end
|  ( 27, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (AST.BinExp(AST.Times,expression1,expression2))
end)
 in ( LrTable.NT 0, ( result, expression1left, expression2right), 
rest671)
end
|  ( 28, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.expression (fn _ => let val  (NUM as 
NUM1) = NUM1 ()
 in (AST.NumExp(NUM))
end)
 in ( LrTable.NT 0, ( result, NUM1left, NUM1right), rest671)
end
|  ( 29, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression2, _, _)) :: ( _, ( MlyValue.expression expression1, _, _))
 :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (AST.AppExp(expression1,expression2))
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Ass2_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun Fn (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
end
end
