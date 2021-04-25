structure EVALUATOR  =
struct
open AST

val brokenTypes = Fail "Error in evaluation!"

fun evalStatement(st:statement,env:environment,stlist:statementList): statementList = (* is a type statementList *)
    case (st) of 
        Statement(f,s) => (evalFormula(f,env)::evalStatement(s,env,stlist))
        | EOS => (stlist)

and 

evalFormula(f:formula,env:environment): value = (*value type of IntVal,BoolVal and FuncVal*)
    case f of 
        Expression(e) => evalExp(e,env)
        | Function(fu) =>evalFunction(fu,env)


and

evalFunction(func:function,env:environment):value =  (* return of type FuncVal *)
    case func of 
        Fn(id1,type1,type2,f) => evalFn(id1,type1,type2,f,env) 
        | Fun(id1,id2,type1,type2,f) => evalFun(id1,id2,type1,type2,f,env)

and 

evalFn(id1:id,type1:Type,type2:Type,f:formula,env:environment):value = (* v1 contains Intval,boolVal or FuncVal if e is exp else StatementVal for exp statement*)
        funcVal (id1,type1,type2,f)  


and 

evalFun(id1:id,id2:id,type2:Type,type1:Type,f:formula,env:environment):value = (* return should be funcVal*)
    funcVal (id2,type1,type2,f) 



and
evalExp(e:exp, env:environment):value = (*this value can only be of type intval or boolval or FuncVal*)
    case e of
	    NumExp i                => IntVal i
        | IfElseThen(e1,e2,e3)  => evalIF(e1, e2, e3, env)
        | BinExp (b, e1, e2)    => evalBinExp(b, e1, e2, env)
        | UnaryExp(uni,e)        => evalUnaryExp(uni,e,env)
        | Const(s)              => convertToBool(s)
        | VarExp x              => envLookup (x, env)		(* can give FuncVal *)		  
        | LetExp(decl, e)       => evalLetExp(decl,e,env)
        | AppExp (e1,e2)        => evalAppExp(e1,e2,env)
        | _ => raise brokenTypes

and

evalBinExp(b:binop, e1:exp, e2:exp, env:environment):value = (*this value can only be of type intval or boolval*)
case (b, evalExp(e1, env), evalExp(e2, env))  of
    (Plus, IntVal i1, IntVal i2) => IntVal (i1+i2)
  |   (Minus, IntVal i1, IntVal i2) => IntVal (i1-i2)
  |   (Times, IntVal i1, IntVal i2) => IntVal (i1*i2)
  |    (LESSTHAN, IntVal i1, IntVal i2) => BoolVal (i1<i2)
  |    (GREATERTHAN, IntVal i1, IntVal i2) => BoolVal (i1>i2)
  | (EQUALS,IntVal i1, IntVal i2) => BoolVal (i1=i2)
  | (EQUALS,BoolVal b1, BoolVal b2) => BoolVal (b1=b2)
  (* | (And, BoolVal b1,BoolVal b2) => BoolVal (b1 andalso b2)
  | (OR, BoolVal b1,BoolVal b2) => BoolVal (b1 orelse b2)
    | (Xor, BoolVal b1,BoolVal b2) => BoolVal ((b1 orelse b2) andalso not(b1 andalso b2))
    | (Implies, BoolVal b1,BoolVal b2) => BoolVal ((not b1) orelse b2) *)
   |   _  => raise brokenTypes

and 

evalUnaryExp(uni:uniop, e:exp, env:environment):value = (*this value can only be of type intval or boolval*)
case (uni,evalExp(e,env)) of 
    (Not, BoolVal b) => BoolVal (not b)
    |(Negate, IntVal i) => IntVal (~i)
    | _ => raise brokenTypes

and 

convertToBool(s:string):value = (*this value can only be of type boolval*)
case s of 
    "TRUE" => BoolVal true
    | "FALSE" => BoolVal false 
    | _ => raise brokenTypes

and 

evalLetExp (decl:decl, e2:exp, env:environment): value = (*this value can only be of type intval or boolval*)
case (decl) of 
    ValDecl(id1,formula1) => let val v1=evalFormula(formula1,env) 
                            in 
                                evalExp(e2,envAdd(id1,v1,env))
                            end

and 

evalAppExp(e1:exp,e2:exp,env:environment):value = (*this value can only be of type intval or boolval or FuncVal*)
    case (evalExp(e1,env),evalExp(e2,env)) of 
        (funcVal f, IntVal i) => IntVal i
        | _ => raise brokenTypes


and
evalIF(e1:exp,e2:exp,e3:exp, env:environment):value = (* contains Intval or BoolVal*)
    case(evalExp(e1,env),evalExp(e2,env),evalExp(e3,env)) of 
        (BoolVal x,IntVal i1,IntVal i2)=> if x then IntVal i1 else IntVal i2
        | (BoolVal x,BoolVal b1,BoolVal b2)=> if x then BoolVal b1 else BoolVal b2
        | _ => raise brokenTypes
        


end