structure EVALUATOR  =
struct
open AST

val brokenTypes = Fail "Error in evaluation!" 
val TypeNotMatching = Fail "Types donot match"
val functionTypeError = Fail "Function type declaration and argument Do Not Match"
val notFunctionError = Fail "Given id is not a function"
val thenElseError = Fail "Type Inside Then and Else are not same"
val valueInsideIfIsNotBoolean = Fail "Value inside If Does Not evaluate to bool"
val functionApplicationError = Fail "Argument to a function Cannot Be function during Application" 
 
fun evalStatement(sta:statement,env:environment) = (* is a type statementList *)
    case (sta) of 
        Statement(f,s) => let val (formVal,envi) = evalFormula(f,env) 
                        in 
                            case  formVal of 
                                funcVal(_,_,_,_) => AST.List(formVal,evalStatement(s,envi))
                                | _ => AST.List(formVal,evalStatement(s,envi))
                        end
        | EOS => AST.End

and 

evalFormula(f:formula,env:environment) =  (*value, env*)
    case f of 
        Expression(e) => (evalExp(e,env),env)
        | Function(fu) => evalFunction(fu,env)


and

evalFunction(func:function,env:environment) =  (* funcVal, env *)
    case func of 
        Fn(id1,type1,type2,f) => evalFn(id1,type1,type2,f,env) 
        | Fun(id1,id2,type1,type2,f) => evalFun(id1,id2,type1,type2,f,env)

and 

evalFn(id1:id,type1:Type,type2:Type,ex:exp,env:environment) = (* funcVal, env*)
        (funcVal (id1,type1,type2,ex),env)  


and 

evalFun(id1:id,id2:id,type1:Type,type2:Type,ex:exp,env:environment) = (*funcVal, env*)
        (funcVal (id2,type1,type2,ex),envAdd(id1,funcVal (id2,type1,type2,ex),env)) 



and
evalExp(e:exp, env:environment):value = (*this value can only be of type intval or boolval or FuncVal*)
    case e of
	    NumExp i                => IntVal i
        | IfElseThen(e1,e2,e3)  => evalIF(e1, e2, e3, env)
        | BinExp (b, e1, e2)    => evalBinExp(b, e1, e2, env)
        | UnaryExp(uni,e)        => evalUnaryExp(uni,e,env)
        | Const(s)              => (if s="TRUE" then BoolVal true else BoolVal false)
        | VarExp x              => envLookup (x, env)		(* can give FuncVal *)		  
        | LetExp(decl, e)       => evalLetExp(decl,e,env)
        | AppExp (e1,e2)        => evalAppExp(e1,e2,env)
        | TERM                  => nullValue
        (* | _ => raise brokenTypes *)

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
    | (Xor, BoolVal b1,BoolVal b2) => BoolVal (((not b1) andalso b2) orelse ((not b2) andalso b1))
    | (Implies, BoolVal b1,BoolVal b2) => BoolVal ((not b1) orelse b2) *)
   |   _  => raise TypeNotMatching

and 

evalUnaryExp(uni:uniop, e:exp, env:environment):value = (*this value can only be of type intval or boolval*)
case (uni,evalExp(e,env)) of 
    (Not, BoolVal b) => BoolVal (not b)
    |(Negate, IntVal i) => IntVal (~i)
    | _ => raise TypeNotMatching

(* and 

convertToBool(s:string):value = (*this value can only be of type boolval*)
case s of 
    "TRUE" => BoolVal true
    | "FALSE" => BoolVal false 
    | _ => raise brokenTypes *)

and 

evalLetExp (decl:decl, e2:exp, env:environment): value = (*this value can only be of type intval or boolval*)
case decl of 
    ValDecl(id1,form1) => case form1 of 
                            Expression e => let val v1 = evalExp(e,env)
                                            in 
                                                evalExp(e2,envAdd(id1,v1,env))
                                            end 
                            | Function f => case f of 
                                            Fn (id2,type1,type2,ex) => let val v1 = funcVal(id2,type1,type2,ex) 
                                                                    in 
                                                                        evalExp(e2,envAdd(id1,v1,env))
                                                                    end 
                                            | Fun (idd1,idd2,type1,type2,ex) => let val v1 = funcVal(idd2,type1,type2,ex)
                                                                                in 
                                                                                    evalExp(e2,envAdd(id1,v1,env))
                                                                                end

and 

evalAppExp(id1:id,e2:exp,env:environment):value = (*this value can only be of type intval or boolval or FuncVal*)
    let val v1 = envLookup(id1,env) (*This gives the value of id1 which can be funcVal or IntVal or BoolVal *)
        val v2 = evalExp(e2,env) (*This gives value of e2 IntVal or BoolVal or funcVal*)
    in
        case v1 of 
            funcVal (argument,typeOfargument,returnType,ex) => if(typeCheck(returnType ,evalExp(ex,envAdd(argument,v2,env)),envAdd(argument,v2,env)) andalso typeCheck(typeOfargument,envLookup(argument,envAdd(argument,v2,env)),envAdd(argument,v2,env))) 
                                            then evalExp(ex,envAdd(argument,v2,env)) else raise functionTypeError
            | _ => raise notFunctionError 
    end


and

typeCheck(type1:Type,v1:value,env:environment) = 
    case (type1,v1) of 
        (INT,IntVal _ ) => true
        |(BOOL,BoolVal _ ) => true
        |(Arrow(t1,t2),funcVal(argument1,typeOfargument1,returnType1,ex1)) => (t1=typeOfargument1 andalso t2=returnType1)
        | _ => raise functionTypeError

and 
evalIF(e1:exp,e2:exp,e3:exp, env:environment):value = (* contains Intval or BoolVal*)
    case(evalExp(e1,env)) of 
        BoolVal bx => (if(bx=true) then (case (evalExp(e2,env),evalExp(e3,env)) of  
                            (BoolVal b1,BoolVal b2) => BoolVal b1 
                            | (IntVal i1,IntVal i2) => IntVal i1
                            | _ => raise thenElseError)
                            else (case (evalExp(e2,env),evalExp(e3,env)) of  
                            (BoolVal b1,BoolVal b2) => BoolVal b2 
                            | (IntVal i1,IntVal i2) => IntVal i2
                            | _ => raise thenElseError))
        | _ => raise valueInsideIfIsNotBoolean 
         
        


end