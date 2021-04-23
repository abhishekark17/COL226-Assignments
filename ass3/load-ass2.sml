structure Ass2LrVals = Ass2LrValsFun(structure Token = LrParser.Token)
structure Ass2Lex = Ass2LexFun(structure Tokens = Ass2LrVals.Tokens);
structure Ass2Parser =
      Join(structure LrParser = LrParser
               structure ParserData = Ass2LrVals.ParserData
               structure Lex = Ass2Lex)
val parsingerror:bool ref=ref false;
val parseroutput:string ref=ref "";
fun helper (nil) = ()
| helper (hd::nil : (string * int * int) list) = (print (#1 (hd)))
| helper (hd::tl : (string * int * int) list) = (print ( (#1 (hd)) ^ ","); helper (tl));


fun printList (sList : (string * int * int) list ref) = (
  let val x = List.rev (!sList) in (
    print ("[");
    helper (x);
    print ("]\n")
  )
  end
);



fun invoke lexstream =
	let 
	fun print_error (s,pos:int, cpos :int) =
                (if ((!parsingerror) = false) then (printList (Ass2Lex.UserDeclarations.lexeroutput);
    						parsingerror := true;
						TextIO.output(TextIO.stdOut,"Syntax Error:" ^ (Int.toString pos) ^ ":" ^ (Int.toString cpos) ^ ":" ^ s ^ "\n")) else ()) 
        in
            Ass2Parser.parse(0,lexstream,print_error,())
        end

fun stringToLexer file =
    
    let 
    	val input = TextIO.openIn file
    	val str : string = TextIO.inputAll input
    	val done = ref false
        val lexer=  Ass2Parser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
    lexer
    end 
        
fun parse (lexer) =
    let val dummyEOF = Ass2LrVals.Tokens.EOF(0,0)
        val (result, lexer) = invoke lexer
    val (nextToken, lexer) = Ass2Parser.Stream.get lexer
    in
        (if Ass2Parser.sameToken(nextToken, dummyEOF) then result
    else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result))
    end

fun lexerOutput (x :string) = let val mylexer = stringToLexer (x) in (
			Ass2Lex.UserDeclarations.linenum := 1 ;
			Ass2Lex.UserDeclarations.cpos := 1 ;
			Ass2Lex.UserDeclarations.lexeroutput := [];
			parse (mylexer);
			printList (Ass2Lex.UserDeclarations.lexeroutput);
			parsingerror:=false
			(* print(!parseroutput) *)
			 ) end

fun parseFile (x:string) = parse (stringToLexer(x))



