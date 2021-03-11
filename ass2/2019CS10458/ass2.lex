val lexeroutput : (string *int * int) list ref = ref [];
structure Tokens=Tokens
	type pos = int
	type linenum=int ref
	type cpos= int ref
	type svalue  = Tokens.svalue
	type ('a,'b) token = ('a,'b) Tokens.token
	type lexresult = (svalue,pos) token
	
	val linenum = ref 1
	val pos = ref 1
	val cpos = ref 1
	val eof = fn () => Tokens.EOF(!linenum,!linenum)
	val error = fn (x,line:int,current:int) => TextIO.output(TextIO.stdOut,"Unknown Token:"^(Int.toString(line))^":"^(Int.toString(current))^x^"\n")
	
	fun increase (x:int ref,y:int) = (x:=(!x)+y)
	
	
	
%%

%header (functor Ass2LexFun(structure Tokens:Ass2_TOKENS));
alpha = [a-zA-Z];
ws = [\ \t\n];
%%

\n		=>(linenum := !linenum+1;cpos:= 1; increase(pos,1); lex());
{ws}+		=>(increase (cpos,size(yytext));lex());
"("		=>(lexeroutput:= ("LPAREN \"(\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,1);Tokens.LPAREN(!linenum,!cpos));
")"		=>(lexeroutput:= ("RPAREN \")\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,1);Tokens.RPAREN(!linenum,!cpos));
";"		=>(lexeroutput:= ("TERM \";\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,1);Tokens.TERM(!linenum,!cpos));
"IF"		=>(lexeroutput:= ("IF \"IF\"",(!linenum),(!cpos))::(!lexeroutput); increase (cpos,2);Tokens.IF(!linenum,!cpos));
"THEN"		=>(lexeroutput:= ("THEN \"THEN\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,4);Tokens.THEN(!linenum,!cpos));
"ELSE"		=>(lexeroutput:= ("ELSE \"ELSE\"",(!linenum),(!cpos))::(!lexeroutput); increase (cpos,4);Tokens.ELSE(!linenum,!cpos));
"IMPLIES"	=>(lexeroutput:= ("IMPLIES \"IMPLIES\"",(!linenum),(!cpos))::(!lexeroutput); increase (cpos,7);Tokens.IMPLIES(!linenum,!cpos));
"AND"		=>(lexeroutput:= ("AND \"AND\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,3);Tokens.AND(!linenum,!cpos));
"OR"		=>( lexeroutput:= ("OR \"OR\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,2);Tokens.OR(!linenum,!cpos));
"XOR"		=>( lexeroutput:= ("XOR \"XOR\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,3);Tokens.XOR(!linenum,!cpos));
"EQUALS"	=>(lexeroutput:= ("EQUALS \"EQUALS\"",(!linenum),(!cpos))::(!lexeroutput); increase (cpos,6);Tokens.EQUALS(!linenum,!cpos));
"NOT"		=>( lexeroutput:= ("NOT \"NOT\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,3);Tokens.NOT(!linenum,!cpos));
"TRUE"		=>(lexeroutput:= ("CONST \"TRUE\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,4);Tokens.CONST(yytext,!linenum,!cpos));
"FALSE"	=>( lexeroutput:= ("CONST \"FALSE\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,5);Tokens.CONST(yytext,!linenum,!cpos));
{alpha}+	=>(lexeroutput:= ("ID \""^yytext^"\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,size(yytext));Tokens.ID(yytext,!linenum,!cpos));

.		=>(error (yytext,!linenum,!cpos);increase (cpos,size(yytext));
					lex());

