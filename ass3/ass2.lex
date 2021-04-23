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
digit=[0-9];
%%

\n		=>(linenum := !linenum+1;cpos:= 1; increase(pos,1); lex());
{ws}+		=>(increase (cpos,size(yytext));lex());
{digit}+ => (lexeroutput:= ("NUM \""^yytext^"\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,size(yytext));
				Tokens.NUM(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),!linenum, !cpos));
"PLUS"		=>(lexeroutput:= ("PLUS \"PLUS\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,4);Tokens.PLUS(!linenum,!cpos));
"MINUS"		=>(lexeroutput:= ("MINUS \"MINUS\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,5);Tokens.MINUS(!linenum,!cpos));
"TIMES"		=>(lexeroutput:= ("TIMES \"TIMES\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,5);Tokens.TIMES(!linenum,!cpos));
"NEGATE"		=>(lexeroutput:= ("NEGATE \"NEGATE\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,6);Tokens.NEGATE(!linenum,!cpos));
"LESSTHAN"		=>(lexeroutput:= ("LESSTHAN \"LESSTHAN\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,8);Tokens.LESSTHAN(!linenum,!cpos));
"GREATERTHAN"	=>(lexeroutput:= ("GREATERTHAN \"GREATERTHAN\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,11);Tokens.GREATERTHAN(!linenum,!cpos));

"("			=>(lexeroutput:= ("LPAREN \"(\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,1);Tokens.LPAREN(!linenum,!cpos));
")"			=>(lexeroutput:= ("RPAREN \")\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,1);Tokens.RPAREN(!linenum,!cpos));
";"			=>(lexeroutput:= ("TERM \";\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,1);Tokens.TERM(!linenum,!cpos));
"if"		=>(lexeroutput:= ("IF \"if\"",(!linenum),(!cpos))::(!lexeroutput); increase (cpos,2);Tokens.IF(!linenum,!cpos));
"then"		=>(lexeroutput:= ("THEN \"then\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,4);Tokens.THEN(!linenum,!cpos));
"else"		=>(lexeroutput:= ("ELSE \"else\"",(!linenum),(!cpos))::(!lexeroutput); increase (cpos,4);Tokens.ELSE(!linenum,!cpos));
"fi"		=>(lexeroutput:= ("FI \"fi\"",(!linenum),(!cpos))::(!lexeroutput); increase (cpos,2);Tokens.FI(!linenum,!cpos));
"IMPLIES"	=>(lexeroutput:= ("IMPLIES \"IMPLIES\"",(!linenum),(!cpos))::(!lexeroutput); increase (cpos,7);Tokens.IMPLIES(!linenum,!cpos));
"AND"		=>(lexeroutput:= ("AND \"AND\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,3);Tokens.AND(!linenum,!cpos));
"OR"		=>( lexeroutput:= ("OR \"OR\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,2);Tokens.OR(!linenum,!cpos));
"XOR"		=>( lexeroutput:= ("XOR \"XOR\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,3);Tokens.XOR(!linenum,!cpos));
"EQUALS"	=>(lexeroutput:= ("EQUALS \"EQUALS\"",(!linenum),(!cpos))::(!lexeroutput); increase (cpos,6);Tokens.EQUALS(!linenum,!cpos));
"NOT"		=>( lexeroutput:= ("NOT \"NOT\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,3);Tokens.NOT(!linenum,!cpos));
"TRUE"		=>(lexeroutput:= ("CONST \"TRUE\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,4);Tokens.CONST(yytext,!linenum,!cpos));
"FALSE"	=>( lexeroutput:= ("CONST \"FALSE\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,5);Tokens.CONST(yytext,!linenum,!cpos));
"let"		=>(lexeroutput:= ("LET \"Let\"",(!linenum),(!cpos))::(!lexeroutput); increase (cpos,3);Tokens.LET(!linenum,!cpos));
"in"		=>(lexeroutput:= ("IN \"in\"",(!linenum),(!cpos))::(!lexeroutput); increase (cpos,2);Tokens.IN(!linenum,!cpos));
"end"		=>(lexeroutput:= ("END \"end\"",(!linenum),(!cpos))::(!lexeroutput); increase (cpos,3);Tokens.END(!linenum,!cpos));
"="			=>(lexeroutput:= ("EQ \"=\"",(!linenum),(!cpos))::(!lexeroutput); increase (cpos,1);Tokens.EQ(!linenum,!cpos));
"fn"		=>(lexeroutput:= ("Fn \"fn\"",(!linenum),(!cpos))::(!lexeroutput); increase (cpos,2);Tokens.Fn(!linenum,!cpos));
"fun"		=>(lexeroutput:= ("FUN \"fun\"",(!linenum),(!cpos))::(!lexeroutput); increase (cpos,3);Tokens.FUN(!linenum,!cpos));
":"			=>(lexeroutput:= ("COLON \":\"",(!linenum),(!cpos))::(!lexeroutput); increase (cpos,1);Tokens.COLON(!linenum,!cpos));
"->"		=>(lexeroutput:= ("ARROW \"->\"",(!linenum),(!cpos))::(!lexeroutput); increase (cpos,2);Tokens.ARROW(!linenum,!cpos));
"=>"		=>(lexeroutput:= ("ASSIGN \"=>\"",(!linenum),(!cpos))::(!lexeroutput); increase (cpos,2);Tokens.ASSIGN(!linenum,!cpos));
"int"		=>(lexeroutput:= ("INT \"INT\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,3);Tokens.INT(!linenum,!cpos));
"bool"		=>(lexeroutput:= ("BOOL \"BOOL\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,4);Tokens.BOOL(!linenum,!cpos));

{alpha}+	=>(lexeroutput:= ("ID \""^yytext^"\"",(!linenum),(!cpos))::(!lexeroutput);increase (cpos,size(yytext));Tokens.ID(yytext,!linenum,!cpos));

.		=>(error (yytext,!linenum,!cpos);increase (cpos,size(yytext));
					lex());

