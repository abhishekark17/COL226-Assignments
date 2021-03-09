structure Tokens=Tokens
	type pos = int
	type linenum = int
	type cpos = int
	type svalue  = Tokens.svalue
	type ('a,'b) token = ('a,'b) Tokens.token
	type lexresult = (svalue,linenum)
	
	val linenum = ref 1
	val cpos = ref 0
	val eof = fn () => Tokens.EOF(!pos,!pos)
	val error = fn (x,line:int,current:int) => TextIO.output(TextIO.stdOut,"problem of "^x^"on line: "^(Int.toString(line))^" position: "^(Int.toString(current)))
	
	
%%

%header (functor Ass2LexFun(structure Tokens:Ass2_TOKENS));
alpha = [a-zA-Z];
ws = [\ \t];
%%

\n			=>(linenum := !linenum+1; cpos := yypos:: !cpos; lex());
{ws}+		=>(lex());
";"			=>(Tokens.TERM(yypos,yypos+1));
"TRUE"		=>( Tokens.CONST(yypos,yypos+4));
"FALSE"		=>( Tokens.CONST(yypos,yypos+5));
"AND"		=>( Tokens.AND(yypos,yypos+3));
"OR"		=>( Tokens.OR(yypos,yypos+2));
"XOR"		=>( Tokens.XOR(yypos,yypos+3));
"EQUALS"	=>( Tokens.EQUALS(yypos,yypos+6));
"NOT"		=>( Tokens.NOT(yypos,yypos+3));
"IMPLIES"	=>( Tokens.IMPLIES(yypos,yypos+7));
"IF"		=>( Tokens.IF(yypos,yypos+2));
"THEN"		=>( Tokens.THEN(yypos,yypos+4));
"ELSE"		=>( Tokens.ELSE(yypos,yypos+4));
{alpha}+	=>(Tokens.ID(yytext,yypos,yypos+size(yytext)));
"("			=>(Tokens.LPAREN(yypos,yypos+1));
")"			=>(Tokens.RPAREN(yypos,yypos+1));
"."			=>(error (yytext,!linenum,yypos);
					lex());

