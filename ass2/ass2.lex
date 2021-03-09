structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^ ": " ^ e ^ "\n")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))
  
%%
%header (functor CalcLexFun(structure Tokens:Calc_TOKENS));

alpha=[A-Za-z];
ws = [\ \t];
%%
\n	 => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
";"	 => (Tokens.TERM(!pos,!pos));
"TRUE"|"FASLE" => (Tokens.CONST(!pos,!pos));
"NOT"	 => (TOkens.NOT(!pos,!pos));
"AND"	 => (TOkens.AND(!pos,!pos));
"OR"	 => (TOkens.OR(!pos,!pos));
"XOR"	 => (TOkens.XOR(!pos,!pos));
"EQUALS" => (TOkens.EQUALS(!pos,!pos));
"IMPLIES"=> (TOkens.IMPLIES(!pos,!pos));
"IF"	 => (TOkens.IF(!pos,!pos));
"THEN"	 => (TOkens.THEN(!pos,!pos));
"ELSE"	 => (TOkens.ELSE(!pos,!pos));
"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos));
{alpha}+ => (Tokens.ID(yytext,!pos,!pos));

"."      => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());

