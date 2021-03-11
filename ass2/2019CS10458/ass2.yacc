

%%
(* required declarations *)

%name Ass2

%term
  EOF | TERM | CONST of string
  | NOT | AND | OR | XOR | EQUALS | IMPLIES 
  | IF | THEN | ELSE | LPAREN | RPAREN  | ID of string

%nonterm formula of string | program of string | statement of string 

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%right IF THEN ELSE
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT

(* %right *)
(* %nonassoc*)
%start program

%verbose

%%
program: statement (statement^"program => statement,")

statement: formula TERM statement(formula^"TERM ;,"^statement^"statement => formula TERM statement,")
	|("statement => <epsilon>,")
	
formula: IF formula THEN formula ELSE formula ("IF IF,"^formula1^"THEN THEN,"^formula2^"THEN THEN,"^formula3^"formula => IF formula THEN formula ELSE formula,")
	|formula IMPLIES formula (formula1^"IMPLIES IMPLIES,"^formula2^"formula => formula IMPLIES formula,")
	|formula AND formula (formula1^"AND AND,"^formula2^"formula => formula AND formula,") 
	|formula OR formula (formula1^"OR OR,"^formula2^"formula =>formula OR formula,")
	|formula XOR formula (formula1^"XOR XOR,"^formula2^"formula => formula XOR formula,")
	|formula EQUALS formula (formula1^"EQUALS EQUALS,"^formula2^"formula => formula EQUALS formula,")
	|NOT formula ("NOT NOT,"^formula1^"formula => NOT formula,")
	|LPAREN formula RPAREN ("LPAREN ("^formula1^"RPAREN )"^"formula => LPAREN formula RPAREN,")
	|CONST ("CONST "^CONST1^",formula => CONST,")
	|ID ("ID "^ID1^",formula => ID,")



