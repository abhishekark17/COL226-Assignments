(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 

%%
(* required declarations *)
%name Ass2

%term
  EOF | TERM | CONST 
  | NOT | AND | OR | XOR | EQUALS | IMPLIES 
  | IF | THEN | ELSE | LPAREN | RPAREN  | ID of string

%nonterm EXP | PROGRAM option 

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%right NOT
%left AND OR XOR EQUALS
%right IMPLIES
%right IF ELSE THEN

(* %right *)
  (* %nonassoc*)
%start PROGRAM

%verbose

%%
PROGRAM: EXP (SOME EXP)
      |    (NONE)


