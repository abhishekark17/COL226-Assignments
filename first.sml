(* Name - Abhishek Kumar *)
(* Entry Number - 2019CS10458 *)
(*----------------------------*)


(* Declaration of Exceptions *)
exception emptyInputFile
exception UnevenFields of int*int*int;
exception notAllowed
exception parenthesisDoesNotMatch
exception unEscapedQoutes

(*------------------------------------*)

(* helper Functions*)

(* This functions wraps the field containing delim2 in qoutes *)

fun wrap(x) = "\""^x^"\"";

fun printUnevenException (x,y,z)=
	print("Expected: "^Int.toString(x)^" fields, Present:"^Int.toString(y)^" on Line "^Int.toString(z)^"\n");

(* This function has been used several times due to my code design so it was better to define it separately to shorten the code *)
fun checkAndRaiseUnEscapedException(currentField,qoutesInCurrentField)=
	if(!currentField <> "") then (
								if  (!qoutesInCurrentField mod 2 = 0 andalso !qoutesInCurrentField <> 0) then (
																									raise unEscapedQoutes
																									) 
								else ()
								)
	else ();

(* This helper(only raises the exception, handling has been done separately) function converts from one delimeted file to another delimeted file *)

fun convert(infile,outfile,delim1,delim2:char)=(
	let
	val input=TextIO.openIn infile
	val output=TextIO.openOut outfile
	val numOfField=ref 0 (* Fields to be counted on row 1 *)
	val currentRow=ref 1 
	val currentFieldNum=ref 0
	val qoutesInCurrentField=ref 0
	val currentField=ref "" (* value of current field *)
	val toBeWrapped=ref false  (* if field contains delim2 in unqouted field then it must be qouted *)
	
    (* helper function for looping through each character one by one *)
	fun conversion() =
		let val character= TextIO.input1 input
		in(
			case character of
			NONE =>(
				if (!currentRow=1 andalso (!currentFieldNum=0 andalso !currentField="")) then (
																										raise emptyInputFile;
																										TextIO.closeIn input;
																										TextIO.closeOut output
																										) else ();
				if (!qoutesInCurrentField mod 2 <>0) then (
															raise parenthesisDoesNotMatch)
				else();
				TextIO.output(output,!currentField);
				TextIO.closeIn input;
				TextIO.closeOut output
				)
			|SOME(c)=>(
				if (c=delim1) then (
							if (!currentRow=1 andalso (!currentFieldNum=0 andalso !currentField="")) then (
																										raise notAllowed;
																										TextIO.closeIn input;
																										TextIO.closeOut output
																										)
							
							else if (!qoutesInCurrentField mod 2 = 0) then 
													(
													if(!currentRow=1) then 
																(
																numOfField:=(!numOfField+1)
																) else ();
													if(!toBeWrapped = true) then (
																					currentField:=wrap(!currentField);
																					toBeWrapped:=false
																				)
													else ();
													currentFieldNum:=(!currentFieldNum)+1;
													TextIO.output(output,!currentField);
													currentField:="";
													qoutesInCurrentField:=0;
													TextIO.output1(output,delim2)
													)
							else (
								checkAndRaiseUnEscapedException(currentField,qoutesInCurrentField);
								currentField:=(!currentField)^String.str(delim1)
								)
							)
				else if (c = delim2) then (
											checkAndRaiseUnEscapedException(currentField,qoutesInCurrentField);
											if (!qoutesInCurrentField mod 2 <> 0) then (
																						currentField:=(!currentField)^String.str(c)
																						)
											else (
													currentField:=(!currentField)^String.str(c);
													toBeWrapped:=true
													)
											)
				else if (c = #"\"") then (
								qoutesInCurrentField:=(!qoutesInCurrentField)+1;
								currentField:=(!currentField)^String.str(c)
								
								)
					 
				else if(c = #"\n") then (
								if (!qoutesInCurrentField mod 2 <> 0) then (
																			checkAndRaiseUnEscapedException(currentField,qoutesInCurrentField);
																			currentField:=(!currentField)^String.str(c)
																		)
								else (
									if(!toBeWrapped = true) then (
																currentField:=wrap(!currentField);
																toBeWrapped:=false
																	)
									else ();
									currentFieldNum:=(!currentFieldNum)+1;
									qoutesInCurrentField:=0;
									TextIO.output(output,!currentField);
									TextIO.output1(output,c);
									if(!currentRow=1) then (numOfField:=(!numOfField)+1)
									else (
										if (!currentFieldNum <> !numOfField) then raise UnevenFields (!numOfField,!currentFieldNum,!currentRow) else()
										);
									currentRow:=(!currentRow)+1;
									currentField:="";
									currentFieldNum:=0
									)
								)
				else(
					checkAndRaiseUnEscapedException(currentField,qoutesInCurrentField);
					currentField := (!currentField) ^ String.str(c)
					
					);
				conversion()
				)
		)
		end;
		in (
			conversion()
			)
		end
		)

(* End of helper Functions *)

(* Main Functions Starts here *)
		
fun convertDelimeters(infilename,delim1,outfilename,delim2:char) = convert(infilename,outfilename,delim1,delim2)
	handle emptyInputFile=>print("Input File in Empty\n")
	|UnevenFields (totalFields,currentFields,lineNumber) => printUnevenException(totalFields,currentFields,lineNumber)
	|notAllowed => print("First character of file cannot be a delimeter\n")
	|parenthesisDoesNotMatch => print("parenthesis Does Not Match\n")
	|unEscapedQoutes => print("Qoutes in input File have not been escaped\n");
	
fun csv2tsv(infilename,outfilename)= convertDelimeters(infilename,#",",outfilename,#"\t");

fun tsv2csv(infilename,outfilename) = convertDelimeters(infilename,#"\t",outfilename,#",");	

