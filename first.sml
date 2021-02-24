exception emptyInputFile
exception UnevenFields
exception notAllowed

fun convert(infile,outfile,delim1,delim2)=(
	let
	val input=TextIO.openIn infile
	val output=TextIO.openOut outfile
	val numOfField=ref 0
	val currentRow=ref 1
	val currentFieldNum=ref 0
	val qoutesInCurrentField=ref 0
	val currentField=ref ""
	
	fun firstTimeRead () = 
    let val firstRead = TextIO.input1 input in ( 
        case firstRead of
        NONE => raise emptyInputFile
        |SOME(c) => (
            if (c = #"\"") then qoutesInCurrentField := 1 else ();
            currentField := (!currentField) ^ String.str(c) 
            )
    )
    end;
    
	fun conversion() =
		let val firstChar= TextIO.input1 input
		in(
			case firstChar of
			NONE =>(
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
																numOfField:=(!numOfField+1);
																currentFieldNum:=(!currentFieldNum)+1;
																TextIO.output(output,!currentField);
																currentField:="";
																qoutesInCurrentField:=0;
																TextIO.output1(output,delim2)
																)
													else (
														currentField:=(!currentField)^String.str(delim1)
														)
													)
							else (
								currentField:=(!currentField)^String.str(delim1)
								)
							)
				else if (c = #"\"") then (
								qoutesInCurrentField:=(!qoutesInCurrentField)+1;
								currentField:=(!currentField)^String.str(c)
								)
					 
				else if(c = #"\n") then (
								if (!qoutesInCurrentField mod 2 <> 0) then (
																			currentField:=(!currentField)^String.str(c)
																		)
								else (
									currentFieldNum:=(!currentFieldNum)+1;
									qoutesInCurrentField:=0;
									TextIO.output(output,!currentField);
									TextIO.output1(output,c);
									if(!currentRow=1) then (numOfField:=(!numOfField)+1)
									else (
										(*if (!currentFieldNum <> !numOfField) then raise UnevenFields else();*)
										
										);
									currentRow:=(!currentRow)+1;
									currentField:="";
									currentFieldNum:=0
									)
								)
				else(
					currentField := (!currentField) ^ String.str(c)
					);
				conversion()
				)
		)
		end;
		in (
			firstTimeRead();
			conversion()
			)
		end
		)
		
fun convertDelimeter(infile,outfile,delim1,delim2) = convert(infile,outfile,delim1,delim2)
	handle emptyInputFile=>print("nson")
	|UnevenFields => print("adfa")
	|notAllowed => print("dfa");
	
convertDelimeter("TestCases/himym.csv","b.txt",#",",#";");
	

