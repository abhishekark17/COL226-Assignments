(*  NAME:: ABHINAV SINGHAL 
    ENTRY NUMBER:: 2019CS50768 *)

exception emptyInputFile
exception UnevenFieldException of int*int*int; (*expected vs actual vs lineNo*)
exception doesNotEndWithNewLineException of int ref; (*lineNo*)
exception quotesDontMatch 
exception quoteBeganInMiddle    (*Field contains a double quote but was not enclosed in double quotes*)
exception danglingSingleQuoteFound  (*last field's double quote was not closed *)

(*-----------------------------------  Converting Files Between Different Delimiter-Sepatated-Formats  ------------------------------------*)

(*either both first and last characters of the string are " or not. otherwise we raise an exception while taking input of this string*)
val fieldHasDelim2 = ref false;       (*checking if the field has delim2, if it has, we need to place the field in double quotes if not already*)

fun convertThisField ("") = ""                  (*This function pads the field element with double quotes on both sides if needed*)
| convertThisField (s:string) = (
    let val output = ref "" in (
        output := s;
        if (String.substring(s,0,1) <> "\"" andalso (!fieldHasDelim2 = true)) then (output := "\"" ^ (!output) ^ "\"") else ()
    );
    !output
    end
)   (*O(1) time complexity :: appends double quotes if needed at both ends :: constant time*)

fun ConDel (infilename,delim1, outfilename, delim2) = (         (*Convert Delimiter function calls this function and then handles exceptions separately*)
    let 
    val ins = TextIO.openIn infilename 
    val outs = TextIO.openOut outfilename
    val numColumnsinRow = ref 0     (*number of columns currently traversed*)
    val rowNum = ref 1              (*current row*)
    val numColumnsMax = ref 0       (*number of columns in first row*)
    val numQuotesInField = ref 0    (*number of double quotes in field in input file*)
    val buffer = ref ""             (*temp buffer to store string between two delim1 delimiters*)

    fun firstTimeRead () =          (*reading input for the first time*)
    let val firstRead = TextIO.input1 ins in ( 
        case firstRead of
        NONE => raise emptyInputFile
        |SOME(c) => (
            if (c = #"\"") then numQuotesInField := 1 else ();
            buffer := (!buffer) ^ String.str(c) 
            )
    )
    end;

    fun helper () = 
    let val tempo = TextIO.input1 ins in (
        case tempo of 
        NONE => (  
            if (!numQuotesInField mod 2 <> 0) then raise danglingSingleQuoteFound else ();
            if (!buffer <> "") then (if (String.sub(!buffer,(size (!buffer)-1)) <> #"\n" ) then raise doesNotEndWithNewLineException rowNum else TextIO.output(outs,convertThisField(!buffer))) 
            else ();
            TextIO.closeIn ins;
            TextIO.closeOut outs
        )
        |SOME(c) =>(
            if (c = delim2) then fieldHasDelim2 := true else ();
            if (!numQuotesInField mod 2 <> 0) then buffer := (!buffer) ^ String.str(c) 
            else (
                if (c = delim1) then (
                    numColumnsinRow := !numColumnsinRow + 1;
                    numQuotesInField := 0;
                    TextIO.output(outs,convertThisField(!buffer));
                    buffer := "";
                    fieldHasDelim2 := false;
                    TextIO.output1(outs,delim2)
                )
                else if (c = #"\n") then (
                    numColumnsinRow := !numColumnsinRow + 1;
                    numQuotesInField := 0;
                    if (!numColumnsMax = 0)  then numColumnsMax := !numColumnsinRow
                    else if (!numColumnsinRow <> !numColumnsMax) then raise UnevenFieldException (!numColumnsMax,!numColumnsinRow,!rowNum)
                    else ();
                    rowNum := !rowNum + 1;
                    numColumnsinRow := 0;
                    TextIO.output(outs,convertThisField(!buffer));
                    buffer := "";
                    fieldHasDelim2 := false;
                    TextIO.output1(outs,#"\n")

                )
                else if (c = #"\"") then (
                    if (!numQuotesInField = 0 andalso (!buffer <> "")) then raise quoteBeganInMiddle 
                    else buffer := (!buffer) ^ String.str(c)
                )
                else ( 
                    if (!numQuotesInField = 0) then buffer := (!buffer) ^ String.str(c) 
                    else  raise quotesDontMatch
                )
                
            );
            if (c = #"\"") then numQuotesInField := !numQuotesInField + 1 
            else ();
            helper()
        )
    )
    end;
    in (
        firstTimeRead();
        helper()
    )
    end
)(*Linear time complexity :: just reads every character once and does some constant time operations*)

fun convertDelimiters(infilename, delim1, outfilename, delim2) = ConDel(infilename, delim1, outfilename, delim2)
    handle emptyInputFile => print("exception emptyInputFile\n")
    | UnevenFieldException (x,y,z) => print("Expected: "^Int.toString(x)^" fields, Present: "^Int.toString(y)^" fields on Line "^Int.toString(z))
    | doesNotEndWithNewLineException x => print("exception doesNotEndWithNewLineException"^"INPUT FILE ERROR:: DOES NOT END WITH NEWLINE CHARACTER :: LINE " ^ Int.toString(!x)  ^"\n")
    | quoteBeganInMiddle => print("exception quoteBeganInMiddle "^"INPUT FILE ERROR:: FIELD CONTAINS UNESCAPED QUOTE\n")
    | quotesDontMatch => print ("exception quotesDon'tMatch "^"INPUT FILE ERROR:: FILE CONTAINS FIELD WITH INCONSISTENT QUOTES\n")
    | danglingSingleQuoteFound => print("exception danglingSingleQuoteFound "^"INPUT FILE ERROR:: LAST QUOTE NOT CLOSED\n");


fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename,#",",outfilename,#"\t");
fun tsv2csv(infilename, outfilename) = convertDelimiters(infilename,#"\t",outfilename,#",");


(*-----------------------------------  Converting Files Between Different Newline Conventions  ------------------------------------------*)

fun convertNewlines(infilename, newline1, outfilename, newline2) = 
  let
    val ins = TextIO.openIn infilename 
    val outs = TextIO.openOut outfilename
    val len1 :int = size newline1
    val temp :string ref = ref (TextIO.inputN(ins,len1))

    fun helper () = (
      let val inChar = TextIO.inputN(ins,1) in 
      if ((size (!temp)) < len1) then ( 
          TextIO.output(outs,(!temp)); 
          TextIO.closeIn ins; 
          TextIO.closeOut outs 
          )
      else if ((!temp) = newline1) then ( 
          TextIO.output(outs,newline2); 
          temp := inChar ^ TextIO.inputN(ins,len1-1); 
          helper()
        )
      else (
        TextIO.output1(outs,String.sub(!temp,0));
        temp :=  String.substring((!temp),1, len1-1) ^ inChar;
        helper()
        )
    end
    )
    
  in helper() end;

fun unix2dos (infilename,outfilename) = convertNewlines (infilename,"\n",outfilename,"\r\n");
fun dos2unix (infilename,outfilename) = convertNewlines (infilename,"\r\n",outfilename,"\n");
csv2tsv("TestCases/himym.csv","b.txt");
(*-----------------------------------  END OF ASSIGNMENT  ------------------------------------------*)
