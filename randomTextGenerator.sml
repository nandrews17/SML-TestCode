(* randomTextGenerator.sml *)

(* nandrews17@georgefox.edu *)
(* 2021 CSIS420 *)
(* Compiled with mlton *)


(* Grammar datatype *)
datatype Grammar = grammar  of {symbol:string, productions:string list};

(* Functions which retrieve the symbol and productions from a grammar *)
fun getSymbol(g as grammar {symbol=s, productions=p}) = s;

fun getProductions(g as grammar {symbol=s, productions=p}) = p;


(* Function which goes through a file and returns a list of the text file's contents *)
fun readToList(infile:string) =
  let
    val ins = TextIO.openIn infile 
    fun loop ins = 
     case TextIO.inputLine ins of 
        SOME line => line :: loop ins 
      | NONE      => [] 
  in 
    loop ins before TextIO.closeIn ins
end;

(* Function which goes over a list and gets symbols from a grammar *)
fun parseSymbol s =
  let
    val START = #"<"
    val END = #">"
    fun parse [] = [] 
      | parse(c'::cs) =
        if c' = START
        then parse cs
        else if c' = END
          then [] 
          else c'::parse cs
  in 
    implode(parse(explode s))
end;

(* Function which goes over a list and gets productions from a grammar *)
fun parseProductions [] = [] 
  | parseProductions(s'::ss) =
  let
    val GRAM_END = #"}"
    fun production s =
      let
        val PROD_END = #";"
        fun parse [] = [] 
          | parse(c'::cs) =
            if c' = PROD_END
            then [] 
            else c'::parse cs
      in 
        implode(parse(explode s))
    end;
  in 
    if List.nth((explode s'), 0) = GRAM_END
    then [] 
    else production s'::parseProductions ss
end;

(* Function which goes over a list and gets productions and symbols for each grammar *)
fun parseGrammars [] = [] 
  | parseGrammars(s'::ss) =
  let
    val GRAM_START = #"{"
    fun parse [] = [] 
      | parse(ps'::pss) =
      grammar {symbol=parseSymbol ps', productions=parseProductions pss}
		::parseGrammars pss
  in 
    if List.nth((explode s'), 0) = GRAM_START
    then parse ss
    else parseGrammars ss
end;

(* Function which takes a start symbol and a list of grammars to get the grammar which matches *)
fun getGrammar(sym, gramList) =
  let
    fun loop [] = [] 
	  | loop(g'::gs) =
	  if (getSymbol g') = sym
	  then [g']
	  else loop gs
  in 
    List.nth((loop gramList), 0)
end;

(* Takes in a file and returns a list of grammars *)
fun getGrammarFromFile() =
  let
    fun grammarList text = parseGrammars(readToList text)
  in 
    (case CommandLine.arguments() of 
        []     => [] 
      | [arg1] => grammarList(arg1)
      | args   => [] )
end;

(* Main function of program, looping over the file and moving from start to end *)
fun main() =
  let
    val grammarList = getGrammarFromFile()
	val seed = Int.toLarge(Real.floor(Time.toReal(Time.now())))
	fun getRand range = Int.fromLarge(((Word.toLargeInt(MLton.Random.rand())
		- seed) * seed) mod range)
	fun getNextProd gram = List.nth(getProductions(gram),
		getRand(Int.toLarge(length(getProductions(gram)))))
	fun parseResult s =
	  let
		val START = #"<"
		val END = #">"
		fun removeSpace [] = [] 
		  | removeSpace(c'::cs) = cs
		fun removeSymbol [] = [] 
		  | removeSymbol(c'::cs) =
		    if c' = END
			then removeSpace cs
			else removeSymbol cs
		fun parse [] = [] 
		  | parse(c'::cs) =
			if c' = START
			then parseResult(getNextProd(getGrammar(parseSymbol(implode(c'::cs)),
				grammarList)))::parse(removeSymbol cs)
			else implode([c'])::parse cs
	  in 
		String.concatWith "" (parse(explode s))
	end;
  in 
    if grammarList = [] 
	then print("Usage: randomTextGenerator <file.g>\n")
	else print(parseResult(getNextProd(getGrammar("start", grammarList))) ^ "\n")
end;


(* SML MLton way of specifying the start of the main program *)
val _ = main();
