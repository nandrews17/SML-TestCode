(* personSorter.sml *)

(* nandrews17@georgefox.edu *)
(* 2021 CSIS420 *)
(* Compiled with mlton *)


(* Person datatype with a name and age *)
datatype Person = person  of {name:string, age:int};

(* Functions for retrieving a person's name and age *)
fun getPersonName (p as person {name = n, age = a}) = n;

fun getPersonAge (p as person {name = n, age = a}) = a;


(* Takes in a file of strings and returns an organized list *)
fun readToList (infile:string) =
  let
    val ins = TextIO.openIn infile 
    fun loop ins = 
     case TextIO.inputLine ins of 
        SOME line => line :: loop ins 
      | NONE      => [] 
  in 
    loop ins before TextIO.closeIn ins 
end;

(* Functions for removing alphabetic characters from a string or digits from a string *)
fun remNonAlph s =
  let
    fun rem [] = [] 
      | rem (c'::cs) =
        if (ord c' > 96) andalso (ord c' < 123)
        then c'::rem cs
        else if (ord c' > 64) andalso (ord c' < 91)
          then c'::rem cs
          else rem cs
  in 
    implode (rem (explode s))
end;

fun remNonDigits s =
  let
    fun rem [] = [] 
      | rem (c'::cs) =
        if (ord c' > 47) andalso (ord c' < 58)
        then c'::rem cs
        else rem cs
  in 
    implode (rem (explode s))
end;

(* Function which takes a string and returns a list of persons *)
fun makePersonList s =
  let
    fun form [] = [] 
      | form (s'::cs) = (person {name=(remNonAlph s'), age=valOf(Int.fromString(remNonDigits s'))})::form cs
  in 
    form s
end;

(* Implimentation of merge sort for the organization of a list of persons *)
fun sortPersonList [] = [] 
  | sortPersonList [p] = [p]
  | sortPersonList cp =
  let
    fun merge [] mR = mR
	  | merge mL [] = mL
	  | merge (mL as x::xp) (mR as y::yp) =
	  if ord(List.nth(explode (getPersonName x), 0)) = ord(List.nth(explode (getPersonName y), 0))
	  then if ord(List.nth(explode (getPersonName x), 1)) < ord(List.nth(explode (getPersonName y), 1))
	    then x :: (merge xp mR)
		else y :: (merge mL yp)
	  else if ord(List.nth(explode (getPersonName x), 0)) < ord(List.nth(explode (getPersonName y), 0))
	    then x :: (merge xp mR)
		else y :: (merge mL yp)
    fun split P =
	  let
	    val t = (length P) div 2
      in 
	    (List.take (P,t), List.drop(P,t))
	  end
    val (rp, lp) = split cp
  in 
    merge (sortPersonList rp) (sortPersonList lp)
end;

(* Function which prints a list of persons one at a time *)
fun printPersonList p =
  let
    fun prnt [] = print ("\n")::[] 
      | prnt (p'::cp) = print (", " ^ getPersonName p')::prnt cp
	fun prntFirst [] = [] 
      | prntFirst (p'::cp) = print (getPersonName p')::prnt cp
  in 
    prntFirst p
end;

(* Function which prints an integer average of a list of persons' ages *)
fun printPersonListAgeAverage p =
  let
	fun sum [] = 0
      | sum (p'::cp) = getPersonAge p' + (sum cp)
  in
    print ((Int.toString((sum p) div (length p))) ^ "\n")
end;

(* Function which simply prints a comma seperated list *)
fun printList xs = print(String.concatWith ", " xs ^ "\n");


(* Start of program using input file *)
val infile = "input.txt";
val newlist =  readToList(infile);
val persons = makePersonList newlist;
val sortedPersons = sortPersonList persons;

printPersonList sortedPersons;
printPersonListAgeAverage sortedPersons;
