(* factorial.sml *)

(* nandrews17@georgefox.edu *)
(* 2021 CSIS420 *)


(* Tail recursive function for factorial *)
fun factorial n =
  let
    fun loop (0, acc) = acc
    |   loop (m, acc) = loop (m - 1, m * acc)
  in
    loop (n, 1)
end;

(* Main Function of program which takes in command line arguments *)
fun main () =
  (case CommandLine.arguments () of
      []     => raise Option
    | [arg1] => print (Int.toString(factorial (valOf(Int.fromString(arg1)))) ^ "\n")
    | args   => raise Option)
	handle Option => print ("Usage: factorial <int>\n")


(* SML and MLton way of specifying a main entry to program *)
val _ = main ();