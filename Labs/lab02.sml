(* helper functions for testing -- you don't need to read these! *)

(* test a function that returns an int *)
fun testi (s : string) (n : int) (m : int) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Int.toString m ^ "\n    Got: " ^ Int.toString n ^ "\n")

(* test a function that returns a bool *)
fun testb (s : string) (n : bool) (m : bool) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Bool.toString m ^ "\n    Got: " ^ Bool.toString n  ^ "\n")

(* test a function that returns a string *)
fun tests (s : string) (n : string) (m : string) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ m ^ "\n    Got: " ^ n ^ "\n")


(* ********************************************************************** 
   Lab code starts here
   **********************************************************************  *)
            
(* Purpose: double the number n
   Examples:
   double 0 ==> 0
   double 2 ==> 4
 *)
fun double (n : int) : int =
    case n of
      0 => 0
    | _ => 2 + double (n - 1)

(* Testing function for double():
   in SMLNJ do
   - test_double();
   to run the tests for this function *)
fun test_double() =
    (testi "da" (double 0) 0;
     testi "db" (double 2) 4;
     testi "dc" (double 3) 6)

(* Purpose: determine whether the number is even
   Examples:
   evenP 0 ==> true
   evenP 3 ==> false
   evenP 12 ==> true
   evenP 27 ==> false
*)
fun evenP (n : int) : bool =
    case n of
      0 => true
    | 1 => false
    | _ => evenP (n - 2)

(* Testing function for evenP *)
fun test_evenP() =
    (testb "ea" (evenP 0) true;
     testb "eb" (evenP 1) false;
     testb "ec" (evenP 12) true;
     testb "ed" (evenP 27) false)

(* TASK: Write summorial, oddP, divisibleByThree, laughs, add 
   Put tests for the functions you write in each testing function.  
*)

fun summorial (n : int) : int =
    case n of
       0 => 0
     | _ => n + summorial(n-1)

fun test_summorial() =
    (testi "sa" (summorial 3) 6;
     testi "sb" (summorial 6) 21
     )

fun oddP (n : int) : bool =
    case n of
       0 => false
     | 1 => true
     | _ => oddP(n-2)
    
fun test_oddP() =
    (testb "oa" (oddP 3) true;
     testb "ob" (oddP 6) false;
     testb "oc" (oddP 1) true
     )

fun divisibleByThree (n : int) : bool =
    case n of
       0 => true
     | 1 => false
     | 2 => false
     | 3 => true
     | _ => divisibleByThree(n-3)
     
fun test_divisibleByThree() =
    (testb "divA" (divisibleByThree 3) true;
     testb "divB" (divisibleByThree 9) true;
     testb "divC" (divisibleByThree 10) false
     )

fun laughs (n : int) : string =
    case n of
       0 => ""
     | _ => (case evenP n of
       	    	  true => "a"^  (laughs (n-1))
		| false => "h" ^ (laughs (n-1)))

fun test_laughs() =
    (tests "la" (laughs 2) "ah";
     tests "lb" (laughs 5) "hahah";
     tests "lc" (laughs 0) "";
     tests "ld" (laughs 3) "hah";
     tests "le" (laughs 1) "h"
     )

fun add (x : int, y : int) : int =
    case x of
       0 => y
     | _ => 1 + (case y of
       	    	 0 => add(x-1, 0)
	       | _ => 1 + add(x-1, y-1))

fun test_add() =
    (testi "ada" (add (5,5)) 10;
     testi "adb" (add (2,7)) 9;
     testi "adc" (add (0,6)) 6;
     testi "add" (add (6,0)) 6
     )

fun run() =
    (test_double();
     test_evenP();
     test_summorial();
     test_oddP();
     test_divisibleByThree();
     test_laughs();
     test_add()
     )
