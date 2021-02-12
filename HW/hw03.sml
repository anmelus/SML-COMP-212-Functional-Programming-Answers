
(* for testing *)

fun ilToString(l : int list) : string =
    case l of
        [] => "[]"
      | x :: xs => Int.toString x ^ "::" ^ ilToString(xs)

fun slToString(l : string list) : string =
    case l of
        [] => "[]"
      | x :: xs => x ^ "::" ^ slToString(xs)

fun islToString(l : (int * string) list) : string =
    case l of
        [] => "[]"
      | (n,s) :: xs => "(" ^ Int.toString n ^ "," ^ s ^ ")" ^ "::" ^ islToString(xs)

fun testisl (s : string) (n : (int * string) list) (m : (int * string) list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ islToString m ^ "\n    Got: " ^ islToString n ^ "\n")

fun testilsl (s : string) ((is,ss) : int list * string list) ((is',ss') : int list * string list) : unit =
    case (is,ss) = (is',ss') of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ilToString is' ^ "," ^ slToString ss' ^ "\n    Got: " ^ ilToString is ^ "," ^ slToString ss ^  "\n")

fun testb (s : string) (n : bool) (m : bool) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Bool.toString m ^ "\n    Got: " ^ Bool.toString n  ^ "\n")

fun testili (s : string) ((is,i) : int list * int) ((is',i') : int list * int) : unit =
    case (is,i) = (is',i') of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ilToString is' ^ "," ^ Int.toString i' ^ "\n    Got: " ^ ilToString is ^ "," ^ Int.toString i ^  "\n")
            
fun testil (s : string) (n : int list) (m : int list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ilToString m ^ "\n    Got: " ^ ilToString n ^ "\n")


(* ---------------------------------------------------------------------- *)

fun zip (l1 : int list, l2 : string list) : (int * string) list =
    case (l1, l2) of
        ([], l2) => []
      | (l1, []) => []
      | (x :: xs, y :: ys) => (x,y) :: zip(xs , ys)

(* Tests for zip *)
fun test_zip() =
    (testisl "z1" (zip ([1,2], ["a","b"])) [(1,"a"), (2,"b")]
     (* write your tests here *)
     )

fun unzip (l : (int * string) list) : int list * string list =
    case l of
    	 ([]) => ([], [])
	| (a,b) :: xs => let val (l1, l2) = unzip xs
	  	      	     	      	    in (a::l1, b::l2) end

(* Tests for unzip *)
fun test_unzip() =
    (testilsl "u1" (unzip [(1,"a"), (2,"b")]) ([1,2], ["a","b"])
     (* write your tests here *)
     )

fun runWith (_:int, [] : int list) : int list * int list = ([], [])
  | runWith (x, y::L) =
    if x = y then
      let
        val (repeats, tail) = runWith(x, L)
      in
        (x::repeats, tail)
      end
    else
      ([], y::L)

val ([1], []) = runWith (1, [1])
val ([1, 1], [2]) = runWith (1, [1, 1, 2])
val ([1,1,1,1],[5,2]) = runWith (1, [1,1,1,1,5,2])
val ([], [1,1,1,1,5,2]) = runWith (2, [1,1,1,1,5,2])

fun look_and_say (l : int list) : int list = raise Fail "las unimplemented"

(* Tests for look_and_say *)
fun test_look_and_say() =
    (testil "las1" (look_and_say [1]) [1,1]
     (* write your tests here *)
     )

fun subset_sum (l : int list, s : int) : bool =
    case l of
    	 [] => (case s of
	       0 => true
	     | _ => false)
       | x :: xs => (case x=s of
       	      	    true => true
		  | false =>  if(subset_sum(xs,s-x)=true) then subset_sum(xs,s-x)
		    	       else subset_sum(xs,s) )

(* Tests for subset_sum *)
fun test_subset_sum() =
    (testb "ss1" ( subset_sum ([], 0)) true;
     testb "ss2" ( subset_sum ([1,2], 3)) true;
     testb "ss3" ( subset_sum ([3,2,4], 6)) true;
     testb "ss4" ( subset_sum ([3,2,4,5], 7)) true
     )
    

fun run() =
    (test_zip();
     test_unzip();
     test_lasHelp();
     test_look_and_say();
     test_subset_sum())