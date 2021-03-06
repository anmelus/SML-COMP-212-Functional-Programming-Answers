fun ilToString(l : int list) : string =
    case l of
        [] => "[]"
      | x :: xs => Int.toString x ^ "::" ^ ilToString(xs)

fun testii (s : string) (n : int * int) (m : int * int) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => let val (x,y) = n
                     val (x',y') = m
                 in
                     print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Int.toString x' ^ " , " ^ Int.toString y'  ^ "\n    Got: " ^ Int.toString x ^ " , " ^ Int.toString y ^ "\n")
                 end

fun testil (s : string) (n : int list) (m : int list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ilToString m ^ "\n    Got: " ^ ilToString n ^ "\n")

(* ---------------------------------------------------------------------- *)

fun fib (n : int) : int =
    case n
     of
       ~1 => 0
      | 0 => 1
      | 1 => 1
      | _ => fib(n-1) + fib(n-2)

(* TASK *)
fun fastfib (n : int) : int * int =
    case n of
    	 0 => (0,0)
	| 1 => (1,1)
	| _ => (fib(n-1),fib(n))

(* tests for fastfib *)
fun test_fastfib() =
    (testii "ff1" (fastfib 3) (3,3))

(* TASK *)
fun merge (n : int list, l : int list) : int list =
    case (n,l) of
         ([], l) => l
       | (n, []) => n
       | (x :: xs, y :: ys) => (case x < y of
       	       	     	       	     true => x :: (merge (xs, l))
				   | false => y :: (merge (n, ys)) )
         

fun test_merge()=
    (testil "m1" (merge([1,2],[])) [1,2];
     testil "m2" (merge([2,3],[1])) [1,2,3]) 

fun run() =
    (test_fastfib();
     test_merge())

