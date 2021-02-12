fun ilToString(l : int list) : string =
    case l of
        [] => "[]"
      | x :: xs => Int.toString x ^ "::" ^ ilToString(xs)

fun testil (s : string) (n : int list) (m : int list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ilToString m ^ "\n    Got: " ^ ilToString n ^ "\n")

(* ---------------------------------------------------------------------- *)
            
fun evenP(n : int) : bool =
    case n
     of 0 => true
      | 1 => false
      | _ => evenP(n-2)

fun evens(l : int list) : int list =
    case l of
      [] => []
      | x :: xs => (case evenP(x) of
      	     	   true => x :: evens(xs)
		 | false => evens(xs) )

fun test_evens() =
    (testil "e1" (evens [1,2,3,4,5]) [2,4])

fun append (l : int list, b : int list) : int list =
    case l of
    	 [] => b
	| x :: xs => x :: (append(xs, b))

fun test_append() =
    (testil "a1" (append([1,2,3] , [4,5])) [1,2,3,4,5])

fun reverse (l : int list) : int list =
    case l of
      [] => []
    | x :: xs => reverse(xs) @ [x]

fun test_reverse() =
    (testil "r1" (reverse([1,2,3])) [3,2,1])

fun run () =
    (test_evens();
     test_append();
     test_reverse())