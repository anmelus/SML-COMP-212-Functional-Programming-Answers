exception Unimplemented

(* map *)

fun map (f : 'a -> 'b, l : 'a list) : 'b list =
    case l of 
        [] => []
      | x :: xs => f x :: map (f , xs)

(* ---------------------------------------------------------------------- *)
(* filter *)

fun evenP (n : int) : bool = (n mod 2) = 0

fun evens (l : int list) : int list =
    case l of
	 [] => []
	| x :: xs => case evenP x of
			  true => x :: evens xs
			 |false => evens xs

fun keepUpper (l : char list) : char list =
    case l of 
        [] => []
      | x :: xs => (case Char.isUpper x of 
                        true => x :: keepUpper xs
                      | false => keepUpper xs)

(* TASK *)
fun filter (p : 'a -> bool, l : 'a list) : 'a list =
    case l of
	[] => []
      | x :: xs => if p x then x :: filter(p, xs)
		   else filter(p, xs)

(* TASK *)
fun evens (l : int list) : int list = filter(evenP, l)
fun keepUpper (l : char list) : char list = filter(Char.isUpper, l)

(* keepUpper [#"a", #"B"] should be [#"B"] *)

(* TASK *)
		(* very difficult to understand without anonymous functions! *)				  
fun quicksort_l (l : int list) : int list =
    case l of
    [] => []
  | [x] => [x]
  | x :: xs => let val (l1,l2) = (filter(fn a => a < x,xs),filter(fn a => a >= x,xs))
	       in
		   quicksort_l(l1) @ x::quicksort_l(l2) end

(* ---------------------------------------------------------------------- *)
(* map and filter *)

(* TASK *) (* did not read instructions, forgot to use map and filter*)
fun ages_over_18_mistake (l : (string * int) list) : (string * int) list =
    case l of
	[] => []
      | (x,y) :: xs => case ((2020-y-1) > 18) of
			   true => (x,2020-y-1) :: ages_over_18_mistake(xs)
			 | false => ages_over_18_mistake(xs)

fun ages_over_18 (l : (string * int) list) : (string * int) list = 
  filter(fn (_,x) => x > 18, (map(fn (y,z) => (y,2020 - z - 1),l)))

(* ---------------------------------------------------------------------- *)
(* all *)

fun allPos (l : int list) : bool =
    case l of
         [] => true
       | x :: xs => (x > 0) andalso allPos xs

fun allOfLength (len : int, l : 'a list list) : bool =
     case l of
          [] => true
         | x :: xs => ( (List.length x = len) andalso allOfLength(len, xs))

(* TASK: define a function named all *)

fun all (p : 'a -> bool, l : 'a list) : bool =
   case l of
     []      => true
   | x :: xs => p(x) andalso all(p,xs) 

(* TASK *)
fun allPos (l : int list) : bool = all(fn x => x > 0, l)
fun allOfLength (len : int, l : 'a list list) : bool = all(fn x => List.length x = len, l)

fun square(l : 'a list list) : bool = all(fn x => allOfLength(List.length l, l), l)
								      


(* ---------------------------------------------------------------------- *)
(* reduce *)

fun sum (l : int list) : int = 
   case l of 
        [] => 0
      | x :: xs => x + (sum xs)

fun join (l : string list) : string = 
    case l of 
        [] => ""
      | x :: xs => x ^ join xs

(* TASK *)
fun reduce(c : 'a * 'a -> 'a, n : 'a, l : 'a list) : 'a =
    case l of
	[] => n
	| x :: xs => c(x , reduce(c , n , xs))

(* TASK *)
fun sum (l : int list) : int = reduce(fn (x,y) => x+y, 0, l)
fun join (l : string list) : string = reduce(fn (x,y) => x ^ y, "", l) 


(* ---------------------------------------------------------------------- *)
(* reduce *)

fun lines (s : string) : string list =
    (String.tokens (fn #"\n" => true | _ => false) s)

fun words (s : string) : string list =
    (String.tokens (fn #" " => true | #"\n" => true | _ => false) s)

fun wordcount (s : string) : int = sum (map ((fn _ => 1), (words s)))

(* TASK *)
fun longestline (s : string) : int = reduce(Int.max,0,map(wordcount,lines(s)))

(*
longestline "for life’s not a paragraph\nAnd death i think is no parenthesis\n"
should be 7

wordcount "for life’s not a paragraph\nAnd death i think is no parenthesis\n"
should be 12
*)
