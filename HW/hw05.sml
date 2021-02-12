(* remove this when you're done to make sure you didn't miss anything *)
exception Unimplemented;

Control.Print.printDepth := 100;
use "hw05-lib.sml";

(* ---------------------------------------------------------------------- *)
(* quicksort on trees *)

fun combine (t1 : tree, t2 : tree) : tree = 
    case t1 of
        Empty => t2
      | Node(l1,x1,r1) => Node(combine(l1,r1),x1,t2)

(* Task *)
fun filter (t : tree, i : int, r : rel) : tree =
    case t of
    Empty => Empty
    | Node(l1,x1,r1) => case (r) of
                      LT => (case (x1<i) of
                              true => Node(filter(l1,i,r), x1, filter(r1,i,r))
                              |false => filter(combine(l1,r1), i, r)
                              )
                      |GEQ => (case (x1>= i) of
                                true => Node(filter(l1,i,r), x1, filter(r1,i,r))
                                |false=> filter(combine(l1,r1), i, r)
                                )

(* filter is hard to test on its own because the order of the items in the resulting tree 
   is up to you.  you should try running filter on some trees in smlnj and 
   see if the results look reasonable.*)

val a_tree = Node(Node(leaf 1, 2, leaf 3), 4, Node(leaf 5, 6, leaf 7))
    
(* Task *)
fun quicksort_t (t : tree) : tree =
    case t of
    	 Empty => Empty
       | Node(l1,x1,r1) => Node(quicksort_t(filter(combine(l1,r1), x1, LT)), x1, quicksort_t(filter(combine(l1,r1), x1, GEQ)))

fun test_quicksort()=
    ((* one way to test is to convert to and from lists -- 
        this way different arrangements of the same items in a tree 
        won't affect the results
        *)
     testil "f1" (tolist (quicksort_t (fromlist [8,1,4,5,6,7,3,2]))) [1,2,3,4,5,6,7,8]
     (* write your tests here *)
     )
    
(* ---------------------------------------------------------------------- *)
(* rebalance *)

(* Task *)
fun takeanddrop (t : tree, i : int) : tree * tree =
    case t of
    	 Empty => (Empty, Empty)
       | Node(l,x,r) => (case (i <= size(l)) of
       	 		   	  true => let val (l2, lr) = takeanddrop(l, i)
          			          in (l2, Node(lr, x, r)) end
     			        | false => let val (r2, rr) = takeanddrop(r, i-size(l)-1)
				  	   in ( Node(l, x, r2), rr) end )

fun test_tad()=
    (
     testtt "tad1" (takeanddrop (Node(leaf 1, 2, leaf 3), 0)) (Empty, Node(leaf 1, 2, leaf 3));
     testtt "tad2" (takeanddrop (Node(leaf 1, 2, leaf 3), 1)) (leaf 1, Node(Empty, 2, leaf 3));
     testtt "tad3" (takeanddrop (Node(leaf 1, 2, leaf 3), 2)) (Node(leaf 1, 2, Empty), leaf 3)
     (* write your tests here *)
     )
    
(* the rest of rebalance interms of your takeanddrop *)
fun halves (t : tree) : tree * int * tree =
    let
      val (l , vr) = takeanddrop (t , (size t) div 2)
      val (Node (Empty, v , Empty) , r) = takeanddrop (vr , 1)
    in
      (l , v , r)
    end

fun rebalance (t : tree) : tree =
    case t
     of Empty => Empty
      | _ =>
        let
          val (l , x , r) = halves t
        in
          Node (rebalance l , x , rebalance r)
        end

fun run() =
    (test_quicksort();
     test_tad())