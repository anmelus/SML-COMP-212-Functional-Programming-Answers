
(* We'll talk about this next week! *)

fun go (show : 'model -> string,
        respond : 'model * string -> 'model,
        m : 'model) : unit =
    let val () = print (show m ^ "\n")
    in
        case TextIO.inputLine TextIO.stdIn of
            NONE => raise Fail "text input errored"
          | SOME input => go(show, respond, respond(m,String.substring(input,0,String.size input -1)))
    end


fun intToString(x : int) : string = Int.toString x

(* ---------------------------------------------------------------------- *)

(* TASK 1: Add constructors to the type 'model' to model the states of the shopping application *)
    
datatype model =
   	  EnterName
	| Purchase of string * int * int * int * int
	| Checkout of string * int * int * int * int
	

val apples = 0
val bananas = 0
val cookies = 0
val name = ""

val amount = 0
	       
(* TASK 2: write a function that displays the model as a string 	*)
    
fun show (m : model) : string =
    case m of
	EnterName => "Please enter your name:"
      | Purchase(name,apples,bananas,cookies,amount) => "Hi, " ^ name ^ " What would you like to buy?\n apples $1/pound\n bananas $2/bunch\n cookies $2/box"
      | Checkout(name,apples,bananas,cookies,amount) => "Hi, " ^ name ^ "\nYour cart contains " ^ intToString(apples) ^ " apples " ^ intToString(bananas) ^ " bananas " ^ intToString(cookies) ^ " cookies.\n" ^ "I will charge you $" ^ intToString(amount) ^ ".\n" ^ "Type 'pay' to pay."		

(* TASK 3: write a function that updates the model based on user input *)

fun counter(t : string) : int =
    case t of
	"apples" => apples+1
      | "bananas" => bananas+1
      | "cookies" => cookies+1
    
fun update(m : model, text : string) : model =
    case m of
    EnterName => Purchase(text,apples,bananas,cookies,amount)
  | Purchase(name,apples,bananas,cookies,amount) => (case text of 
		            "apples" => Purchase(name,apples+1,bananas,cookies,amount+1)
			  | "bananas" => Purchase(name,apples,bananas+1,cookies,amount+2)
			  | "cookies" => Purchase(name,apples,bananas,cookies+1,amount+2)
			  | "checkout" => Checkout(name,apples,bananas,cookies,amount)
		          | _ => Purchase(name,apples,bananas,cookies,amount))
  | Checkout(name,apples,bananas,cookies,amount) => (case text of
			   "pay" => EnterName
		         | _ => Checkout(name,apples,bananas,cookies,amount) )

    
    
fun respond(m : model, text : string) : model =
    case m of
	EnterName => update(m, text)
      | Purchase(name,apples,bananas,cookies,amount)  => update(m, text)			    
      | Checkout(name,apples,bananas,cookies,amount) => update(m, text)


			
(* TASK 4: choose an initial state, fill it, and uncomment this, 
           then run() will run the application *)
    
fun run() = go(show,respond, EnterName)


