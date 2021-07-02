(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)

fun only_capitals lst = 
	List.filter (fn x => Char.isUpper (String.sub (x, 0))) lst

(* 2 *)
fun longest_string1 lst =
	foldl (fn (x, y) => if String.size y >= String.size x then y else x)
		""
		lst

(* 3 *)
fun longest_string2 lst =
	foldl (fn (x, y) => if String.size x >= String.size y then x else y)
		""
		lst

(* 4 *)
fun longest_string_helper f lst str = foldl  f str lst

fun longest_string3 lst = longest_string_helper 
	(fn (x, y) => if String.size y >= String.size x then y else x)
	lst
	""

fun longest_string4 lst = longest_string_helper 
	(fn (x, y) => if String.size x >= String.size y then x else y)
	lst
	""

(* 5 *)
val  longest_capitalized = longest_string3 o only_capitals

(* 6 *)
val rev_string = String.implode o List.rev o String.explode

(* 7 *)
fun first_answer f lst =
	if null lst then raise NoAnswer
	else
		case f (hd lst) of
			SOME v => v
			| NONE => first_answer f (tl lst)

(* 8 *)
fun all_answers f lst =
	let fun helper lst result =
		if null lst then result
		else
			let val eval = f (hd lst) 
			in 
				case eval of
				SOME n => helper (tl lst) (n @ result) 
				| NONE => raise NoAnswer
			end
	in 
		SOME (helper lst [])
		handle NoAnswer => NONE
	end

(* 9 *)

(* 9a *)
val count_wildcards = g (fn () => 1) (fn x => 0)

(* 9b *)
fun count_wild_and_variable_lengths lst = count_wildcards lst + g (fn () => 0) 
	(fn x => 1) lst

(* 9c *)
fun count_some_var (str, pat) = g (fn () => 0) (fn x => if x = str then 1 else
	0) pat
