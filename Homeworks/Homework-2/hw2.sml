(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* 1 (a) *)
fun all_except_option (toFind, lst) =
	let 
		fun contains (lst) =
			case lst of
				[] => false
				| hd :: tl => if same_string (hd, toFind)
							  then true
							  else contains (tl)
		fun remove_element (lst) =
			case lst of
				[] => []
				| hd :: tl => if same_string (hd, toFind)
							  then remove_element (tl)
							  else hd :: remove_element (tl)
	in
		case contains (lst) of
			false => NONE
			| true => SOME (remove_element lst)
	end

(* 1 (b) *)
fun get_substitutions1 (lstOflst, s) =
	case lstOflst of
		[] => []
		| hd :: tl =>
			case all_except_option (s, hd) of
				NONE => [] @ get_substitutions1 (tl, s)
				| SOME e => e @ get_substitutions1 (tl, s)

(* 1 (c) *)
fun get_substitutions2 (lstOflst, s) =
	let 
		fun substitution_helper (list_left, result) =
		case list_left of
			[] => result
			| hd :: tl => case all_except_option (s, hd) of
						  	NONE => substitution_helper (tl, result)
						  	| SOME e => substitution_helper (tl, result @ e)
	in
		substitution_helper (lstOflst, [])
	end

(* 1 (d) *)
fun similar_names (lstOflst, name) =
	let 
		val {first=f, middle=m, last=l} = name
		val names = get_substitutions2 (lstOflst, f)
		fun similar_names_helper (list_names, result) =
			case list_names of
				[] => result
				| hd :: tl => 
				similar_names_helper (tl, result @ [{first = hd, middle = m,
					last = l}])
	in
		similar_names_helper (names, [name])
	end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* Solutions for problem 2 *)

(* 2 (a) *)
fun card_color (aCard) =
	case aCard of
		(Clubs, _) => Black
		| (Spades, _) => Black
		| (Hearts, _) => Red
		| (Diamonds, _) => Red

(* 2 (b) *)
fun card_value (aCard) =
	case aCard of
		(_, Num i) => i
		| (_, Ace) => 11
		| _ => 10

(* 2 (c) *)
fun remove_card (listOfCards, c, e) =
	case listOfCards of
		[] => raise e
		| hd:: tl => if hd = c then tl
					 else hd :: remove_card (tl, c, e)

(* 2 (d) *)
fun all_same_color (listOfCards) =
	case listOfCards of
		[] => true
		| [e] => true
		| hd :: neck :: tl => if card_color hd = card_color neck
						   then all_same_color (neck :: tl)
						   else false

(* 2 (e) *)
fun sum_cards (listOfCards) =
	let 
		fun sum_cards_helper (remaining, result) =
			case remaining of
				[] => result
				| hd :: tl => sum_cards_helper (tl, card_value hd + result)
	in
		sum_cards_helper (listOfCards, 0)
	end

(* 2 (f) *)
fun score (listOfCards, target) =
	let 
		val total = sum_cards (listOfCards)
	in
		let val preliminary_score = if total > target 
									then 3 * (total - target)
									else (target - total)
		in
			if all_same_color (listOfCards)
			then preliminary_score div 2
			else preliminary_score
		end
	end

(* 2 (g) *)
fun officiate (listofcards, moves_list, goal) =
	let 
		fun officiate_helper (remaining, moves_left, held) =
			case moves_left of
				[] => score (held, goal)
				| Discard c :: tl => officiate_helper (remaining, tl,
					remove_card
					(held, c, IllegalMove))
				| Draw :: tl => 
					case remaining of
						[] => score (held, goal)
						| x :: xs => if sum_cards (x :: held) > goal
									 then score (x :: held, goal)
									 else officiate_helper (xs, tl, x :: held)
	in officiate_helper (listofcards, moves_list, [])
	end

(* Challenge Problems *)

(* 3 (a) *)
fun card_value_one (aCard: card) =
	let val (s: suit, r: rank) = aCard 
	in
		case r of
			Ace => 11
			| Num i => i
			| _ => 10
	end

fun card_value_two (aCard: card) =
	let val (s: suit, r: rank) = aCard 
	in
		case r of
			Ace => 11
			| Num i => i
			| _ => 10
	end

fun sum_cards_challenge (listOfCards, card_value_type) =
	let 
		fun sum_cards_helper (lol, result) =
			case lol of
				[] => result
				| hd :: tl => sum_cards_helper (tl, card_value_type hd +
					result)
	in
		sum_cards_helper (listOfCards, 0)
	end

fun calculate_score_challenge (listOfCards, target, card_value_type) =
	let val total = sum_cards_challenge (listOfCards, card_value_type)
	in
		let val preliminary_score = if total > target 
									then 3 * (total - target)
									else (target - total)
		in
			if all_same_color (listOfCards)
			then preliminary_score div 2
			else preliminary_score
		end
	end

fun score_challenge (listofcards, target) =
	let 
		val first = calculate_score_challenge (listofcards, target, card_value_one)
		val second = calculate_score_challenge (listofcards, target, card_value_two)
	in 
		if first < second
		then first
		else second
	end
