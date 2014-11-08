(* Homework 3, Steve Muench. See "tests.sml" for tests *)
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

(* 
 * Ex 1: only_capitals: Return subset of string list with Strings beginning
 * ----  with a capital letterAssume all strings have at least one character
 *       
 *       NOTE: We can assume any string has at least 1 character!
 *)
fun only_capitals string_list =
    List.filter (fn x => Char.isUpper(String.sub(x,0))) string_list 

(* 
 * Ex 2: longest_string1: Returns longest string in the string list
 * ----
 *       NOTE: If list is empty return empty string ("") 
 *)
fun longest_string1 string_list =
    List.foldl (fn (x,y) => if String.size x > String.size y then x else y) "" string_list

(* 
 * Ex 3: longest_string2: Returns longest string in the string list
 * ----
 *       NOTE: If list is empty return empty string ("") 
 *             In case of tie, return string closest to end of list
 *)
fun longest_string2 string_list =
    List.foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" string_list

(* Ex 4: longest_string_helper: 
 *
 *
 *)
fun longest_string_helper compare_two_ints string_list =
    List.foldl (fn (x,y) => if compare_two_ints(String.size x,String.size y) then x else y) "" string_list

(* 
 * Ex 4: longest_string3: Returns longest string in the string list
 * ----
 *       NOTE: If list is empty return empty string ("") 
 *             In case of tie, return string closest to beginning of list
 *)
val longest_string3 = longest_string_helper (fn (x,y) => x > y)

(* 
 * Ex 4: longest_string4: Returns longest string in the string list
 * ----
 *       NOTE: If list is empty return empty string ("") 
 *             In case of tie, return string closest to end of list
 *)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

(*
 * Ex 5: longest_capitalized: returns longest string in the list that begins with capital letter
 *
 *       NOTES: - We can assume any string has at least 1 character!
 *              - If list is empty return empty string ("") 
 *              - If there are ties, return string closest to beginning of list (like 
 *                longest_string1 in problem 2
 *)
val longest_capitalized = longest_string1 o only_capitals

(*
 * Ex 6: rev_string: reverses the order of characters in a string
 *)
val rev_string = String.implode o List.rev o String.explode

(*
 * Ex 7: first_answer: returns first non-NONE result for
 *                     the values in a list, mapped by the
 *                     ('a -> 'b option) function passed in
 *)
fun first_answer a_to_b_option_func a_list =
    case (List.mapPartial a_to_b_option_func a_list) of
	[] => raise NoAnswer
      | first_answer::other_answers => first_answer

(*
 * Ex 8: all_answers: applies first argument to elements of the second argument
 *                    if any returns NONE, then result is NONE
 *                    Otherwise, returns flattened list of 'b option 
 *)
fun all_answers a_to_b_option_list a_list =
    case (a_list,List.mapPartial a_to_b_option_list a_list) of
	([],_) => SOME[]
      | (_,[]) => NONE
      | (_,first_answer::other_answers) => 
        let
            val aSize = List.length a_list
	in
            if aSize > 0 andalso List.length other_answers = aSize - 1 
            then SOME (first_answer @ List.concat other_answers)
            else NONE
        end

(* 
 * Ex 9a: count_wildcards: return count of wildcard patterns contained in a pattern
 *)
val count_wildcards = g (fn wc => 1) (fn var_name => 0) 

(* 
 * Ex 9b: count_wild_and_variable_lengths: return count of wildcard patterns
 *                                         plus the sum of the string lengths
 *                                         of variable names (not counting
 *                                         constructor names)
 *)
val count_wild_and_variable_lengths = g (fn wc => 1) (fn var_name => String.size var_name) 

(* 
 * Ex 9c: count_some_var: return count of wildcard patterns
 *                                         plus the sum of the string lengths
 *                                         of variable names (not counting
 *                                         constructor names)
 *)
fun count_some_var (s,p) =
    g (fn x => 0) (fn var_name => if var_name = s then 1 else 0) p

(*
 * Ex 10: check_pat: returns true if pattern contains 
 *)
fun check_pat p =
    let
        fun has_repeats slist =
	    case slist of
		[] => false
	      | first_string::[] => false
	      | first_string::other_strings => (List.exists (fn x => x = first_string) other_strings) 
                                               andalso not (has_repeats (other_strings))

	fun variable_list pat = 
	    case pat of
		Variable x        => [x]
	      | TupleP ps         => List.foldl (fn (ptn,i) => variable_list ptn @ i) [] ps
	      | ConstructorP(_,ptn) => variable_list ptn
	      | _                 => []
    in
	not (has_repeats (variable_list p))
    end

(* 
 * Ex 11: match: takes '(valu*pattern)' and returns '(string*valu) list option'
 *               returns NONE if pattern does not match
 *                       SOME lst of bindings if it does match
 *
 *               NOTE:   If the value matches but the pattern has no
 *                       patterns of the form 'Variable s' then the result
 *                       is SOME []
 *
 *   To use all_answers we have:
 *
 *      'a = valu * pattern
 *      'b = string * valu
 *      ListPair.zip(values,patterns) = [(v,p)]  playing the role of the 'a list = (valu*pattern) list
 *      match playing the role of the 'a -> 'b list option function (returning (string*valu) list option)
 *
 *      all_answers produces the flattened (string*value) list option
 *)

fun match (v,p) =
    if check_pat p then
	case (v,p) of
	    (_,Wildcard) => SOME []
	  | (vl,Variable str)  => SOME [(str,vl)]
	  | (Unit,UnitP) => SOME []
	  | (Const c1,ConstP c2) => if c1 = c2 then SOME [] else NONE
	  | (Tuple values,TupleP patterns) => if List.length values = List.length patterns 
                                              then
						  case all_answers match (ListPair.zip(values,patterns)) of
						      NONE => NONE
						    | SOME bindings => SOME bindings
					      else NONE
	  | (Constructor (s1,v),ConstructorP(s2,p)) => if s1 = s2 then match(v,p) else NONE 
	  | _ => NONE
    else NONE

(*
 * Ex 12: first_match: returns the first matching binding or NONE if there are none
 *)
fun first_match v ps = 
    let
	val val_pat_list = List.map (fn cur_pattern => (v,cur_pattern)) ps
    in
     SOME (first_answer (fn (v,p) => match(v,p)) val_pat_list) handle NoAnswer => NONE
    end
