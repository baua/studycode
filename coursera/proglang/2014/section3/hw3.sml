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

(* task 1 *)
(*val only_capitals = fn : string list -> string list *)
fun only_capitals xs =
    List.filter (fn x => (Char.isUpper(String.sub(x,0)))) xs


(* task 2 *)
(* val longest_string1 = fn : string list -> string *)
fun longest_string1 xs =
  List.foldl (fn (x,y) => if String.size(x) > String.size(y) then x else y) "" xs

(* task 3 *)
(* val longest_string2 = fn : string list -> string *)
fun longest_string2 xs =
  List.foldl (fn (x,y) => if String.size(x) >= String.size(y) then x else y) "" xs

(* task 4 *)
(* val longest_string_helper = fn : (int * int -> bool) -> string list -> string *)
fun longest_string_helper f =
  List.foldl ( fn(x,acc) => if f(String.size(x), String.size(acc)) then x else acc) ""

(* val longest_string3 = fn : string list -> string *)
val longest_string3 =  longest_string_helper (fn (x,y) => x > y)

(* val longest_string4 = fn : string list -> string *)
val longest_string4 =  longest_string_helper (fn (x,y) => x >= y)

(* task 5 *)
(* val longest_capitalized = fn : string list -> string *)
val longest_capitalized = longest_string1 o only_capitals

(* task 6 *)
(* val rev_string = fn : string -> string *)
val rev_string = String.implode o List.rev o String.explode

(* task 7 *)
(* val first_answer = fn : (’a -> ’b option) -> ’a list -> ’b *)
fun first_answer f xs =
  case xs of
      [] => raise NoAnswer
    | h::t => case f h of
                NONE => first_answer f t
              | SOME(v) => v

(* task 8 *)
(* val all_answers = fn : (’a -> ’b list option) -> ’a list -> ’b list option *)
fun all_answers f xs =
    let
      fun aux(xs, acc) =
        case xs of
            [] => SOME acc
          | h::t => case f h of
                        NONE => NONE
                      | SOME(v) => aux(t,v @ acc)
    in
      aux(xs,[])
    end

(* task 9 *)
(* task 9a *)
(* val count_wildcards = fn : pattern -> int *)
fun count_wildcards p =
  g (fn() => 1) (fn x => 0) p

(* task 9b *)
(* val count_wild_and_variable_lengths = fn : pattern -> int *)
fun count_wild_and_variable_lengths p =
  g (fn() => 1 ) (fn s => String.size(s)) p

(* task 9c *)
(* val count_some_var = fn : string * pattern -> int *)
fun count_some_var (s: string, p: pattern ) =
  g ( fn() => 0 ) ( fn x => if x = s then 1 else 0 ) p

(* task 10 *)
(* val check_pat = fn : pattern -> bool *)
fun check_pat pattern =
  let
    fun aux(p,acc) =
      case p of
          Variable x => x::acc
          | TupleP ps => List.foldl (fn(ph,yh) => aux(ph,yh)) acc ps
          | ConstructorP(_,ps) => aux(ps,acc)
          | _ => acc
    fun unique xs =
      case xs of
         [] => true
       | h::t => if List.exists ( fn y => h = y ) t then
                    false
                 else
                   unique t
  in
    unique(aux(pattern,[]))
  end

(* task 11 *)
(* val match = fn : valu * pattern -> (string * valu) list option *)
fun match (v,p) =
  case (v,p) of
       ( _, Wildcard) => SOME []
     | (va, Variable s) =>SOME[(s,va)]
     | (Unit, UnitP ) => SOME []
     | (Const cv, ConstP cp) => if cv = cp then SOME[] else NONE
     | (Tuple vs, TupleP ps) => if List.length ps = List.length vs then
                                  all_answers match (ListPair.zip(vs,ps))
                                else NONE
     | (Constructor(s2,v), ConstructorP(s1,p)) => if s1 = s2 then match(v,p) else NONE
     | _ => NONE

(* task 12 *)
(* val first_match = fn : valu -> pattern list -> (string * valu) list option *)
fun first_match v pl =
  SOME(first_answer (fn px => match(v,px)) pl) handle NoAnswer => NONE
