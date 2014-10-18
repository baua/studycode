(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* problem 1 *)
(* task a *)
(* val all_except_option = fn : string * string list -> string list option *)
fun all_except_option( s, xs) =
  let
    fun all_except(xsh) =
      case xsh of
          [] => []
        | h::t => case same_string(s, h ) of
                      true => t
                    | false => h::all_except(t)
      val rv = all_except(xs)
  in
    case rv = xs of
        true => NONE
      | false => SOME(rv)
  end

fun all_except_option2( s, xs ) =
  let
    fun accu(axs: string list,acc: string list) =
      case axs of
          [] => acc
        | h::t => case same_string( s, h ) of
                      true => acc
                    | false => accu(t, acc@[h])
    val rv = accu(xs,[])
  in
    case rv = xs of
        true => NONE
      | false => SOME(rv)
  end

(* task b *)
(* val get_substitutions1 = fn : string list list * string -> string list *)
fun get_substitutions1 ( xss, s ) =
  case xss of
      [] => []
    | h::t => case all_except_option(s,h) of
                  NONE => get_substitutions1(t,s)
                | SOME(x) => x@get_substitutions1(t,s)

(*
val get_substitutions2 = fn : string list list * string -> string list
val similar_names = fn : string list list * {first:string, last:string,
middle:string}
-> {first:string, last:string, middle:string} list
val card_color = fn : card -> color
val card_value = fn : card -> int
val remove_card = fn : card list * card * exn -> card list
val all_same_color = fn : card list -> bool
val sum_cards = fn : card list -> int
val score = fn : card list * int -> int
val officiate = fn : card list * move list * int -> int
*)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
