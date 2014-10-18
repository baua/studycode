(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* task 1 *)
(* (a) *)
fun all_except_option(s1: string, slist: string list) =
    let
      fun aux (sList: string list) =
        case sList of
             [] => []
           | x::sList' =>
               case same_string(s1,x) of
                               true => aux(sList')
                             | false => x::aux(sList');
     val result = aux(slist)
  in
    case result = slist of
         false => SOME(result)
        |true => NONE
    end

(* (b) *)
fun get_substitutions1(sublist: (string list) list , sub: string ) =
  case sublist of
       [] => []
    |  h::sublist' => case all_except_option(sub,h) of
                        NONE => get_substitutions1(sublist',sub)
                      | SOME(sublist'') => sublist'' @ get_substitutions1(sublist',sub)


(* (c) *)
fun get_substitutions2(sublist: (string list) list , sub: string ) =
    let
        fun aux(strList: string list list,acc: string list) =
            case strList of
                h::strList' => ( case all_except_option(sub,h ) of
                                     NONE           => aux(strList',acc)
                                   | SOME(result)   => aux(strList',result @ acc)
                               )
              | [] => acc
    in
        aux(sublist,[])
    end

(* (d) *)
fun similar_names( ssl: string list list, fullname: { first: string,middle: string, last:
  string}) =
  let
    val { first=a, middle=b ,last=c } = fullname
    fun createFNs ( FNs: string list ) =
      case FNs of
           [] => []
        | h::tail => {first=h,middle=b,last=c}::createFNs(tail)
  in
    case fullname of
      { first = FN, middle = MN, last = LN } => createFNs( FN::get_substitutions1(ssl,FN) )
  end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank 

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* Task 2 *)
fun card_color(c:card) =
    case c of 
        (Clubs, _) => Black
    |   (Diamonds,_) => Red
    |   (Spades, _) => Black
    |   (Hearts,_) => Red

(* (b) *)
fun card_value ( c: card ) = 
  case c of
       (_, Ace) => 11
    | (_, Num i ) => i
    | (_,_) => 10

(* (c) *)
fun remove_card( cs: card list, c: card, e: exn) =
     case cs of
         h::tail => if h = c then 
                        tail
                    else
                        h::remove_card(tail,c,e)
          | []   => raise e

(* (d) *)
fun all_same_color(cs: card list) =
    let
        fun aux(cs': card list,color: color) =
           case cs' of
            hd::tail => if color = card_color(hd) then
                           aux(tail,color)
                        else
                           false
            | [] => true
    in
        case cs of
            hd::tail => aux(tail,card_color(hd))
            | [] => true
    end

(* (e) *)
fun sum_cards(cs: card list) =
    let
        fun aux(cs': card list, acc: int) =
            case cs' of
                hd::tail => aux(tail,acc + card_value(hd))
                | [] => acc
    in
        case cs of
            hd::tail => aux(tail, card_value(hd))
           | [] => 0
    end

 (* (f) *)
fun score( cs: card list, goal: int) =
    let
        val sum = sum_cards(cs);
        val result = case sum > goal of
                         true => 3 * ( sum - goal)
                      | false => ( goal - sum );
    in
       if all_same_color(cs) then
         result div 2
       else
         result
    end

(* (g) *)
fun officiate(cl: card list, ml: move list, goal: int) =
  let
    fun aux(heldList: card list, cardList: card list, moveList: move list ) =
      case moveList of
        mhd::mtail => ( case mhd of
                          Draw    => ( case cardList of
                                         chd::ctail => if sum_cards( chd::heldList) > goal 
                                                       then
                                                         chd::heldList
                                                       else 
                                                         aux(chd::heldList,remove_card(cardList,chd,IllegalMove),mtail)
                                           | []     => heldList
                                     )
                      | Discard c => aux(remove_card(heldList,c,IllegalMove),cardList,mtail)
                     )

             | [] => heldList
    in
        score(aux([], cl, ml),goal)
    end
(*
datatype move = Discard of card | Draw
exception IllegalMove
*)
