(* Programming Languages (Coursera / University of Washington), 2013, Assignment 2 -={ *)

(* Notes to the Reader/Assessors -={

Notes
=====

    This code is also available for download (scroll to the bottom of this
    page section).

    I use the code-folding braces `-={' and `}=-' which makes working with
    larger files much easier as the irrelevant chunks of code can be folder
    away from view; just ignore the comments containing such braces.

}=- *)

(* Auxilliary Junk for Part 1 -={ *)
fun same_string(s1:string, s2:string) =
    s1 = s2
;
(* }=- *)
(* Part 1a -={ *) fun all_except_option(s:string, sl:string list) =
    let
        fun aux(sl:string list) =
            case sl of
                s'::sl' => (
                    case same_string(s, s') of
                        true  => aux(sl')
                    |   false => s'::aux(sl')
                )
            |   []      => []
        ;
        val result = aux(sl)
    in
        case result = sl of
            false => SOME(result)
        |   true  => NONE
    end
;
(* }=- *)
(* Part 1b -={ *) fun get_substitutions1(sll:string list list, s:string) =
    case sll of
        sl::ssl' => (
            case all_except_option(s, sl) of
                NONE      => get_substitutions1(ssl', s)
            |   SOME(sl') => sl'@get_substitutions1(ssl', s)
        )
    |         [] => []
;
(* }=- *)
(* Part 1c -={ *) fun get_substitutions2(sll:string list list, s:string) =
    let
        fun aux (sll:string list list, tail:string list) =
            case sll of
                sl::ssl' => (
                    case all_except_option(s, sl) of
                        NONE      => aux(ssl', tail)
                    |   SOME(sl') => aux(ssl', sl'@tail)
                )
            | []         => tail
    in
        aux(sll, [])
    end
;
(* }=- *)
(* Part 1d -={ *) fun similar_names(sll:string list list, {first:string, middle:string, last:string}) =
    let
        fun aux(sl:string list) =
            case sl of
                first::[]  => { first=first, middle=middle, last=last }::[]
            |   first::sl' => { first=first, middle=middle, last=last }::aux(sl')
        ;
    in
        aux(first::get_substitutions2(sll, first))
    end
;
(* }=- *)

(* Auxilliary Junk for Part 2 -={ *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove
(* }=- *)
(* Part 2a -={ *) fun card_color(c:card) =
    case c of
        (Clubs, _) => Black
    |   (Spades, _) => Black
    |   (Hearts, _) => Red
    |   (Diamonds, _) => Red
;
(* }=- *)
(* Part 2b -={ *) fun card_value(c:card) =
    case c of
        (_, Ace)   => 11
    |   (_, King)  => 10
    |   (_, Queen) => 10
    |   (_, Jack)  => 10
    |   (_, Num i) => i
;
(* }=- *)
(* Part 2c -={ *) fun remove_card(cl:card list, c:card, e:exn) =
    let
        fun aux(cl:card list) =
            case cl of
                c'::cl' => (
                    case c = c' of
                        true  => cl'
                    |   false => c'::aux(cl')
                )
            |   []      => []
        ;
        val result = aux(cl)
    in
        case result <> cl of
            true  => result
        |   false => raise e
    end
;

fun remove_card_solution (cs,c,e) =
    case cs of
        []      => raise e
    |   x::cs'  =>
        if x = c
        then cs'
        else x :: remove_card(cs',c,e)
;

(* }=- *)
(* Part 2d -={ *) fun all_same_color(cl:card list) =
    let
        fun aux(cl:card list) =
            case cl of
                c1::(c2::cl') => (card_color(c1) = card_color(c2)) andalso aux(c2::cl')
            |   _             => true
        ;
    in
        aux(cl)
    end
;
(* }=- *)
(* Part 2e -={ *) fun sum_cards(cl:card list) =
    let
        fun aux(cl, sum) =
            case cl of
                c::cl' => aux(cl', sum+card_value(c))
            |   []     => sum
        ;
    in
        aux(cl, 0)
    end
;
(* }=- *)
(* Part 2f -={ *) fun score(cl:card list, goal:int) =
    let
        val sum = sum_cards(cl);
        val prelim = if(sum > goal) then (3 * (sum - goal)) else (goal - sum);
    in
        if all_same_color(cl) then (prelim div 2) else prelim
    end
;
(* }=- *)
(* Part 2g -={ *) fun officiate(cl:card list, ml:move list, goal:int) =
    let
        fun play(cl:card list, ml:move list, hcl:card list) =
            case ml of
                Discard(c)::ml' => play(cl, ml', remove_card(hcl, c, IllegalMove))
            |   Draw::ml' => (
                    case cl of
                        c::cl' =>
                            if(sum_cards(c::hcl) > goal)
                            then score(c::hcl, goal)
                            else play(cl', ml', c::hcl)
                    |   [] => score(hcl, goal)
                )
            |   [] => score(hcl, goal)
        ;
    in
        play(cl, ml, [])
    end
;
(* }=- *)

(* Auxilliary Junk for Part 3 -={ *)
fun sum_reduce(cl:card list, goal:int) =
    let
        fun aces(cl:card list) =
            case cl of
                (_, Ace)::cl' => aces(cl') + 1
            |   _::cl'        => aces(cl')
            |   []            => 0
        ;
        fun aux(sum:int, aces:int) =
            if(sum <= goal) orelse (aces = 0) then sum
            else aux(sum-10, aces-1)
        ;
    in
        aux(sum_cards(cl), aces(cl))
    end
;
fun has_card(cl:card list, c:card) =
    case cl of
        c'::cl' => ( case c <> c' of false => has_card(cl', c) | true => true )
    |   []      => false
;
(* }=- *)
(* Part 3a1 -={ *) fun score_challenge(cl:card list, goal:int) =
    let
        val sum = sum_reduce(cl, goal);
        val prelim = if(sum > goal) then (3 * (sum - goal)) else (goal - sum);
    in
        if all_same_color(cl) then (prelim div 2) else prelim
    end
;
(* }=- *)
(* Part 3a2 -={ *) fun officiate_challenge(cl:card list, ml:move list, goal:int) =
    let
        fun play(cl:card list, ml:move list, hcl:card list) =
            case ml of
                Discard(c)::ml' => play(cl, ml', remove_card(hcl, c, IllegalMove))
            |   Draw::ml' => (
                    case cl of
                        c::cl' =>
                            if(sum_reduce(c::hcl, goal) > goal)
                            then score_challenge(c::hcl, goal)
                            else play(cl', ml', c::hcl)
                    |   [] => score_challenge(hcl, goal)
                )
            |   [] => score_challenge(hcl, goal)
        ;
    in
        play(cl, ml, [])
    end
;
(* }=- *)
(* Part 3b -={ * fun careful_player(cl:card list, goal:int) =
    let
        fun mlgen(ml:move list, cl:card list, hcl:card list) =
            let
                val score = score_challenge(hcl, goal)
                val sum = sum_reduce(hcl, goal);
                val hope = sum_reduce(c::hcl, goal) - goal
            in
                case (score, cl) of
                    (0, _) => ml
                |   (_, c::cl') =>
                        if(goal>sum+10) orelse score_challenge(c::hcl, goal) = 0
                        then mlgen(Draw::ml, cl', c::hcl)
                        else if hope <= 11
                            let
                                fun mkcard(s:suit, v:int) =
                                    case rank of
                                        11 => Ace
                                    |   10 => Jack
                                ;
                                val cC = mkcard(Clubs, hope)
                                val cD = mkcard(Diamonds, hope)
                                val cH = mkcard(Hearts, hope)
                                val cS = mkcard(Spades, hope)
                            in
                                if has_card(hcl, cC) then Discard(cC)::ml
                                else if has_card(hcl, cD) then Discard(cD)::ml
                                else if has_card(hcl, cS) then Discard(cS)::ml
                                else if has_card(hcl, cH) then Discard(cH)::ml
                                else ml
                            end
                |   (_, []) => ml
            end
        ;
    in
        mlgen([], [])
    end
;
* }=- *)
