(* Programming Languages (Coursera / University of Washington), 2013, Assignment 2 Unit Test -={ *)
use "hw2.sml";
Control.Print.printLength := 256;

(* Part 1a : all_except_option -={ *)
val test1a1 = all_except_option("XXX", ["XXX"]) = SOME [];
val test1a2 = all_except_option("XXX", ["cat", "XXX", "dog"]) = SOME ["cat", "dog"];
val test1a3 = all_except_option("XXX", ["cat", "mouse", "dog"]) = NONE
(* }=- *)
(* Part 1b : get_substitutions1() -={ *)
val test1b1 = get_substitutions1([["foo"],["there"]], "foo") = []; 
val test1b2 = get_substitutions1([["foo", "jack"],["there"]], "foo") = ["jack"];

val test1b3 = get_substitutions1([["F","J","K"],["X", "Z"],["A","B","C","J","D"]], "J")
= ["F", "K", "A", "B", "C", "D"];
(* }=- *)
(* Part 1c : get_substitutions2() -={ *)
val test1c1 = get_substitutions2([["foo"],["there"]], "foo") = [];
val test1c2 = get_substitutions2([["foo", "jack"],["there"]], "foo") = ["jack"];

val test1c3 = get_substitutions2([["F","J","K"],["X", "Z"],["A","B","C","J","D"]], "J")
= ["A", "B", "C", "D", "F", "K"];
(* }=- *)
(* Part 1d : similar_names() -={ *)
val test1d1 = similar_names(
    [
        ["Fred", "Fredrick"],
        ["Elizabeth", "Betty"],
        ["Freddie", "Fred", "F"]
    ], {first="Fred", middle="W", last="Smith"}
) = [
    {first="Fred", last="Smith", middle="W"},
    {first="Freddie", last="Smith", middle="W"},
    {first="F", last="Smith", middle="W"},
    {first="Fredrick", last="Smith", middle="W"}
];
(* }=- *)

(* Part 2a : card_color() -={ *)
val test2a1 = card_color(Clubs, Jack) = Black;
val test2a2 = card_color((Clubs, Num 2)) = Black;
val test2a3 = card_color((Hearts, Num 4)) = Red;
(* }=- *)
(* Part 2b : card_value() -={ *)
val test2b1 = card_value((Clubs, Num 2)) = 2;
val test2b2 = card_value(Clubs, Jack) = 10;
val test2b3 = card_value(Clubs, Ace) = 11;
(* }=- *)
(* Part 2c : remove_card() -={ *)
val test2c1 = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = [];
val test2c2 = remove_card(
    [
        (Clubs, Jack),
        (Clubs, Num(6)),
        (Clubs, Num(6)),
        (Clubs, Num(9)),
        (Clubs, Num(3)),
        (Clubs, Num(4))
    ], (Clubs, Num(6)), IllegalMove
) = [
    (Clubs, Jack),
    (Clubs, Num(6)),
    (Clubs, Num(9)),
    (Clubs, Num(3)),
    (Clubs, Num(4))
]
(* }=- *)
(* Part 2d : all_same_color() -={ *)
val test2d1 = all_same_color([(Hearts, Ace), (Hearts, Ace)]) = true;
val test2d2 = all_same_color([(Spades, Ace), (Hearts, Ace)]) = false;
val test2d3 = all_same_color([(Spades, Ace), (Clubs, Ace)]) = true;
val test2d4 = all_same_color([(Diamonds, Ace), (Hearts, Ace)]) = true;
val test2d5 = all_same_color(
    [
        (Clubs, Num(6)),
        (Spades, Num(9)),
        (Clubs, Num(3)),
        (Clubs, Num(4)),
        (Spades, Jack)
    ]
) = true;
(* }=- *)
(* Part 2e : sum_cards() -={ *)
val test2e1 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4;
val test2e2 = sum_cards(
    [
        (Clubs, Num(6)),
        (Spades, Num(9)),
        (Clubs, Num(3)),
        (Clubs, Num(4)),
        (Spades, Jack)
    ]
) = 32;
(* }=- *)
(* Part 2f : score() -={ *)
val test2f1 = score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4;
(* }=- *)
(* Part 2g : officiate() -={ *)
val test2g1 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6;
val test2g2 = officiate(
    [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
    [Draw,Draw,Draw,Draw,Draw], 42
) = 3;
val test2g3 = (
    (officiate(
        [(Clubs,Jack),(Spades,Num(8))],
        [Draw,Discard(Hearts,Jack)],
        42); false)
    handle IllegalMove => true);
(* }=- *)

(* Part 3a : score_challenge() -={ *)
val test3a1 = score_challenge([(Hearts, Num 2),(Clubs, Num 4)],10) = 4;
val test3a2 = score_challenge([(Clubs, Ace)], 13) = 1;
val test3a3 = score_challenge([(Clubs, Ace)], 11) = 0;
val test3a4 = score_challenge([(Clubs, Ace)], 3)  = 1;
val test3a5 = score_challenge([(Clubs, Ace)], 1)  = 0;
val test3a6 = score_challenge([(Clubs, Ace),(Clubs, Ace),(Clubs, Ace)], 3) = 0;
val test3a7 = score_challenge([(Clubs, Ace),(Clubs, Ace),(Clubs, Ace)], 13) = 0;
val test3a8 = score_challenge([(Clubs, Ace),(Clubs, Ace),(Clubs, Ace)], 23) = 0;
val test3a9 = score_challenge([(Clubs, Ace),(Hearts, Ace),(Clubs, Ace)], 33) = 0;
val test3a9 = score_challenge([(Clubs, Ace),(Hearts, Ace),(Clubs, Ace)], 43) = 10;
val test3a9 = score_challenge([(Clubs, Ace),(Spades, Ace),(Clubs, Ace)], 43) = 5;
val test3aA = score_challenge([(Clubs, Ace),(Hearts, Num(9))], 10) = 0;
val test3aB = score_challenge([(Clubs, Ace),(Hearts, Num(9))], 20) = 0;
val test3aC = score_challenge([(Clubs, Ace),(Hearts, Num(9))], 30) = 10;
(* }=- *)

(* }=- END; exit program *)
OS.Process.exit(OS.Process.success);
