(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_2 = only_capitals ["A","b","C"] = ["A","C"]
val test1_3 = only_capitals ["Aaa","bBB","cC"] = ["Aaa"]

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2_2 = longest_string1 ["Aaa","bc","C"] = "Aaa"
val test2_3 = longest_string1 ["Aaa","bc","Ccc"] = "Aaa"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3_2 = longest_string2 ["Aaa","bc","C"] = "Aaa"
val test3_3 = longest_string2 ["Aaa","bc","Ccc"] = "Ccc"

val test4a= longest_string3 ["A","bc","C"] = "bc"
val test4a_2 = longest_string3 ["Aaa","bc","C"] = "Aaa"
val test4a_3 = longest_string3 ["Aaa","bc","Ccc"] = "Aaa"

val test4b= longest_string4 ["A","B","C"] = "C"
val test4b_2 = longest_string4 ["Aaa","bc","C"] = "Aaa"
val test4b_2 = longest_string4 ["Aaa","bc","Ccc"] = "Ccc"

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test5_2 = longest_capitalized ["Aaa","bc","C"] = "Aaa"
val test5_3 = longest_capitalized ["A","bc","CC"] = "CC"
val test5_4 = longest_capitalized ["a","bc","c"] = ""

val test6 = rev_string "abc" = "cba"
val test6_2 = rev_string "" = ""
val test6_3 = rev_string "ABcD" = "DcBA"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_2 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []
val test8_3 = all_answers(fn a => if a = 0 then NONE else SOME [a]) [] = SOME []
val test8_4 = all_answers(fn a => if a = 0 then NONE else SOME [a]) [1, 2] = SOME[2,1]

(*
* - all_answers(fn a => if a = 0 then NONE else SOME [a]) [1, 2, 3, 4];
* val it = SOME [1,2,3,4] : int list option
* - all_answers(fn a => if a = 0 then NONE else SOME [a]) [1, 2, 0, 4];
* val it = NONE : int list option
* - all_answers(fn a => if a = 0 then NONE else SOME [a]) [1, 2, 4];
* val it = SOME [1,2,4] : int list option
* *)
val test9a = count_wildcards Wildcard = 1
val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_2 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "aa"]) = 3
val test9c = count_some_var ("x", Variable("x")) = 1
val test10 = check_pat (Variable("x")) = true
val test10_2 = check_pat (ConstructorP ("hi",TupleP[Variable "x",Variable "x"])) = false
val test10_3 = check_pat (ConstructorP ("hi",TupleP[Variable "x",ConstructorP ("yo",TupleP[Variable "x",UnitP])])) = false
val test10_1 = check_pat(TupleP [Variable "aa", Variable "aa"]) = false
val test11 = match (Const(1), UnitP) = NONE
val test11_2 = match ((Const 1), (Variable "x")) = SOME [("x", Const 1)]
val test11_3 = match(Constructor("pig", Const 5), ConstructorP ("pig", Variable "myvar")) = SOME [("myvar", Const 5)]
val test11_4 = match(Constructor("pig", Const 5), ConstructorP ("sheep", Variable "myvar")) = NONE
val test12 = first_match Unit [UnitP] = SOME []

(*
check_pat: Called check_pat on input: ConstructorP ("hi",TupleP[Variable
"x",Variable "x"]), should have gotten: false but your function returned
otherwise. [incorrect answer]
check_pat: Called check_pat on input: ConstructorP ("hi",TupleP[Variable
"x",ConstructorP ("yo",TupleP[Variable "x",UnitP])]), should have gotten: false
but your function returned otherwise. 
*)
