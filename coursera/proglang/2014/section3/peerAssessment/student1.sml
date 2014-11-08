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

val only_capitals = List.filter (fn x => Char.isUpper(String.sub(x, 0)))

val longest_string1 = List.foldl (fn(x,y) => if String.size x > String.size y then x else y) ""

val longest_string2 = List.foldl (fn(x,y) => if String.size x >= String.size y then x else y) ""

fun longest_string_helper f = List.foldl (fn(x,y) => if f(String.size x, String.size y) then x else y) ""
val longest_string3 = longest_string_helper (fn(x,y) => x > y)
val longest_string4 = longest_string_helper (fn(x,y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs =
    case xs of
      [] => raise NoAnswer
      | x::xs' => (case f x of
                    NONE => first_answer f xs'
                    | SOME v => v)
                    
fun all_answers f xs = 
  let
    fun all_answers_helper f xs acc = 
      case xs of
        [] => SOME acc
        | x::xs' => (case f x of
                      NONE => NONE
                      | SOME lst => all_answers_helper f xs' (acc @ lst))
  in
    all_answers_helper f xs []
  end

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) (fn x => String.size x)

fun count_some_var (str, pattern) = g (fn _ => 0) (fn x => if x = str then 1 else 0) pattern

val check_pat = 
  let
    fun get_var_names pattern = 
      case pattern of
        Variable s           => [s]
        | TupleP ps          => List.foldl (fn(p,lst) => (get_var_names p) @ lst) [] ps
        | ConstructorP(_,p)  => get_var_names p
        | _                  => []
        
    fun all_distinct xs =
      case xs of
        [] => true
        | x::xs' => if List.exists (fn y => x = y) xs' 
                    then false
                    else all_distinct xs'
  in
    all_distinct o get_var_names
  end
  
  fun match pair =
  let
    val EmptyBinding = SOME []
  in
    case pair of
        (_, Wildcard)                      => EmptyBinding
      | (v, Variable s)                    => SOME [(s, v)]
      | (Unit, UnitP)                      => EmptyBinding
      | (Const v, ConstP p)                => if v = p then EmptyBinding else NONE
      | (Tuple vs, TupleP ps)              => 
                                          ((all_answers  (fn x => match x) (ListPair.zipEq(vs,ps)))
                                            handle UnequalLengths => NONE)
      |  (Constructor(s1, v), ConstructorP(s2, p))  => 
                                          if (s1 = s2) then match (v, p) else NONE
      | _ => NONE
  end
  
  fun first_match v ps =
    (SOME (first_answer (fn p => match(v,p)) ps))
    handle NoAnswer => NONE
    
