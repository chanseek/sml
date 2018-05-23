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
fun only_capitals(xs)=
  List.filter (fn (x) => Char.isUpper(String.sub(x, 0))) xs

fun longest_string1(xs) =
  foldl (fn (x, y) => if String.size(x) > String.size(y) then x else y) "" xs

fun longest_string2(xs) =
  foldl (fn (x, y) => if String.size(x) >= String.size(y) then x else y) "" xs

fun longest_string_helper f xs =
  foldl (fn (x, y) => if f(String.size(x), String.size(y)) then x else y) "" xs

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o rev o explode

fun first_answer f xs =
  case xs of
       [] => raise NoAnswer
     | x::xs' => case f x of
                      SOME x' => x'
                    | _ => first_answer f xs'

fun all_answers f xs =
let 
  fun helper xs acc =
    case xs of
         [] => SOME acc
       | x::xs' => case x of
                        NONE => NONE
                      | SOME x' => helper xs' (acc@x')
in
  helper (List.map (fn x => f x) xs) []
end

val count_wildcards = fn x => g (fn () => 1) (fn x => 0) x 

val count_wild_and_variable_lengths = fn x => g (fn () => 1) (fn x => String.size x) x 

fun count_some_var(v, p) = g (fn () => 0) (fn x => if v = x then 1 else 0) p


fun check_pat p =
let 
  fun find_str(p,acc) =
    case p of
         Wildcard          => acc
       | Variable x        => acc@[x]
       | TupleP ps         => List.foldl (fn (p, i) => find_str(p, i)) acc ps
       | ConstructorP(_,p) => find_str(p, acc)
  fun check_not_dup xs =
    case xs of
         [] => true
       | x::xs' =>
           if (List.exists (fn x' => x = x') xs') then false
           else check_not_dup(xs')
in
  check_not_dup(find_str(p, []))
end
