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

(* 1 *)
fun only_capitals(xs)=
  List.filter (fn (x) => Char.isUpper(String.sub(x, 0))) xs

(* 2 *)
fun longest_string1(xs) =
  foldl (fn (x, y) => if String.size(x) > String.size(y) then x else y) "" xs

(* 3 *)
fun longest_string2(xs) =
  foldl (fn (x, y) => if String.size(x) >= String.size(y) then x else y) "" xs

(* 4 *)
fun longest_string_helper f xs =
  foldl (fn (x, y) => if f(String.size(x), String.size(y)) then x else y) "" xs

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
val rev_string = implode o rev o explode

(* 7 *)
fun first_answer f xs =
  case xs of
       [] => raise NoAnswer
     | x::xs' => case f x of
                      SOME x' => x'
                    | _ => first_answer f xs'

(* 8 *)
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

(* 9-a *)
val count_wildcards = fn x => g (fn () => 1) (fn x => 0) x 

(* 9-b *)
val count_wild_and_variable_lengths = fn x => g (fn () => 1) (fn x => String.size x) x 

(* 9-c *)
fun count_some_var(v, p) = g (fn () => 0) (fn x => if v = x then 1 else 0) p

(* 10 *)
fun check_pat p =
let 
  fun find_str(p,acc) =
    case p of
         Variable x        => acc@[x]
       | TupleP ps         => List.foldl (fn (p, i) => find_str(p, i)) acc ps
       | ConstructorP(_,p) => find_str(p, acc)
       | _ => acc

  fun check_not_dup xs =
    case xs of
         [] => true
       | x::xs' =>
           if (List.exists (fn x' => x = x') xs') then false
           else check_not_dup(xs')
in
  check_not_dup(find_str(p, []))
end

(* 11 *)
fun match(v, p) =
    case (v, p) of
         (_, Wildcard) => SOME []
       | (x, Variable y) => SOME [(y, x)]
       | (Unit, UnitP) => SOME []
       | (Const(x), ConstP y) =>
           if x = y then SOME [] else NONE
       | (Tuple x, TupleP y) =>
           if length(x) = length(y) then
             all_answers (fn (x, y) => match(x, y)) (ListPair.zip(x, y))
           else NONE
       | (Constructor(s, v), ConstructorP(s', p)) =>
           if s = s then match(v, p) else NONE
       | (_, _) => NONE

(* 12 *)
fun first_match v ps =
  SOME(first_answer (fn x => x) (List.map (fn p => match(v, p)) ps))
  handle NoAnswer => NONE
