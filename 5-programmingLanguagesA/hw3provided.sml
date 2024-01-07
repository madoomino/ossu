(* Dan Grossman, CSE341 Spring 2013, HW3 Provided Code *)

exception NoAnswer

(**** you can put all your code here ****)

fun only_capitals str_lst =
    List.filter (fn str => Char.isUpper (String.sub(str,0))) str_lst

fun longest_string1 str_lst = 
    foldl (fn (str,acc) => if  String.size str > String.size acc then str else acc) "" str_lst

fun longest_string2 str_lst = 
    foldl (fn (str,acc) => if  String.size acc > String.size str then acc else str) "" str_lst

fun longest_string_helper  f str_lst =
 foldl f "" str_lst

val longest_string3 =
 longest_string_helper (fn (str,acc) => if  String.size str > String.size acc then str else acc)

val longest_string4 =
 longest_string_helper (fn (str,acc) => if  String.size acc > String.size str then acc else str)

fun longest_capitalized str_lst =
    longest_string3 (only_capitals str_lst)

fun rev_string s =
    String.implode (List.rev (String.explode s))
fun first_answer f lst =
    let val filterd = List.filter f lst
    in case filterd of 
		[] => raise NoAnswer 
		| x::_ => x end

fun all_answers f lst =
    case lst of
    [] => true
    | x::xs => (f x) andalso (all_answers f xs)
    (* OTHER SOLUTION -> all f lst *)