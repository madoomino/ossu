(* Dan Grossman, CSE341 Spring 2013, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str, str_lst) =
    case str_lst of
        [] => NONE
        | x::[] => if same_string(str, x) then SOME [] else NONE
        | x::rst =>
            let 
                val rest = all_except_option(str,rst)
            in
                if same_string(str, x) then SOME rst
                else case rest of
                NONE => NONE
                | SOME rst => SOME (x::rst)
            end

fun get_substitutions1 (lol, s) =
    case lol of
    [] => []
    | l::ls =>
        let 
            val subs = get_substitutions1(ls,s)
        in
            case all_except_option(s,l) of
            NONE => subs
            | SOME l => l @ subs
        end

fun get_substitutions2 (lol, s) =
    let fun helper(lol, rsf) =
            case lol of
            [] => rsf
            | l::ls =>
                case all_except_option(s,l) of
                    NONE => helper(ls, rsf)
                    | SOME l => helper(ls, l @ rsf)
    in
        helper(lol, [])
    end

fun similar_names(subs, {first,middle,last}) = 
    let
        val processed_subs = get_substitutions2(subs, first)
        fun helper (processed_subs, rsf) =
            case processed_subs of
            [] => rsf
            | sub::subs =>
                let
                    val created_name = {first=sub,middle=middle, last=last}
                in
                    helper(subs, created_name :: rsf)
                end
            
    in
        helper(processed_subs, [])
    end



(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color card =
    case card of
    (Spades, _) => Black
    | (Clubs, _) => Black
    | _ => Red

fun card_value card =
    case card of
    (_, Num(num)) => num
    | (_, Ace) => 11
    | _ => 10


fun remove_card (cards, card, exn) =
    let 
        fun helper (cards, rsf) = 
            case cards of
            [] => raise exn
            | c::cs => if c = card 
                    then rsf @ cs
                    else helper(cs, c::rsf)
    in
        helper (cards, [])
    end


fun all_same_color cards =
    case cards of
    [] => true
  | card :: [] => true
  | x :: y :: card_s =>
        if card_color(x) = card_color(y)
        then all_same_color(y::card_s)
        else false


fun sum_cards cards = 
    let 
        fun helper (cards, rsf) =
            case cards of
            [] => rsf
            | x :: card_s => helper ( card_s, card_value(x) + rsf) 
    in
        helper(cards, 0)
    end


fun score (cards, goal) =
    let
        val cards_sum = sum_cards cards
        val is_all_cards_same_color = all_same_color cards
        val preliminary_score =
            if cards_sum > goal 
            then 3 * (cards_sum - goal)
            else goal - cards_sum
    in
        case cards_sum of
        0 => 0
        | _ => 
        if is_all_cards_same_color
        then preliminary_score div 2
        else preliminary_score
    end

fun officiate (card_list, move_list, goal) =
    let
        fun helper (card_list, move_list, held_cards) =
            case move_list of
            [] => score (held_cards, goal)
            | Discard(card) :: moves =>
            helper (card_list, moves, remove_card (held_cards, card, IllegalMove))
            | Draw :: moves =>
            case card_list of
            [] => score (held_cards, goal)
            | card::cards =>
            let 
                val total_score = card_value card + sum_cards held_cards
            in
                if total_score > goal
                then total_score
                else helper (cards, moves, card :: held_cards)
            end
    in
        helper (card_list, move_list, [])
    end

