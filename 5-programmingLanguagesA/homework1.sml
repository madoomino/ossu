fun is_older(date1: (int * int * int), date2: (int * int * int)) =
    (*  is_older takes 2 dates (triples), produces true if the 1st date is older *)
    let
        val (year1, month1, day1) = date1
        val (year2, month2, day2) = date2
    in
        year1 < year2 orelse
        (year1 = year2 andalso month1 < month2) orelse
        (year1 = year2 andalso month1 = month2 andalso day1 < day2)
    end


fun number_in_month(dates_list: (int * int * int) list, month: int) =
    (* number_in_month -> (int * int * int) list, int : int 
       produces the number of months in a given dates_list that have the same given month *)
    let 
        fun calc_month(dates_list: (int * int * int) list, final_result) =
        (* calc_month -> dates_list final_result : Int
           produces the final result of how many dates contains the given month *)

            if null dates_list
            then final_result
            else 
                if (#2 (hd dates_list)) = month
                then calc_month(tl dates_list, final_result + 1)
                else calc_month(tl dates_list, final_result)
    in
        calc_month(dates_list, 0)
    end

fun number_in_months(dates_list: (int * int * int) list, months_list: int list) =
    (* number_in_month -> (int * int * int) list, int list : int 
       produces the number of months in a given dates_list 
       that have the same month number of any of the monthes in a given months_list *)
    let 
        fun has_month(date: (int * int * int), months_list: int list) =
        (*  has_month -> date months_list : Boolean
            produces true if the given date's month is in the given months_list *)

            if null months_list
            then false
            else
                if (#2 date) = (hd months_list)
                then true
                else has_month(date, tl months_list)


        fun calc_month(dates_list: (int * int * int) list, final_result: int) =
        (* calc_month -> dates_list final_result : Int
           produces the final result of how many months in the dates_list 
           exist in the months_list *)
        
            if null dates_list
            then final_result
            else 
                if has_month(hd dates_list, months_list)
                then calc_month(tl dates_list, final_result + 1)
                else calc_month(tl dates_list, final_result)

    in
        calc_month(dates_list, 0)
    end
    
fun dates_in_month(dates_list: (int * int * int) list, month: int) =
    (* dates_in_month -> (int * int * int) list, int : (int * int * int) list 
       produces a list contains all the dates that have the same month number *)
    let 
        fun final_dates(dates_list: (int * int * int) list, final_result: (int * int * int) list) =
        (* calc_month -> dates_list final_result : Int
           produces a list contains all dates that contain the given month *)

            if null dates_list
            then final_result
            else 
                if (#2 (hd dates_list)) = month
                then final_dates(tl dates_list, final_result @ [(hd dates_list)])
                else final_dates(tl dates_list, final_result)
    in
        final_dates(dates_list, [])
    end

fun dates_in_months(dates_list: (int * int * int) list, months_list: int list) =
    (* dates_in_month -> (int * int * int) list, int list : (int * int * int) list 
       produces a list contains all the dates that have any of the months in months_list *)
    let 
        fun has_month(date: (int * int * int), months_list: int list) =
        (*  has_month -> date months_list : Boolean
            produces true if the given date's month is in the given months_list *)

            if null months_list
            then false
            else
                if (#2 date) = (hd months_list)
                then true
                else has_month(date, tl months_list)


        fun calc_months(dates_list: (int * int * int) list, final_result: (int * int * int) list) =
        (* calc_months ->  (int * int * int) list, int list : (int * int * int) list
           produces a list contains all the dates that has any of the months_list months *)
        
            if null dates_list
            then final_result
            else 
                if has_month(hd dates_list, months_list)
                then calc_months(tl dates_list, final_result @ [hd dates_list])
                else calc_months(tl dates_list, final_result)

    in
        calc_months(dates_list, [])
    end

fun get_nth(str_list: string list, n: int) = 
    (* get_nth -> string list, int : string 
       produces the nth element of the string_list *)
    let
        fun find_nth(str_lst: string list, pos: int) =
        (* find_nth -> string list, int : string 
           produces the nth element of the string_list *)
        (* pos -> int starts from 0; keeps track of the current element position *)
            if null str_lst then ""
            else 
                if pos = n then hd str_lst
                else find_nth(tl str_lst, pos + 1)
    in 
        find_nth(str_list, 0)
    end

fun date_to_string(date: (int * int * int)) = 
    (* date_to_string ->  (int * int * int) : string 
       produces a strign of the given date in the form of "Month Day, Year" *)

    let
        val MONTHS = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

        fun string_date(date: (int * int * int)) =
            (* date_to_string ->  (int * int * int) : string 
               produces a strign of the given date in the form of "Month Day, Year" *)
            (* find the month by using get_nth helper *)
            get_nth(MONTHS, #2 date - 1) ^ " " ^ Int.toString(#1 date)  ^ ", " ^ Int.toString(#3 date)

    in
        if (#2 date > 12 orelse #2 date < 1) orelse (#1 date > 31 orelse #1 date < 1)
        then "Incorrect date"
        else string_date(date)
    end

fun number_before_reaching_sum(sum: int, numbers: int list) = 
    (* number_before_reaching_sum -> int, int list : int 
       produces an int that represents the number of sumed numbers before reaching sum, (n+1 >= sum) *)       

    let
        fun find_num(lst: int list, rsf: int, n: int) =
            (* find_num -> int list, int, int : int  
               produces an int that represents the number of sumed numbers before reaching sum, (n+1 >= sum) *)
            (* rsf (result so far); keeps track of the sum of all elements before the current element *)
            if hd lst + rsf >= sum then n+1
            else find_num(tl lst, rsf + hd lst, n + 1)
    in
        find_num(numbers, 0, 0)
    end

fun what_month(day: int) =
    (* what_month -> int : int *)
    (* by using number_before_reaching_sum *)
    (* WITHOUT RECURSION -> : (day * 12) div 365 : *)
    let 
        val MONTHS_NUMBERS = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
        number_before_reaching_sum(day, MONTHS_NUMBERS)
    end

fun month_range(day1: int, day2: int) = 
    (* month_range -> int, int : int list 
       produces a list of the months starts from the month of day1
       and ends at the month of day2*)
    if day1 > day2
    then []
    else
        let 
            val month1 = what_month(day1)
            val month2 = what_month(day2)
            fun build_months_list(rsf: int list, acc: int) =
                (* build_months_list -> int list, int : int list 
                   produces a list with all the months starts with month1, and ends with month2*)
                if acc = month2
                then rsf
                else build_months_list(rsf @ [acc], acc + 1)
        in
            build_months_list([], month1) @ [month2]
        end
    
(* As I understood the "dates_in_months" problem, my implementation is the same for the challenging problem, so I wanted to make it a bit harder and implement it to count how many times the month mentioned (so having 2 dates with the same month will count only once. ) *)
fun number_in_months_challenge(dates_list: (int * int * int) list, months_list: int list) =
    (* number_in_month -> (int * int * int) list, int list : int 
       produces the number of months in a given dates_list 
       that have the same month number of any of the monthes in a given months_list *)
    let 
        fun calc_month(dates_list: (int * int * int) list, final_result: int, duplicated_months: int list) =
            (* calc_month -> dates_list final_result : Int
               produces the final result of how many months in the dates_list 
               exist in the months_list *)
           let
                fun is_duplicated(element: int, lst: int list) =
                    (* is_duplicated -> int, int list : boolean
                       produces true if the given list has the given element *)

                    if null lst then false
                    else 
                        if hd lst = element then true
                        else is_duplicated(element, tl lst)

                fun has_month(date: (int * int * int), months_list: int list) =
                    (*  has_month -> (int * int * int), int list : boolean
                        produces true if the given date's month is in the given months_list *)

                        if null months_list
                        then false
                        else
                            if (#2 date) = (hd months_list)
                            then true
                            else has_month(date, tl months_list)

            in
                if null dates_list then final_result
                else
                    if is_duplicated(#2 (hd dates_list), duplicated_months) 
                    then calc_month(tl dates_list, final_result, duplicated_months)
                    else 
                        if has_month(hd dates_list, months_list)
                        then calc_month(tl dates_list, final_result + 1, #2 (hd dates_list)::duplicated_months)
                        else calc_month(tl dates_list, final_result, duplicated_months)
            end
    in
        calc_month(dates_list, 0, [])
    end

fun dates_in_months_challenge(dates_list: (int * int * int) list, months_list: int list) =
    (* dates_in_month -> (int * int * int) list, int list : (int * int * int) list 
       produces a list contains all the dates that have any of the months in months_list *)
    let 
        fun has_month(date: (int * int * int), months_list: int list) =
        (*  has_month -> date months_list : Boolean
            produces true if the given date's month is in the given months_list *)

            if null months_list
            then false
            else
                if (#2 date) = (hd months_list)
                then true
                else has_month(date, tl months_list)


        fun calc_months(dates_list: (int * int * int) list, final_result: (int * int * int) list) =
        (* calc_months ->  (int * int * int) list, int list : (int * int * int) list
           produces a list contains all the dates that has any of the months_list months *)
        
            if null dates_list
            then final_result
            else 
                if has_month(hd dates_list, months_list)
                then calc_months(tl dates_list, final_result @ [hd dates_list])
                else calc_months(tl dates_list, final_result)

    in
        calc_months(dates_list, [])
    end

(* VALUSE OF CREATED FUNCTIONS *)


val isoldr = is_older((16, 7, 2002), (17,7,2002)) (* true *)
val nim = number_in_month([(16,7,2002), (24,9,1992)], 7) (* 1 *) (* the same for the challenging problem *) 
val nims = number_in_months([(28,6,2022),(17,5,2005),(5,9,1773)], [1,2,4,8,6,9]) (* 2 *)
val dsim = dates_in_month([(28,6,2022),(17,5,2005),(5,6,1773)], 6) (* [(28,6,2022), (5,6,1773)] *)
val dsims = dates_in_months([(28,6,2022),(17,5,2005),(5,6,1773)], [1,2,6]) (* [(28,6,2022), (5,6,1773)] *)
val gnt = get_nth(["mado","mino","kiko"], 2) (* "kiko" *)
val dts = date_to_string((16, 12, 1996)) (* "Dec 16, 1996" *)
val nbrs = number_before_reaching_sum(6, [1,2,3,4,5,6]) (* 3 *)
val wm = what_month(248) (* 9 *)
val mr = month_range(30, 152) (* [1,2,3,4,5,6] *)
val nims_challenge = number_in_months_challenge([(28,2,2022),(17,1,2005),(5,6,1773), (25,1,2011)], [1,2,6]) (* 3 *)
(* the "dates_in_months_challenge" is the same idea as "number_in_months_challenge" *)