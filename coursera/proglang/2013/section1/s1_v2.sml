val monthsStrings = [ "January", "February", "March", "April", "May", "June",
"July", "August", "September", "October", "November", "December" ]
val monthsDays = [ 31,28,31,30,31,30,31,31,30,31,30,31 ]
(* task 1 *)
(* FIXME *)

fun date_to_int(d: int * int * int) =
        ( #1 d ) * 10000 + ( #2 d ) * 100 + #3 d

fun int_to_date(i: int) =
        let
            val year = i div 10000
            val month = (i - ( year * 10000 )) div 100
            val day = (i - ( year *10000) - ( month *100 )) div 1 
        in
            (year, month, day)
        end

fun date_list_to_int_list (dl: (int * int * int) list) =
        if null dl
        then []
        else
            date_to_int(hd dl)::date_list_to_int_list(tl dl)

fun is_older ( date1 : int*int*int, date2 : int*int*int) =
        date_to_int(date1) < date_to_int(date2)


(* task 2 *)
fun number_in_month ( dates: (int * int * int) list, month: int ) =
        if null dates
        then 0
        else
                if #2 (hd dates) = month
                then 1 + number_in_month( tl dates, month)
                else number_in_month( tl dates, month)

(* task 3 *)
fun number_in_months ( dates: (int * int * int) list, months: int list ) =
        if null months
        then 0
        else number_in_month(dates, hd months) + number_in_months( dates, tl
        months)

(* task 4 *)
fun dates_in_month ( dates: (int * int * int) list, month: int ) =
        if null dates
        then []
        else
                if #2 (hd dates) = month
                then (hd dates)::dates_in_month(tl dates, month)
                else dates_in_month(tl dates, month)

(* task 5 *)
fun dates_in_months ( dates: (int * int * int) list, months: int list) =
        if null months
        then []
        else dates_in_month(dates, hd months) @ dates_in_months(dates, tl
        months)

(* task 6 *)
fun get_nth (strings: string list, n: int ) =
        if n = 1
        then hd strings
        else get_nth(tl strings, n-1)

(* task 7 *)
fun date_to_string (d: int * int * int) =
       get_nth(monthsStrings,#2 d)^" "^Int.toString(#3 d)^", "^Int.toString(#1 d)

(* task 8 *)
fun number_before_reaching_sum ( sum: int, intList: int list) =
        if sum <=  hd intList
        then 0
        else 1 + number_before_reaching_sum ( sum - (hd intList), tl intList)

(* task 9 *)
fun what_month (day: int) =
        number_before_reaching_sum(day,monthsDays)+1

(* task 10 *)
fun month_range(day1: int, day2: int) =
        let
                val monthDay1 = what_month(day1)
        in
                if day1 > day2
                then []
                else
                    if day1 = day2
                    then
                            [monthDay1]
                    else
                            monthDay1::month_range(day1 + 1, day2)
        end

(* task 11 *)
(* return smallest int in a list *)
fun smallest_int ( il: int list ) =
        if null il
        then []
        else
            let
                val tl_il = tl il
                val hd_il = hd il
            in
                if  null tl_il
                then [hd_il]
                else
                    if hd_il > hd (tl_il)
                    then
                       smallest_int( tl_il )
                    else
                       smallest_int( hd_il::tl (tl_il))
            end

fun oldest ( date_list: (int * int * int) list ) =
        let
            val si = smallest_int(date_list_to_int_list(date_list))
        in
            if null si
            then NONE
            else SOME (int_to_date(hd si))
        end

