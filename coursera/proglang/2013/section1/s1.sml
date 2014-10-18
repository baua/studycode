val monthsStrings = [ "January", "February", "March", "April", "May", "June",
"July", "August", "September", "October", "November", "December" ]
val monthsDays = [ 31,28,31,30,31,30,31,31,30,31,30,31 ]
(* task 1 *)
(* FIXME *)
fun is_older ( date1 : int*int*int, date2 : int*int*int) =
        if #1 date1 < #1 date2
        then true
        else         
          if #1 date1 > #1 date2
          then false
          else true


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
        if sum < hd intList
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
                if day1 >= day2
                then
                        [monthDay1]
                else  
                        monthDay1::month_range(day1 + 1, day2)
        end

(* task 11 *)
