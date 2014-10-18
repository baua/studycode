fun get_year(date : int * int * int) =
    #1 date
fun get_month(date : int * int * int) =
    #2 date
fun get_day(date : int * int * int) =
    #3 date

fun is_older(date1 : int * int * int, date2 : int * int * int) =
    let
	val year1 = get_year date1
	val year2 = get_year date2
	val month1 = get_month date1
	val month2 = get_month date2
	val day1 = get_day date1
	val day2 = get_day date2
    in
	if year1 < year2
	then true
	else if year1 > year2
	then false
	else if month1 < month2
	then true
	else if month1 > month2
	then false
	else if day1 < day2
	then true
	else false
    end

fun number_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else if get_month(hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

fun number_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else
	let val current_date = hd dates
	in
	    if get_month(current_date) = month
	    then current_date :: dates_in_month(tl dates, month)
	    else dates_in_month(tl dates, month)
	end

fun dates_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth(tl strings, n - 1)

fun date_to_string(date : int * int * int) =
    let
	val month_names = ["January", "February", "March",
			   "April", "May", "June",
			   "July", "August", "September",
			   "October", "November", "December"];
	val year = Int.toString(get_year date)
	val month = get_nth(month_names, get_month date)
	val day = Int.toString(get_day date)
    in
	month ^ " " ^ day ^ ", " ^ year
    end

fun number_before_reaching_sum(sum : int, nums : int list) =
    let val current_sum = sum - hd nums
    in
	if current_sum <= 0
	then 0
	else 1 + number_before_reaching_sum(current_sum, tl nums)
    end

fun what_month(day_of_year : int) =
    let
	val days_of_each_month = [31, 28, 31, 30,
				  31, 30, 31, 31,
				  30, 31, 30, 31]
    in
	number_before_reaching_sum(day_of_year, days_of_each_month) + 1
    end

fun month_range(day_of_year1 : int, day_of_year2 : int) =
    if day_of_year1 > day_of_year2
    then []
    else what_month(day_of_year1) :: month_range(day_of_year1 + 1, day_of_year2)

fun oldest(dates : (int * int * int) list) =
    if null dates
    then NONE
    else let
	fun find_oldest(dates : (int * int * int) list, current_oldest : int * int * int) =
	    if null dates
	    then current_oldest
	    else if is_older(current_oldest, hd dates)
	    then find_oldest(tl dates, current_oldest)
	    else find_oldest(tl dates, hd dates)
    in
	SOME(find_oldest(tl dates, hd dates))
    end
