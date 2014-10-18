(*helper functions*)
fun get_nth_int(l : int list, n : int) =
	if n = 1 then hd l
	else get_nth_int(tl l, n - 1)

fun is_leap_year(year : int) =
	((year mod 400) = 0) orelse (((year mod 4) = 0) andalso (not((year mod 100) = 0)))

	
fun month_duration(year : int) =
	if is_leap_year(year) then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

fun remove_duplicates(months : int list) =
	let 
		fun in_list(l : int list, value : int) =
			if null months then false
			else if hd months = value orelse in_list(tl l, value) then true
			else false
	in 
		if null months then []
		else if in_list(tl months, hd months) then remove_duplicates(tl months)
		else (hd months)::remove_duplicates(tl months)
	end

fun append(l1 : (int * int * int) list, l2 : (int * int * int) list) =
	if null l1 then l2
	else if null l2 then l1
	else (hd l1)::append(tl l1, l2)
	
(*problems*)
fun is_older(d1 : int * int * int , d2 : int * int * int) =
	#1 d1 < #1 d2 orelse (#1 d1 = #1 d2
		andalso (#2 d1 < #2 d2 orelse (#2 d1 = #2 d2 andalso #3 d1 < #3 d2)))
		
fun number_in_month(dates : (int * int * int) list, month : int) =
	if null dates then 0
	else if (#2 (hd dates)) = month then 1 + number_in_month(tl dates, month)
	else number_in_month(tl dates, month)
	
fun number_in_months(dates : (int * int * int) list, months : int list) =
	if null dates orelse null months then 0
	else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int * int * int) list, month : int) =
	if null dates then []
	else if (#2 (hd dates)) = month then (hd dates)::dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)
	
fun dates_in_months(dates : (int * int * int) list, months : int list) =
	if null dates orelse null months then []
	else append(dates_in_month(dates, (hd months)), dates_in_months(dates, (tl months)))

fun get_nth(l : string list, n : int) =
	if n = 1 then hd l
	else get_nth(tl l, n - 1)

fun date_to_string(date : int * int * int) =
		let val months = ["January", "February", "March", "April",
			"May", "June", "July", "August", "September", "October", "November", "December"]
		in get_nth(months, #2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
		end

fun number_before_reaching_sum(sum : int, l : int list) = 
	if hd l >= sum then 0
	else 1 + number_before_reaching_sum(sum - hd l, tl l)
	
fun what_month(day : int) = 
	let val month_duration = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in number_before_reaching_sum(day, month_duration) + 1
	end
		
fun month_range(day1 : int, day2 : int) =
	if day1 > day2 then []
	else what_month(day1)::month_range(day1 + 1, day2)
		
fun oldest(dates : (int * int * int) list) =
	if null dates then NONE
	else if isSome(oldest(tl dates)) andalso is_older(valOf(oldest(tl dates)), (hd dates))
	then oldest(tl dates)
	else SOME(hd dates)
	
fun number_in_months_challenge(dates : (int * int * int) list, months : int list) =
	number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge(dates : (int * int * int) list, months : int list) = 
	dates_in_months(dates, remove_duplicates(months))

fun reasonable_date(date : int * int * int) =
	(#1 date > 0) andalso (#2 date > 0) andalso (#2 date < 13) andalso (#3 date > 0) 
		andalso get_nth_int(month_duration(#1(date)), #2(date)) >= (#3 date)
					