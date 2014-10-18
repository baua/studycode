fun is_older (a: int * int * int, b: int * int * int) =
    if (#1 a) <> (#1 b)
    then (#1 a) < (#1 b)
    else if (#2 a) <> (#2 b)
    then (#2 a) < (#2 b)
    else (#3 a) < (#3 b)

fun number_in_month (a: (int * int * int) list, m: int) =
    if null a
    then 0
    else if (#2 (hd a)) = m
    then 1 + number_in_month(tl a, m)
    else number_in_month(tl a, m)

fun number_in_months (a: (int * int * int) list, m: int list) =
    if null m
    then 0
    else number_in_month(a, hd m) + number_in_months(a, tl m)

fun dates_in_month (a: (int * int * int) list, m: int) =
    if null a
    then []
    else if (#2 (hd a)) = m
    then (hd a) :: dates_in_month(tl a, m)
    else dates_in_month(tl a, m)

fun dates_in_months (a: (int * int * int) list, m: int list) =
    if null m
    then []
    else dates_in_month(a, hd m) @ dates_in_months(a, tl m)

fun get_nth (a: string list, n: int) =
    if n = 1
    then hd a
    else get_nth(tl a, n - 1)

fun date_to_string (a: int * int * int) =
    let
	val month = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(month, #2 a) ^ " " ^ Int.toString(#3 a) ^ ", " ^ Int.toString(#1 a)
    end

fun number_before_reaching_sum (s: int, a: int list) =
    if (hd a) >= s
    then 0
    else 1 + number_before_reaching_sum(s - (hd a), tl a)

fun what_month (s: int) =
    let
	val a = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	1 + number_before_reaching_sum(s, a)
    end

fun month_range (a: int, b: int) =
    if a > b
    then []
    else (what_month a) :: month_range(a + 1, b)

fun oldest (a: (int * int * int) list) =
    if null a
    then NONE
    else let
	fun oldest_noempty (a: (int * int * int) list) =
	    if null (tl a)
	    then hd a
	    else let
		val old = oldest_noempty(tl a)
	    in
		if is_older(old, hd a)
		then old
		else hd a
	    end
    in
	SOME (oldest_noempty a)
    end

fun unique (a: int list) =
    let
	fun remove_duplicate (a: int list, b: int list) =
	    let
		fun find (a: int list, b: int) =
		    if null a
		    then false
		    else if (hd a) = b
		    then true
		    else find(tl a, b)
	    in
		if (null b)
		then []
		else if find(a, hd b)
		then remove_duplicate(a, tl b)
		else (hd b) :: remove_duplicate((hd b) :: a, tl b)
	    end
    in
	remove_duplicate([], a)
    end

fun number_in_months_challenge (a: (int * int * int) list, m: int list) =
    number_in_months(a, unique m)

fun dates_in_months_challenge (a: (int * int * int) list, m: int list) =
    dates_in_months(a, unique m)

fun reasonable_date (a: int * int * int) =
    let
	fun leap (y: int) =
	    (y mod 400 = 0) orelse ((y mod 4 = 0) andalso (y mod 100 <> 0))
	fun days (y: int, m: int) =
	    let
		fun get_kth (d: int list, k: int) =
		    if k = 1
		    then hd d
		    else get_kth(tl d, k - 1)
	    in
		if m <> 2 orelse not (leap y)
		then get_kth([31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], m)
		else 29
	    end
	fun range (a: int, l: int, r: int) =
	    (a >= l) andalso (a <= r)
    in
	(#1 a > 0) andalso range(#2 a, 1, 12) andalso range(#3 a, 1, days(#1 a, #2 a))
    end
