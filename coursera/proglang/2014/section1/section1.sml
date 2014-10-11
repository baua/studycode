
(* task 1 *)
fun is_older (x: int * int * int, y: int * int * int) =
  let
    fun date_to_int( x: int * int * int) =
        #1 x * 10000 + #2 x * 100 + #3 x 
  in
    if date_to_int(x) < date_to_int(y) then 
      true
    else 
      false
  end

(* task 2 *)
fun number_in_month(mlist: ( int * int * int) list, month: int) =
  if null mlist then 
    0
  else 
    if #2 (hd mlist) = month then 
      1 + number_in_month(tl mlist, month)
    else 
      number_in_month(tl mlist,month)

(* task 3 *)
fun number_in_months(mlist: ( int * int * int) list, months: int list ) =
  if null months
    then 0
    else number_in_month(mlist,hd months) + number_in_months(mlist, tl months)


(* task 4 *)
fun dates_in_month(mlist: ( int * int * int) list, month: int) =
  if null mlist
    then []
    else
      if #2 (hd mlist) = month
        then
          hd mlist::(dates_in_month(tl mlist,month))
        else
          dates_in_month(tl mlist, month)

(* task 5 *)
fun dates_in_months(mlist: ( int * int * int) list, months: int list ) =
 if null months
   then []
   else
     dates_in_month(mlist, hd months)@dates_in_months(mlist, tl months)

(* task 6 *)
fun get_nth(strings: string list, pos: int) =
  if pos = 1
  then
    hd strings
  else
    get_nth(tl strings,pos - 1)

(* task 7 *)
fun date_to_string( d: int*int*int ) =
  let
    val month_strings = [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" ]
  in
    get_nth(month_strings, #2 d)^" "^Int.toString(#3 d)^", "^Int.toString(#1 d)
  end

(* task 8 *)
fun number_before_reaching_sum(sum: int,intlist: int list) =
  if null intlist then
    1
  else
    let
      val rest = sum - hd intlist
    in
        if rest > 0 then
          1 + number_before_reaching_sum(rest,tl intlist)
        else
          0
    end

(* task 9 *)
fun what_month(day: int) =
  let
    val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
    number_before_reaching_sum(day,days_in_month) + 1
  end

(* task 10 *)
fun month_range(day1: int, day2:int) =
  if day1 >= day2 then
    [what_month(day2)]
  else
      what_month(day1)::month_range(day1+1,day2)

(* task 11 *)
fun oldest ( dates: (int*int*int) list ) =
  if null dates then
    NONE
  else
    if null (tl dates) then
      SOME (hd dates)
    else
      if is_older(hd dates,hd (tl dates)) then
        oldest((hd dates)::(tl (tl dates)))
      else
        oldest(tl dates)
