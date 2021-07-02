(* Homework 1 *)

(* 1 *)
fun is_older (date_one: int * int * int, date_two: int * int * int) =
	let
		val year1 = #1 date_one
		val year2 = #1 date_two
		val month1 = #2 date_one
		val month2 = #2 date_two
		val day1 = #3 date_one
		val day2 = #3 date_two
	in
		(year1 < year2)
		orelse (year1 = year2 andalso month1 < month2)
		orelse (year1 = year2 andalso month1 = month2 andalso day1 < day2)
	end

(* 2 *)
fun number_in_month (dates: (int * int * int) list, month: int) =
	if null dates
	then 0
	else
		if #2 (hd dates) = month
		then 1 + number_in_month (tl dates, month)
		else number_in_month (tl dates, month)

(* 3 *)
fun number_in_months (dates: (int * int * int) list, months: int list) =
	if null dates orelse null months
	then 0
	else number_in_month (dates, hd months) + number_in_months (dates, tl
		months)

(* 4 *)
fun dates_in_month (dates: (int * int * int) list, month: int) =
	if null dates
	then []
	else
		if #2 (hd dates) = month
		then (hd dates) :: dates_in_month (tl dates, month)
		else dates_in_month (tl dates, month)

(* 5 *)
fun dates_in_months (dates: (int * int * int) list, months: int list) =
	if null dates orelse null months
	then []
	else dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)

(* 6 *)
fun get_nth (strings_list: string list, n: int) =
	if n = 1
	then hd strings_list
	else get_nth (tl strings_list, n - 1)

(* 7 *)
fun date_to_string (date: int * int * int) =
	let 
		val months = ["January", "February", "March", "April" , "May" , 
		"June" , "July", "August", "September", "October", "November", 
		"December"]
	in 
		get_nth (months, (#2 date)) ^ " " ^ Int.toString (#3 date) ^ ", " ^ 
		Int.toString(#1 date)
	end

(* 8 *)
fun number_before_reaching_sum (sum: int, number_list: int list) =
	if sum <= hd (number_list)
	then 0
	else 1 + number_before_reaching_sum (sum - hd number_list, tl number_list)

(* 9 *)
fun what_month (day: int) =
	let 
		val days_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in 
		number_before_reaching_sum (day, days_months) + 1
	end

(* 10 *)
fun month_range (day1: int, day2: int) =
	if day1 > day2
	then []
	else what_month (day1) :: month_range (day1 + 1, day2)

(* 11 *)
fun oldest (dates: (int * int * int) list) =
	if null dates
	then NONE
	else 
		let fun oldest_non_empty 
			(non_empty_dates: (int * int * int) list) =
			if null (tl non_empty_dates)
			then hd non_empty_dates
			else
				let val oldest_tl = oldest_non_empty (tl non_empty_dates)
				in
					if is_older (hd non_empty_dates, oldest_tl)
					then hd non_empty_dates
					else oldest_tl
				end
		in SOME (oldest_non_empty (dates))
		end

(* 12 *)
fun member (num: int, lst: int list) =
	if null lst
	then false
	else 
		if (hd lst) = num 
        then true
        else member (num, tl lst)

fun remove_duplicates (nums: int list) =
	if null nums
	then []
	else 
		if not (member (hd nums, tl nums))
		then (hd nums) :: remove_duplicates (tl nums)
		else remove_duplicates (tl nums)

fun number_in_months_challenge (dates: (int * int * int) list, months: int list) =
    number_in_months (dates, remove_duplicates (months))

fun dates_in_months_challenge (dates: (int * int * int) list, months: int list) =
    dates_in_months (dates, remove_duplicates (months))

(* 13 *)
fun get_nth_list_item (lst: int list, n: int) =
    if lst = []
    then NONE
    else if n = 1
        then SOME (hd lst)
        else get_nth_list_item (tl lst, n - 1)

fun max_days_in_month (date: int * int * int) =
    let val days_months_regular = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 
        val days_months_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in 
        let fun is_leap (year: int) =
            (year mod 400 = 0) orelse (year mod 4 = 0 andalso year mod 100 <> 0)
        in
            if is_leap (#1 date)
            then valOf (get_nth_list_item (days_months_leap, (#2 date)))
            else valOf (get_nth_list_item (days_months_regular, (#2 date)))
        end
    end

fun reasonable_date (date: int * int * int) =
	let
		val year = #1 date
		val month = #2 date
		val day = #3 date
	in
    	if year < 1 orelse month < 1 orelse month > 12 orelse 
    	day < 1 orelse day > 31
    	then false
    	else day <= max_days_in_month (date)
    end