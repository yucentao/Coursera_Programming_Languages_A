fun is_older(date1 : int*int*int, date2 : int*int*int) = 
    if (#1 date1) < (#1 date2)
    then true
    else if (#1 date1) > (#1 date2)
    then false
    else if (#2 date1) < (#2 date2)
    then true
    else if (#2 date1) > (#2 date2)
    then false
    else if (#3 date1) < (#3 date2)
    then true
    else false

fun number_in_month(dates : (int*int*int) list, month : int) = 
    if null dates
    then 0
    else (if (#2 (hd dates)) = month then 1 else 0) + number_in_month(tl dates, month)

fun number_in_months(dates : (int*int*int) list, months : int list) = 
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int*int*int) list, month : int) = 
    if null dates
    then []
    else
        let 
            val tl_ans = dates_in_month(tl dates, month)
        in
            if (#2 (hd dates)) = month
            then (hd dates) :: tl_ans
            else tl_ans
        end

fun dates_in_months(dates : (int*int*int) list, months : int list) = 
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(elements : string list, n : int) = 
    if n = 1
    then hd elements
    else get_nth(tl elements, n - 1)

fun date_to_string(date : int*int*int) = 
    let val month_words = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth(month_words, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum : int, numbers : int list) = 
    if sum <= (hd numbers)
    then 0
    else 1 + number_before_reaching_sum(sum - (hd numbers), tl numbers)

fun what_month(day : int) = 
    let val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in number_before_reaching_sum(day, month_days) + 1
    end

fun month_range(day1 : int, day2 : int) = 
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates : (int*int*int) list) = 
    if null dates
    then NONE
    else 
        let 
            val tl_ans = oldest(tl dates)
        in 
            if isSome tl_ans andalso is_older(valOf tl_ans, hd dates)
            then tl_ans
            else SOME (hd dates)
        end

fun num_not_in_list(num : int, numbers : int list) = 
    if null numbers
    then true
    else num <> (hd numbers) andalso num_not_in_list(num, tl numbers)

fun dedup(numbers : int list) = 
    if null numbers
    then []
    else
        let
            val tl_ans = dedup(tl numbers)
        in
            if num_not_in_list(hd numbers, tl_ans)
            then (hd numbers) :: tl_ans
            else tl_ans
        end

fun number_in_months_challenge(dates : (int*int*int) list, months : int list) = 
    number_in_months(dates, dedup(months))

fun dates_in_months_challenge(dates : (int*int*int) list, months : int list) = 
    dates_in_months(dates, dedup(months))

fun day_of_month(days : int list, month : int) = 
    if month = 1
    then hd days
    else day_of_month(tl days, month - 1)

fun reasonable_date(date : int*int*int) = 
    if (#1 date) <= 0 orelse (#2 date) < 1 orelse (#2 date) > 12
    then false
    else if (#1 date) mod 400 = 0 orelse ((#1 date) mod 4 = 0 andalso (#1 date) mod 100 <> 0)
    then
        let
            val days = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        in
            if day_of_month(days, #2 date) >= (#3 date)
            then true
            else false
        end
    else
        let
            val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        in
            if day_of_month(days, #2 date) >= (#3 date)
            then true
            else false
        end
        