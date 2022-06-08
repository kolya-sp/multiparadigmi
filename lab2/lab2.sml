(* 1 Напишіть функцію is_older, яка приймає дві дати та повертає значення true або false. 
Оцінюється як true, якщо перший аргумент - це дата, яка раніша за другий аргумент. 
(Якщо дві дати однакові, результат хибний.) *)
fun is_older(d1:int*int*int,d2:int*int*int)=
    if ((#1 d1) < (#1 d2))
    then true
    else if ((#1 d1) = (#1 d2))
    then if ((#2 d1) < (#2 d2))
    then true 
    else if ((#2 d1) = (#2 d2))
    then if ((#3 d1) < (#3 d2))
    then true
    else false
    else false
    else false;
is_older((1900,12,30),(1900,12,31));

(* 2 Напишіть функцію number_in_month, яка приймає список дат і місяць 
(тобто int) і повертає скільки дат у списку в даному місяці. *)
fun number_in_month (dates: (int*int*int) list, month: int) =
    if null dates 
    then 0
    else if (#2 (hd dates) = month)
    then 1+number_in_month(tl dates, month)
    else number_in_month(tl dates, month);
number_in_month ([(1999,12,3),(1222,2,4),(222,3,5),(1222,12,7)],12);

(* 3 апишіть функцію number_in_months, яка приймає список дат і список місяців (тобто список int) і повертає кількість дат у списку дат, 
які знаходяться в будь-якому з місяців у списку місяців. Припустимо, що в списку місяців немає повторюваних номерів. 
Підказка: скористайтеся відповіддю до попередньої задачі.*)
fun number_in_months(dates:(int*int*int) list, month: int list) =
    if null month
    then 0
    else number_in_month(dates,hd month)+number_in_months(dates,tl month);
number_in_months ([(1999,12,3),(1222,2,4),(222,3,5),(1222,12,7)],[12,3]);

(* 4 Напишіть функцію dates_in_month, яка приймає список дат і число місяця (тобто int) і повертає список, 
що містить дати з аргументу “список дат”, які знаходяться в переданому місяці. 
Повернутий список повинен містять дати в тому порядку, в якому вони були надані спочатку.*)
(* якщо передається список дат і день місяця
fun dates_in_month(dates:(int*int*int) list, day:int) =
    if null dates
    then []
    else if (#3 (hd dates) = day) 
    then (hd dates) :: dates_in_month(tl dates, day)
    else dates_in_month(tl dates, day);
dates_in_month([(1999,12,3),(1222,2,4),(222,3,5),(1222,12,5)],5);*)
(*якщо передається список дат і номер місяця (в 4 неоднозначна умова, 
але функція з номером місяця використовується у наступному завданні) *)
fun dates_in_month(dates:(int*int*int) list, month:int) =
    if null dates
    then []
    else if (#2 (hd dates) = month) 
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month);
dates_in_month([(1999,12,3),(1222,2,4),(222,3,5),(1222,12,5)],12); 

(* 5 Напишіть функцію dates_in_months, яка приймає список дат і список місяців (тобто список int) і повертає список, 
що містить дати зі списку аргументів дат, які знаходяться в будь-якому з місяців у списку місяців. 
Для простоти, припустимо, що в списку місяців немає повторюваних номерів. 
Підказка: Використовуйте свою відповідь на попередню задачу та оператор додавання списку SML (@).*)

fun dates_in_months (dates:(int*int*int) list, month: int list) =
    if null month
    then []
    else dates_in_month(dates, hd month) @ dates_in_months(dates, tl month);
dates_in_months([(1999,12,23),(2000,4,15),(2001,8,14),(2008,12,31)],[12,8]);

(* 6 Напишіть функцію get_nth, яка приймає список рядків і int n та повертає n-й елемент списку, 
де голова списку є першим значенням. Не турбуйтеся якщо в списку занадто мало елементів: 
у цьому випадку ваша функція може навіть застосувати hd або tl до порожнього списку, і це нормально. *)

fun get_nth(st: string list, n:int) = 
if n=1 
then hd st
else get_nth(tl st, n-1);
get_nth(["odun","dva","tri"],3);

(* 7 Напишіть функцію date_to_string, яка приймає дату і повертає рядок у вигляді 
“February 28, 2022” Використовуйте оператор ^ для конкатенації рядків і бібліотечну 
функцію Int.toString для перетворення int в рядок. Для створення частини з місяцем 
не використовуйте купу розгалужень. Замість цього використайте список із 12 рядків 
і свою відповідь на попередню задачу. Для консистенції пишіть кому після дня та 
використовуйте назви місяців англійською мовою з великої літери. *)
fun date_to_string(date: (int*int*int) ) = 
let 
    val month = ["January","February","March","April","May","June","July","August","September","October","November","December"]
in
    get_nth(month,#2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
end;
date_to_string(1999,12,31);

(* 8 Напишіть функцію number_before_reaching_sum, яка приймає додатний int під назвою sum, 
та список int, усі числа якої також додатні. Функція повертає int. Ви повинні повернути 
значення int n таке, щоб перші n елементів списку в сумі будуть менші sum, але сума значень 
від n + 1 елемента списку до кінця був більше або рівний sum. *)
fun number_before_reaching_sum(sum:int, l_int: int list)=
let fun next (sum:int, l_int: int list, n:int) =
    if sum<=0
    then n-1
    else next(sum-(hd l_int), tl l_int, n+1)
in 
    next(sum,l_int,0)
end;
number_before_reaching_sum(21,[10,10,20,10,10]);

(* 9 Напишіть функцію what_month, яка приймає день року (тобто int між 1 і 365) 
і повертає в якому місяці цей день (1 для січня, 2 для лютого тощо). 
Використовуйте список, що містить 12 цілих чисел і вашу відповідь на попередню задачу. *)
(* якщо треба повернути результат рядок назва місяця
fun what_month(day:int)=
let 
    val day_in_month = [31,28,31,30,31,30,31,31,30,31,30,31];
    val month = ["January","February","March","April","May","June","July","August","September","October","November","December"];
in 
    get_nth(month, number_before_reaching_sum(day,day_in_month)+1)
end;
what_month(60);
*)
(* якщо результатом має бути номер місяця, використовується в наступній задачі *)
fun what_month(day:int)=
let 
    val day_in_month = [31,28,31,30,31,30,31,31,30,31,30,31];
in 
    number_before_reaching_sum(day,day_in_month)+1
end;
what_month(60);

(* 10 Напишіть функцію month_range, яка приймає два дні року day1 і day2 
і повертає список int [m1,m2,...,mn] де m1 – місяць day1, m2 – місяць day1+1, ..., а mn – місяць day2. 
Зверніть увагу, що результат матиме довжину day2 - day1 + 1 або довжину 0, якщо day1>day2. *)
fun month_range(day1:int,day2:int)=
if day1>day2
then []
else what_month(day1)::month_range(day1+1,day2);
month_range(29,33);

(* 11 Напишіть найстарішу функцію, яка бере список дат і оцінює параметр (int*int*int). 
Він має оцінюватися як NONE, якщо список не містить дат, і SOME d, якщо дата d є найстарішою датою у списку.*)
fun better_max (xs : (int*int*int) list) =
    if null xs
    then NONE
    else
        let val tl_ans = better_max(tl xs)
        in if isSome tl_ans andalso is_older(hd xs,valOf tl_ans)
            then tl_ans
            else SOME (hd xs)
        end;
better_max([(1999,12,30),(1998,10,31),(1999,12,31)]);