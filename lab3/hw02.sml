(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
(* если вы используете эту функцию для сравнения двух строк (возвращает true, 
если одна и та же строка), то вы избегаете некоторых функций в задаче 1, 
имеющих полиморфные типы, которые могут сбивать с толку *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2;

(* put your solutions for problem 1 here *)
(*поместите свои решения для задачи 1 здесь*)
(*(a) Напишіть функцію all_except_option, яка приймає string і string list. Поверніть NONE,
якщо рядка немає у списку, інакше поверніть SOME lst, де lst ідентичний списку
аргументів, за винятком того, що рядка в ньому немає. Ви можете вважати, що рядок є в
списку щонайбільше один раз. Використовуйте рядок, наданий вам, для порівняння
рядків. Приклад рішення становить близько 8 строк.*)
fun  all_except_option (st:string, lst:string list) =
   case lst of
      [] => NONE
      | (x::xs) => if same_string(st,x) 
            then SOME(xs) 
            else case all_except_option(st,xs) of 
                     NONE=>NONE
                     |SOME(t)=>SOME(x::t);
all_except_option("test2",["test1","test2","test3"]);
(*(b) Напишіть функцію get_substitutions1, яка приймає string list list (список списків рядків,
замін ) і string s і повертає string list. Результат містить всі рядки, які є в якомусь із
списків замін, які також мають s, але сам s не повинен бути в результаті*)
fun get_substitutions1(sll:string list list, s:string) = 
   case sll of
      []=>[]
      | (x::xs) => case all_except_option(s,x) of
                     NONE=>get_substitutions1(xs,s)
                     |SOME(t)=>t@get_substitutions1(xs,s);
get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred");
get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff");
(*(c) Напишіть функцію get_substitutions2, схожу на get_substitutions1, за винятком того, що
вона використовує хвостову рекурсивну локальну допоміжну функцію.*)
fun get_substitutions2(sll:string list list, s:string)=
   let 
      fun f(sll:string list list, s:string, acc:string list) = 
         case sll of
            []=>acc
            |(x::xs)=> case all_except_option(s,x) of
                           NONE=>f(xs,s,acc)
                           |SOME(t)=>f(xs,s,acc@t)
   in
      f(sll,s,[])
   end;
get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred");
get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff");
(*(d) Напишіть функцію similar_names, яка приймає string list list із
підстановками (як у частинах (b) і (c)) і повне ім'я типу {first:string,middle:string,last:string} і
повертає список повних імен (тип {first:string,middle:string,last:string} list). Результатом є всі
повні імена, які ви можете створити, замінивши ім’я (і лише ім’я), використовуючи заміни тачастини (b) або (c). 
Відповідь має починатися з оригінальної назви (тоді мати 0 або більше інших імен).*)
fun similar_names (subs, name:{first:string, middle:string, last:string}) =
   case name of {first=f,middle=m,last=l} =>
      let fun aux(subs, acc) =
         case subs of
            [] => acc
            | (x::xs) => aux(xs, acc @ [{first=x, middle=m, last=l}]) 
      in
         aux(get_substitutions2(subs, f), [name])
      end;
similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],{first="Fred", middle="W", last="Smith"});
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
(* вы можете предположить, что Num всегда используется со значениями 2, 3, ..., 10, 
   хотя на самом деле это не подходит *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove
(* put your solutions for problem 2 here *)
(*поместите свои решения для задачи 2 здесь*)

(*(a) Напишіть функцію card_color, яка бере карту і повертає її колір (піки і трефи чорні,
бубни і чирви червоні). Примітка: достатньо одного case-виразу*)
fun card_color(suit,rank) = 
   case suit of
      Clubs => Black
      | Diamonds => Red
      | Hearts => Red
      | Spades => Black;
card_color(Clubs,Num 8);
card_color(Diamonds,Jack);
card_color(Hearts,Ace);
(*(b) Напишіть функцію card_value, яка бере карту та повертає її значення (нумеровані
карти мають свій номер як значення, тузи — 11, все інше — 10)*)
fun card_value (suit,rank) = 
   case rank of
      Num n => n
      | Ace => 11
      | _ => 10;
card_value(Clubs,Num 8);
card_value(Diamonds,Jack);
card_value(Hearts,Ace);
(*(c) Напишіть функцію remove_card, яка бере список карт cs, картку c та виняток e. Функція
повертає список, який містить усі елементи cs, крім c. Якщо c є у списку більше одного
разу, видаліть лише перший. Якщо c немає у списку, поверніть виняток e. Ви можете
порівнювати карти з =.*)
fun remove_card(cs:card list, c:card, e) = 
   case cs of
      []=> raise e
      | x::xs => if x=c 
                     then xs
                     else case remove_card(xs,c,e) of
                              []=>[x]
                              |t => x::t;
remove_card ([(Clubs,Num 8), (Diamonds,Jack),(Hearts,Ace)], (Diamonds,Jack), IllegalMove); 
(* remove_card ([], (Diamonds,Jack), IllegalMove); *) 

(*(d) Напишіть функцію all_same_color, яка приймає список карт і повертає true, якщо всі
карти в списку мають однаковий колір*)
fun all_same_color(cs:card list) = 
   case cs of
      []=>true
      |x::[]=>true
      |a::(b::c)=> if card_color(a)=card_color(b)
                     then all_same_color(b::c)
                     else false;
all_same_color([(Clubs,Num 10),(Clubs,Jack)]);
all_same_color([(Clubs,Num 10),(Clubs,Jack),(Diamonds,Ace)]);

(*(e) Напишіть функцію sum_cards, яка бере список карт і повертає суму їх значень.
Використовуйте локально визначену допоміжну функцію, яка є хвостово-рекурсивною.*)
fun sum_cards(cs:card list) =
   let fun af(cs1:card list, acc: int) =
      case cs1 of
         []=>acc
         |x::xs=>af(xs,acc+card_value(x))
   in
      af(cs,0)
   end;
sum_cards([(Clubs,Num 10),(Clubs,Jack),(Diamonds,Ace)]);
(*(f) Напишіть функцію score, яка отримує на вхід card list (картки, що утримуються) та int
(ціль) і обчислює рахунок, як описано вище.*)
fun score(cs:card list,goal:int) = 
   let 
      val sum=sum_cards(cs);
      fun pre_score(cs,goal)=
            if sum>goal 
               then 3*(sum-goal) 
               else goal-sum 
            
   in
      if all_same_color(cs) 
         then pre_score(cs,goal) div 2
         else pre_score(cs,goal)
   end;
score([(Clubs,Num 10),(Clubs,Jack),(Diamonds,Ace)],21);
score([(Clubs,Num 10),(Clubs,Jack),(Diamonds,Ace)],41);
score([(Clubs,Num 10),(Clubs,Jack),(Spades,Ace)],41);

(*(g) Напишіть функцію officiate, яка «запускає гру». Вона приймає на вхід card list (список
карт), move list (що гравець «робить» у кожній точці) та int (ціль) і повертає рахунок у кінці
гри після обробки (частину чи всі ) переміщення в списку переміщень по порядку.
Використовуйте локально визначену рекурсивну допоміжну функцію, яка приймає кілька
аргументів, які разом представляють поточний стан гри. Як описано вище:*)

fun officiate (cs, ms, goal) =
   let fun process_moves(cs, ms, held) =
      case ms of
         [] => held
         | m::ms_tail => case m of
                           Discard card => process_moves(cs, ms_tail, remove_card(held, card, IllegalMove))
                           | Draw => case cs of
                                       [] => held
                                       | c::_ => if sum_cards(c::held) > goal 
                                                   then c::held
                                                   else process_moves(remove_card(cs, c, IllegalMove), ms_tail, c::held)                                                                                                              
   in
      score(process_moves(cs, ms, []), goal) 
   end;
officiate ([(Clubs,Num 8),(Spades,Ace),(Clubs,Num 7),(Spades,Num 6),(Diamonds, Num 7)], [Draw, Discard (Clubs,Num 8), Draw, Draw, Draw], 42);