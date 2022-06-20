(*1. Напишіть функцію  only_capitals яка приймає на вхід string list та  повертає string list що має тільки рядки що починаються з Великої  літери. 
Вважайте, що всі рядки мають щонайменше один символ. Використайте List.filter, Char.isUpper, та String.sub щоб створити рішення в 1-2 рядки. *)
fun only_capitals(xs) = 
    List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs;
(* можна це зробити і через композицію функцій, наприклад так: fun only_capitals(xs) = 
    List.filter (fn x => (Char.isUpper o String.sub)(x, 0)) xs; *)    
val tes1=only_capitals (["T1","t2","T3"]);
(*2. Напишіть функцію longest_string1 що приймає string list та повертає найдовший string в списку. 
Якщо список пустий, поверніть "". У випадку наявності декількох однакових кандидатів, поверніть рядок, що найближче до початку списку. 
Використайте foldl, String.size, та ніякої рекурсії (окрім як використання foldl що є рекурсивним).*)
fun longest_string1(xs) = 
    foldl (fn (x,acc) => if String.size x > String.size acc then x else acc) "" xs;
val test2_1=longest_string1(["1234","12345","1234t","12"]);
val test2_2=longest_string1([]);
(*3. Напишіть функцію longest_string2 яка точно така сама як longest_string1 
окрім як у випадку однакових кандидатів вона повертає найближчого до кінця кандидата. 
Ваше рішення має бути майже копією longest_string1. Так само використайте foldl та String.size.*)
fun longest_string2(xs) = 
    foldl (fn (x,acc) => if String.size x >= String.size acc then x else acc) "" xs;
val test3_1=longest_string2(["1234","12345","1234t","12"]);
val test3_2=longest_string2([]);
(*4. Напишіть функції longest_string_helper, longest_string3, та longest_string4 такі що: 
• longest_string3 має таку саму поведінку як longest_string1 та longest_string4 має таку саму поведінку як longest_string2.
• longest_string_helper має тип (int * int -> bool) -> string list -> string (зверніть увагу на curry). 
Ця функція буде схожа на  longest_string1 та longest_string2 але вона є більш загальною так як приймає функцію як аргумент.
• Якщо longest_string_helper отримує на вхід функцію яка має поведінку як > (тобто повертає true тоді коли перший аргумент строго більше другого), 
тоді функція має таку саме поведінку як  longest_string1.
• longest_string3 та longest_string4 є визначеними через val-прив’язки і часткове використання longest_string_helper.*)
fun longest_string_helper f xs =
    foldl (fn (x,acc)=> if f(String.size x, String.size acc) then x else acc) "" xs;
val longest_string3=longest_string_helper(fn (x,y) => x > y);
val longest_string4=longest_string_helper(fn (x,y) => x >=y);
val test4_1=longest_string3(["1234","12345","1234t","12"]);
val test4_2=longest_string4(["1234","12345","1234t","12"]);
(*5. Напишіть функцію longest_capitalized що приймає на вхід string list та повертає найдовший рядок в списку яка починається з Великої літери , 
або "" якщо таких рядків немає. Вважайте, що всі рядки мають щонайменше один символ. Використовуйте val-прив’язки 
та  ML бібліотечний o оператор для композиції функцій. Вирішіть проблему з однаковими результатами за прикладом завдання 2.*)
val longest_capitalized = (longest_string1 o only_capitals);
val test5_1=longest_capitalized(["1234","12345","1234t","12"]);
val test5_2=longest_capitalized(["A234","12345","1234t","D12"]);
(*6. Напишіть функцію rev_string, що приймає на вхід string та повертає  string що має ті самі символи в зворотньому порядку. 
Використайте ML o оператор, бібліотечну функцію rev для перевертання списків, та дві бібліотечні функції з String модулю. 
(Перегляньте документацію, щоб знайти найкращі підходящі)
Наступні дві проблеми передбачають написання функцій над списками які будуть використані в більш пізніх задачах. *)
val rev_string = String.implode o rev o String.explode;
val test6=rev_string("abc");
(*7. Напишіть функцію first_answer типу (’a -> ’b option) -> ’a list -> ’b (зауважте 2 аргументи curry). 
Перший аргумент має бути застосований до елементів другого аргументу до того моменту, як він поверне SOME v для деякого v і 
тоді v є результатом виклику first_answer. Якщо перший аргумент повертає NONE для всіх елементів списку, тоді має повернути виключення NoAnswer. 
Підказка: Приклад розв'язку має  5 рядків і не робить нічого складного. *)
exception NoAnswer;
fun first_answer f [] = raise NoAnswer
    | first_answer f (x :: xs') = case f x of 
                                    SOME x' => x'
                                    |NONE => first_answer f xs';
(*8. Напишіть функцію all_answers типу (’a -> ’b list option) -> ’a list -> ’b list option (зауважте 2 аргументи curry). 
Перший аргумент має бути застосований до елементів другого аргументу. Якщо результатом є NONE для будь якого з елементів, 
то результатом all_answers є NONE. Інакше виклики першого аргументу мають повернути SOME lst1, SOME lst2, ... 
SOME lstn та результатом all_answers буде SOME lst де lst є lst1, lst2, ..., lstn що складаються разом(порядок не важливий). 
Підказки: Приклад розв'язку має  8 рядків. Він використовує допоміжні функції з акумулятором та  @. Зауважте all_answers f [] має отримати тип SOME [].*)
fun all_answers f [] = SOME []
  | all_answers f xs = 
    let fun loop(acc, []) = SOME acc
	  | loop(acc, SOME(y)::ys) = loop(acc @ y, ys)
	  | loop(acc, NONE::ys) = NONE
    in
	loop([], map f xs)
    end;
(*Задачі що залишилися використовують наступні визначення типів, що були
створені за образом вбудованої реалізації ML порівняння з шаблоном:
datatype pattern = Wildcard | Variable of string | UnitP |
ConstP of int | TupleP of pattern list | ConstructorP of
string * pattern
datatype valu = Const of int | Unit | Tuple of valu list |
Constructor of string * valu
Дано valu v та pattern p, або p співпадає з v або ні. Якщо так,співпадіння
створює список string * valu пар; порядок в списку не має значення.
Правила порівняння мають бути наступними:
• Wildcard співпадає з усім і створює пустий список прив’язок.
• Variable s співпадає з будь яким значенням v та створює одно
елементний список що містить (s,v).
• UnitP співпадає тільки з Unit та створює пустий список прив’язок.
• ConstP 17 співпадає тільки з Const 17 та створює пустий список
прив’язок (так само для інших цілих чисел).
• TupleP ps співпадає з значенням форми Tuple vs якщо ps та vs
мають однакову довжину і для всіх i, iий елемент ps співпадає з iим елементом
vs. Список прив’язок що створюється в результаті є усіма списками вкладених
порівнянь з шаблоном що об’єднані в один список.
• ConstructorP(s1,p) співпадає з Constructor(s2,v) якщо s1 та s2
є однаковою строкою (ви можете порівняти їх з =) та p співпадає з v. Список
прив’язок створюється із вкладених порівнянь із шаблоном. Ми називаємо
рядки s1 та s2 іменами конструкторів.
• Все інше не має значення.
9. (Ця задача використовує pattern тип даних але не зовсім про порівняння із
шаблоном.) Функція g надана в файлі.
(1) Використайте g для визначення функції count_wildcards, що приймає
на вхід pattern та повертає скільки Wildcard pattern-ів він містить.*)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


(**** you can put all your code here ****)

fun count_wildcards(p) = g (fn () => 1) (fn x => 0) p;
(*(2) Використайте g для визначення функції
count_wild_and_variable_lengths що приймає на вхід pattern та
повертає кількість Wildcard pattern-ів які він містить плюс суму довжин
рядків всіх змінних що містяться у змінній patterns. (Використайте
String.size. Нам важливі тільки імена змінних; імена конструкторів не
важливі.)*)
fun count_wild_and_variable_lengths(p) = g (fn () => 1) (fn x => String.size x) p;
(*(3) Використайте g для визначення функції count_some_var що приймає на
вхід строку та pattern (як пару) та повертає кількість входжень строки як змінної
в pattern. Нам важливі тільки імена змінних; імена конструкторів не важливі.*)
fun count_some_var(s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p;

(*10. Напишіть функцію check_pat що приймає на вхід pattern та повертає true
тоді і тільки тоді коли всі змінні що з’являються в pattern відрізняються один від
одного (наприклад, використовують різні рядки). Імена конструкторів не
важливі. Підказки: Приклад розв’язку має 2 допоміжні функції. Перша приймає
pattern та повертає список всіх рядків які він використовує для змінних.
Використовуючи foldl з функцією яка використовує append може бути
корисним. Друга функція приймає на вхід список рядків і вирішує чи він має
повтори. List.exists може бути корисним. Приклад розв'язку має 15 рядків.
Підказка: foldl та List.exists не обов’язкові, але можуть допомогти.*)
fun check_pat(p) = 
    let fun list_vars (Variable x) = [x]
	  | list_vars (TupleP ps) = List.foldl (fn (p', acc) => acc @ list_vars(p')) [] ps
	  | list_vars (_) = []
	fun has_repeats ([]) = false
	  | has_repeats (x::xs) = List.exists (fn x' => x = x') xs orelse has_repeats xs
    in
	(not o has_repeats o list_vars) p
    end;
(*11. Напишіть функцію first_match що приймає на вхід value та список
шаблонів та повертає (string * valu) list option, тобто NONE якщо
ніякий паттерн зі списку не підходить або SOME lst де lst це список
прив’язок для першого паттерну в списку який підійшов. Використайте
first_answer та handle-вираз. Підказка: Приклад розв'язку має 3 рядки.*)
fun match(v, p) = 
    case (p, v) of
	(Wildcard, _) => SOME []
      | (Variable s, _) => SOME [(s,v)]
      | (UnitP, Unit) => SOME []
      | (ConstP cp, Const cv) => if cp = cv then SOME [] else NONE
      | (TupleP ps, Tuple vs) => if List.length ps = List.length vs 
				 then all_answers (fn (vs',ps') => match(vs',ps')) (ListPair.zip(vs,ps))
				 else NONE
      | (ConstructorP(s1,pp), Constructor(s2,pv)) => if s1 = s2 then match(pv,pp) else NONE
      | _ => NONE;

fun first_match v ps = 
    ( SOME(first_answer (fn p => match(v,p)) ps) ) handle NoAnswer => NONE;