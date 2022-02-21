#include <iostream>
#include <fstream>
#include <string>
using namespace std;

int main()
{

	cout << "data.txt pomisti v papku lab1(2), programma opratsovue do 10 000 sliv po >=4 simvoliv tse priblizno 80 000 simvoliv\n";
	string line;
	string text;
	string slovo = "";
	struct st_slovo { // інформація про слово
		string slovo;
		int chastota = 0;
		string storinki = "";
		int last_st = 0; // остання сторінка на якій зустрілось слово, потрібно для уникнення повторів сторінок
	};
	st_slovo mas[10000];
	int k_slov = 0;
	st_slovo tmp;
	int n_st = 1;
	int n_line = 1;
	int min_ind = 10000;

	ifstream in("data.txt"); // окрываем файл для чтения
	if (in.is_open())
	{
	lab1: /*замінів while на мітку1*/ if (getline(in, line))
	{
		// for (int j = 0; j < line.size(); j++) замінив на мітку11
		int j = 0;
	lab11: if (j < line.size())
	{
		if (line[j] >= 'A' && line[j] <= 'Z') line[j] += 'a' - 'A'; // заміна великих букв маленькими, щоб всі слова стали однаковго регістру

		j++;
		goto lab11;
	}
	text += line + "\n";

	goto lab1;
	}
	}
	in.close();     // закрываем файл
	cout << /*text + */"kilkist simvoliv v data.txt=" << text.size() << "\n";

	//for (int i = 0; i < text.size(); i++) замінив на мітку2
	int i = 0;
lab2: if (i < text.size())
{
	if (text[i] != ' ' and text[i] != '\n' and (text[i] >= 'a' and text[i] <= 'z' or text[i] >= 'A' and text[i] <= 'Z')) { // знаходимо в тексту слово, що складаэться з англ букв
		slovo += (string)"" + text[i];
	}
	else { // додаємо слово в масив слів або збільшуємо частоту, якщо це слово там вже є
		if (slovo != "") {
			if (slovo.size() <= 3) slovo = ""; // ігноруємо слова довжиною менше або рівною 3
			// for (int j = 0; j < k_slov; j++) замінив на мітку12
			int j = 0;
		lab12: if (j < k_slov)
		{ // змінюємо частоту
			if (mas[j].slovo == slovo) {
				mas[j].chastota++;
				if (mas[j].last_st != n_st) {
					mas[j].storinki += (string)", " + to_string(n_st);
					mas[j].last_st = n_st;
				}
				slovo = "";
			}

			j++;
			goto lab12;
		}
		if (slovo != "") { // додаємо в кінець
			mas[k_slov].slovo = slovo;
			mas[k_slov].chastota++;
			mas[k_slov].storinki += to_string(n_st);
			mas[k_slov].last_st = n_st;
			slovo = "";
			k_slov++;
		}
		}
		if (text[i] == '\n') { // рахуємо номер сторінки
			n_line++;
			n_st = (n_line - 1) / 45 + 1;
		}
	}

	i++;
	goto lab2;
};

// for (int i = 0; i < k_slov; i++) замінив на мітку3
i = 0;
lab3: if (i < k_slov)
{ // сортуємо бульбашкою по частоті
	// for (int j = 0; j < k_slov-i-1; j++) замінив на мітку4
	int j = 0;
lab4: if (j < k_slov - i - 1)
{
	if (mas[j].chastota < mas[j + 1].chastota) {
		tmp = mas[j];
		mas[j] = mas[j + 1];
		mas[j + 1] = tmp;
	}

	j++;
	goto lab4;
}

i++;
goto lab3;
}

// for (int i = 0; i < k_slov; i++) замінив на мітку6
i = 0;
lab6: if (i < k_slov)
{ //знаходимо індекс в відсортованому по спаданню за частотою масиві, починаючи з якого слова зустрічаються не більше 100 разів
	if (mas[i].chastota <= 100) {
		min_ind = i;
		// break; замінив на мітку7
		goto lab7;
	}

	i++;
	goto lab6;
} lab7:

// for (int i = min_ind; i < k_slov; i++) замінив на мітку8
i = min_ind;
lab8: if (i < k_slov)
{ // сортуємо по алфавіту слова які зустрічаються не частіше 100 разів
	// for (int j = min_ind; j < k_slov - i - 1; j++) замінив на мітку9
	int j = min_ind;
lab9: if (j < k_slov - i - 1)
{
	if (mas[j].slovo > mas[j + 1].slovo) {
		tmp = mas[j];
		mas[j] = mas[j + 1];
		mas[j + 1] = tmp;
	}

	j++;
	goto lab9;
}

i++;
goto lab8;
}

// for (int i = min_ind; i < k_slov; i++) замінив на мітку10
i = min_ind;
lab10: if (i < k_slov)
{ // виводимо відсортовані слова зі сторінками, на яких вони зустрічаються
	cout << mas[i].slovo << " - " << mas[i].storinki << "\n";

	i++;
	goto lab10;
}
cout << "\nkilkist sliv v data.txt=" << k_slov << "\n";
cout << "End of program" << std::endl;
return 0;

}

// Запуск программы: CTRL+F5 или меню "Отладка" > "Запуск без отладки"
// Отладка программы: F5 или меню "Отладка" > "Запустить отладку"
