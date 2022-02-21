
#include <iostream>
#include <fstream>
#include <string>
using namespace std;

int main()
{

    cout << "data.txt pomisti v papku lab1(1), programma opratsovue do 10 000 sliv po >=4 simvoliv tse priblizno 60-80 000 simvoliv\n";
    string line;
    string text;
    string slovo="";
    struct st_slovo { // інформація про слово
        string slovo;
        int chastota = 0;
        string storinki="";
        int last_st=0; // остання сторінка на якій зустрілось слово, потрібно для уникнення повторів сторінок
    };
    st_slovo mas[10000];
    int k_slov=0;
    st_slovo tmp;
    int n_st = 1;
    int n_line=1;
    int min_ind=10000;
    int N;

    cout << "vvadi kilkist sliv dla vivodu N=";
    cin >> N;
    ifstream in("data.txt"); // окрываем файл для чтения
    if (in.is_open())
    {
        lab1: /*замінів while на мітку1*/ if(getline(in, line))
        {
            // for (int j = 0; j < line.size(); j++) замінив на мітку11
            int j = 0;
            lab11: if (j<line.size())
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
    lab2: if (i<text.size())
    {
        if (text[i] != ' ' and text[i] != '\n' and (text[i]>='a' and text[i]<='z' or text[i]>='A' and text[i]<='Z')) { // знаходимо в тексту слово, що складаэться з англ букв
            slovo += (string)"" + text[i];
        }
        else { // додаємо слово в масив слів або збільшуємо частоту, якщо це слово там вже є
            if (slovo != "") {
                if (slovo.size() <= 3) slovo = ""; // ігноруємо слова довжиною менше або рівною 3
                // for (int j = 0; j < k_slov; j++) замінив на мітку12
                int j = 0;
                lab12: if(j<k_slov)
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
                n_st = (n_line-1) / 45 + 1;
            }
        }

        i++; 
        goto lab2;
    };

    // for (int i = 0; i < k_slov; i++) замінив на мітку3
    i = 0;
    lab3: if(i<k_slov)
    { // сортуємо бульбашкою по частоті
        // for (int j = 0; j < k_slov-i-1; j++) замінив на мітку4
        int j = 0;
        lab4: if (j<k_slov-i-1)
        {
            if (mas[j].chastota < mas[j+1].chastota) {
                tmp=mas[j];
                mas[j] = mas[j+1];
                mas[j+1] = tmp;
            }

            j++; 
            goto lab4;
        }

        i++;
        goto lab3;
    }
    // for (int i = 0; i < k_slov and i<N ; i++) замінив на мітку5
    i = 0;
    lab5: if(i<k_slov and i<N)
    { // виводимо слова і їх частоту
        cout << mas[i].slovo << "-" << mas[i].chastota << "\n";

        i++;
        goto lab5;
    }
    cout << "\nkilkist sliv v data.txt=" << k_slov << "\n";
    cout << "End of program" << std::endl;
    return 0;

}

// Запуск программы: CTRL+F5 или меню "Отладка" > "Запуск без отладки"
// Отладка программы: F5 или меню "Отладка" > "Запустить отладку"
