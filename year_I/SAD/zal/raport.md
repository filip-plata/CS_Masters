# Protein data

#### Wstępny opis danych

O znaczeniu danych zbyt wiele nie można powiedzieć, gdyż nazwy kolumn zostały zanonimizowane. Wszystkie są typu double, kolumn jest 2000. Widać więc, iż na przeszukiwanie wszystkich możliwych podzbiorów parametrów nie będzie można sobie pozwolić. Nie jest to problem klasyfikacyjny, więc w kontekście przedmiotu należy zastosować pewien rodzaj regresji.

Wiele predyktorów jest zależnych liniowo od innych. Dlatego też ostateczny model regresji będzie używał - dzięki LASSO - tylko pięciu wyraźnie najistotniejszych predyktorów. W pliku z kodem w części protein i pod sekcją linear dependence otrzymujemy 1001 parametrów które są liniowo zależne od innych. Ze względu na te liniowe zależności i wymogi techniczne parametru VIF (brak liniowej zależności predykatów), postanowiłem przygotować je tylko dla pięciu istotnych parametrów. Okazują się one dobrze liniowo niezależne. Dla wybranej piątki predyktorów dostajemy wykres VIF:

![Protein VIF](protein_vif.png)

Liczby przyjmują wartości w przedziale około [-124, 299], więc otrzymywane błędy MSE na poziomie 50 wydaje się iż można zaakceptować.

## Wybór predyktorów

W celu selekcji cech, użyjemy Lasso, poprzez pakiet glmnet. Wybór ten wynika z faktu spodziewania się małej liczby istotnych parametrów - co sugeruje opis danych (wpływ na poziom pewnego białka - wiele potencjalnych danych może być zupełnie bez związku z obserwowanym stężeniem) oraz sama treść zadania.

Po wykonaniu wykresu widać 5 najistotniejszych cech, które ręcznie przepisałem do kodu. Na ich podstawie tworzyłem resztę modeli.

## Model pierwszy - regresja liniowa

Jeśli nie wiadomo od czego zacząć, warto od regresji liniowej. Modele liniowa waliduję za pomocą biblioteki lmvar. Ze względu na użycie tylko pięciu predyktorów, można liczyć na dużą sztywność modelu i podobne zachowanie również na danych testowych.

## Model drugi - losowy las

Druga ze wspomnianych na wykładzie metod uczenia statystycznego dla problemu regresji. Korzystając z uzyskanej wiedzy o istotnych predyktorach, wybieramy z danych tylko istotne kolumny. Chciałem też porównać możliwości drzew w stosunku do zwykłej regresji. Oprócz tego, dla małej liczby predyktorów można bardzo szybko konstruować duże lasy. Chciałem też sprawdzić, jak lasy losowe radzą sobie z danymi w małej liczbie wymiarów.

Dla lasów danych nie warto skalować. Przycinanie liczby wymiarów w lesie jest istotne ze względu na czas potrzebny na obliczenia.

## Predykcje

Zostały uzyskane poprzez model liniowy dla pięciu istotnych predyktorów. Po prostu osiągał najmniejszy błąd. Oprócz tego, jeśli dane testowe pochodzą z tego samego rozkładu, wyniki powinny być zbliżone do testowych.

# Cancer data

#### Wstępny opis danych

Dane również dotyczą pewnego problemu regresji. Są to liczby zmiennoprzecinkowe, natomiast przewidywane zmienne są być może pewnymi prawdopodobieństwami. Dlatego też, choć nie przewidują tego standardowe modele, warto obciąć otrzymane przewidywania do przedziału [0,1]. Może to tylko poprawić MSE dla ostatecznego wyniku.

Wymiarów w danych jest bardzo dużo, szczególnie w stosunku do ich ilości. Dlatego też w pierwszym kroku ustaliłem istotniejsze parametry za pomocą regresji liniowej z AIC.

Ustalone istotne parametry wykorzystuję do obliczenia statystyk VIF. Widać istotne zależności liniowe pomiędzy predyktorami:

![Cancer VIF](cancer_vif.png)

## Model pierwszy - regresja liniowa

Pewien rodzaj regresji i tak należy do ewaluacji wybrać, ze względu na konieczność dostarczenia trzech metod. Ze wzgledu na problemy techniczne z rozmiarem stosu w R, podzbiór istotnych parametrów wybierany jest w dwóch krokach. W pierwszym dla każdej kolejnej paczki predyktorów o ustalonym rozmiarze za pomocą AIC wyznaczamy mniejszy podzbiór predyktorów. Operację powtarzamy jeszcze jeden raz. W ten sposób otrzymujemy przyzwoity model liniowy, omijając problem znacznego wymiaru danych (prowadził on do stack overflow w rstudio). Sprawdzenie jednak zależności liniowych na wybranych 100 predyktorach sugeruje jednak, że dane są istotnie zależne liniowo, więc prawdopodobnie regresja liniowa nie jest optymalnym modelem.

## Model drugi - las losowy

Las losowy dla regresji wydaje sie ciekawym pomysłem. Postanowiłem go zastosować ze względu na znaczną liczbę wymiarów danych, intuicja sugeruje iż las losowy powinien mieć wtedy przy małej liczbie danych więcej "luzu" dla cięć przestrzeni. Również konieczność doboru 100 najbardziej istotnych parametrów wymaga dobrania pewnej metodologii. Lasy losowe dostarczają prostej metody na wybór najbardziej istotnych zmiennych, mianowicie można je posortować wedle ważności i wybrać pierwsze 100. Oprócz tego miałem możliwość skorzystania z obliczeń podczas nocy, dlatego też uznałem iż warto wytworzyć duży las losowy. Duża ilość drzew nie powinna prowadzić do overfittingu, natomiast może poprawić predykcje.

Obliczenia wykonywane były równolegle - okazuje się, że lasy można w prosty sposób sklejać. Jednak pomimo tych optymalizacji, wciąż jest to powolny proces.

## Model trzeci - knn dla regresji

Wydaje się to ciekawy model - znajdujemy k najbliższych sąsiadów i uśredniamy ich wartości, aby otrzymać predykcję. Jest wiedzą potoczną, iż knn powinien źle działać dla dużej liczby wymiarów. Uznałem, że warto sprawdzić to stwierdzenie w porównaniu do reszty modeli. Szybko okazało się to prawdą, dlatego też w kodzie pozostawiłem wybór konkretnej liczby sąsiadów - gdyż dla różnych próbowanych wartości skuteczność była bliska przewydywania wartości średniej kolumny Y. Dlatego też ten model z pewnością wybrany nie będzie. Wybieranie kolejnych K prowadziło do zbliżania się wyniku do średniej, nie zaobserwowałem żadnych ciekawych wyników na tym modelu.

## Predykcje

Zdecydowanie najlepsze rezultaty dawała regresja liniowa. Dlatego też uznałem, iż to jego predykcje zostaną użyte.

Knn i lasy losowe w tym problemie spisują się słabo, nieznacznie lepiej od średniej. Może to wynikać ze znacznej liczby wymiarów danych.

Predykcje przycinam do przedziału [0, 1].

# Wnioski

## Rezultaty modeli

Poniżej wyniki walidacji krzyżowej. W przypadku lasów losowych bardzo dobrym przybliżeniem jest zwykłe wzięcie predykcji dla danych OOB, tak więc też uczyniłem.

| Dane        | Metoda        | MSE          |
| ----------- | ------------- | ------------ |
| Protein     | Liniowy       | 36.5         |
| Protein     | Random Forest | 129.         |
| Cancer      | Liniowy       | 0.033        |
| Cancer      | KNN           | 0.08825      |
| Cancer      | Random Forest | 0.08216      |

Widać, iż knn poradził sobie gorzej niż regresja liniowa. Ponieważ wartości Y znajdują się w przedziale [0, 1], MSE rzędu 0.01 oznacza średnio błąd bezwzględny rzędu 0.1, czyli dość znaczny. Warto wspomnieć, iż średnia wartości dla danych cancer ma MSE rzędu 0.085. Wnik modelu liniowego na danych cancer był mierzony poprzez tworzenie podziału na dane testowe i walidacyjne, a także poprzez gotową metodę z paczki. Oba sposoby pomiarów dawały niezgodne wyniki - tj. cross walidacja z paczki mniejszy - rzędu 0.035. Okazało się to wynikać z przyjmowanego rozmiaru splitu. Być może posiadam zbyt wiele parametrów w modelu liniowym dla cancer i dlatego trudniej uzyskać stabilne wpasowanie do danych.

Dla obu problemów wybrałem model liniowy. Dla protein wybór oczywisty - mniejszy błąd, zbliżone błędy treningowe i testowe; natomiast dla cancer gdyż tylko on udało mi się otrzymać zauważalnie lepszy od średniej zmiennych objaśnianych. Dla cancer pewne sukcesy rzędu 0.075 udało się osiągnąć dla bootstrappingu z bardzo dużymi lasami losowymi - na podstawie nich wybierałem predyktory, a potem tworzyłem większe lasy na wybranych zmiennych - jest to jednak zbyt czasochłonne by prowadzić więcej eksperymentów (policzyłem raz przez noc, po kilku eksperymentach przypadkiem nadpisałem zmienną i postanowiłem się z tym pogodzić, gdyż rezultaty nie były bardzo korzystne. Drzewo było 6000 na pełnym zbiorze predyktorów cancer).
Należy jednak wspomnieć, iż regresja liniowa nie osiąga bardzo dobrych wyników na danych cancer w porównaniu do średniej. Z pewnością ciekawym eksperymentem byłoby zastosowanie prostej sieci neuronowej do tych danych.

## Dobór parametrów

### Protein

Z wykresu dla regresji liniowej z Lasso wyraźnie widać 5 najistotniejszych parametrów, są to:

* x821
* x1395
* x860
* x848
* x1982

Metodologie ich wyboru można doprecyzować jako wybranie wszystkich niezerowych parametrów w regresji liniowej z LASSO, dla ostatniego parametru lambda dla którego liczba niezerowych predyktorów wynosi 5.

### Cancer

Wybranie stu parametrów jest mniej jednoznaczne niż w poprzednim przypadku. Postanowiłem użyć najistotniejszych predyktorów dla lasu losowego dla dużego lasu losowego. Są to:

|                 |                 |                 |                 |
| --------------- | --------------- | --------------- | --------------- |
| ENSG00000158163 | ENSG00000179546 | ENSG00000137392 | ENSG00000157856 |
| ENSG00000108604 | ENSG00000135378 | ENSG00000197891 | ENSG00000100889 |
| ENSG00000178163 | ENSG00000198657 | ENSG00000129455 | ENSG00000214513 |
| ENSG00000114686 | ENSG00000128052 | ENSG00000008277 | ENSG00000174579 |
| ENSG00000162877 | ENSG00000018699 | ENSG00000158158 | ENSG00000186017 |
| ENSG00000020922 | ENSG00000124104 | ENSG00000046604 | ENSG00000188580 |
| ENSG00000100593 | ENSG00000175564 | ENSG00000105991 | ENSG00000168813 |
| ENSG00000174898 | ENSG00000161958 | ENSG00000157911 | ENSG00000189430 |
| ENSG00000149212 | ENSG00000114739 | ENSG00000143771 | ENSG00000166133 |
| ENSG00000027869 | ENSG00000183605 | ENSG00000178971 | ENSG00000143157 |
| ENSG00000172830 | ENSG00000237541 | ENSG00000100796 | ENSG00000139572 |
| ENSG00000176410 | ENSG00000177459 | ENSG00000177511 | ENSG00000179119 |
| ENSG00000163191 | ENSG00000112514 | ENSG00000101199 | ENSG00000177234 |
| ENSG00000260023 | ENSG00000165119 | ENSG00000161551 | ENSG00000176225 |
| ENSG00000144406 | ENSG00000143554 | ENSG00000106305 | ENSG00000100811 |
| ENSG00000164616 | ENSG00000176533 | ENSG00000188375 | ENSG00000170920 |
| ENSG00000157734 | ENSG00000186907 | ENSG00000178607 | ENSG00000168993 |
| ENSG00000165672 | ENSG00000101773 | ENSG00000188878 | ENSG00000266753 |
| ENSG00000167123 | ENSG00000161040 | ENSG00000162650 | ENSG00000255154 |
| ENSG00000127824 | ENSG00000095794 | ENSG00000112561 | ENSG00000172819 |
| ENSG00000100399 | ENSG00000163399 | ENSG00000161649 | ENSG00000111319 |
| ENSG00000165323 | ENSG00000182307 | ENSG00000105656 | ENSG00000163900 |
| ENSG00000166840 | ENSG00000122965 | ENSG00000065060 | ENSG00000178222 |
| ENSG00000107187 | ENSG00000102572 | ENSG00000176927 | ENSG00000105278 |
| ENSG00000113638 | ENSG00000187772 | ENSG00000126012 | ENSG00000135870 |
