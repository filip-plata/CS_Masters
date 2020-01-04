Rozwiązanie składa się z kilku plików. Jest cmake, kompilacja standardowo,
mkdir build; cd build; cmake ..; make; czy cokolwiek innego. Krótki opis plików:

crossld.c -> główna funkcja programu
elf_loader.c -> ładuje ELF do pamięci
crossld_helper.c -> wiele funkcji pomocniczych, przygotowyujących
                    potrzebne elementy rozwiązania, jak stos czy trampoliny,
		    a także sprzątanie

dynamic_load.c -> zajmuje się rozwiązaniem relokacji - podstawia pod
	       	  adresy PLT utworzone funkcje.
cross_call.c -> wsparcie dla dynamicznego wołania zdefiniowanych funkcji

asm.s -> szablony trampolin, wszystko co można wydzielić z kodu w C

Przejdę teraz przez rozwiązanie, z perspektywy crossld_start.

Robię walidację otrzymanych danych - sprawdzam unikalność nazw, i czy nie
ma tam funkcji exit. Nie ma tego w treści, ale moje rozwiązanie i tak nie
zadziała inaczej.

Do funkcji które trzeba utworzyć dodaję funkcję exit. Funkcja exit potrzebuje kilku
informacji - adres powrotu, rbp, które przekazuję poprzez wstawienie do szablonu
adresu zmiennych na stosie, które tuż przed skoczeniem do 32bit kodu wypełniam
poprawnymi wartościami np. ostatnia instrukcja to wyznaczenie adresu powrotu.
RBP również trzeba, bo wszystkie odniesienia do zmiennych są poprzez rbp.

Ładowanie
ELF wydaje się zupełnie standardowe, jest jedna nietypowa sztuczka,
żeby wiedzieć co posprzątać gdyby któreś z kolejnych ładowań się
nie powiodło. Nie jestem sprytny, oddzielnie łąduję dane z plików
i robię mmap. Nie ma wtedy oddzielnego przypadku (podobno jest to
mniej wydajne bo cos jest dwa razy w cache?).

Stos budowany jest chyba standardowo. Trampoliny tworzę jednym mmap, które
potem wypełniam wskaźnikami na adresy (struct function), któremu ta trampolina
ma odpowiadać.

Sama trampolina może być mniej typowa. Wejście do niej jest w asemblerze,
potem woła ona funkcję cross_call w C. Nie jestem biegły w asemblerze,
a to umożliwia mi wygodne zrobienie wszystkiego co chcę. Potem po
obliczeniu np. tablicy argumentów (stały rozmiar 8 bajtów), przechodzę
do ostatniej trampoliny w assemblerze. Ustawia ona argumenty, woła kod i zwraca.
Bałem się robić wstawki w kodzie funkcji w C (bez kolejnej funkcji w asm),
już zdążyłem zauważyć że łatwo o trudno wykrywalne błędy.

Sprawdzam chyba wszystkie możliwe kody błędów, również z mprotect (to się może
nie udać?)
