# instant

Kompilujemy poleceniem `make`. Uruchamiamy zgodnie z treścią,
`./kompilator ścieżka_do_pliku`, w wyniku uzyskując plik .class bądź .bc.
Nie można przesuwać kompilatora do jvm do innego folderu, ze względu
na zależność od lib/jasmin.jar.
Kompilator do llvm potrzebuje do działania biblioteki standardowej C,
gdyż korzysta z printf.

Aby uruchomić program w javie, należy dodać kataolg w którym się znajduje
do classpath.

## Opis rozwiązania

Wykonane jest w Haskellu. Domyślnie ograniczam liczby do 32 bitów ze znakiem.
Plik z gramatyką został wykorzystany do wygenerowania
parsera; wygenerowane pliki włączyłem bezpośrednio do źródeł.
Samo rozwiązanie składa się z taki samych
koncepcyjnie warstw dla obu kompilatorów:

- Analiza poprawności - czy używane są niezadeklarowane zmienne - wtedy zwracam błąd
- Wyznaczenie statystyk pomocniczych w drzewie składni
- Kompilacja do listy abstrakcyjnych instrukcji - tutaj LLVM i JVM się rozwidlają, ze względu na SSA w LLVM
- Na podstawie listy, wyemitowanie stosownego tekstu asemblerów

Oprócz kompilatorów, można też zbudować interpreter, za pomocą
`make insc_interpreter`

### Użyte biblioteki

Oprócz biblioteki standardowej, używam też

- array
- containers - Map, Set
- mtl - monada stan
- filepath - operacje na ścieżkach do plików
- directory - getCurrentDirectory
- process - uruchamianie podprocesów (jasmin.jar)

Opis zależności jest w package.yaml

### Struktura projektu

Wspólne źródła znajdują się w katalogu src, natomiast main'y
do kompilatorów w odzielnych plikach, bezpośrednio w katalogu app.

#### ! Budowanie projektu korzysta z cabal'a, więc potrzebny jest internet
