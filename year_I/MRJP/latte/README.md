# Latte

Kompilujemy poleceniem `make`. Uruchamiamy zgodnie z treścią,
`./kompilator ścieżka_do_pliku`, w wyniku uzyskując plik .class bądź .bc.
Nie można przesuwać kompilatora do jvm do innego folderu, ze względu
na zależność od lib/latte_runtime.bc.
Kompilator do llvm potrzebuje do działania biblioteki standardowej C,
gdyż korzysta z printf.

## Rozszerzenia

- Tablice
- Obiekty z metodami wirtualnymi

## Opis rozwiązania

Wykonane jest w Haskellu.
Rozwiązanie składa się z warstw:

- Parser - napisany bezpośrednio w Haskellu. Anotuje on AST z danymi gdzie
w źródle umieszczony jest dany element AST
- Analiza poprawności - w tym miejscu dokonywane są też pewne uproszczenia programu, 
jak zwijanie stałych wyrażeń bądź if ze stałym warunkiem. Analiza poprawności
może łatwo zostać przerobiona na zwracanie wielu błędów na raz, na ten moment
zwraca tylko pierwszy napotkany. Anotuje drzewo typami, wraz z informacją
o symbolach i informacją czy dane poddrzewo musić wywołać return. W tym też
miejscu wyznaczam tablicę symboli, która zawiera kilka przydatnych
informacji. Także niektóre konstrukcje są dekompilowane do prostszych,
takie jak foreach czy if bez else.
- Generowanie LLVM - generowany jest w możliwie najprostszy sposób, przez co wynikowy
kod jest daleki od optymalnego. W tym miejscu następuje podstawianie metod, które
w istocie nie są wirtualne - oszczędza to odwołań do tablicy wirtualnej
- Optymalizacja - kilka podstawowych optymalizacji, jak eliminowanie wspólnych
podwyrażeń, bitcastów czy niepotrzebnych instrukcji phi.

Wypisywanie llvm nie implementowałem, pochodzi z biblioteki.

### Użyte biblioteki

Oprócz biblioteki standardowej, używam też

- mtl >= 2.2 && < 3
- containers >= 0.6 && < 1
- safe >= 0.3 && < 1
- llvm-hs-pure >= 7 && < 8
- megaparsec
- parser-combinators
- split
- bytestring >= 0.10 && < 1
- Unique >= 0.4 && < 1
- llvm-hs-pretty == 0.6.1.0
- text
- uniplate >= 1.6 && < 2

llvm-hs-pure było uzgodnione, zawiera typy LLVM w Haskellu. parser-combinators
wraz z megaparsec służą do parsowania. Natomiast uniplate to biblioteka do
metaprogramowania, która umożliwa wyrażenie w małej liczbie lini kodu
transformacji AST - do optymalizacji.

Opis zależności jest w package.yaml

### Struktura projektu

Wspólne źródła znajdują się w katalogu src, natomiast main'y
do kompilatorów w odzielnych plikach, bezpośrednio w katalogu app.
Jest tam tylko frontend oraz cały kompilator do LLVM.

#### ! Budowanie projektu korzysta z cabal'a, więc potrzebny jest internet
