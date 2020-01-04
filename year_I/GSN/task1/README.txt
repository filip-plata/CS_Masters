Całość jest w jednym skrypcie image_net.py.

Aby uzyskać ogląd rozwiązania, należy uruchomić skrypt z flagą
'-h', która wypisze pomoc. Skrypt wykonuje sekwencyjnie kroki
sterowane konfiguracją.

Sposób budowy sieci pytorchowej może wydawać się skomplikowany
(w implementacji). Jednak jest to prosty kod, który konsumuje
listę opisów wyjścia następnej warstwy. Opisu wejścia nie potrzeba,
gdyż można go policzyć.

Do okluzji używam koloru białego, tła. Wtedy,jeśli sieć poprawnie przewiduje
rezultat, można zauważyć, czego zakrycie zmusi ją do błędu.
Okluzja działa długo, ale to chyba konieczność, gdyż robi 100 * 100 predykcji.

Gradienty normalizuję. Wydawało mi się że wtedy obrazki są gładsze. 
Przykładowe wizualizacje załączyłem - polecam find.

Całość osiąga 97,3%. Nie jest to imponujące, podczas eksperymentów
łatwo osiagałem wyniki rzedu 99,5%, ale postanowiłem zmniejszyć rozmiar
sieci, żeby łatwiej uruchamiać na swojej maszynie.

Wytrenowanej sieci nie wysyłam, bo zajmuje za dużo miejsca (>2MB)
