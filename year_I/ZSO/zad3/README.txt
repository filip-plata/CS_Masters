Rozwiązanie opiera się na pliku kernel/lense.c. Zawarte
w nim funkcje wołane są przy sprawdzaniu danych. Teraz wiem,
że powinno być lens, ale tak już zostanie.

Użyte słowa przechowywane są jako maska 64 bitowa. Inicjalizowana
jest ona na zero podczas kopiowania procesu, oraz w inicie.

W najprostszym przypadku, sprawdzamy podany adres wirtualny dla obecnego
task struct.
Podczas zwalniania stron sytuacja jest bardziej skomplikowana. Jesteśmy
wtedy w dobrym kontekście, ale stronę użytkownika należy zamontować do
pamięci jądra.

Najmniej przyjemny jest kswapd, czyli demon swap. Podczas wyrzucania strony
na dysk trzeba wyciągnąć informację poprzez rmap jakie procesy używają tej
strony a następnie wykonać sprawdzenie dla każdego z nich. Wykrywanie słów
mogłoby być tu wydzielone, ale wybrałem prostotę.

Do parsowania argumentów jądra używam zmodyfikowanego makra z moduleparam.
Makro to nie zgłosi błędu, jeśli podamy więcej słów niż 64, całość będzie
działać jakby zostało podane tylko pierwsze 64. Sama długość słów też nie ma
znaczenia dla tego rozwiązania - w lense.c dostaję tylko wskaźniki do napisów,
które w jądrze już są.

Wypisywanie z konieczności (limit do 1024 znaków) odbywa się poprzez
kilka printk. Używam KERN_CONT, mimo iż oczywiście to pusty napis i 
gwarancji na ciągłość logów nie ma. Bufor do wypisywania jest wspólny
statyczny ze spinlockiem - powinno się to zdarzać rzadko, na stosie
nie wypada alokować tyle bajtów, a kmalloc może się nie udać co zaburzyłoby
typ void głównych funkcji.

Z moich testów wynika, że najlepiej explicite tworzyć nowy proces, np.
/bin/echo test &
gdyż argumenty do jądra wydają się siedzieć gdzieś w pamięci shella pierwszego
zalogowanego użytkownika (albo wpiąłem się jeszcze za wcześnie do exit).
