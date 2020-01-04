Układ sterownika wzorowałem na sterowniku noveau.

Nietypowym rozwiązaniem może być użycie ifdefów w kodzie. Do
zdefiniowania bądź nie jest jedna opcje, należy użyć domyślnego makefila:

- USE_CMD_BUF - przekazuje polecenia do urządzenia za pomocą bufora poleceń,
  jeśli nie jest zdefiniowane sterownik używa MMIO
Miałem też konfigurowalną synchroniczność/asynchroniczność, ale ostatecznie
jest to większy kłopot niż pomoc.

Układ rozwiązania, pliki "orbitalne":
- podstawowe funkcje sterownika w doom_base.c, w tym inicjalizacja
  i zamykanie modułu. Zgdonie z treścią, jest globalna tablica 256 urządzeń.
  Strukty opisujące urządzenie są alokowane dynamicznie, co ogranicza
  zużycie pamięci.
- rejestracja w systemach linuxa - pci, cdev - w doom_reg.c. Wydzielona,
  gdyż jest nudnym kodem.
- doom_device zawiera definicję urządzenia, jak i podstawowe operacje na nim
- doom_context definiuje kontekst - obiekt, który sterownik przydziela użytkownikowi
  podczas otwierania urządzenia. Zawiera on przede wszystkim wskaźniki na używane bufory
- doom_abi.c wraz z harddoom_interface.h to interfejs urządzenia widzany przez resztę
  sterownika. Co ważne, funkcje związane z interruptami zostały wydzielone do doom_irqs.c.
- doom_irqs.c jest ciekawy, gdyż funkcje do obsługi interruptów przekazywane są dynamicznie.
- doom_irq_handlers zawiera niektóre podstawowe handlery interruptów. Reszta handlerów jest
  w plikach które w pewien sposób korzystają z funckjonalności interruptu, jak np.
  doom_command_wait.c
- Wspomniany doom_command_wait.c odpowiada czekaniu na miejsce w kolejce poleceń.
  Tam też zdefiniowany jest np. handler interruptu pong_async. Opiera się on
  na pamiętaniu, ile miejsca w kolejce zostało zarezerwowane oraz na sprawdzaniu,
  ile miejsca faktycznie jest w kolejce poprzez odpytanie urządzenia.
  Dzięki temu moduł jest zamknięty sam w sobie i nic nie wie o innych zamkach, gdyż
  wolne miejsce na urządzeniu może tylko rosnąć - chyba, że ktoś z przydzielonym
  miejscem zacznie go używać.
- doom_fence.c natomiast posiada obsługę oczekiwania na wykonanie batcha poleceń
  poprzez fence_counter/fence_wait, razem z handlerem. Opiera się na oczekiwaniu
  przepisanym z treści. Po przejrzeniu kodu nouveau widzę, że można bardziej
  skomplikowane rzeczy robić, ale do odpalenia dooma wystarcza. Zliczam tam
  overflowy, gdyż przeciwnie jeśli ktoś będzie długo zwlekał z readem na buforze,
  możemy nie wiedzieć na co chcemy czekać.
- doom_debug.h oraz doom_statistics.h to puste pliki. Gdyby to był poważny sterownik, w
  nich byłyby umieszczone odpowiednio eksportowanie danych debugowych i statystyk do sysfs
- debug.h zawiera makro do debugu. Skopiowane ze scull.h
- doom_validate.c zawiera wszelka logikę walidującą wejście od użytkownika.
- doom_parse_cmd.c to logika parsowaniu komend użytkownika na komendy urządzenia.
  Reszta kodu powinna manipulować tylko flagami ping_sync, ping_async, interlock i fence.
  Są one wystawione jako pola struktury cmd_format_all z doom_parse_cmd.h
- doom_batch.c posiada logikę związaną z batchem poleceń. To tu spinane walidacja
  i parsowanie poleceń. Po zdobyciu urządzenia na wyłączność powinna być
  wołana tylko metoda zwracająca widok batcha (czyli lekki strukt).
  W ten sposób można wrzucić tam logikę potrzebną tuż przed wysłaniem.
- doom_dma.c zawiera wszystko o i z buforami. Reszta kodu nie zna definicji
  struktury doom_raw_buf, posiada tylko wskaźniki na nią. Wszystkie bufory z
  zadania to cienkie opakowania różnych typów na doom_raw_buf (bufor poleceń
  tylko mentalnie, gdyż nic by to nie wniosło). Ciekawy wydaje mi się kod
  do przetwarzania pofragmentowanego buforu - poprzez funkcję generic_process_range,
  która z funkcji operującej na spójnym fragmencie pamięci tworzy funkcję
  operującą na zfragmentowanym buforze.


Istotą rozwiązania jest doom_engine.c. W nim znajdują się metody których może
używać użytkownik jak ioctl czy write na urządzeniu.
Nietypowa w doom_engine.c może wydawać się nadalokacja miejsca w kolejce. Otóż
do momentu uzyskania zamka na urządzeniu, nie wiadomo czy trzeba zmienić setup
czy nie. MAX_BATCH_SIZE uwzględnia to, a kod czekający na polecenia musi sobie
poradzić z posiadaniem niedokładnej informacji na temat wolnego miejsca.
Takie rozwiązanie wydawało się lepsze - batch użytkownika zawsze wykona się
w całości jeśli nie jest większy niż przyjęty limit.

Sterownik wspiera siłowe usunięcie urządzenia - pozostawia wtedy strukturę
doom_device, aż wszyscy użytkownicy ją zamkną. Stara się to zrobić bezpiecznie,
na moich testach się udawało wyjść czysto przy hot-unplugu. Rzucam wyzwanie,
by spróbować coś popsuć z userspace w sterowniku - pewnie doświadczone oko
da radę, ale może nauczę się o czym jeszcze myśleć przy rozważaniu co może
pójść źle.
