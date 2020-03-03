#include "contiki.h"

PROCESS_NAME(stdin);
PROCESS_NAME(radio_process);

PROCESS(startup_process, "Startup");
AUTOSTART_PROCESSES(&startup_process);


PROCESS_THREAD(startup_process, ev, data)
{
  PROCESS_BEGIN();

  process_start(&stdin, NULL);
  process_start(&radio_process, NULL);

  PROCESS_END();
}
