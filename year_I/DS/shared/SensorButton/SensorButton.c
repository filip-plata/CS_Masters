#include "contiki.h"

process_event_t trigger_event;

PROCESS_NAME(sensor_process);
PROCESS_NAME(button_process);

PROCESS(startup_process, "Startup");
AUTOSTART_PROCESSES(&startup_process);


PROCESS_THREAD(startup_process, ev, data)
{
  PROCESS_BEGIN();

  trigger_event = process_alloc_event();

  process_start(&sensor_process, NULL);
  process_start(&button_process, NULL);

  PROCESS_END();
}
