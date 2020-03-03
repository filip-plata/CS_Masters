#include "contiki.h"

#include "cfs/cfs.h"
#include "cfs/cfs-coffee.h"

#ifndef NEED_FORMATTING
#define NEED_FORMATTING 0
#endif

PROCESS_NAME(button_process);
PROCESS_NAME(radio_process);

PROCESS(startup_process, "S");
AUTOSTART_PROCESSES(&startup_process);


PROCESS_THREAD(startup_process, ev, data)
{
  PROCESS_BEGIN();

#if NEED_FORMATTING
  cfs_coffee_format();
#endif

  process_start(&button_process, NULL);
  process_start(&radio_process, NULL);

  PROCESS_END();
}
