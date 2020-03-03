#ifndef SENSORBUTTON_H_
#define SENSORBUTTON_H_

#include "sys/process.h"

extern process_event_t trigger_event;

PROCESS_NAME(sensor_process);
PROCESS_NAME(button_process);

#endif /* SENSORBUTTON_H_ */
