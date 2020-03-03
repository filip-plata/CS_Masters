#ifndef SENSORBUTTON_H_
#define SENSORBUTTON_H_

#include "sys/process.h"

void read_channel_info();

PROCESS_NAME(sensor_process);
PROCESS_NAME(button_process);

#endif /* SENSORBUTTON_H_ */
