#include "contiki.h"
#include "dev/sht11/sht11-sensor.h"
#include "dev/light-sensor.h"

#include <stdio.h>
#include <math.h>

#include "sensor-button.h"

PROCESS(sensor_process, "Sensor_Process");

PROCESS_THREAD(sensor_process,ev,data)
{
      static int val;
      static float s = 0;
      static int dec;
      static float frac;

      PROCESS_BEGIN();

      while(1) {
         PROCESS_WAIT_EVENT_UNTIL(ev == trigger_event);
         printf("Started sensing\!");

    	   SENSORS_ACTIVATE(light_sensor);
         SENSORS_ACTIVATE(sht11_sensor);


         val = sht11_sensor.value(SHT11_SENSOR_TEMP);
      	 if(val != -1) {
		        s = ((0.01*val) - 39.60);
      	  	dec = s;
      	  	frac = s - dec;
      	  	printf("\nTemperature=%d.%02u C (%d)\n", dec, (unsigned int)(frac * 100),val);
         }

    	   val=sht11_sensor.value(SHT11_SENSOR_HUMIDITY);
    	   if(val != -1) {
		        s = (((0.0405*val) - 4) + ((-2.8 * 0.000001)*(pow(val,2))));
      	  	dec = s;
      	  	frac = s - dec;
      	  	printf("Humidity=%d.%02u %% (%d)\n", dec, (unsigned int)(frac * 100),val);
         }

         val = light_sensor.value(LIGHT_SENSOR_TOTAL_SOLAR);
    	   if(val != -1) {
    		    s = (float)(val * 0.4071);
    	  	  dec = s;
    	  	  frac = s - dec;
    	  	  printf("Light=%d.%02u lux (%d)\n", dec, (unsigned int)(frac * 100),val);
         }

    	   SENSORS_DEACTIVATE(light_sensor);
    	   SENSORS_DEACTIVATE(sht11_sensor);

      }

      PROCESS_END();

}
