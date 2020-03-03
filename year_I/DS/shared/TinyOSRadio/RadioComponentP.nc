#include <printf.h>
#include <AM.h>
#include "Radio.h"

module RadioComponentP {
  provides {
    interface RadioComponent;
  }
  uses {
    interface AMSend as SendReadings;
    interface Receive as ReceiveReadings;
    interface Timer<TMilli> as TimerForSending;
    interface Random;
  }
}
implementation {
  bool busy = FALSE;
  uint32_t readingsID = 0;
  message_t readings;

  int roundToDecimal(float f) {
    return ((int)(f * 1000) % 1000);
  }

  command void RadioComponent.sendReadings(float temp, float humid) {
    txt_readings_t *txt;

    if (busy)
      return;

    busy = TRUE;
    txt = (txt_readings_t*) call SendReadings.getPayload(&readings,
                                                         sizeof(txt_readings_t));
    if (txt == NULL) {
      printf("Could not send readings!\r\n");
      printfflush();
      return;
    }

    txt->studentID = 371335;
    txt->deviceID = TOS_NODE_ID;
    txt->readingsID = readingsID++;
    txt->tempInt = (int) temp;
    txt->tempDec = roundToDecimal(temp);
    txt->humidInt = (int) humid;
    txt->humidDec = roundToDecimal(humid);

    call TimerForSending.startOneShot(call Random.rand32() % (5 * 1024));
  }

  event void TimerForSending.fired() {
    error_t res = call SendReadings.send(5, &readings, sizeof(txt_readings_t));

    if (res != SUCCESS)
      busy = FALSE;
  }

  event void SendReadings.sendDone(message_t *msg, error_t status) {
    busy = FALSE;
  }


  event message_t* ReceiveReadings.receive(message_t *msg,
                                           void *payload,
                                           uint8_t len) {
    txt_readings_t *txt;
    if (len != sizeof(txt_readings_t)) {
      return msg;
    }

    txt = (txt_readings_t*)payload;

    printf("Student ID: %llu, device ID: %d, reading ID: %d\r\n",
           txt->studentID, (int) txt->deviceID, (int) txt->readingsID);
    printf("Temperature: %d.%d, Humidity: %d.%d\r\n",
      txt->tempInt, txt->tempDec,
      txt->humidInt, txt->humidDec);
    printfflush();

    return msg;
  }

}
