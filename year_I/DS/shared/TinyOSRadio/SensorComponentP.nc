#include <printf.h>

module SensorComponentP {
  provides {
    interface SensorComponent;
  }
  uses {
    interface Read<uint16_t> as ReadTemp;
    interface Read<uint16_t> as ReadHumid;
    interface Timer<TMilli> as TimerForSensing;
    interface RadioComponent;
  }
}
implementation {
  char humidSt, tempSt;
  uint16_t humidVal, tempVal;

  float calculateTemp(uint16_t vT) {
    return 0.01 * vT - 39.6;
  }

  float calculateHumid(uint16_t vH, float t) {
    return (-4.) + 0.0405 * vH - 0.0000028 * vH * vH + (t - 25.) * (0.01 + 0.00008 * vH);
  }

  void checkForSending() {
    float temp = calculateTemp(tempVal);
    float humid = calculateHumid(humidVal, temp);

    if (humidSt && tempSt)
      call RadioComponent.sendReadings(temp, humid);
  }

  command void SensorComponent.startSensing() {
    call TimerForSensing.startPeriodic(10000);
  }

  event void TimerForSensing.fired() {
    humidSt = tempSt = 0;
    call ReadTemp.read();
    call ReadHumid.read();
  }

  event void ReadTemp.readDone(error_t status, uint16_t val) {
    if (status != SUCCESS) {
      tempSt = 0;
    }
    else {
      tempVal = val;
      tempSt = 1;
      checkForSending();
    }
  }

  event void ReadHumid.readDone(error_t status, uint16_t val) {
    if (status != SUCCESS) {
      humidSt = 0;
    }
    else {
      humidVal = val;
      humidSt = 1;
      checkForSending();
    }
  }
}
