#include <printf.h>


configuration SensorAppC {
}
implementation {
  components SensorAppP as AppP;

  components MainC;
  AppP.Boot -> MainC.Boot;

  components new TimerMilliC() as TimerForSensingP;
  AppP.TimerForSensing -> TimerForSensingP;

  components TemperatureSensorC;
  AppP.ReadTemp -> TemperatureSensorC;

  components HumiditySensorC;
  AppP.ReadHumid -> HumiditySensorC;

  components PrintfC, SerialStartC;
}
