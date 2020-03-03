configuration RadioAppC {
}
implementation {
  components RadioAppP as AppP;
  components RadioComponentP as RadioP;
  components SensorComponentP as SensorP;

  // AppP
  components MainC;
  
  AppP.Boot -> MainC.Boot;
  AppP.SensorComponent -> SensorP;

  // SensorP
  components HumiditySensorC;
  components TemperatureSensorC;
  components new TimerMilliC() as TimerForSensingP;

  SensorP.TimerForSensing -> TimerForSensingP;
  SensorP.ReadTemp -> TemperatureSensorC;
  SensorP.ReadHumid -> HumiditySensorC;
  SensorP.RadioComponent -> RadioP;

  // RadioP
  components RandomC;
  components new TimerMilliC() as TimerForSendingP;
  components new AMSenderC(AM_TXT_SENSOR_READINGS) as SendReadingsC;
  components new AMReceiverC(AM_TXT_SENSOR_READINGS) as ReceiveReadingsC;

  RadioP.Random -> RandomC;
  RadioP.TimerForSending -> TimerForSendingP;
  RadioP.SendReadings -> SendReadingsC;
  RadioP.ReceiveReadings -> ReceiveReadingsC;

  // Others
  components PrintfC, SerialStartC, RadioStartC;
}
