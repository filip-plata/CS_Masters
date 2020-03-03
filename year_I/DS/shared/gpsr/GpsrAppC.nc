configuration GpsrAppC {
}
implementation {
  components MainC;
  components GpsrAppP as AppP;
  components RadioComponentC;
  components RouterC;

  AppP.ReadRouter -> RouterC;

  AppP.Boot -> MainC.Boot;

  components new TimerMilliC() as TimerSensorP;
  AppP.TimerSensor -> TimerSensorP;

  components SophisticatedSensorC;
  AppP.ReadReading -> SophisticatedSensorC;
  AppP.ReadRecipient -> SophisticatedSensorC;

  components ReadingsStoreC;

  AppP.RadioComponent -> RadioComponentC;
  AppP.SaveReading -> ReadingsStoreC;

  components SerialStartC, RadioStartC, BeaconC;
}
