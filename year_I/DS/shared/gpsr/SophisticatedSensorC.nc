/**
 * The SophisticatedSensor (see Assignment 1).
 */
configuration SophisticatedSensorC {
  provides {
    interface Read<uint16_t> as ReadReading;
    interface ReadRecipient;
  }
}
implementation {
  components SimpleSophisticatedSensorP as SensorP;

  ReadReading = SensorP;
  ReadRecipient = SensorP;
}
