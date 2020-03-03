/**
 * The RadioComponent configuration
 */
configuration RadioComponentC {
  provides interface RadioComponent;
}
implementation {
  components RouterC;
  components ReadingsStoreC, RadioComponentP;

  components new AMSenderC(AM_SENSOR_READINGS) as SendRadio;
  components new AMReceiverC(AM_SENSOR_READINGS) as ReceiveRadio;
  components new TimerMilliC() as TimerResend;

  RadioComponentP.UpdateRouter -> RouterC;
  RadioComponentP.ReadRouter -> RouterC;
  RadioComponentP.SaveReading -> ReadingsStoreC;
  RadioComponentP.SendRadio -> SendRadio;
  RadioComponentP.ReceiveRadio -> ReceiveRadio;
  RadioComponentP.Ack -> SendRadio.Acks;
  RadioComponentP.TimerResend -> TimerResend;

  RadioComponent = RadioComponentP;
}
