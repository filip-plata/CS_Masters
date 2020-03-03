/**
 * The Beacon configuration
 */
configuration BeaconC {}
implementation {
  components MainC;
  components RouterC;
  components ReadingsStoreC, BeaconP;
  components RandomC;

  components new TimerMilliC() as TimerBeaconP;
  components new AMSenderC(AM_BEACON_POSITION) as SendRadio;
  components new AMReceiverC(AM_BEACON_POSITION) as ReceiveRadio;

  BeaconP.Boot -> MainC;
  BeaconP.UpdateRouter -> RouterC;
  BeaconP.TimerBeacon -> TimerBeaconP;
  BeaconP.SendRadio -> SendRadio;
  BeaconP.ReceiveRadio -> ReceiveRadio;
  BeaconP.Random -> RandomC;
  BeaconP.ReadRouter -> RouterC;
}
