#include "Radio.h"

configuration SimulationAppC {
}
implementation {
  components SimulationAppP as AppP;

  components MainC;
  AppP.Boot -> MainC.Boot;

  components new TimerMilliC() as TimerForSending;
  AppP.TimerForSending -> TimerForSending;

  components new AMSenderC(AM_TXT_MESSAGE) as SendMessage;
  components new AMReceiverC(AM_TXT_MESSAGE) as ReceiveMessage;


  AppP.SendMessage -> SendMessage;
  AppP.ReceiveMessage -> ReceiveMessage;

  //components PrintfC, SerialStartC, RadioStartC;
  components SerialStartC, RadioStartC;
}
