#include <AM.h>
#include "Radio.h"


module BeaconP {
  uses {
    interface Boot;
    interface Timer<TMilli> as TimerBeacon;

    interface AMSend as SendRadio;
    interface Receive as ReceiveRadio;

    interface UpdateRouter;
    interface ReadRouter;
    interface Random;
  }
}

implementation {
  const uint32_t BEACON_PERIOD_MILI = 1200;
  bool busy = FALSE;
  message_t message;

  void send_beacon();

  event void Boot.booted() {
    send_beacon();
    call TimerBeacon.startPeriodic(3 * BEACON_PERIOD_MILI / 4 + (call Random.rand32() % BEACON_PERIOD_MILI) / 10);
  }

  event void TimerBeacon.fired() {
    send_beacon();
  }

  event message_t* ReceiveRadio.receive(message_t *msg,
                                         void *payload,
                                         uint8_t len) {
    beacon_position_t *pos;
    if (len != sizeof(beacon_position_t))
      return msg;

    pos = (beacon_position_t*) payload;

    call UpdateRouter.update_beacon(pos->originID, pos->longitude, pos->latitude);
    return msg;
  }

  event void SendRadio.sendDone(message_t *msg, error_t status) {
    busy = FALSE;
  }

  void send_beacon() {
    error_t res;
    uint32_t latitude, longitude;
    beacon_position_t *pos;

    if (busy)
      return;

    res = call ReadRouter.self_cords(&latitude, &longitude);
    if (res != SUCCESS)
      return ;

    busy = TRUE;
    pos = (beacon_position_t*) call SendRadio.getPayload(
      &message, sizeof(beacon_position_t));

    pos->originID = TOS_NODE_ID;
    pos->latitude = latitude;
    pos->longitude = longitude;

    call SendRadio.send(AM_BROADCAST_ADDR, &message, sizeof(beacon_position_t));
  }
}
