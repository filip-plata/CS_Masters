#include <printf.h>

#include "Radio.h"

module SimulationAppP {
  uses {
    interface Boot;
    interface AMSend as SendMessage;
    interface Receive as ReceiveMessage;
    interface Timer<TMilli> as TimerForSending;
  }
}
implementation {
  uint8_t node, N = 3;
  bool busy = FALSE;
  uint32_t readingsID = 0;
  message_t message;

  event void Boot.booted()
  {
    dbg("Boot", "Application booted.\n");
    node = TOS_NODE_ID;
    call TimerForSending.startPeriodic( 3000 );
  }

  void sendMessage() {
    txt_message_t *txt;
    error_t res;

    if (busy)
      return;

    txt = (txt_message_t*) call SendMessage.getPayload(&message,
                                                         sizeof(txt_message_t));
    if (txt == NULL) {
      dbg("DEBUG", "Error when sending message\n");
      return;
    }

    txt->messageID = readingsID++;
    txt->receiverID = (node + 1) % N;
    txt->senderID = node;

    res = call SendMessage.send((node + 1) % N, &message, sizeof(txt_message_t));

    if (res != SUCCESS) {
      dbg("DEBUG", "%d\n", res);
    }
    else {
      busy = TRUE;
      dbg("AppS", "Sent message to %lu with number %llu.\n",
          txt->receiverID, txt->messageID);
    }
  }

  event void TimerForSending.fired() {
     dbg("DEBUG", "Fired timer\n");
     sendMessage();
  }

  event void SendMessage.sendDone(message_t *msg, error_t status) {
    dbg("DEBUG", "Send done with status: %d\n", status);
    busy = FALSE;
  }


  event message_t* ReceiveMessage.receive(message_t *msg,
                                           void *payload,
                                           uint8_t len) {
    txt_message_t *txt;

    if (len != sizeof(txt_message_t)) {
      dbg("DEBUG", "Message invalid length\n");
      return msg;
    }

    txt = (txt_message_t*)payload;

    if (txt->receiverID == node)
      dbg("AppR", "Received message from %lu with number %llu.\n",
          txt->senderID, txt->messageID);

    return msg;
  }

}
