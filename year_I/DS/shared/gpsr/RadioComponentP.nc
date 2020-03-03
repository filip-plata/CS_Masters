#include <AM.h>
#include "Radio.h"

#define MOTE_MAX_ 1000

enum {
  QUEUE_SIZE = 32,
  RESEND_QUEUE_SIZE_THR = 24,
  RESEND_MAX_RETRY = 8,
  DSN_HISTORY_SIZE = 8,
  RETRY_MS = 150,
};

module RadioComponentP {
  provides interface RadioComponent;

  uses {
    interface AMSend as SendRadio;
    interface Receive as ReceiveRadio;
    interface PacketAcknowledgements as Ack;

    interface SaveReading;
    interface UpdateRouter;
    interface ReadRouter;
    interface Timer<TMilli> as TimerResend;
  }
}

implementation {
  const uint16_t MOTE_ID_LIMIT = MOTE_MAX_;
  bool busy = FALSE;

  struct dsn_history {
    uint8_t hist[DSN_HISTORY_SIZE];
    uint8_t start;
    uint8_t end;
  };
  static struct dsn_history MOTE_LAST_PACKET[MOTE_MAX_];

  uint8_t radio_queue_read = 0, radio_queue_write = 0;
  static uint32_t PACKET_NUMBER = 1;

  message_t msgs[QUEUE_SIZE];
  uint16_t targets[QUEUE_SIZE];
  uint8_t resend_ctr = 0, DSN;

  error_t put_radio_msg(message_t *, uint16_t);
  error_t radio_send();
  bool is_queue_empty();
  uint8_t queue_size();
  bool is_duplicate(sensor_readings_t *);


  command error_t RadioComponent.send_value( uint16_t value, uint16_t targetId, uint32_t latitude, uint32_t longitude) {
    sensor_readings_t *txt;
    message_t msg;
    error_t res;
    uint16_t next_mote;

    txt = (sensor_readings_t*) call SendRadio.getPayload(
      &msg, sizeof(sensor_readings_t));
    txt->latitude = latitude;
    txt->longitude = longitude;
    txt->header = GREEDY_FORWARDING;
    txt->e_0_senderID = MOTE_ID_LIMIT;
    txt->e_0_receiverID = MOTE_ID_LIMIT;

    res = call ReadRouter.next_hop(txt, &next_mote);
    if (res != SUCCESS)
      return res;

    txt->packetID = PACKET_NUMBER++;
    txt->data = value;
    txt->targetID = targetId;
    txt->originID = TOS_NODE_ID;
    txt->ttl = MOTE_ID_LIMIT;

    return put_radio_msg(&msg, next_mote);
  }

  event message_t* ReceiveRadio.receive(message_t *msg,
                                         void *payload,
                                         uint8_t len) {
    message_t new_msg;
    sensor_readings_t *txt;
    error_t res;
    uint16_t next_mote;

    if (len != sizeof(sensor_readings_t))
      return msg;

    txt = (sensor_readings_t*)payload;

    if (is_duplicate(txt))
      return msg;

    if (txt->targetID == TOS_NODE_ID) {
      call SaveReading.save(txt->originID, txt->data);
    } else {
      new_msg = *msg;

      txt = (sensor_readings_t*) call SendRadio.getPayload(
        &new_msg, sizeof(sensor_readings_t));

      if (!(--txt->ttl)) {
        dbg("DEBUG", "TTL reached zero\n");
        return msg;
      }
      res = call ReadRouter.next_hop(txt, &next_mote);

      if (res == SUCCESS) {
        dbg("DEBUG", "Forwarding message targetting %ld via %u\n", txt->targetID, next_mote);
        put_radio_msg(&new_msg, next_mote);
      } else {
        dbg("DEBUG", "Error when obtaining next hop. Dropping message\n");
      }
    }

    return msg;
  }

  event void TimerResend.fired() {
    radio_send();
  }

  event void SendRadio.sendDone(message_t *msg, error_t status) {
    bool is_ok = status == SUCCESS && call Ack.wasAcked(msg);

    if (!is_ok &&
        (queue_size() < RESEND_QUEUE_SIZE_THR &&
        resend_ctr < RESEND_MAX_RETRY)) {
      resend_ctr++;
      call TimerResend.startOneShot(RETRY_MS);
      return ;
    } else {
      if (!is_ok)
        dbg("DEBUG", "Dropping resend %d %d\n", queue_size(), resend_ctr);
    }

    /* Regardless, the queue needs to progress */
    resend_ctr = 0;
    radio_queue_read += 1;
    radio_queue_read %= QUEUE_SIZE;

    if (!is_queue_empty()) {
      radio_send();
    } else {
      busy = FALSE;
    }
  }

  error_t put_radio_msg(message_t *msg, uint16_t target) {
    sensor_readings_t *sen;

    if ((radio_queue_write + 1 - radio_queue_read) % QUEUE_SIZE == 0) {
      dbg("DEBUG", "No space in msg queue\n");
      return EBUSY;
    }

    call Ack.requestAck(msg);
    targets[radio_queue_write] = target;
    sen = (sensor_readings_t*) call SendRadio.getPayload(
      msg, sizeof(sensor_readings_t));
    sen->dsn = DSN++;
    msgs[radio_queue_write++] = *msg;
    radio_queue_write %= QUEUE_SIZE;

    if (!busy)
      return radio_send();

    return EBUSY;
  }

  error_t radio_send() {
    message_t *msg;

    busy = TRUE;
    msg = msgs + radio_queue_read;

    return call SendRadio.send(targets[radio_queue_read], msg, sizeof(sensor_readings_t));
  }

  bool is_queue_empty() {
    return radio_queue_read == radio_queue_write;
  }

  uint8_t queue_size() {
    return (radio_queue_write - radio_queue_read + QUEUE_SIZE) % QUEUE_SIZE;
  }

  bool is_duplicate(sensor_readings_t *msg) {
    uint8_t idx;
    if (msg->senderID >= 1000)
      return TRUE;

    idx = MOTE_LAST_PACKET[msg->senderID].start;

    while (idx != MOTE_LAST_PACKET[msg->senderID].end) {
      if (MOTE_LAST_PACKET[msg->senderID].hist[idx] == msg->dsn)
        return TRUE;
      idx++;
      idx %= DSN_HISTORY_SIZE;
    }

    MOTE_LAST_PACKET[msg->senderID].hist[MOTE_LAST_PACKET[msg->senderID].end] = msg->dsn;
    MOTE_LAST_PACKET[msg->senderID].end++;
    MOTE_LAST_PACKET[msg->senderID].end %= DSN_HISTORY_SIZE;

    if (MOTE_LAST_PACKET[msg->senderID].end == MOTE_LAST_PACKET[msg->senderID].start) {
      MOTE_LAST_PACKET[msg->senderID].start++;
      MOTE_LAST_PACKET[msg->senderID].start %= DSN_HISTORY_SIZE;
    }

    return FALSE;
  }
}
