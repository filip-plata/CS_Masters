module GpsrAppP {
  uses {
    interface Boot;
    interface Timer<TMilli> as TimerSensor;

    interface Read<uint16_t> as ReadReading;
    interface ReadRecipient;

    interface ReadRouter;
    interface RadioComponent;

    interface SaveReading;
  }
}

implementation {
  /* Declare the struct with integer members x, y */
  struct target {
     uint16_t moteId;
     uint32_t longitude;
     uint32_t latitude;
  };

  typedef struct target target;

  const uint32_t SENSOR_PERIOD_MILI = 5000;
  uint8_t rounds = 0;
  int32_t value = -1;
  target tg, *recipient = NULL;
  bool recipient_in_progress = FALSE, value_in_progress = FALSE;

  void initiate_sensor_reads();

  event void Boot.booted() {
    initiate_sensor_reads();
    call TimerSensor.startPeriodic(SENSOR_PERIOD_MILI);
  }

  event void TimerSensor.fired() {
    rounds++;
    if (recipient_in_progress || value_in_progress)
      return;

    if (value != -1 && recipient != NULL && rounds == 1) {
      if (recipient->moteId == TOS_NODE_ID) {
        call SaveReading.save(TOS_NODE_ID, value);
        goto finalize;
      }

      call RadioComponent.send_value(
          value, recipient->moteId,
          recipient->latitude, recipient->longitude);
    }

finalize:
    initiate_sensor_reads();
  }

  event void ReadRecipient.readDone(error_t result, uint16_t moteId, uint32_t longitude, uint32_t latitude) {
    recipient_in_progress = FALSE;

    if (result !=  SUCCESS) {
      recipient = NULL;
    } else {
      recipient = &tg;
      recipient->moteId = moteId;
      recipient->longitude = longitude;
      recipient->latitude = latitude;
    }
  }

  event void ReadReading.readDone(error_t status, uint16_t val) {
    value_in_progress = FALSE;

    if (status != SUCCESS) {
      value = -1;
    }
    else {
      value = (int32_t) val;
    }
  }

  void initiate_sensor_reads() {
    error_t res;
    rounds = 0;
    value = -1;
    recipient = NULL;

    res = call ReadRecipient.read();
    recipient_in_progress = res == SUCCESS;
    if (res != SUCCESS)
      recipient = NULL;

    res = call ReadReading.read();
    value_in_progress = res == SUCCESS;
    if (res != SUCCESS)
      value = -1;
  }
}
