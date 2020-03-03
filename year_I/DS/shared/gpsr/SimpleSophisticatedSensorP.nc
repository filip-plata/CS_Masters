/**
 * Simple (and dumb) implementation of the SophisticatedSensor component.
 */

enum {
  MAX_MOTES = 5,
};

module SimpleSophisticatedSensorP {
  provides interface Read<uint16_t> as ReadReading;
  provides interface ReadRecipient;
}
implementation {
  bool readingValue = FALSE;
  bool readingRecipient = FALSE;
  uint16_t readings = 0;

  task void readValue() {
    readingValue = FALSE;
    signal ReadReading.readDone(SUCCESS, readings++);
  }

  task void readRecipient() {
    uint16_t recipientId;

    readingRecipient = FALSE;

    recipientId = (TOS_NODE_ID + 1) % MAX_MOTES; // Send to mote (my_id + 1)
    // Let mote X be placed at [X, X]:
    signal ReadRecipient.readDone(SUCCESS, recipientId, recipientId, recipientId);
  }

  command error_t ReadReading.read() {
    if (readingValue)
      return EBUSY;

    readingValue = TRUE;
    post readValue();
    return SUCCESS;
  }

  command error_t ReadRecipient.read() {
    if (readingRecipient)
      return EBUSY;

    readingRecipient = TRUE;
    post readRecipient();
    return SUCCESS;
  }

}
