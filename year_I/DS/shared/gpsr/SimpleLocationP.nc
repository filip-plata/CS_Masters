/**
 * Simple (and dumb) implementation of the Location component.
 */
module SimpleLocationP {
  provides interface ReadLocation;
}
implementation {
  bool reading = FALSE;

  task void readLocation() {
    reading = FALSE;
    // Let mote X be placed at [X, X]:
    signal ReadLocation.readDone(SUCCESS, TOS_NODE_ID, TOS_NODE_ID);
  }

  command error_t ReadLocation.read() {
    if (reading)
      return EBUSY;

    reading = TRUE;
    post readLocation();
    return SUCCESS;
  }
}
