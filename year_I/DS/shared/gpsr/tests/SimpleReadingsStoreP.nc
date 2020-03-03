/**
 * Simple (and dumb) implementation of the ReadingsStore component.
 */
module SimpleReadingsStoreP {
  provides interface SaveReading;
}
implementation {
  command error_t SaveReading.save(uint16_t moteId, uint16_t value) {
    dbg("SimpleReadingsStore", "Saving %u from %u\n", value, moteId);
    return SUCCESS;
  }
}
