/**
 * The ReadingsStore (see Assignment 1).
 */
configuration ReadingsStoreC {
  provides interface SaveReading;
}
implementation {
  components SimpleReadingsStoreP as StoreP;

  SaveReading = StoreP;
}
