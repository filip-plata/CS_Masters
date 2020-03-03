/**
 * The Location (see Assignment 1).
 */
configuration LocationC {
  provides interface ReadLocation;
}
implementation {
  components SimpleLocationP as LocationP;

  ReadLocation = LocationP;
}
