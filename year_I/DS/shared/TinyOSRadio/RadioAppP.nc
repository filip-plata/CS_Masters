module RadioAppP {
  uses {
    interface Boot;
    interface SensorComponent;
  }
}
implementation {
  event void Boot.booted() {
    call SensorComponent.startSensing();
  }
}
