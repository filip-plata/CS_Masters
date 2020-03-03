configuration RouterC {
  provides interface ReadRouter;
  provides interface UpdateRouter;
}
implementation {
  components MainC, LocationC, RouterP;
  components new TimerMilliC() as TimerLocationReadP;

  RouterP.Boot -> MainC.Boot;
  RouterP.ReadLocation -> LocationC;
  RouterP.TimerLocationRead -> TimerLocationReadP;

  ReadRouter = RouterP;
  UpdateRouter = RouterP;
}
