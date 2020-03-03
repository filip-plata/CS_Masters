/*
 * Oprogramowanie opracowane na potrzeby
 * Uniwersytetu Mlodych Wynalazcow
 *
 * Copyright (C) 2015, Konrad Iwanicki
 */


/**
 * Komponent dostarczajacy odczytow
 * wilgotnosci.
 *
 * @author Konrad Iwanicki
 */
configuration HumiditySensorC
{
    provides interface Read<uint16_t>;
}
implementation
{
#ifdef TOSSIM
    components new SineSensorC(100.0, 1400.0) as SensorP;
    components MainC;
    MainC.SoftwareInit -> SensorP;
    Read = SensorP;
#else
    components new SensirionSht11C() as SensorP;
    Read = SensorP.Humidity;
#endif

}
