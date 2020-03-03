/*
 * Oprogramowanie opracowane na potrzeby
 * Uniwersytetu Mlodych Wynalazcow
 *
 * Copyright (C) 2015, Konrad Iwanicki
 */


/**
 * Komponent dostarczajacy odczytow
 * temperatury.
 *
 * @author Konrad Iwanicki
 */
configuration TemperatureSensorC
{
    provides interface Read<uint16_t>;
}
implementation
{
#ifdef TOSSIM
    components new SineSensorC(1000.0, 6000.0) as SensorP;
    components MainC;
    MainC.SoftwareInit -> SensorP;
    Read = SensorP;
#else
    components new SensirionSht11C() as SensorP;
    Read = SensorP.Temperature;
#endif

}
