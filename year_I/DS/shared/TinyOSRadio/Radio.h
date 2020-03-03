#ifndef __UMW_RADIO_APP_H__
#define __UMW_RADIO_APP_H__

#include <AM.h>


typedef nx_struct txt_readings
{
    nx_uint64_t   studentID;
    nx_uint16_t   deviceID;
    nx_uint32_t   readingsID;   /* keep a counter for readings */
    nx_int8_t     tempInt;
    nx_uint8_t    tempDec;
    nx_uint8_t    humidInt;
    nx_uint8_t    humidDec;
} txt_readings_t;


enum
{
    AM_TXT_SENSOR_READINGS = 57,
};


#endif /* __UMW_RADIO_APP_H__ */
