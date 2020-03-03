#ifndef __UMW_RADIO_APP_H__
#define __UMW_RADIO_APP_H__

#include <AM.h>


typedef nx_struct sensor_readings
{
    nx_uint32_t   packetID;   /* keep a counter for readings */
    nx_uint16_t   data;
    nx_uint32_t   longitude;
    nx_uint32_t   latitude;
    nx_uint16_t   targetID;   /* final target of message */
    nx_uint16_t   originID;
    nx_uint32_t   p_latitude;
    nx_uint32_t   p_longitude;
    nx_uint64_t   f_dist_sq;
    nx_uint16_t   senderID;
    nx_uint16_t   e_0_senderID;
    nx_uint16_t   e_0_receiverID;
    nx_uint8_t    header;
    nx_uint8_t    dsn;
    nx_uint16_t   ttl;
} sensor_readings_t;


typedef nx_struct beacon_position
{
  nx_uint32_t   longitude;
  nx_uint32_t   latitude;
  nx_uint16_t   originID;
} beacon_position_t;


enum
{
    AM_SENSOR_READINGS = 57,
    AM_BEACON_POSITION = 58,
};

enum {
  GREEDY_FORWARDING = 0,
  PERIMETER_ROUTING = 1,
};


#endif /* __UMW_RADIO_APP_H__ */
