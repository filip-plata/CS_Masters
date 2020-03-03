#ifndef __UMW_RADIO_APP_H__
#define __UMW_RADIO_APP_H__

#include <AM.h>


typedef nx_struct txt_message
{
    nx_uint32_t  messageID;
    nx_uint8_t   senderID;
    nx_uint8_t   receiverID;
} txt_message_t;


enum
{
    AM_TXT_MESSAGE = 57,
};


#endif /* __UMW_RADIO_APP_H__ */
